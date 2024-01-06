//! The Surplus compiler is an [OXC](https://github.com/osx-project)-based
//! transformer that compiles JSX to Surplus runtime calls.
//!
//! Further, the crate provides a `surplus-transform` utility for
//! simple and one-off transformations, which takes standard input and
//! writes to standard output.
//!
//! For more information check https://github.com/surplus.
#![deny(missing_docs)]

use oxc::{
    allocator as alo,
    ast::{
        ast::{self, Argument::Expression as ArgExpr},
        AstBuilder, CommentKind, VisitMut,
    },
    diagnostics::Report,
    parser::ParserReturn,
    span::{Atom, GetSpan, SourceType, Span},
    syntax::reference::ReferenceFlag,
};

/// Provides information for failed parses,
/// otherwise handing back the transformed source code.
#[derive(Debug)]
pub struct TransformResult {
    /// The transformed source code.
    /// `None` if the parse failed.
    pub source: Option<String>,
    /// Any parse errors.
    pub errors: Vec<Report>,
    /// Any trivias.    
    pub trivias: Vec<(u32, u32, CommentKind)>,
    /// Whether or not the parse panicked.
    pub panicked: bool,
}

impl From<ParserReturn<'_>> for TransformResult {
    fn from(result: ParserReturn<'_>) -> Self {
        let source = if result.panicked {
            None
        } else {
            Some(
                oxc_codegen::Codegen::<false>::new(
                    (result.program.span.end * 2).try_into().unwrap_or(4096),
                    Default::default(),
                )
                .build(&result.program),
            )
        };

        Self {
            source,
            errors: result.errors,
            trivias: result.trivias,
            panicked: result.panicked,
        }
    }
}

struct SurplusVisitorMut<'a> {
    options: TransformOptions,
    a: AstBuilder<'a>,
}

/// Trims all whitespace from a string, with a few rules.
/// Must return a `String` instead of a sub-slice since
/// a single whitespace character is synthesized in some
/// cases.
///
/// 1. For the first element (index 0), trim leading whitespace.
/// 2. For the last element (index `total_elements - 1`), trim trailing whitespace.
/// 3. For any remaining case (leading/trailing whitespace for each element),
///    if there is at least one whitespace character (including newlines),
///    first trim all whitespace, then add a single space.
/// 4. If the trimmed string is empty, return at most a single whitespace.
fn trim_whitespace(s: &str, index: usize, total_elements: usize) -> String {
    let trimmed = s.trim();

    if trimmed.is_empty() && !s.is_empty() {
        return " ".to_string();
    }

    let mut start = "";
    let mut end = "";

    if index != 0 && s.chars().next().unwrap_or_default().is_whitespace() {
        start = " ";
    }

    if (index != total_elements - 1) && s.chars().last().unwrap_or_default().is_whitespace() {
        end = " ";
    }

    format!("{}{}{}", start, trimmed, end)
}

impl<'a> SurplusVisitorMut<'a> {
    fn new(allocator: &'a oxc::allocator::Allocator, options: TransformOptions) -> Self {
        Self {
            options,
            a: AstBuilder::new(allocator),
        }
    }

    fn ident(&self, span: Span, id: &str) -> ast::Expression<'a> {
        self.a
            .identifier_reference_expression(ast::IdentifierReference {
                span,
                name: self.a.new_str(id).into(),
                reference_id: Default::default(),
                reference_flag: ReferenceFlag::read(),
            })
    }

    fn ident_name(&self, span: Span, id: &str) -> ast::IdentifierName {
        ast::IdentifierName {
            span,
            name: self.a.new_str(id).into(),
        }
    }

    fn s_member(&self, span: Span, id: &str) -> ast::Expression<'a> {
        self.a.static_member_expression(
            span,
            self.ident(span, &self.options.global),
            self.ident_name(span, id),
            false,
        )
    }

    fn call_s<A: Iterator<Item = ast::Argument<'a>>>(
        &self,
        span: Span,
        id: &str,
        args: A,
    ) -> ast::Expression<'a> {
        self.a.call_expression(
            span,
            self.s_member(span, id),
            alo::Vec::from_iter_in(args, self.a.allocator),
            false,
            None,
        )
    }

    fn create_text_node(
        &self,
        span: Span,
        expr: Option<ast::Expression<'a>>,
    ) -> ast::Expression<'a> {
        self.call_s(
            span,
            "tn",
            expr.map(|expr| self.a.new_vec_single(ast::Argument::Expression(expr)))
                .unwrap_or_else(|| self.a.new_vec())
                .into_iter(),
        )
    }

    fn create_text_node_literal(&self, span: Span, value: Atom) -> ast::Expression<'a> {
        self.create_text_node(
            span,
            Some(
                self.a
                    .literal_string_expression(ast::StringLiteral { span, value }),
            ),
        )
    }

    fn transform_jsx_member_expression(
        &self,
        expr: &ast::JSXMemberExpression<'a>,
    ) -> ast::Expression<'a> {
        let object_expr = match &expr.object {
            ast::JSXMemberExpressionObject::Identifier(ident) => {
                self.ident(ident.span, ident.name.as_str())
            }
            ast::JSXMemberExpressionObject::MemberExpression(expr) => {
                self.transform_jsx_member_expression(expr)
            }
        };

        self.a.static_member_expression(
            expr.span,
            object_expr,
            self.ident_name(expr.property.span, expr.property.name.as_str()),
            false,
        )
    }

    fn transform_elem(&mut self, elem: &mut ast::JSXElement<'a>) -> ast::Expression<'a> {
        let children = self.take_ownership(&mut elem.children);
        let mut new_children = self.a.new_vec_with_capacity(children.len());

        let total_children = children.len();
        for (i, child) in children.into_iter().enumerate() {
            if let Some(child) = self.transform_jsx_child(child, i, total_children) {
                new_children.push(child);
            }
        }

        let (callee, simple_tag, ns) = match &elem.opening_element.name {
            ast::JSXElementName::Identifier(id) => (
                self.ident(id.span, id.name.as_str()),
                Some(id.name.as_str()),
                None,
            ),
            ast::JSXElementName::MemberExpression(expr) => {
                let result = self.transform_jsx_member_expression(expr);
                (result, None, None)
            }
            ast::JSXElementName::NamespacedName(namespaced_name) => {
                let property = namespaced_name.property.name.as_str();

                (
                    self.ident(namespaced_name.property.span, property),
                    Some(property),
                    Some(self.a.literal_string_expression(ast::StringLiteral {
                        span: namespaced_name.namespace.span,
                        value: namespaced_name.namespace.name.as_str().into(),
                    })),
                )
            }
        };

        let is_builtin_tag = simple_tag
            .and_then(|t| t.chars().next())
            .map(|c| c == '-' || c.is_lowercase())
            .unwrap_or(false);

        // Kind of a hack, not sure how to do this better, but it has to be
        // done here as opposed to down below since we take a mutable reference
        // to the opening element directly after.
        //
        // If you can come up with a cleaner way to do this, please open an
        // issue or PR!
        let tag_string = if is_builtin_tag {
            Some(self.a.literal_string_expression(ast::StringLiteral {
                span: elem.opening_element.name.span(),
                value: simple_tag.unwrap().into(),
            }))
        } else {
            None
        };

        let attributes = self.take_ownership(&mut elem.opening_element.attributes);

        let mut props = self.a.new_vec_with_capacity(attributes.len() + 1);
        let mut fns = self.a.new_vec();

        if !new_children.is_empty() {
            props.push(ast::ObjectPropertyKind::ObjectProperty(
                self.a.object_property(
                    elem.span,
                    ast::PropertyKind::Init,
                    ast::PropertyKey::Identifier(
                        self.a.alloc(self.ident_name(elem.span, "children")),
                    ),
                    self.a.array_expression(elem.span, new_children, None),
                    None,
                    false,
                    false,
                    false,
                ),
            ));
        }

        for attr in attributes.into_iter() {
            let span = attr.span();

            match attr {
                ast::JSXAttributeItem::Attribute(attr) => {
                    let mut attr = attr.unbox();

                    #[derive(PartialEq, Eq)]
                    enum AttrTarget {
                        Prop,
                        Event,
                    }

                    let (target, target_type) = match &attr.name {
                        ast::JSXAttributeName::Identifier(ident) => {
                            (ident.name.clone(), AttrTarget::Prop)
                        }
                        ast::JSXAttributeName::NamespacedName(namespaced_name) => {
                            let namespace = namespaced_name.namespace.name.clone();
                            let property = namespaced_name.property.name.clone();

                            if namespace == "on" {
                                (property, AttrTarget::Event)
                            } else if is_builtin_tag {
                                (
                                    self.a
                                        .new_str(&format!("{}:{}", namespace, property))
                                        .into(),
                                    AttrTarget::Prop,
                                )
                            } else {
                                (property, AttrTarget::Prop)
                            }
                        }
                    };

                    let value = match attr.value {
                        None => {
                            if target_type == AttrTarget::Event {
                                None
                            } else {
                                Some(self.a.literal_boolean_expression(ast::BooleanLiteral {
                                    span,
                                    value: true,
                                }))
                            }
                        }
                        Some(ast::JSXAttributeValue::StringLiteral(string)) => {
                            if target_type == AttrTarget::Event {
                                None
                            } else {
                                Some(self.a.literal_string_expression(string.clone()))
                            }
                        }
                        Some(ast::JSXAttributeValue::ExpressionContainer(container)) => {
                            match container.expression {
                                ast::JSXExpression::EmptyExpression(_) => None,
                                ast::JSXExpression::Expression(expr) => Some(expr),
                            }
                        }
                        Some(ast::JSXAttributeValue::Fragment(mut fragment)) => {
                            if target_type == AttrTarget::Event {
                                None
                            } else {
                                Some(self.transform_fragment(fragment.span, &mut fragment.children))
                            }
                        }
                        Some(ast::JSXAttributeValue::Element(ref mut element)) => {
                            if target_type == AttrTarget::Event {
                                None
                            } else {
                                Some(self.transform_elem(element))
                            }
                        }
                    };

                    if let Some(value) = value {
                        let target = if target_type == AttrTarget::Event {
                            format!("on:{target}").into()
                        } else {
                            target
                        };

                        // Special handling for the `fn` prop; they're added
                        // to a separate array and passed as the last argument
                        // to the `el` function.
                        if target_type == AttrTarget::Prop && target == "fn" {
                            fns.push(ast::ArrayExpressionElement::Expression(value));
                        } else {
                            let prop_def = self.a.object_property(
                                span,
                                ast::PropertyKind::Init,
                                ast::PropertyKey::Expression(self.a.literal_string_expression(
                                    ast::StringLiteral {
                                        span: attr.name.span(),
                                        value: target,
                                    },
                                )),
                                self.fn_expr(value.span(), value),
                                None,
                                false,
                                false,
                                false,
                            );

                            props.push(ast::ObjectPropertyKind::ObjectProperty(prop_def));
                        }
                    }
                }
                ast::JSXAttributeItem::SpreadAttribute(spread) => {
                    let spread = spread.unbox();
                    props.push(ast::ObjectPropertyKind::SpreadProperty(
                        self.a.spread_element(spread.span, spread.argument),
                    ));
                }
            }
        }

        let fns = if fns.is_empty() {
            None
        } else {
            Some(ArgExpr(self.fn_expr(
                elem.span,
                self.a.array_expression(elem.span, fns, None),
            )))
        };

        let object = self
            .a
            .object_expression(elem.opening_element.span, props, None);

        match (is_builtin_tag, ns) {
            (true, None) => {
                let mut args = vec![
                    ArgExpr(tag_string.unwrap()),
                    ArgExpr(self.fn_expr(object.span(), object)),
                ];
                if let Some(fns) = fns {
                    args.push(fns);
                }
                self.call_s(elem.span, "el", args.into_iter())
            }
            (true, Some(ns)) => {
                let mut args = vec![
                    ArgExpr(ns),
                    ArgExpr(tag_string.unwrap()),
                    ArgExpr(self.fn_expr(object.span(), object)),
                ];
                if let Some(fns) = fns {
                    args.push(fns);
                }
                self.call_s(elem.span, "ns", args.into_iter())
            }
            (false, _) => {
                let mut args = self.a.new_vec_single(ArgExpr(object));

                if let Some(fns) = fns {
                    args.push(fns);
                }

                // Note that we don't support namespaced custom elements.
                let call = self.fn_expr(
                    elem.span,
                    self.a.call_expression(elem.span, callee, args, false, None),
                );
                self.call_s(elem.span, "ct", [ArgExpr(call)].into_iter())
            }
        }
    }

    fn transform_jsx_expression(
        &mut self,
        expr: ast::JSXExpression<'a>,
    ) -> Option<ast::Expression<'a>> {
        let span = expr.span();
        let expr = match expr {
            ast::JSXExpression::EmptyExpression(_) => return None,
            ast::JSXExpression::Expression(expr) => self.fn_expr(expr.span(), expr),
        };

        Some(self.call_s(span, "ex", [ArgExpr(expr)].into_iter()))
    }

    fn transform_jsx_child(
        &mut self,
        child: ast::JSXChild<'a>,
        child_idx: usize,
        total_children: usize,
    ) -> Option<ast::ArrayExpressionElement<'a>> {
        match child {
            ast::JSXChild::Element(mut el) => Some(ast::ArrayExpressionElement::Expression(
                self.transform_elem(&mut el),
            )),
            ast::JSXChild::ExpressionContainer(expr) => self
                .transform_jsx_expression(expr.expression)
                .map(ast::ArrayExpressionElement::Expression),
            ast::JSXChild::Text(text) => {
                let trimmed = trim_whitespace(text.value.as_str(), child_idx, total_children);

                if trimmed.is_empty() {
                    None
                } else {
                    Some(ast::ArrayExpressionElement::Expression(
                        self.create_text_node_literal(text.span, trimmed.into()),
                    ))
                }
            }
            ast::JSXChild::Spread(spread) => Some(ast::ArrayExpressionElement::SpreadElement(
                self.a.spread_element(spread.span, spread.expression),
            )),
            ast::JSXChild::Fragment(mut fragment) => Some(ast::ArrayExpressionElement::Expression(
                self.transform_fragment(fragment.span, &mut fragment.children),
            )),
        }
    }

    fn fn_expr(&self, span: Span, expr: ast::Expression<'a>) -> ast::Expression<'a> {
        self.a.arrow_expression(
            span,
            true,
            false,
            false,
            self.a.formal_parameters(
                span,
                ast::FormalParameterKind::ArrowFormalParameters,
                self.a.new_vec(),
                None,
            ),
            self.a.function_body(
                span,
                self.a.new_vec(),
                self.a
                    .new_vec_single(self.a.expression_statement(span, expr)),
            ),
            None,
            None,
        )
    }

    fn transform_fragment(
        &mut self,
        span: Span,
        children: &mut alo::Vec<'a, ast::JSXChild<'a>>,
    ) -> ast::Expression<'a> {
        let children = self.take_ownership(children);
        let mut new_children = self.a.new_vec_with_capacity(children.len());

        let total_children = children.len();
        for (i, child) in children.into_iter().enumerate() {
            if let Some(child) = self.transform_jsx_child(child, i, total_children) {
                new_children.push(child);
            }
        }

        self.a.array_expression(span, new_children, None)
    }

    /// Workaround for the allocator headache in oxc.
    fn take_ownership<T>(&self, vec: &mut alo::Vec<'a, T>) -> alo::Vec<'a, T> {
        alo::Vec::from_iter_in(vec.splice(.., std::iter::empty()), self.a.allocator)
    }
}

impl<'a> VisitMut<'a> for SurplusVisitorMut<'a> {
    fn visit_expression(&mut self, expr: &mut ast::Expression<'a>) {
        match expr {
            ast::Expression::JSXFragment(frag) => {
                *expr = self.transform_fragment(frag.span, &mut frag.children);
            }
            ast::Expression::JSXElement(el) => {
                *expr = self.transform_elem(el);
            }
            _ => {}
        }

        self.visit_expression_match(expr);
    }
}

/// Options to modify the behavior of the transformer.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct TransformOptions {
    /// The name of the global variable to use for Surplus.
    pub global: String,
    /// Emit the Surplus runtime import at the beginning of
    /// the transformed source code.
    pub emit_runtime_import: bool,
}

impl Default for TransformOptions {
    fn default() -> Self {
        Self {
            global: String::from("$$S"),
            emit_runtime_import: true,
        }
    }
}

/// Transform JSX to Surplus.
///
/// # Modifies AST
/// This function **modifies** the AST **in place**.
pub fn transform_program<'a>(
    allocator: &'a oxc::allocator::Allocator,
    program: &mut ast::Program<'a>,
    options: TransformOptions,
) {
    let mut visitor = SurplusVisitorMut::new(allocator, options.clone());

    // TODO: It would be better to let the visitor do this. However,
    // TODO: OXC's visitors are a little limited at the moment
    // TODO: and currently require that base visitor methods are
    // TODO: copy-pasted into a downstream visitor implementation.
    // TODO:
    // TODO: Instead of doing that hack, we just do it here, but in the
    // TODO: future we should fix OXC's visitors.
    if options.emit_runtime_import {
        program.body.insert(
            0,
            visitor
                .a
                .module_declaration(ast::ModuleDeclaration::ImportDeclaration(
                    visitor.a.import_declaration(
                        Span {
                            start: program.span.start,
                            end: program.span.start,
                        },
                        Some(visitor.a.new_vec_single(
                            ast::ImportDeclarationSpecifier::ImportDefaultSpecifier(
                                ast::ImportDefaultSpecifier {
                                    span: Span {
                                        start: program.span.start,
                                        end: program.span.start,
                                    },
                                    local: ast::BindingIdentifier {
                                        span: Span {
                                            start: program.span.start,
                                            end: program.span.start,
                                        },
                                        name: visitor.a.new_str(&visitor.options.global).into(),
                                        symbol_id: Default::default(),
                                    },
                                },
                            ),
                        )),
                        ast::StringLiteral {
                            span: Span {
                                start: program.span.start,
                                end: program.span.start,
                            },
                            value: visitor.a.new_str("@surplus/rt").into(),
                        },
                        None,
                        ast::ImportOrExportKind::Value,
                    ),
                )),
        );
    }

    visitor.visit_program(program);
}

/// Transforms raw JSX source code to Surplus source code.
///
/// If you have already parsed the source code, use [`transform_program`] instead.
pub fn transform_string(source: &str, options: TransformOptions) -> TransformResult {
    let allocator = Default::default();

    let parser = oxc::parser::Parser::new(
        &allocator,
        source,
        SourceType::default()
            .with_jsx(true)
            .with_module(true)
            .with_script(true)
            .with_typescript(true)
            .with_always_strict(true),
    );

    let mut result = parser.parse();

    if !result.panicked {
        transform_program(&allocator, &mut result.program, options);
    }

    result.into()
}
