use wasm_bindgen::prelude::*;

/// Compiles the given JSX source to Surplus-compatible JavaScript.
#[wasm_bindgen]
#[no_mangle]
pub fn transform_src(
    src: &str,
    global: Option<String>,
    emit_runtime_import: bool,
) -> Result<String, String> {
    let mut opts = crate::TransformOptions {
        emit_runtime_import,
        ..Default::default()
    };

    if let Some(global) = global {
        opts.global = global.to_string();
    }

    let result = crate::transform_string(src, opts);

    if result.panicked {
        Err(format!("{result:#?}"))
    } else {
        Ok(result.source.unwrap())
    }
}
