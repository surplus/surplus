import S from "@surplus/s";

export default {
	S,
	tn(text) {
		const node = document.createTextNode(text);
		return node;
	},
	ex(expr) {
		const node = document.createTextNode("");
		this.S(() => (node.textContent = expr?.()?.toString() ?? ""));
		return node;
	},
	_ch(tag, children) {
		if (!children) return;
		else if (typeof children === "function") this._ch(tag, children());
		else if (children.forEach) children.forEach((ch) => this._ch(tag, ch));
		else {
			tag.appendChild(children);
			S.cleanup(() => tag.removeChild(children));
		}
	},
	_el(tag, props, fns) {
		return S(() => {
			const { children, ...rest } = props() || {};

			this._ch(tag, children);

			for (const [k, v] of Object.entries(rest)) {
				let [ns, name] = k.split(":", 2);
				if (!name) {
					name = ns;
					ns = null;
				}

				if (ns === "on") {
					this.S(() => {
						tag.addEventListener(name, v());
					});
				} else if (ns) {
					this.S(() => {
						tag.setAttributeNS(ns, name, v());
					});
				} else if (name === "style") {
					this.S(() => {
						const styles = v();
						for (const [k, v] of Object.entries(styles)) {
							tag.style[k] = v;
						}
					});
				} else {
					this.S(() => {
						let value = v();

						if (
							value?.forEach &&
							(name === "class" || name === "className")
						) {
							// Kind of a weird workaround to handle real arrays and Sarray
							const classNames = [];
							value.forEach((v) => v && classNames.push(v));
							value = classNames.join(" ");
						}

						tag.setAttribute(name, value);
					});
				}
			}

			fns?.()?.forEach((fn) => fn?.call(tag, tag));

			return tag;
		})();
	},
	ns(ns, elem, props, fns) {
		const tag = document.createElementNS(ns, elem);
		return this._el(tag, props, fns);
	},
	el(name, props, fns) {
		const tag = document.createElement(name);
		return this._el(tag, props, fns);
	},
	ct(tagFn) {
		return this.S(() => tagFn())();
	},
};
