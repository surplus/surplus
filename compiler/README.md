# Surplus Compiler

This is the base Surplus compiler transformer logic,
and a basic CLI utility called `surplus-transform`.

> **NOTE:** This is only the compiler; it does not include the
> [runtime](https://github.com/surplus/surplus/blob/master/rt).

## Rust Usage

Check the docs. For now, running `cargo doc --open` is the best way.

For the CLI utility, run `cargo run -- --help` for usage.

## Javascript Usage

```sh
npm install @surplus/compiler
```

The compiler is usable via a WASM module.

> **NOTE:** The JavaScript package and the Rust crate do not necessarily
> share the same version numbers. To the extent possible, major and minors
> will remain synchronized.

```javascript
import compileSurplus from '@surplus/compiler';

console.log(
	compileSurplus(
		`<div>Hello!</div>`,

		/* Optional arguments (defaults shown) */
		{
			global: '$$S',
			emitRuntimeImport: true
		}
	)
);
```

## License

Licensed under the [MIT License](LICENSE).
