# Surplus ESBuild Plugin

[ESBuild](https://esbuild.github.io/) plugin for the Surplus
Javascript framework.

> **NOTE:** This plugin strips typescript types from the output.
> If this is affecting you negatively, please open an issue
> with the [compiler](https://github.com/surplus/compiler/issues).

## Usage

```
npm i --save-dev @surplus/esbuild
pnpm add -D @surplus/esbuild
yarn add -D @surplus/esbuild
```

```js
import esbuild from 'esbuild';
import surplus from '@surplus/esbuild';

const surplusOptions = {
	/* see @surplus/compiler for options */
};

await esbuild.build({
	entryPoints: ['app.js'],
	bundle: true,
	minify: true,
	outfile: 'out.js',
	plugins: [surplus(surplusOptions)] // options are optional
});
```

# License

Released under the [MIT License](LICENSE).
