import fsp from 'node:fs/promises';

import compileSurplus from '@surplus/compiler';

export default opts => ({
	name: 'surplus',
	setup(build) {
		const fileExtensions = ['.js', '.jsx', '.ts', '.tsx', '.mjs', '.cjs'];

		for (let ext of fileExtensions) {
			build.onLoad({ filter: new RegExp(`\\${ext}$`) }, async args => {
				try {
					const source = await fsp.readFile(args.path, 'utf8');

					const transpiled = compileSurplus(source, opts);

					return {
						contents: transpiled,
						loader: 'default'
					};
				} catch (err) {
					// Handle any errors from the transpiler
					return { errors: [{ text: err.message }] };
				}
			});
		}
	}
});
