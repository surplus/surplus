import { transform_src } from './pkg/browser/surplus_compiler.js';

function compileSurplus(source, options) {
	const { emitRuntimeImport = true, global } = options || {};

	if (typeof emitRuntimeImport !== 'boolean') {
		throw new Error('options.emitRuntimeImport must be a boolean');
	}

	if (global && typeof global !== 'string') {
		throw new Error('options.global must be a string');
	}

	return surplusCompiler.transform_src(source, global, emitRuntimeImport);
}

export default compileSurplus;
