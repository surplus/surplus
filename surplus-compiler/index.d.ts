interface CompileSurplusOptions {
	emitRuntimeImport?: boolean;
	global?: string;
}

declare function compileSurplus(
	source: string,
	options?: CompileSurplusOptions
): string;

export = compileSurplus;
