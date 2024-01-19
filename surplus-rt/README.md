# Surplus Runtime

This is the runtime for the Surplus framework.

It must be listed as a dependency of any Surplus application,
but should be otherwise unused. The compiler emits the necessary
calls to the runtime API to make the application work, including
the import.

```
npm i @surplus/rt
pnpm add @surplus/rt
yarn add @surplus/rt
```

> **NOTE:** The runtime API is an implementation
> detail of the [compiler](https://github.com/surplus/surplus/blob/master/compiler)
> and should be considered **eternally unstable**.
> It is not intended for direct use by application code.

# License

Released under the [MIT License](LICENSE).
