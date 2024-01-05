use std::io::Read;

pub fn show_usage(arg0: &str) {
    eprintln!("Usage: {} [--help] [-nv] [-g <global>]", arg0);
    eprintln!("Options:");
    eprintln!("  --no-import, -n");
    eprintln!("    Do not emit the Surplus runtime import.");
    eprintln!("  --global, -g <name>");
    eprintln!("    The name of the global variable to use for Surplus.");
    eprintln!("    Default: $$S");
    eprintln!("  --help");
    eprintln!("    Show this help message.");
    eprintln!("  --version, -v");
    eprintln!("    Show the version of this program.");
}

pub fn main() {
    let mut args = std::env::args();
    let arg0 = args.next().unwrap_or("surplus-transform".to_string());

    let mut options = surplus_compiler::TransformOptions::default();

    options.emit_runtime_import = true;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--no-import" | "-n" => options.emit_runtime_import = false,
            arg if arg.starts_with("--global=") => {
                options.global = arg[9..].to_string();
            }
            arg if arg.len() > 2 && arg.starts_with("-g") => {
                options.global = arg[3..].to_string();
            }
            "--global" | "-g" => {
                options.global = args.next().unwrap_or_else(|| {
                    eprintln!("{}: missing argument for {}", arg0, arg);
                    std::process::exit(2);
                });
            }
            "--help" => {
                show_usage(&arg0);
                std::process::exit(2);
            }
            "--version" | "-v" => {
                println!("{} {}", arg0, env!("CARGO_PKG_VERSION"));
                std::process::exit(2);
            }
            unknown if !unknown.starts_with('-') => {
                eprintln!("{}: unexpected positional argument '{}'", arg0, unknown);
                std::process::exit(2);
            }
            unknown => {
                eprintln!("{}: unknown argument {}", arg0, unknown);
                std::process::exit(2);
            }
        }
    }

    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let result = surplus_compiler::transform_string(&input, options);
    if result.panicked {
        eprintln!("failed to parse: {:#?}", result);
        std::process::exit(1);
    } else {
        println!("{}", result.source.unwrap());
    }
}
