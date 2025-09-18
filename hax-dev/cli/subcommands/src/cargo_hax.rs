#![feature(rustc_private)]
use annotate_snippets::{Level, Renderer};
use clap::Parser;
use colored::Colorize;
use hax_types::cli_options::*;
use hax_types::driver_api::*;
use hax_types::engine_api::*;
use is_terminal::IsTerminal;
use serde_jsonlines::BufReadExt;
use std::collections::HashMap;
use std::fs;
use std::io::BufRead;
use std::io::Write;
use std::path::PathBuf;
use std::process;

mod engine_debug_webapp;
use hax_frontend_exporter::id_table;

/// Return a toolchain argument to pass to `cargo`: when the correct nightly is
/// already present, this is None, otherwise we (1) ensure `rustup` is available
/// (2) install the nightly (3) return the toolchain
fn toolchain() -> Option<&'static str> {
    let current_rustc_version = version_check::triple()
        .map(|(_, channel, date)| format!("{channel}-{date}"))
        .unwrap_or("unknown".into());
    if env!("HAX_RUSTC_VERSION") != current_rustc_version {
        const TOOLCHAIN: &str = env!("HAX_TOOLCHAIN");
        // ensure rustup is available
        which::which("rustup").ok().unwrap_or_else(|| {
            println!("Error: {} was not found, but toolchain {} is required while the current toolchain is {}\n\nExiting.", "rustup".bold(), TOOLCHAIN.bold(), current_rustc_version.bold());
            std::process::exit(1)
        });
        // make sure the toolchain is installed
        rustup_toolchain::install(TOOLCHAIN).unwrap();
        // return the correct toolchain
        Some(TOOLCHAIN)
    } else {
        None
    }
}

/// [`get_args`] is a wrapper of `std::env::args` that strips a possible
/// cargo subcommand. This allows for a binary `BINARY` to be called
/// both with `cargo BINARY args...` and `cargo-BINARY args...`.
pub fn get_args(subcommand: &str) -> Vec<String> {
    let mut args: Vec<_> = std::env::args().collect();
    if args.get(1) == Some(&subcommand.to_string()) {
        // we face a call `cargo [subcommand]`: we need to get rid of the first argument
        args = args.into_iter().skip(1).collect();
    }
    args
}

/// Our custom rustc driver will *not* be run in an proper terminal,
/// thus logs would appear uncolored. When no `RUST_LOG_STYLE` env. var.
/// is set, [`rust_log_style`] checks wether the `cargo hax` command was
/// run inside a terminal. If it was inside a terminal,
/// [`rust_log_style`] returns `"always"`, which is the usual default
/// behavior. Otherwise we return `"never"`. When [`RUST_LOG_STYLE`] is
/// set, we just return its value.
const RUST_LOG_STYLE: &str = "RUST_LOG_STYLE";
fn rust_log_style() -> String {
    std::env::var(RUST_LOG_STYLE).unwrap_or_else(|_| {
        if std::io::stderr().is_terminal() {
            "always".to_string()
        } else {
            "never".to_string()
        }
    })
}

/// We set `cfg(hax)` so that client crates can include dependencies
/// or cfg-gate pieces of code.
const RUSTFLAGS: &str = "RUSTFLAGS";
fn rustflags() -> String {
    let rustflags = std::env::var(RUSTFLAGS).unwrap_or("".into());
    [rustflags, "--cfg hax".into()].join(" ")
}

const ENGINE_BINARY_NAME: &str = "hax-engine";
const ENGINE_BINARY_NOT_FOUND: &str = "The binary [hax-engine] was not found in your [PATH].";

/// Dynamically looks for binary [ENGINE_BINARY_NAME].  First, we
/// check whether [HAX_ENGINE_BINARY] is set, and use that if it
/// is. Then, we try to find [ENGINE_BINARY_NAME] in PATH. If not
/// found, detect whether nodejs is available, download the JS-compiled
/// engine and use it.
#[allow(unused_variables, unreachable_code)]
fn find_hax_engine(message_format: MessageFormat) -> process::Command {
    use which::which;

    std::env::var("HAX_ENGINE_BINARY")
        .ok()
        .map(process::Command::new)
        .or_else(|| which(ENGINE_BINARY_NAME).ok().map(process::Command::new))
        .or_else(|| {
            which("node").ok().and_then(|_| {
                if let Ok(true) = inquire::Confirm::new(&format!(
                    "{} Should I try to download it from GitHub?",
                    ENGINE_BINARY_NOT_FOUND,
                ))
                .with_default(true)
                .prompt()
                {
                    let cmd = process::Command::new("node");
                    let engine_js_path: String =
                        panic!("TODO: Downloading from GitHub is not supported yet.");
                    cmd.arg(engine_js_path);
                    Some(cmd)
                } else {
                    None
                }
            })
        })
        .unwrap_or_else(|| {
            fn is_opam_setup_correctly() -> bool {
                std::env::var("OPAM_SWITCH_PREFIX").is_ok()
            }
            HaxMessage::EngineNotFound {
                is_opam_setup_correctly: is_opam_setup_correctly(),
            }
            .report(message_format, None);
            std::process::exit(2);
        })
}

const RUST_ENGINE_BINARY_NAME: &str = "hax-rust-engine";
const RUST_ENGINE_BINARY_NOT_FOUND: &str =
    "The binary [hax-rust-engine] was not found in your [PATH].";

#[allow(unused_variables, unreachable_code)]
fn find_rust_hax_engine(message_format: MessageFormat) -> process::Command {
    use which::which;

    std::env::var("HAX_RUST_ENGINE_BINARY")
        .ok()
        .map(process::Command::new)
        .or_else(|| {
            which(RUST_ENGINE_BINARY_NAME)
                .ok()
                .map(process::Command::new)
        })
        .expect(RUST_ENGINE_BINARY_NOT_FOUND)
}

use hax_types::diagnostics::message::HaxMessage;
use hax_types::diagnostics::report::ReportCtx;

#[extension_traits::extension(trait ExtHaxMessage)]
impl HaxMessage {
    fn report(self, message_format: MessageFormat, rctx: Option<&mut ReportCtx>) {
        match message_format {
            MessageFormat::Json => println!("{}", serde_json::to_string(&self).unwrap()),
            MessageFormat::Human => self.report_styled(rctx),
        }
    }
    fn report_styled(self, rctx: Option<&mut ReportCtx>) {
        let renderer = Renderer::styled();
        match self {
            Self::Diagnostic {
                diagnostic,
                working_dir,
            } => {
                let mut _rctx = None;
                let rctx = rctx.unwrap_or_else(|| _rctx.get_or_insert(ReportCtx::default()));
                diagnostic.with_message(rctx, &working_dir, Level::Error, |msg| {
                    eprintln!("{}", renderer.render(msg))
                });
            }
            Self::EngineNotFound {
                is_opam_setup_correctly,
            } => {
                use colored::Colorize;
                let message = format!("hax: {}\n{}\n\n{} {}\n",
                      &ENGINE_BINARY_NOT_FOUND,
                      "Please make sure the engine is installed and is in PATH!",
                      "Hint: With OPAM, `eval $(opam env)` is necessary for OPAM binaries to be in PATH: make sure to run `eval $(opam env)` before running `cargo hax`.".bright_black(),
                      format!("(diagnostics: {})", if is_opam_setup_correctly { "opam seems okay ✓" } else {"opam seems not okay ❌"}).bright_black()
            );
                let message = Level::Error.title(&message);
                eprintln!("{}", renderer.render(message))
            }
            Self::ProducedFile { mut path, wrote } => {
                // Make path relative if possible
                if let Ok(current_dir) = std::env::current_dir() {
                    if let Ok(relative) = path.strip_prefix(current_dir) {
                        path = PathBuf::from(".").join(relative).to_path_buf();
                    }
                }
                let title = if wrote {
                    format!("hax: wrote file {}", path.display())
                } else {
                    format!("hax: unchanged file {}", path.display())
                };
                eprintln!("{}", renderer.render(Level::Info.title(&title)))
            }
            Self::HaxEngineFailure { exit_code } => {
                let title = format!(
                    "hax: {} exited with non-zero code {}",
                    ENGINE_BINARY_NAME, exit_code,
                );
                eprintln!("{}", renderer.render(Level::Error.title(&title)));
            }
            Self::ProfilingData(data) => {
                fn format_with_dot(shift: u32, n: u64) -> String {
                    let factor = 10u64.pow(shift);
                    format!("{}.{}", n / factor, n % factor)
                }
                let title = format!(
                    "hax[profiling]: {}: {}ms, memory={}, {} item{}{}",
                    data.context,
                    format_with_dot(6, data.time_ns),
                    data.memory,
                    data.quantity,
                    if data.quantity > 1 { "s" } else { "" },
                    if data.errored {
                        " (note: this failed!)"
                    } else {
                        ""
                    }
                );
                eprintln!("{}", renderer.render(Level::Info.title(&title)));
            }
            Self::Stats { errors_per_item } => {
                let success_items = errors_per_item.iter().filter(|(_, n)| *n == 0).count();
                let total = errors_per_item.len();
                let title = format!(
                    "hax: {}/{} items were successfully translated ({}% success rate)",
                    success_items,
                    total,
                    (success_items * 100) / total
                );
                eprintln!("{}", renderer.render(Level::Info.title(&title)));
            }
            Self::CargoBuildFailure => {
                let title =
                    "hax: running `cargo build` was not successful, continuing anyway.".to_string();
                eprintln!("{}", renderer.render(Level::Warning.title(&title)));
            }
            Self::WarnExperimentalBackend { backend } => {
                let title = format!(
                    "hax: Experimental backend \"{}\" is work in progress.",
                    backend
                );
                eprintln!("{}", renderer.render(Level::Warning.title(&title)));
            }
        }
    }
}

/// Runs `hax-engine`
fn run_engine(
    haxmeta: HaxMeta<hax_frontend_exporter::ThirBody>,
    id_table: id_table::Table,
    working_dir: PathBuf,
    manifest_dir: PathBuf,
    backend: &BackendOptions<()>,
    message_format: MessageFormat,
) -> bool {
    let engine_options = EngineOptions {
        hax_version: haxmeta.hax_version,
        backend: backend.clone(),
        input: haxmeta.items,
        impl_infos: haxmeta.impl_infos,
    };
    let mut hax_engine_command = match &engine_options.backend.backend {
        Backend::Fstar(_)
        | Backend::Coq
        | Backend::Ssprove
        | Backend::Easycrypt
        | Backend::ProVerif(_) => find_hax_engine(message_format),
        _ => find_rust_hax_engine(message_format),
    };
    let mut engine_subprocess = hax_engine_command
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .inspect_err(|e| {
            if let std::io::ErrorKind::NotFound = e.kind() {
                panic!(
                    "The binary [{}] was not found in your [PATH].",
                    ENGINE_BINARY_NAME
                )
            }
        })
        .unwrap();

    let mut error = false;
    let mut output = Output {
        diagnostics: vec![],
        files: vec![],
        debug_json: None,
    };
    {
        let mut rctx = hax_types::diagnostics::report::ReportCtx::default();
        let mut stdin = std::io::BufWriter::new(
            engine_subprocess
                .stdin
                .as_mut()
                .expect("Could not write on stdin"),
        );

        macro_rules! send {
            ($value:expr) => {
                serde_json::to_writer(&mut stdin, $value).unwrap();
                stdin.write_all(b"\n").unwrap();
                stdin.flush().unwrap();
            };
        }

        id_table::WithTable::run(id_table, engine_options, |with_table| {
            send!(with_table);
        });

        let out_dir = backend.output_dir.clone().unwrap_or({
            let relative_path: PathBuf = [
                "proofs",
                format!("{}", backend.backend).as_str(),
                "extraction",
            ]
            .iter()
            .collect();
            manifest_dir.join(&relative_path)
        });

        let stdout = std::io::BufReader::new(engine_subprocess.stdout.take().unwrap());
        let mut errors_per_item: HashMap<_, usize> = HashMap::new();
        for msg in stdout.json_lines() {
            let msg = msg.expect(
                "Hax engine sent an invalid json value. \
            This might be caused by debug messages on stdout, \
            which is reserved for JSON communication with cargo-hax",
            );
            use protocol::*;
            match msg {
                FromEngine::Exit => break,
                FromEngine::Diagnostic(diagnostic) => {
                    error = true;
                    if backend.dry_run {
                        output.diagnostics.push(diagnostic.clone())
                    }
                    if let Some(owner_id) = &diagnostic.owner_id {
                        *errors_per_item.entry(owner_id.clone()).or_default() += 1;
                    }
                    HaxMessage::Diagnostic {
                        diagnostic,
                        working_dir: working_dir.clone(),
                    }
                    .report(message_format, Some(&mut rctx));
                }
                FromEngine::File(file) => {
                    if backend.dry_run {
                        output.files.push(file)
                    } else {
                        let path = out_dir.join(&file.path);
                        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
                        let mut wrote = false;
                        if fs::read_to_string(&path).as_ref().ok() != Some(&file.contents) {
                            std::fs::write(&path, file.contents).unwrap();
                            wrote = true;
                        }
                        if let Some(mut sourcemap) = file.sourcemap.clone() {
                            sourcemap.sourcesContent = sourcemap
                                .sources
                                .iter()
                                .map(PathBuf::from)
                                .map(|path| {
                                    if path.is_absolute() {
                                        path
                                    } else {
                                        working_dir.join(path).to_path_buf()
                                    }
                                })
                                .map(|path| fs::read_to_string(path).ok())
                                .collect();
                            let f = std::fs::File::create(path.with_file_name(format!(
                                "{}.map",
                                path.file_name().unwrap().to_string_lossy()
                            )))
                            .unwrap();
                            serde_json::to_writer(std::io::BufWriter::new(f), &sourcemap).unwrap()
                        }
                        HaxMessage::ProducedFile { path, wrote }.report(message_format, None)
                    }
                }
                FromEngine::DebugString(debug) => {
                    output.debug_json = Some(debug);
                }
                FromEngine::PrettyPrintDiagnostic(diag) => {
                    send!(&ToEngine::PrettyPrintedDiagnostic(format!("{}", diag)));
                }
                FromEngine::PrettyPrintRust(code) => {
                    let code = match syn::parse_file(&code) {
                        Ok(file) => match std::panic::catch_unwind(|| prettyplease::unparse(&file))
                        {
                            Ok(pp) => Ok(pp),
                            Err(err) => Err(format!("prettyplease panicked with: {:#?}", err)),
                        },
                        Err(err) => Err(format!("{}", err)),
                    };
                    send!(&ToEngine::PrettyPrintedRust(code));
                }
                FromEngine::ProfilingData(profiling_data) => {
                    HaxMessage::ProfilingData(profiling_data).report(message_format, None)
                }
                FromEngine::ItemProcessed(items) => {
                    for item in items {
                        errors_per_item.insert(item, 0);
                    }
                }
                FromEngine::Ping => {
                    send!(&ToEngine::Pong);
                }
            }
        }
        if backend.stats {
            HaxMessage::Stats {
                errors_per_item: errors_per_item.into_iter().collect(),
            }
            .report(message_format, None)
        }
        drop(stdin);
    }

    let exit_status = engine_subprocess.wait().unwrap();
    if !exit_status.success() {
        HaxMessage::HaxEngineFailure {
            exit_code: exit_status.code().unwrap_or(-1),
        }
        .report(message_format, None);
        std::process::exit(1);
    }

    if backend.dry_run {
        serde_json::to_writer(std::io::BufWriter::new(std::io::stdout()), &output).unwrap()
    }
    if let Some(debug_json) = &output.debug_json {
        use DebugEngineMode;
        match &backend.debug_engine {
            Some(DebugEngineMode::Interactive) => {
                eprintln!("----------------------------------------------");
                eprintln!("----------------------------------------------");
                eprintln!("----------------------------------------------");
                eprintln!("-- Engine debug mode. Press CTRL+C to exit. --");
                eprintln!("----------------------------------------------");
                eprintln!("----------------------------------------------");
                eprintln!("----------------------------------------------");
                engine_debug_webapp::run(|| debug_json.clone())
            }
            Some(DebugEngineMode::File(_file)) if !backend.dry_run => {
                println!("{}", debug_json)
            }
            _ => (),
        }
    }

    error
}

/// Uses `cargo metadata` to compute a derived target directory.
fn target_dir(suffix: &str) -> PathBuf {
    let metadata = cargo_metadata::MetadataCommand::new().exec().unwrap();
    let mut dir = metadata.target_directory;
    dir.push(suffix);
    dir.into()
}

/// Gets hax version: if hax is being compiled from a dirty git repo,
/// then this function taints the hax version with the hash of the
/// current executable. This makes sure cargo doesn't cache across
/// different versions of hax, for more information see
/// https://github.com/hacspec/hax/issues/801.
fn get_hax_version() -> String {
    let mut version = hax_types::HAX_VERSION.to_string();
    if env!("HAX_GIT_IS_DIRTY") == "true" {
        version += &std::env::current_exe()
            .ok()
            .and_then(|exe_path| std::fs::read(exe_path).ok())
            .map(|contents| {
                use std::hash::{DefaultHasher, Hash, Hasher};
                let mut s = DefaultHasher::new();
                contents.hash(&mut s);
                format!("hash-exe-{}", s.finish())
            })
            .expect("Expect read path")
    }

    version
}

/// Returns the path to the custom rustc driver used by cargo-hax.
///
/// This function retrieves the path of the current executable (i.e. `cargo-hax`), determines its
/// parent directory, and then appends the driver executable name `"driver-hax-frontend-exporter"` to it.
/// This path is used to locate the custom rustc driver that computes `haxmeta` files.
fn get_hax_rustc_driver_path() -> PathBuf {
    std::env::current_exe()
        .expect("Could not get the current executable path for `cargo-hax`.")
        .parent().expect("The executable `cargo-hax` is supposed to be a file, which is supposed to have a parent folder.")
        .join("driver-hax-frontend-exporter")
}

/// Calls `cargo` with a custom driver which computes `haxmeta` files
/// in `TARGET`. One `haxmeta` file is produced by crate. Each
/// `haxmeta` file contains the full AST of one crate.
fn compute_haxmeta_files(options: &Options) -> (Vec<EmitHaxMetaMessage>, i32) {
    let mut cmd = {
        let mut cmd = process::Command::new("cargo");
        if let Some(toolchain) = toolchain() {
            cmd.env("RUSTUP_TOOLCHAIN", toolchain);
        }
        cmd.args(["check".into()].iter().chain(options.cargo_flags.iter()));
        const COLOR_FLAG: &str = "--color";
        let explicit_color_flag = options.cargo_flags.iter().any(|flag| flag == COLOR_FLAG);
        if !explicit_color_flag && std::io::stderr().is_terminal() {
            cmd.args([COLOR_FLAG, "always"]);
        }
        const MSG_FMT_FLAG: &str = "--message-format";
        let explicit_msg_fmt_flag = options.cargo_flags.iter().any(|flag| flag == MSG_FMT_FLAG);
        if !explicit_msg_fmt_flag && options.message_format == MessageFormat::Json {
            cmd.args([MSG_FMT_FLAG, "json"]);
        }
        cmd.stderr(std::process::Stdio::piped());
        if !options.no_custom_target_directory {
            cmd.env("CARGO_TARGET_DIR", target_dir("hax"));
        };
        cmd.env("RUSTC_WORKSPACE_WRAPPER", get_hax_rustc_driver_path())
            .env(RUST_LOG_STYLE, rust_log_style())
            .env(RUSTFLAGS, rustflags())
            .env("HAX_CARGO_CACHE_KEY", get_hax_version())
            .env(
                ENV_VAR_OPTIONS_FRONTEND,
                serde_json::to_string(&options)
                    .expect("Options could not be converted to a JSON string"),
            );
        cmd
    };

    let mut child = cmd.spawn().unwrap();
    let haxmeta_files = {
        let mut haxmeta_files = vec![];
        let stderr = child.stderr.take().unwrap();
        let stderr = std::io::BufReader::new(stderr);
        for line in std::io::BufReader::new(stderr).lines() {
            if let Ok(line) = line {
                if let Some(msg) = line.strip_prefix(HAX_DRIVER_STDERR_PREFIX) {
                    use HaxDriverMessage;
                    let msg = serde_json::from_str(msg).unwrap();
                    match msg {
                        HaxDriverMessage::EmitHaxMeta(data) => haxmeta_files.push(data),
                    }
                } else {
                    eprintln!("{}", line);
                }
            }
        }
        haxmeta_files
    };

    let status = child
        .wait()
        .expect("`driver-hax-frontend-exporter`: could not start?");

    let exit_code = if !status.success() {
        HaxMessage::CargoBuildFailure.report(options.message_format, None);
        status.code().unwrap_or(254)
    } else {
        0
    };

    (haxmeta_files, exit_code)
}

/// Run the command given by the user
fn run_command(options: &Options, haxmeta_files: Vec<EmitHaxMetaMessage>) -> bool {
    match options.command.clone() {
        Command::JSON {
            output_file,
            kind,
            include_extra,
            use_ids,
            ..
        } => {
            with_kind_type!(kind, <Body>|| {
                for EmitHaxMetaMessage { path, .. } in haxmeta_files {
                    let (haxmeta, id_table): (HaxMeta<Body>, _) = HaxMeta::read(fs::File::open(&path).unwrap());
                    let dest = output_file.open_or_stdout();

                    (if include_extra {
                        let data = WithDefIds {
                            def_ids: haxmeta.def_ids,
                            impl_infos: haxmeta.impl_infos,
                            items: haxmeta.items,
                            comments: haxmeta.comments,
                        };
                        if use_ids {
                            id_table::WithTable::run(id_table, data, |with_table| {
                                serde_json::to_writer(dest, with_table)
                            })
                        } else {
                            serde_json::to_writer(dest, &data)
                        }
                    } else {
                        if use_ids {
                            id_table::WithTable::run(id_table, haxmeta.items, |with_table| {
                                serde_json::to_writer(dest, with_table)
                            })
                        } else {
                            serde_json::to_writer(dest, &haxmeta.items)
                        }
                    })
                        .unwrap()

                }
            });
            false
        }
        Command::Backend(backend) => {
            use Backend;
            use hax_frontend_exporter::ThirBody as Body;

            if matches!(backend.backend, Backend::Easycrypt | Backend::ProVerif(..)) {
                HaxMessage::WarnExperimentalBackend {
                    backend: backend.backend.clone(),
                }
                .report(options.message_format, None);
            }

            let mut error = false;
            for EmitHaxMetaMessage {
                working_dir,
                manifest_dir,
                path,
            } in haxmeta_files
            {
                let (haxmeta, id_table): (HaxMeta<Body>, _) =
                    HaxMeta::read(fs::File::open(&path).unwrap());

                error = error
                    || run_engine(
                        haxmeta,
                        id_table,
                        working_dir,
                        manifest_dir,
                        &backend,
                        options.message_format,
                    );
            }
            error
        }
    }
}

fn main() {
    let args: Vec<String> = get_args("hax");
    let mut options = match &args[..] {
        [_, kw] if kw == "__json" => serde_json::from_str(
            &std::env::var(ENV_VAR_OPTIONS_FRONTEND).unwrap_or_else(|_| {
                panic!(
                    "Cannot find environnement variable {}",
                    ENV_VAR_OPTIONS_FRONTEND
                )
            }),
        )
        .unwrap_or_else(|_| {
            panic!(
                "Invalid value for the environnement variable {}",
                ENV_VAR_OPTIONS_FRONTEND
            )
        }),
        _ => Options::parse_from(args.iter()),
    };
    options.normalize_paths();

    let (haxmeta_files, exit_code) = compute_haxmeta_files(&options);
    let error = run_command(&options, haxmeta_files);

    std::process::exit(if exit_code == 0 && error {
        1
    } else {
        exit_code
    })
}
