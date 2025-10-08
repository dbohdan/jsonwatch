use chrono::prelude::*;
use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;
use jsonwatch::diff;
use std::{error::Error, fmt::Write, process::Command, str, thread, time};

#[derive(Parser, Debug)]
#[command(
    name = "jsonwatch",
    about = "Track changes in JSON data",
    version = "0.11.0"
)]
struct Cli {
    /// Don't print date and time for each diff
    #[arg(short = 'D', long)]
    no_date: bool,

    /// Don't print initial JSON values
    #[arg(short = 'I', long)]
    no_initial_values: bool,

    /// Exit after a number of changes
    #[arg(short = 'c', long = "changes", value_name = "count")]
    changes: Option<u32>,

    /// Polling interval in seconds
    #[arg(short = 'n', long, value_name = "seconds", default_value = "2")]
    interval: u32,

    /// Verbose mode ('-v' for errors, '-vv' for errors and input data)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Subcommands for different data sources
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Execute a command and track changes in the JSON output
    #[command(aliases(["command"]))]
    Cmd {
        /// Command to execute
        #[arg(value_name = "command")]
        command: String,

        /// Arguments to the command
        #[arg(
            value_name = "arg",
            trailing_var_arg = true,
            allow_hyphen_values = true
        )]
        args: Vec<String>,
    },

    /// Fetch a URL and track changes in the JSON data
    #[command()]
    Url {
        /// URL to fetch
        #[arg(value_name = "url")]
        url: String,

        /// Custom user-agent string
        #[arg(
            short = 'A',
            long = "user-agent",
            value_name = "user-agent",
            default_value = "curl/7.58.0"
        )]
        user_agent: String,

        /// Custom headers in the format "X-Foo: bar"
        #[arg(
            short = 'H',
            long = "header",
            value_name = "header",
            action = clap::ArgAction::Append
        )]
        headers: Vec<String>,
    },

    /// Generate shell completions
    #[command()]
    Init {
        /// The shell to generate completions for
        #[arg(value_enum, value_name = "shell")]
        shell: Shell,
    },
}

const MAX_BODY_SIZE: u64 = 128 * 1024 * 1024;
const TIMESTAMP_FORMAT: &str = "%Y-%m-%dT%H:%M:%S%z";

fn run_command(
    command: &String,
    args: &[String],
) -> Result<String, Box<dyn Error>> {
    if command.is_empty() {
        return Ok(String::new());
    }

    let output = Command::new(command).args(args).output()?;

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn fetch_url(
    url: &str,
    user_agent: &str,
    headers: &[String],
) -> Result<String, Box<dyn Error>> {
    let mut request = ureq::get(url).header("User-Agent", user_agent);

    for header in headers {
        if let Some((name, value)) = header.split_once(':') {
            request = request.header(name.trim(), value.trim());
        }
    }

    Ok(request
        .call()?
        .body_mut()
        .with_config()
        .limit(MAX_BODY_SIZE)
        .read_to_string()?)
}

pub fn escape_for_terminal(input: &str) -> String {
    let mut result = String::with_capacity(input.len());

    for ch in input.chars() {
        match ch {
            // Allow newline and tab for formatting.
            '\n' | '\t' => result.push(ch),

            // Escape other control characters.
            ch if ch.is_control() => {
                write!(&mut result, "\\u{{{:x}}}", ch as u32).unwrap();
            }

            // Keep all other characters.
            _ => result.push(ch),
        }
    }

    result
}

fn print_debug(input_data: &str) {
    let local = Local::now();
    let timestamp = local.format(&TIMESTAMP_FORMAT);

    let multiline =
        input_data.trim_end().contains('\n') || input_data.ends_with("\n\n");
    let escaped = escape_for_terminal(&input_data);

    if multiline {
        eprint!("[DEBUG {}] Multiline input data:\n{}", timestamp, escaped);
    } else {
        eprint!("[DEBUG {}] Input data: {}", timestamp, escaped);
    }

    if !input_data.is_empty() && !input_data.ends_with('\n') {
        eprintln!();
    }
    if multiline {
        eprintln!("[DEBUG {}] End of multiline input data", timestamp);
    }
}

fn watch(
    interval: time::Duration,
    changes: Option<u32>,
    print_date: bool,
    print_initial: bool,
    verbose: u8,
    lambda: impl Fn() -> Result<String, Box<dyn Error>>,
) {
    let mut change_count = 0;
    let input_data = match lambda() {
        Ok(s) => s,
        Err(e) => {
            if verbose >= 1 {
                let local = Local::now();
                let timestamp = local.format(&TIMESTAMP_FORMAT);
                eprintln!("[ERROR {}] {}", timestamp, e);
            }

            String::new()
        }
    };
    let mut data: Option<serde_json::Value> =
        match serde_json::from_str(&input_data) {
            Ok(json) => Some(json),
            Err(e) => {
                if verbose >= 1 {
                    let local = Local::now();
                    let timestamp = local.format(&TIMESTAMP_FORMAT);
                    if input_data.trim().is_empty() {
                        eprintln!("[ERROR {}] Blank response", timestamp);
                    } else {
                        eprintln!(
                            "[ERROR {}] JSON parsing error: {}",
                            timestamp, e
                        );
                    }
                }

                None
            }
        };

    if print_initial {
        if verbose >= 2 {
            print_debug(&input_data);
        }

        if let Some(json) = &data {
            println!("{}", serde_json::to_string_pretty(&json).unwrap())
        }
    }

    loop {
        if let Some(max) = changes {
            if change_count >= max {
                break;
            }
        }

        thread::sleep(interval);

        let input_data = match lambda() {
            Ok(s) => s,
            Err(e) => {
                if verbose >= 1 {
                    let local = Local::now();
                    let timestamp = local.format(&TIMESTAMP_FORMAT);
                    eprintln!("[ERROR {}] {}", timestamp, e);
                }

                continue;
            }
        };
        if verbose >= 2 {
            print_debug(&input_data);
        }

        let prev = data.clone();
        data = match serde_json::from_str(&input_data) {
            Ok(json) => Some(json),
            Err(e) => {
                if verbose >= 1 {
                    let local = Local::now();
                    let timestamp = local.format(&TIMESTAMP_FORMAT);
                    if input_data.trim().is_empty() {
                        eprintln!("[ERROR {}] Blank response", timestamp);
                    } else {
                        eprintln!(
                            "[ERROR {}] JSON parsing error: {}",
                            timestamp, e
                        );
                    }
                }
                continue;
            }
        };

        let diff = diff::diff(&prev, &data);

        let changed = diff.len();
        if changed == 0 {
            continue;
        }

        change_count += 1;

        if print_date {
            let local = Local::now();
            print!("{}", local.format(&TIMESTAMP_FORMAT));

            if changed == 1 {
                print!(" ");
            } else {
                println!();
            }
        }

        if changed == 1 {
            print!("{}", diff);
        } else {
            let s = format!("{}", diff)
                .lines()
                .collect::<Vec<_>>()
                .join("\n    ");
            println!("    {}", s);
        }
    }
}

fn main() {
    let cli = Cli::parse();

    if let Commands::Init { shell } = cli.command {
        let mut cmd = Cli::command();
        clap_complete::generate(
            shell,
            &mut cmd,
            "jsonwatch",
            &mut std::io::stdout(),
        );
        return;
    }

    let lambda: Box<dyn Fn() -> Result<String, Box<dyn Error>>> =
        match &cli.command {
            Commands::Init { .. } => unreachable!(),

            Commands::Cmd { args, command } => {
                let args = args.clone();
                let command = command.clone();
                Box::new(move || run_command(&command, &args))
            }

            Commands::Url {
                url,
                user_agent,
                headers,
            } => {
                let url = url.clone();
                let user_agent = user_agent.clone();
                let headers = headers.clone();
                Box::new(move || fetch_url(&url, &user_agent, &headers))
            }
        };

    watch(
        time::Duration::from_secs(cli.interval as u64),
        cli.changes,
        !cli.no_date,
        !cli.no_initial_values,
        cli.verbose,
        lambda,
    );
}
