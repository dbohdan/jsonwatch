use chrono::prelude::*;
use clap::Parser;
use jsonwatch::diff;
use std::{process::Command, str, thread, time};

#[derive(Debug)]
enum DataSource {
    Command(String),
    URL(String),
}

#[derive(Parser, Debug)]
#[command(name = "jsonwatch")]
#[command(about = "Track changes in JSON data", version = "0.6.0")]
struct Opts {
    /// Command to execute
    #[arg(short, long, value_name = "command", group = "source")]
    command: Option<String>,

    /// URL to fetch
    #[arg(short, long, value_name = "url", group = "source")]
    url: Option<String>,

    /// Polling interval in seconds
    #[arg(short = 'n', long, value_name = "seconds", default_value = "5")]
    interval: u32,

    /// Don't print date and time for each diff
    #[arg(long)]
    no_date: bool,

    /// Don't print initial JSON values
    #[arg(long)]
    no_initial_values: bool,
}

impl Opts {
    fn get_data_source(&self) -> DataSource {
        match (&self.command, &self.url) {
            (Some(cmd), None) => DataSource::Command(cmd.clone()),
            (None, Some(url)) => DataSource::URL(url.clone()),
            _ => unreachable!("clap ensures exactly one source is provided"),
        }
    }
}

const USER_AGENT: &str = "curl/7.58.0";

fn run_command(command: &str) -> String {
    let output = if cfg!(target_os = "windows") {
        Command::new("cmd").arg("/c").arg(command).output()
    } else {
        Command::new("sh").arg("-c").arg(command).output()
    };

    let stdout = match output {
        Ok(output) => str::from_utf8(&output.stdout).unwrap_or("").to_string(),
        _ => "".to_string(),
    };

    stdout
}

fn fetch_url(url: &str) -> String {
    if let Ok(result) = ureq::get(url).set("User-Agent", USER_AGENT).call() {
        result.into_string().unwrap_or("".to_string())
    } else {
        "".to_string()
    }
}

fn watch(
    interval: time::Duration,
    print_date: bool,
    print_initial: bool,
    lambda: impl Fn() -> String,
) {
    let mut data: Option<serde_json::Value> =
        serde_json::from_str(&lambda()).ok();
    if print_initial {
        match &data {
            Some(json) => {
                println!("{}", serde_json::to_string_pretty(&json).unwrap())
            }
            _ => {}
        }
    }

    loop {
        thread::sleep(interval);

        let prev = data.clone();

        data = serde_json::from_str(&lambda()).ok();

        let diff = diff::diff(&prev, &data);

        let changed = diff.len();
        if changed == 0 {
            continue;
        }

        if print_date {
            let local = Local::now();
            print!("{}", local.format("%Y-%m-%dT%H:%M:%S%z"));

            if changed == 1 {
                print!(" ");
            } else {
                println!("");
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
    let opts = Opts::parse();

    let lambda: Box<dyn Fn() -> String> = match opts.get_data_source() {
        DataSource::Command(cmd) => Box::new(move || run_command(&cmd)),
        DataSource::URL(url) => Box::new(move || fetch_url(&url)),
    };

    watch(
        time::Duration::from_secs(opts.interval as u64),
        !opts.no_date,
        !opts.no_initial_values,
        lambda,
    );
}
