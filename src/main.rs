use chrono::prelude::*;
use clap::{App, Arg, ArgGroup};
use jsonwatch::diff;

use std::{process::Command, str, thread, time};

#[derive(Debug)]
enum DataSource {
    Command(String),
    URL(String),
}

#[derive(Debug)]
struct Opts {
    data_source: DataSource,
    interval: u32,
    print_date: bool,
    print_initial: bool,
}

fn cli() -> Opts {
    let matches = App::new("jsonwatch")
        .version("0.6.0")
        .about("Track changes in JSON data")
        .arg(
            Arg::with_name("command")
                .short("c")
                .long("command")
                .value_name("command")
                .help("Command to execute"),
        )
        .arg(
            Arg::with_name("url")
                .short("u")
                .long("url")
                .value_name("url")
                .help("URL to fetch"),
        )
        .group(
            ArgGroup::with_name("source")
                .args(&["command", "url"])
                .required(true),
        )
        .arg(
            Arg::with_name("interval")
                .short("n")
                .long("interval")
                .value_name("seconds")
                .help("Polling interval"),
        )
        .arg(
            Arg::with_name("no-date")
                .long("no-date")
                .help("Don't print date and time for each diff"),
        )
        .arg(
            Arg::with_name("no-initial-values")
                .long("no-initial-values")
                .help("Don't print initial JSON values"),
        )
        .get_matches();

    let data_source = if matches.is_present("command") {
        DataSource::Command(matches.value_of("command").unwrap().to_string())
    } else {
        DataSource::URL(matches.value_of("url").unwrap().to_string())
    };

    let interval = matches
        .value_of("interval")
        .unwrap_or("5")
        .parse::<u32>()
        .expect("Polling interval must be a non-negative whole number");

    Opts {
        data_source: data_source,
        interval: interval,
        print_date: !matches.is_present("no-date"),
        print_initial: !matches.is_present("no-initial-values"),
    }
}

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
    ureq::get(url)
        .set("User-Agent", "curl/7.58.0")
        .call()
        .into_string()
        .unwrap_or("".to_string())
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
    let opts = cli();

    let lambda: Box<dyn Fn() -> String> = match opts.data_source {
        DataSource::Command(cmd) => Box::new(move || run_command(&cmd)),
        DataSource::URL(url) => Box::new(move || fetch_url(&url)),
    };

    watch(
        time::Duration::from_secs(opts.interval as u64),
        opts.print_date,
        opts.print_initial,
        lambda,
    );
}
