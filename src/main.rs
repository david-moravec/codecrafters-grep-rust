#![allow(dead_code)]
use regex_pattern::RegexPattern;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use clap::Parser;

mod regex_pattern;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, action, default_value_t = false)]
    recursive: bool,

    #[arg(short = 'E', action)]
    exec: bool,

    #[arg()]
    pattern: String,

    #[arg(num_args=1.., value_delimiter=' ')]
    paths: Option<Vec<String>>,
}

fn match_pattern(input_line: &str, pattern: &RegexPattern) -> bool {
    // Uncomment this block to pass the first stage
    match pattern.matches(&input_line) {
        Ok(matches) => matches,
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    }
}

fn match_file(paths: &[String], pattern: &RegexPattern) {
    let mut matched = false;
    let len_paths = paths.len();

    for path in paths.iter() {
        if let Ok(file) = File::open(path) {
            let buff = io::BufReader::new(file).lines();

            for line in buff.map_while(Result::ok) {
                if match_pattern(&line, pattern) {
                    if len_paths > 1 {
                        print!("{:}:", path);
                    }
                    println!("{:}", line);
                    matched = true;
                }
            }
        } else {
            println!("Opening of file {:?} failed", path);
        };
    }

    if matched {
        process::exit(0);
    } else {
        process::exit(1);
    }
}

fn match_line(input_line: &str, pattern: &RegexPattern) {
    if match_pattern(&input_line, pattern) {
        process::exit(0);
    } else {
        process::exit(1);
    }
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    let args = Args::parse();

    let regex = match RegexPattern::new(&args.pattern) {
        Ok(r) => r,
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    };

    if let Some(paths) = args.paths {
        match_file(&paths, &regex);
    } else {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        match_line(&input_line, &regex);
    }
}
