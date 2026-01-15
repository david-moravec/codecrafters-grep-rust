#![allow(dead_code)]
use regex_pattern::RegexPattern;
use std::fs::{self, File};
use std::io::{self, BufRead, Read};
use std::path::Path;
use std::process;

use clap::Parser;

mod regex_parser;
mod regex_pattern;
mod regex_scanner;

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
    inputs: Vec<String>,
}

fn match_pattern(input_line: &str, pattern: &RegexPattern) -> bool {
    // Uncomment this block to pass the first stage
    match pattern.matches(&input_line) {
        Ok(matches) => matches,
        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    }
}

fn match_dir(path: &Path, pattern: &RegexPattern) -> bool {
    let mut matched = false;

    if path.is_dir() {
        for entry in fs::read_dir(path).expect(&format!("Failed to read dir in {:?}", path)) {
            let entry = entry.expect(&format!("Failed to read entry in {:?}", path));

            if entry.path().is_dir() {
                matched |= match_dir(&entry.path(), pattern);
            } else {
                matched |= match_file(&[entry.path().to_str().unwrap().to_string()], pattern, true)
            }
        }
    }

    matched
}

fn match_dirs(paths: &[String], pattern: &RegexPattern) -> bool {
    let mut matched = false;

    for path in paths.iter() {
        let path_ = Path::new(path);
        matched |= match_dir(&path_, pattern)
    }

    matched
}

fn match_file(paths: &[String], pattern: &RegexPattern, print_full_path: bool) -> bool {
    let mut matched = false;
    let len_paths = paths.len();

    for path in paths.iter() {
        if let Ok(file) = File::open(path) {
            let buff = io::BufReader::new(file).lines();

            for line in buff.map_while(Result::ok) {
                if match_pattern(&line, pattern) {
                    if print_full_path {
                        print!("{:}:", Path::new(path).to_str().unwrap());
                    } else {
                        if len_paths > 1 {
                            print!("{:}:", path);
                        }
                    }
                    println!("{:}", line);
                    matched = true;
                }
            }
        } else {
            println!("Opening of file {:?} failed", path);
        };
    }

    matched
}

fn match_lines(lines: &str, pattern: &RegexPattern) -> bool {
    let mut matched = false;

    let lines = lines.replace("\\n", "\n");

    let lines_split: Vec<&str> = lines.split('\n').map(|s| s.trim()).collect();

    for line in lines_split.iter() {
        matched |= match_line(line, pattern);
    }

    matched
}

fn match_line(input_line: &str, pattern: &RegexPattern) -> bool {
    if match_pattern(&input_line, pattern) {
        println!("{:}", input_line);
        true
    } else {
        false
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

    let matched;

    if args.inputs.len() == 0 {
        let mut line = String::new();
        io::stdin().read_to_string(&mut line).unwrap();

        matched = match_lines(&line, &regex);
    } else {
        if let Ok(true) = Path::new(&args.inputs[0]).try_exists() {
            if args.recursive {
                matched = match_dirs(&args.inputs, &regex);
            } else {
                matched = match_file(&args.inputs, &regex, false);
            }
        } else {
            matched = match_lines(&args.inputs[0], &regex);
        }
    }

    if matched {
        process::exit(0);
    } else {
        process::exit(1);
    }
}
