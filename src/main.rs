#![allow(dead_code)]
use regex_pattern::RegexPattern;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

mod regex_pattern;

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
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    eprintln!("Logs from your program will appear here!");

    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let args_vec: Vec<String> = env::args().collect();

    let pattern = env::args().nth(2).unwrap();

    let regex = match RegexPattern::new(&pattern) {
        Ok(r) => r,
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    };

    if args_vec.len() > 3 {
        match_file(&args_vec[3..], &regex);
    } else {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        match_line(&input_line, &regex);
    }
}
