use regex::Regex;
use std::fs::File;
use std::io::{BufReader, BufRead};
use std::error::Error;

#[derive(Debug, Clone, Copy)]
struct State {
    sum: i32,
    filter: bool,
}

impl State {
    fn empty() -> Self {
        State { sum: 0, filter: true }
    }
}

fn read_lines(file_path: &str) -> Result<Vec<String>, Box<dyn Error>> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);
    let lines = reader.lines().collect::<Vec<_>>();
    lines.into_iter().collect::<Result<Vec<_>, _>>().map_err(From::from)
}

fn process_command(state: State, command: &str) -> State {
    let mul_pattern = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    match command {
        "do()" => State { filter: true, ..state },
        "don't()" => State { filter: false, ..state },
        _ => {
            if state.filter {
                if let Some(caps) = mul_pattern.captures(command) {
                    let first: i32 = caps[1].parse().unwrap();
                    let second: i32 = caps[2].parse().unwrap();
                    State { sum: state.sum + first * second, ..state }
                } else {
                    state
                }
            } else {
                state
            }
        }
    }
}

fn first_task(lines: Vec<String>) -> i32 {
    let mul_pattern = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    lines.iter()
        .flat_map(|line| {
            mul_pattern.find_iter(line).map(|mat| {
                let caps = mul_pattern.captures(mat.as_str()).unwrap();
                let first: i32 = caps[1].parse().unwrap();
                let second: i32 = caps[2].parse().unwrap();
                first * second
            })
        })
        .sum()
}

fn second_task(lines: Vec<String>) -> i32 {
    let filter_mul_pattern = Regex::new(r"(do\(\)|mul\(\d{1,3},\d{1,3}\)|don't\(\))").unwrap();
    let mut state = State::empty();
    for line in lines {
        for mat in filter_mul_pattern.find_iter(&line) {
            state = process_command(state, mat.as_str());
        }
    }
    state.sum
}

fn main() -> Result<(), Box<dyn Error>> {
    let source_file_name = "./d3/input.txt";
    let lines = read_lines(source_file_name).unwrap();

    let first_result = first_task(lines.clone());
    println!("1st Task: Uncorrupted mul result: {}", first_result);

    let second_result = second_task(lines);
    println!("2nd Task: Filtered Uncorrupted mul result: {}", second_result);

    Ok(())
}