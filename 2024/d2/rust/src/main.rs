use std::fs::File;
use std::io::{self, BufRead, BufReader};
use log::{info, LevelFilter};
use env_logger::Builder;

fn read_lists(filename: &str) -> io::Result<Vec<Vec<i32>>> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut lists = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let numbers: Vec<i32> = line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();
        lists.push(numbers);
    }

    Ok(lists)
}

fn calculate_safe_levels(list: &[i32]) -> Vec<bool> {
    let mut safe_levels = Vec::new();
    let mut maybe_increasing = None;

    for window in list.windows(2) {
        if let [first, second] = window {
            let diff = second - first;
            let abs_diff = diff.abs();

            if maybe_increasing.is_none() && diff != 0 {
                maybe_increasing = Some(diff > 0);
            }

            let is_safe = abs_diff >= 1 && abs_diff <= 3 && maybe_increasing.unwrap_or(true) == (diff > 0);
            safe_levels.push(is_safe);
        }
    }

    safe_levels
}

fn first_task(lists: &[Vec<i32>]) -> usize {
    lists.iter()
        .filter(|list| calculate_safe_levels(list).iter().all(|&x| x))
        .count()
}

fn second_task(lists: &[Vec<i32>]) -> usize {
    lists.iter()
        .filter(|list| {
            let safe_levels = calculate_safe_levels(list);
            if safe_levels.contains(&false) {
                list.iter().enumerate().any(|(i, _)| {
                    let new_list: Vec<i32> = list.iter().enumerate()
                        .filter(|&(j, _)| j != i)
                        .map(|(_, &val)| val)
                        .collect();
                    calculate_safe_levels(&new_list).iter().all(|&x| x)
                })
            } else {
                safe_levels.iter().all(|&x| x)
            }
        })
        .count()
}

fn main() -> io::Result<()> {
    // Initialize logger with environment variable
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();

    let source_file_name = "./d2/input.txt";

    info!("Read source data from {}", source_file_name);

    let lists = read_lists(source_file_name)?;

    let first_result = first_task(&lists);
    println!("1st Task: Number of safe reports: {}", first_result);

    let second_result = second_task(&lists);
    println!("2nd Task: Safe reports with single bad level: {}", second_result);

    Ok(())
}