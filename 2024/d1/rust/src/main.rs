use std::fs::File;
use std::io::{BufReader, BufRead};
use std::path::Path;
use log::{info, LevelFilter};
use env_logger::Builder;

fn read_lists(filename: &str) -> Result<Vec<(i32, i32)>, Box<dyn std::error::Error>> {
    let file = File::open(Path::new(filename))?;
    let reader = BufReader::new(file);
    
    let mut pairs = Vec::new();
    
    for line in reader.lines() {
        let line = line?;
        let numbers: Vec<i32> = line
            .split("   ")
            .filter_map(|s| s.trim().parse().ok())
            .collect();
        
        if numbers.len() == 2 {
            pairs.push((numbers[0], numbers[1]));
        }
    }
    
    Ok(pairs)
}

fn first_task(pairs: &[(i32, i32)]) -> i32 {
    let mut left: Vec<i32> = pairs.iter().map(|&(l, _)| l).collect();
    let mut right: Vec<i32> = pairs.iter().map(|&(_, r)| r).collect();
    
    left.sort();
    right.sort();
    
    left.iter()
        .zip(right.iter())
        .map(|(l, r)| (l - r).abs())
        .sum()
}

fn second_task(pairs: &[(i32, i32)]) -> i32 {
    let mut left: Vec<i32> = pairs.iter().map(|&(l, _)| l).collect();
    let right: Vec<i32> = pairs.iter().map(|&(_, r)| r).collect();
    
    left.sort();
    
    left.iter()
        .map(|&d| d * right.iter().filter(|&&r| r == d).count() as i32)
        .sum()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logger with environment variable
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();
    
    let source_file_name = "./d1/input.txt";
    
    info!("Read source data from {}", source_file_name);
    
    let pairs = read_lists(source_file_name)?;
    
    let first_result = first_task(&pairs);
    println!("1st Task: Total distance is: {}", first_result);
    
    let second_result = second_task(&pairs);
    println!("2nd Task: Similarity score is: {}", second_result);
    
    Ok(())
} 