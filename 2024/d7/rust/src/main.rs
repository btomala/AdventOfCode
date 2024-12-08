use std::fs;
use std::io::{self, BufRead};
use std::path::Path;
use std::str::FromStr;

type EQS = Vec<(i64, Vec<i64>)>;

fn read_lists<P>(filename: P) -> io::Result<EQS>
where
    P: AsRef<Path>,
{
    let file = fs::File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut eqs = Vec::new();

    for line in reader.lines() {
        let line = line?;
        if !line.is_empty() {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() == 2 {
                let head = i64::from_str(parts[0].trim()).unwrap();
                let numbers: Vec<i64> = parts[1]
                    .trim()
                    .split_whitespace()
                    .filter_map(|s| s.parse::<i64>().ok())
                    .collect();
                eqs.push((head, numbers));
            }
        }
    }
    Ok(eqs)
}

fn concat(x: i64, y: i64) -> i64 {
    format!("{}{}", x, y).parse().unwrap()
}

fn calc1(agg: i64, ing: &[i64], result: i64) -> bool {
    match ing {
        [] => agg == result,
        [head] => agg * head == result || agg + head == result ,
        [head, tail @ ..] => {
            calc1(head * agg, tail, result) || calc1(head + agg, tail, result)
        }
    }
}

fn calc2(agg: i64, ing: &[i64], result: i64) -> bool {
    match ing {
        [] => agg == result,
        [head] => agg * head == result || agg + head == result || concat(agg, *head) == result,
        [head, tail @ ..] => {
            calc2(head * agg, tail, result)
                || calc2(head + agg, tail, result)
                || calc2(concat(agg, *head), tail, result)
        }
    }
}

fn first_task(eqs: &EQS) -> i64 {
    eqs.iter()
        .filter_map(|&(result, ref numbers)| {
            if calc1(numbers[0], &numbers[1..], result) {
                Some(result)
            } else {
                None
            }
        })
        .sum()
}

fn second_task(eqs: &EQS) -> i128 {
    eqs.iter()
        .filter_map(|&(result, ref numbers)| {
            if calc2(numbers[0], &numbers[1..], result) {
                Some(result as i128)
            } else {
                None
            }
        })
        .sum()
}

fn main() -> io::Result<()> {
    let source_file_name = "./d7/input.txt";
    println!("Read source data from {}", source_file_name);
    let eqs = read_lists(source_file_name)?;

    let first_result = first_task(&eqs);
    println!("1st Task: {}", first_result);

    let second_result = second_task(&eqs);
    println!("2nd Task: {}", second_result);

    Ok(())
}