use env_logger::Builder;
use log::{error, info, LevelFilter};
use std::fs;
use std::io::{self, BufRead};
use std::collections::VecDeque;

fn extract<I>(lines: &mut I) -> (Vec<String>, Vec<String>)
where
    I: Iterator<Item = Result<String, io::Error>>,
{
    let mut patterns = Vec::new();
    while let Some(line) = lines.next() {
        let line = line.unwrap();
        if line.trim().is_empty() {
            break;
        }
        patterns = line.trim().split(", ").map(|str| str.trim().to_string()).collect::<Vec<String>>();
    
    }

    let mut designs = Vec::new();
    while let Some(line) = lines.next() {
        let line = line.unwrap();
        if line.trim().is_empty() {
            break;
        }
        let design = line.trim().to_string();
        designs.push(design.clone());
    }
    (patterns, designs)
}

async fn read_file(file_path: &str) -> Result<(Vec<String>, Vec<String>), io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = io::BufReader::new(file);

    let mut lines = reader.lines();
    Ok(extract(&mut lines))
}

fn find_pattern(dsgn: &str, patterns: &Vec<String>) -> bool {
    let mut queue: VecDeque<String> = VecDeque::new();
    queue.push_back(dsgn.to_string());
    let mut found = false;
    while let Some(design) = queue.pop_back()  {
        for pattern in patterns.iter() {
            if design.starts_with(pattern) {
                let mut d = design.clone();
                d.replace_range(0..pattern.len(), "");
                queue.push_back(d);
            }
        }
        if design == "" {
            found = true;
            break;
        }
    }
    println!("{:?} - {:?}", dsgn, found);
    found
}

fn find_all_patterns(dsgn: &str, patterns: &Vec<String>) -> usize {
    let mut queue: VecDeque<String> = VecDeque::new();
    queue.push_back(dsgn.to_string());
    let mut found = 0;
    while let Some(design) = queue.pop_back()  {
        for pattern in patterns.iter() {
            if design.starts_with(pattern) {
                let mut d = design.clone();
                d.replace_range(0..pattern.len(), "");
                if d == "" {
                    found += 1;
                } else {
                    queue.push_back(d);
                }
            }
        }
    }
    println!("{:?} - {:?}", dsgn, found);
    found
}

fn find_all_possible_patterns(dsgn: &str, patterns: &Vec<(String, usize)>) -> usize {
    let mut queue: VecDeque<(String, usize)> = VecDeque::new();
    queue.push_back((dsgn.to_string(), 1));
    let mut found = 0;
    while let Some((design, possible_pat)) = queue.pop_back()  {
        for (pattern, subpat) in patterns.iter() {
            if design.starts_with(pattern) {
                let mut d = design.clone();
                d.replace_range(0..pattern.len(), "");
                queue.push_back((d, possible_pat * subpat));
            }
        }
        if design == "" {
            found = possible_pat;
            break;
        }
    }
    println!("{:?} - {:?}", dsgn, found);
    found
}

fn first_task(patterns: Vec<String>, designs: Vec<String>) -> usize {
    let mut possible = 0;
    for design in designs {
        let found = find_pattern(&design, &patterns);
        if found {
            // println!("{:?}", design);
            possible += 1;
        }
    }
    possible
}

fn second_task(patterns: Vec<String>, designs: Vec<String>) -> usize {
    let mut possible = 0;
    let mut patterns_with_subpatterns = Vec::new();
    for pat in patterns.clone() {
        let subpatterns = find_all_patterns(&pat, &patterns);
        patterns_with_subpatterns.push((pat.clone(), subpatterns));

    }
    println!("======");
    for design in designs {
        possible += find_all_possible_patterns(&design, &patterns_with_subpatterns);
    }
    possible
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_from_str(input: &str) -> (Vec<String>, Vec<String>) {
        let mut lines = input.lines().map(|line| Ok(line.to_string()));
        extract(&mut lines)
    }
    
    const INPUT: &str = 
r#"r, wr, b, g, bwu, rb, gb, br, wut, rrbgbr

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
bwut
"#;

    #[test]
    fn first_task_with_first_sample() {
        let (patterns, designs) = read_from_str(INPUT);
        let mut patterns_desc = patterns.clone();
        patterns_desc.sort_by(|a, b| b.len().cmp(&a.len()));

        let result = first_task(patterns_desc, designs);
        assert_eq!(result, 7);
    }

    #[test]
    fn second_task_with_first_sample() {
        let (patterns, designs) = read_from_str(INPUT);

        let result = second_task(patterns, designs);
        assert_eq!(result, 17);
    }

}

#[tokio::main]
async fn main() {
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();
    
    let source_file_name = "./d19/input.txt";

    match read_file(source_file_name).await {
        Ok((patterns, designs)) => {
            let mut patterns_desc = patterns.clone();
            patterns_desc.sort_by(|a, b| b.len().cmp(&a.len()));
        
            let first_result = first_task(patterns_desc.clone(), designs.clone());
            info!("1st Task: {}", first_result);
            let second_result = second_task(patterns_desc.clone(), designs.clone()); 
            info!("2nd Task: {}", second_result);
        }
        Err(e) => {
            error!("Error reading source data: {}", e);
        }
    }
}
