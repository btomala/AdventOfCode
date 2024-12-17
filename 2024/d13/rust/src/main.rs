use env_logger::Builder;
use log::{error, info, LevelFilter};
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct Vector {
    x: i64,
    y: i64,
}

#[derive(Debug)]
struct Machine {
    button_a: Vector,
    button_b: Vector,
    prize: Vector
}

fn extract_vector(line: &str, re: &regex::Regex) -> Vector {
    let caps = re.captures(line).unwrap();
    Vector {
        x: caps[1].parse().unwrap(),
        y: caps[2].parse().unwrap()
    }
}

fn extract_machines<I>(lines: &mut I) -> Vec<Machine>
where
    I: Iterator<Item = Result<String, io::Error>>,
{
    let mut aoa = Vec::new();
    let button_a_re = regex::Regex::new(r"Button A: X\+(\d+), Y\+(\d+)").unwrap();
    let button_b_re = regex::Regex::new(r"Button B: X\+(\d+), Y\+(\d+)").unwrap();
    let prize_re = regex::Regex::new(r"Prize: X=(\d+), Y=(\d+)").unwrap();

    while let Some(line_a) = lines.next() {
        let line_a = line_a.unwrap();
        let line_b = lines.next().unwrap().unwrap();
        let line_p = lines.next().unwrap().unwrap();
        let _ = lines.next();
        aoa.push(Machine {
            button_a: extract_vector(&line_a, &button_a_re),
            button_b: extract_vector(&line_b, &button_b_re),
            prize: extract_vector(&line_p, &prize_re),
        });
    }
    aoa
}

async fn read_file(file_path: &str) -> Result<Vec<Machine>, io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = io::BufReader::new(file);

    let mut lines = reader.lines();
    Ok(extract_machines(&mut lines))
}

const A_COST: usize = 3;
const B_COST: usize = 1;

fn find_lowest_cost(a_pressed: usize, b_pressed: usize, costs: usize) -> usize {
    let current_cost = a_pressed * A_COST + b_pressed * B_COST;
    if costs > current_cost || costs == 0 {
        current_cost
    } else {
        costs
    }
}

fn find_lowest_cost_with_limit(a_pressed: usize, b_pressed: usize, costs: usize) -> usize {
    if a_pressed <= 100 && b_pressed <= 100 {
        find_lowest_cost(a_pressed, b_pressed, costs)
    } else {
        costs
    }
}

fn find_chepest_prize_with_limit(machine: &Machine) -> usize {
    let mut a_pressed: usize = 0;
    let mut b_pressed: usize = 0;

    let mut x = machine.prize.x;
    let mut y = machine.prize.y;
    let mut costs : usize = 0;

    while y > 0 && x > 0 {
        if x % machine.button_b.x == 0 {
            if y % machine.button_b.y == 0 {
                if x/machine.button_b.x == y/machine.button_b.y {
                    b_pressed = (x / machine.button_b.x) as usize;
                    costs = find_lowest_cost_with_limit(a_pressed, b_pressed, costs);
                }
            } 
        }
        x -= machine.button_a.x;
        y -= machine.button_a.y;
        a_pressed += 1;
    }

    costs
}

fn first_task(machines: &Vec<Machine>) -> usize {
    let mut count = 0;

    for machine in machines.iter() {
        let min_tokens = find_chepest_prize_with_limit(&machine);
        println!("{:?} - {:?}", min_tokens, &machine);
        count += min_tokens
    }
    count
}

const PRECISION_ERROR: i64 = 10000000000000;

fn find_chepest_prize(machine: &Machine) -> usize {
    let mut a_pressed: usize = 0;
    let mut b_pressed: usize = 0;

    let mut x = machine.prize.x;
    let mut y = machine.prize.y;
    let mut costs : usize = 0;

    while y > 0 && x > 0 {
        if x % machine.button_b.x == 0 {
            if y % machine.button_b.y == 0 {
                if x/machine.button_b.x == y/machine.button_b.y {
                    b_pressed = (x / machine.button_b.x) as usize;
                    costs = find_lowest_cost_with_limit(a_pressed, b_pressed, costs);
                }
            } 
        }
        x -= machine.button_a.x;
        y -= machine.button_a.y;
        a_pressed += 1;
    }
    costs
}

fn second_task(machines: &Vec<Machine>) -> usize {
    let fixed_machines: Vec<Machine> = machines.iter().map(|machine| Machine {
        button_a: machine.button_a.clone(),
        button_b: machine.button_b.clone(),
        prize: Vector { x: machine.prize.x + PRECISION_ERROR, y: machine.prize.y + PRECISION_ERROR }
    }).collect();
    let mut count = 0;

    for machine in fixed_machines.iter() {
        let min_tokens = find_chepest_prize(&machine);
        println!("{:?} - {:?}", min_tokens, &machine);
        count += min_tokens
    }
    count
}

#[tokio::main]
async fn main() {
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();

    let source_file_name = "./d13/input.txt";

    match read_file(source_file_name).await {
        Ok(machines) => {
            let first_result = first_task(&machines);
            info!("1st Task: {}", first_result);
            let second_result = second_task(&machines);
            info!("2nd Task: {}", second_result);

        }
        Err(e) => {
            error!("Error reading source data: {}", e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_machines_from_str(input: &str) -> Vec<Machine> {
        let mut lines = input.lines().map(|line| Ok(line.to_string()));
        extract_machines(&mut lines)
    }
    
    const MACHINES: &str = 
     r#"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"#;

    #[test]
    fn first_task_with_first_sample() {
        let machines: Vec<Machine> = read_machines_from_str(MACHINES);

        let result = first_task(&machines);
        assert_eq!(result, 480);
    }

    #[test]
    fn second_task_with_first_sample() {
        let machines: Vec<Machine> = read_machines_from_str(MACHINES);

        let result = second_task(&machines);
        assert_eq!(result, 0);
    }

}