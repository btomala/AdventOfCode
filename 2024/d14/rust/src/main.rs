use env_logger::Builder;
use log::{error, info, LevelFilter};
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct Vector {
    x: i32,
    y: i32,
}

#[derive(Debug, Clone)]
struct Robot {
    position: Vector,
    velocity: Vector
}

impl Robot {
    fn move_to(&self, wide: i32, tall: i32) -> Robot {
        let new_x_position = {
            let position= self.position.x + self.velocity.x;
            if position >= wide {
                position - wide
            } else if position < 0 {
                wide + position
            } else {
                self.position.x + self.velocity.x
            }
        };
        let new_y_position = {
            let position= self.position.y + self.velocity.y;
            if position >= tall {
                position - tall
            } else if position < 0 {
                tall + position
            } else {
                self.position.y + self.velocity.y
            }
        };
        Robot {
            position: Vector {
                x: new_x_position,
                y: new_y_position
            },
            velocity: self.velocity.clone()
        }
    }
        
}
fn extract_robot(line: &str, re: &regex::Regex) -> Robot {
    let caps = re.captures(line).unwrap();
    Robot {
        position: Vector {
            x: caps[1].parse().unwrap(),
            y: caps[2].parse().unwrap()
        },
        velocity: Vector {
            x: caps[3].parse().unwrap(),
            y: caps[4].parse().unwrap()
        }
    }
}

fn extract_robots<I>(lines: &mut I) -> Vec<Robot>
where
    I: Iterator<Item = Result<String, io::Error>>,
{
    let mut aoa = Vec::new();
    let robot_re = regex::Regex::new(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)").unwrap();

    while let Some(line) = lines.next() {
        let line = line.unwrap();
        if !line.trim().is_empty() {
            aoa.push(extract_robot(&line, &robot_re));
        }
    }
    aoa
}

async fn read_file(file_path: &str) -> Result<Vec<Robot>, io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = io::BufReader::new(file);

    let mut lines = reader.lines();
    Ok(extract_robots(&mut lines))
}

fn first_task(machines: &[Robot], wide: usize, tall: usize) -> i32 {
    const MAX_ITERATIONS: usize = 100;
    let mut robots: Vec<Robot> = machines.to_vec();

    for _ in 0..MAX_ITERATIONS {
        robots = robots.into_iter().map(|robot| {
            robot.move_to(wide as i32, tall as i32)
        }).collect();
    }

    let middle_wide = (wide / 2) as i32;
    let middle_tall = (tall / 2) as i32;
    println!("{:?} {:?}", middle_wide, middle_tall);
    let mut top_left: i32 = 0;
    let mut top_right:i32 = 0;
    let mut bottom_left:i32 = 0;
    let mut bottom_right:i32 = 0;
    robots.iter().for_each(|robot| {
        println!("{:?}" , robot);
        if robot.position.x < middle_wide && robot.position.y < middle_tall {
            top_left += 1;
        } else if robot.position.x > middle_wide && robot.position.y < middle_tall {
            top_right += 1;
        } else if robot.position.x < middle_wide && robot.position.y > middle_tall {
            bottom_left += 1;
        } else if robot.position.x > middle_wide && robot.position.y > middle_tall {
            bottom_right += 1;
        }
    });
    println!("{:?} {:?} {:?} {:?}", top_left, top_right, bottom_left, bottom_right);
    top_left * top_right * bottom_left * bottom_right
}


#[tokio::main]
async fn main() {
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();

    let source_file_name = "./d14/input.txt";
    const SPACE_WIDE: usize = 101;
    const SPACE_TALL: usize = 103;

    match read_file(source_file_name).await {
        Ok(machines) => {
            let first_result = first_task(&machines, SPACE_WIDE, SPACE_TALL);
            info!("1st Task: {}", first_result);
            // let second_result = second_task(&machines);
            // info!("2nd Task: {}", second_result);

        }
        Err(e) => {
            error!("Error reading source data: {}", e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_robots_from_str(input: &str) -> Vec<Robot> {
        let mut lines = input.lines().map(|line| Ok(line.to_string()));
        extract_robots(&mut lines)
    }
    
    const MACHINES: &str = 
r#"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"#;

    #[test]
    fn first_task_with_first_sample() {
        let machines: Vec<Robot> = read_robots_from_str(MACHINES);

        let result = first_task(&machines, 11, 7);
        assert_eq!(result, 12);
    }

}