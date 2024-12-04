use env_logger::Builder;
use log::{error, info, LevelFilter};
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Clone)]
struct Matrix {
    aoa: Vec<Vec<char>>,
}

impl Matrix {
    fn x_size(&self) -> i32 {
        self.aoa[0].len() as i32
    }

    fn y_size(&self) -> i32 {
        self.aoa.len() as i32
    }

    fn get(&self, point: &Vector) -> Option<char> {
        if 0 <= point.x && point.x < self.x_size() && 0 <= point.y && point.y < self.y_size() {
            Some(self.aoa[point.y as usize][point.x as usize])
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct Vector {
    x: i32,
    y: i32,
}

impl Vector {
    fn add(&self, v: &Vector) -> Vector {
        Vector {
            x: self.x + v.x,
            y: self.y + v.y,
        }
    }

    fn sub(&self, v: &Vector) -> Vector {
        Vector {
            x: self.x - v.x,
            y: self.y - v.y,
        }
    }
}

const ALL_DIRECTIONS: [Vector; 8] = [
    Vector { x: -1, y: -1 },
    Vector { x: -1, y: 0 },
    Vector { x: -1, y: 1 },
    Vector { x: 0, y: -1 },
    Vector { x: 0, y: 1 },
    Vector { x: 1, y: -1 },
    Vector { x: 1, y: 0 },
    Vector { x: 1, y: 1 },
];

const X_DIRECTIONS: [Vector; 4] = [
    Vector { x: -1, y: -1 },
    Vector { x: -1, y: 1 },
    Vector { x: 1, y: -1 },
    Vector { x: 1, y: 1 },
];
fn find_pattern(matrix: &Matrix, pattern: &str, position: Vector, shift: Vector) -> i32 {
    if !pattern.is_empty() {
        let next_position = position.add(&shift);
        if let Some(ch) = matrix.get(&next_position) {
            if ch == pattern.chars().next().unwrap() {
                return find_pattern(matrix, &pattern[1..], next_position, shift);
            }
        }
        0
    } else {
        1
    }
}

async fn read_lists(file_path: &str) -> Result<Matrix, io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = io::BufReader::new(file);
    let mut aoa = Vec::new();

    for line in reader.lines() {
        let line = line?;
        aoa.push(line.chars().collect());
    }

    Ok(Matrix { aoa })
}

async fn first_task(matrix: &Matrix) -> i32 {
    let pattern = "XMAS";
    let first_char = pattern.chars().next().unwrap();
    let mut count = 0;

    for (y, x_array) in matrix.aoa.iter().enumerate() {
        for (x, &char) in x_array.iter().enumerate() {
            if char == first_char {
                let current_position = Vector {
                    x: x as i32,
                    y: y as i32,
                };
                count += ALL_DIRECTIONS
                    .map(|shift| {
                        find_pattern(matrix, &pattern[1..], current_position.clone(), shift)
                    })
                    .iter()
                    .sum::<i32>();
            }
        }
    }
    count
}

async fn second_task(matrix: &Matrix) -> i32 {
    let mut count = 0;

    for (y, x_array) in matrix.aoa.iter().enumerate() {
        for (x, &c) in x_array.iter().enumerate() {
            if c == 'A' {
                let a_position = Vector {
                    x: x as i32,
                    y: y as i32,
                };
                count += X_DIRECTIONS
                    .iter()
                    .flat_map(|shift| {
                        matrix
                            .get(&a_position.add(shift))
                            .filter(|&x| x == 'M')
                            .into_iter()
                            .flat_map(|_| matrix.get(&a_position.sub(shift)))
                            .filter(|&x| x == 'S')
                            .map(|_| 0.5)
                            .collect::<Vec<_>>()
                    })
                    .sum::<f32>() as i32;
            }
        }
    }

    count
}

#[tokio::main]
async fn main() {
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();

    let source_file_name = "./d4/input.txt";

    match read_lists(source_file_name).await {
        Ok(matrix) => {
            let first_result = first_task(&matrix).await;
            info!("1st Task: XMAS occurs: {}", first_result);

            let second_result = second_task(&matrix).await;
            info!("2nd Task: X-MAS occurs: {}", second_result);
        }
        Err(e) => {
            error!("Error reading source data: {}", e);
        }
    }
}
