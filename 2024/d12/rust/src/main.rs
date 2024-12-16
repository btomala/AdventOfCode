use env_logger::Builder;
use log::{error, info, LevelFilter};
use std::fs;
use std::io::{self, BufRead};
use std::collections::{HashMap, HashSet, VecDeque};

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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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
}

const DIRECTIONS: [Vector; 4] = [
    Vector { x: -1, y: 0 },
    Vector { x: 0, y: -1 },
    Vector { x: 0, y: 1 },
    Vector { x: 1, y: 0 },
];

fn find_cluster(matrix: &Matrix, start_position: Vector) -> HashMap<Vector, usize> {
    matrix.get(&start_position).map_or(HashMap::new(), |start_char| {
        let mut cluster = HashMap::new();
        let mut stack = VecDeque::new();
        stack.push_back(start_position);

        while let Some(position) = stack.pop_front() {
            let mut neighbour = 0;
            if cluster.contains_key(&position) {
                continue;
            }

            for shift in DIRECTIONS.iter() {
                let next_position = position.add(shift);
                if matrix.get(&next_position).map_or(false, |ch| ch == start_char) {
                    neighbour += 1;
                    stack.push_back(next_position);
                }
            }
            let fances = 4 - neighbour;
            cluster.insert(position.clone(), fances);

        }

        cluster
    })
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

fn first_task(matrix: &Matrix) -> usize {
    let mut count = 0;
    let mut visited: HashSet<Vector> = HashSet::new();

    for (y, x_array) in matrix.aoa.iter().enumerate() {
        for (x, _) in x_array.iter().enumerate() {
            let current_position = Vector {
                x: x as i32,
                y: y as i32,
            };
            if visited.contains(&current_position) {
                continue;
            }
            let cluster = find_cluster(&matrix, current_position);
            count += cluster.len() * cluster.values().sum::<usize>();
            let cluster_keys: Vec<_> = cluster.keys().cloned().collect();
            visited.extend(cluster_keys);
        }
    }
    count
}

fn second_task(matrix: &Matrix) -> usize {
    first_task(matrix)
}

#[tokio::main]
async fn main() {
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();

    let source_file_name = "./d12/input.txt";

    match read_lists(source_file_name).await {
        Ok(matrix) => {
            let first_result = first_task(&matrix);
            info!("1st Task: {}", first_result);

            let second_result = second_task(&matrix);
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

    #[test]
    fn first_task_with_first_sample() {
        let first_sample_map = Matrix {
            aoa: vec![
                vec!['A', 'A', 'A', 'A'],
                vec!['B', 'B', 'C', 'D'],
                vec!['B', 'B', 'C', 'C'],
                vec!['E', 'E', 'E', 'C'],
            ]
        };

        let result = first_task(&first_sample_map);
        assert_eq!(result, 140);
    }

    #[test]
    fn first_task_with_second_sample() {
        let second_sample_map = Matrix {
            aoa: vec![
                vec!['O', 'O', 'O', 'O', 'O'],
                vec!['O', 'X', 'O', 'X', 'O'],
                vec!['O', 'O', 'O', 'O', 'O'],
                vec!['O', 'X', 'O', 'X', 'O'],
                vec!['O', 'O', 'O', 'O', 'O'],
            ]
        };

        let result = first_task(&second_sample_map);
        assert_eq!(result, 772);
    }

    #[test]
    fn first_task_with_third_sample() {
        let third_sample_map = Matrix {
            aoa: vec![
                vec!['R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'F', 'F'],
                vec!['R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'C', 'F'],
                vec!['V', 'V', 'R', 'R', 'R', 'C', 'C', 'F', 'F', 'F'],
                vec!['V', 'V', 'R', 'C', 'C', 'C', 'J', 'F', 'F', 'F'],
                vec!['V', 'V', 'V', 'V', 'C', 'J', 'J', 'C', 'F', 'E'],
                vec!['V', 'V', 'I', 'V', 'C', 'C', 'J', 'J', 'E', 'E'],
                vec!['V', 'V', 'I', 'I', 'I', 'C', 'J', 'J', 'E', 'E'],
                vec!['M', 'I', 'I', 'I', 'I', 'I', 'J', 'J', 'E', 'E'],
                vec!['M', 'I', 'I', 'I', 'S', 'I', 'J', 'E', 'E', 'E'],
                vec!['M', 'M', 'M', 'I', 'S', 'S', 'J', 'E', 'E', 'E'],
            ]
        };

        let result = first_task(&third_sample_map);
        assert_eq!(result, 1930);
    }
}