use env_logger::Builder;
use log::{error, info, LevelFilter};
use std::cmp::max;
use std::collections::VecDeque;
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct Memory {
    x: usize,
    y: usize,
}

impl Memory {
    fn to_string(&self) -> String {
            format!("{},{}", self.x, self.y)
        }
    
}
fn extract_memory<I>(lines: &mut I) -> Vec<Memory>
where
    I: Iterator<Item = Result<String, io::Error>>,
{
    let mut falling_bytes = Vec::new();
    while let Some(line) = lines.next() {
        let line = line.unwrap();
        if line.trim().is_empty() {
            break;
        }
        let parts = line.trim().split(',').collect::<Vec<&str>>();
        if parts.len() == 2 {
            if let (Ok(x), Ok(y)) = (parts[0].parse::<usize>(), parts[1].parse::<usize>()) {
                falling_bytes.push(Memory { x, y });
            }
        }
    }
    falling_bytes
}

async fn read_file(file_path: &str) -> Result<Vec<Memory>, io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = io::BufReader::new(file);

    let mut lines = reader.lines();
    Ok(extract_memory(&mut lines))
}

fn print_memory(memory: &Vec<Vec<bool>>) {
    for row in memory {
        for cell in row {
            print!("{}", if *cell { '#' } else { '.' });
        }
        println!();
    }
}

fn is_valid(x: usize, y: usize, dimension: usize, memory: &Vec<Vec<bool>>) -> bool {
    x < dimension && y < dimension && !memory[y][x]
}

fn find_shortest_path(memory: &Vec<Vec<bool>>, from: Memory, to: Memory) -> usize {
    let directions: Vec<(isize, isize)> = vec![(0, 1), (1, 0), (0, -1), (-1, 0)];
    let mut queue = VecDeque::new();
    let mut visited = vec![vec![false; memory.len()]; memory.len()];

    queue.push_back((from.clone(), 0));
    visited[from.y][from.x] = true;

    while let Some((current, distance)) = queue.pop_front() {
        if current.x == to.x && current.y == to.y {
            return distance;
        }

        for &(dx, dy) in &directions {
            let moved_x = current.x.checked_add_signed(dx);
            let moved_y = current.y.checked_add_signed(dy);

            match (moved_x, moved_y) {
                (Some(new_x), Some(new_y)) => {
                    if is_valid(new_x, new_y, memory.len(), memory) && !visited[new_y][new_x] {
                        visited[new_y][new_x] = true;
                        queue.push_back((Memory { x: new_x, y: new_y }, distance + 1));
                    }
                }
                _ => {}
            }
        }
    }
    0
}

fn first_task(falling_memory: Vec<Memory>, itterations: usize, dimension: usize) -> usize {
    let mut memory = vec![vec![false; dimension]; dimension];

    let mut counter = 0;
    for byte in falling_memory.iter() {
        memory[byte.y][byte.x] = true;
        counter += 1;
        if counter >= itterations {
            break;
        }
    }
    print_memory(&memory);

    find_shortest_path(&memory, Memory { x: 0, y: 0 }, Memory { x: dimension-1, y: dimension-1 })
}

fn construct_memory_graph(falling_memory: Vec<Memory>, itterations: usize, counter: usize, map: Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let mut memory = map.clone();
    let mut count = counter;
    for byte in falling_memory.iter() {
        memory[byte.y][byte.x] = true;
        count += 1;
        if count >= itterations {
            break;
        }
    }
    memory
}
fn second_task(falling_memory: Vec<Memory>, itter: usize, dimension: usize) -> Memory {
    let size = falling_memory.len();
    let mut map = vec![vec![false; dimension]; dimension];
    let mut itterations = itter + (size - itter)/2;
    let mut counter = 0;
    let mut last_path_size = 0;
    let mut zero_at = 0;
    let from = Memory { x: 0, y: 0 };
    let to = Memory { x: dimension-1, y: dimension-1 };
    while true {
        let next_map= construct_memory_graph(falling_memory.clone(), itterations, 0, map.clone());
        let path_size = find_shortest_path(&next_map, from.clone(), to.clone());
        println!("{} {} {}", counter, itterations, path_size);
        if path_size > 0 && last_path_size == 0 && counter == itterations {
            print_memory(&next_map);
            last_path_size = path_size; 
            break;
        } else if path_size == 0 {
            zero_at = itterations;
            itterations = itterations - max((itterations - counter)/2,1);
            last_path_size = path_size;
        } else {
            counter = itterations;
            itterations = itterations + max((size - itterations)/2,1);
            // map = next_map;
            last_path_size = path_size;
        }
    }
    falling_memory[itterations].clone()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_memory_from_str(input: &str) -> Vec<Memory> {
        let mut lines = input.lines().map(|line| Ok(line.to_string()));
        extract_memory(&mut lines)
    }
    
    const MACHINES: &str = 
r#"5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"#;

    #[test]
    fn first_task_with_first_sample() {
        let falling_bytes: Vec<Memory> = read_memory_from_str(MACHINES);

        let result = first_task(falling_bytes, 12, 7);
        assert_eq!(result, 22);
    }

    #[test]
    fn second_task_with_first_sample() {
        let falling_bytes: Vec<Memory> = read_memory_from_str(MACHINES);

        let result = second_task(falling_bytes, 12, 7);
        assert_eq!(result.to_string(), "6,1");
    }

}

#[tokio::main]
async fn main() {
    Builder::new()
        .parse_env("LOG_LEVEL")
        .filter_level(LevelFilter::Info)
        .init();
    
    const ITTERATIONS: usize = 1024;
    const DIMENSION: usize = 71;

    let source_file_name = "./d18/input.txt";

    match read_file(source_file_name).await {
        Ok(falling_memory) => {
            let first_result = first_task(falling_memory.clone(), ITTERATIONS, DIMENSION);
            info!("1st Task: {}", first_result);
            let second_result = second_task(falling_memory.clone(), ITTERATIONS, DIMENSION);
            info!("2nd Task: {}", second_result.to_string());

        }
        Err(e) => {
            error!("Error reading source data: {}", e);
        }
    }
}
