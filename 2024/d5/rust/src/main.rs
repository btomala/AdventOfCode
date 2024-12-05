use std::collections::{HashMap, HashSet};
use std::io::{self};
use tokio::fs;
use tokio::io::{AsyncBufReadExt, BufReader};

type OrderRules = HashMap<usize, HashSet<usize>>;
type Pages = Vec<Vec<usize>>;

async fn read_lists(file_path: &str) -> io::Result<(OrderRules, Pages)> {
    let file = fs::File::open(file_path).await?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let mut orders = HashMap::new();
    let mut pages = Vec::new();

    // Read orders
    while let Some(line) = lines.next_line().await? {
        if line.trim().is_empty() {
            break;
        }
        let parts: Vec<usize> = line.split('|').map(|s| s.trim().parse().unwrap()).collect();
        if let [before, after] = parts[..] {
            orders
                .entry(before)
                .or_insert_with(HashSet::new)
                .insert(after);
        }
    }

    // Read pages
    while let Some(line) = lines.next_line().await? {
        let page: Vec<usize> = line.split(',').map(|s| s.trim().parse().unwrap()).collect();
        pages.push(page);
    }

    Ok((orders, pages))
}

fn identify_correct_updates(order_rules: &OrderRules, pages: &[usize]) -> (bool, Vec<usize>) {
    let is_broken = pages.iter().any(|&page| {
        let empty_set = HashSet::new();
        let after_page = order_rules.get(&page).unwrap_or(&empty_set);
        let before = &pages[..pages.iter().position(|&p| p == page).unwrap()];
        before.iter().any(|&b| after_page.contains(&b))
    });
    (is_broken, pages.to_vec())
}

fn first_task(order_rules: &OrderRules, updates: &[Vec<usize>]) -> usize {
    updates
        .iter()
        .map(|pages| identify_correct_updates(order_rules, pages))
        .filter(|(is_broken, _)| !is_broken)
        .flat_map(|(_, pages)| pages.get((pages.len() / 2) as usize).cloned())
        .sum()
}

fn second_task(order_rules: &OrderRules, updates: &[Vec<usize>]) -> usize {
    updates
        .iter()
        .map(|pages| identify_correct_updates(order_rules, pages))
        .filter(|(is_broken, _)| is_broken)
        .flat_map(|(_, pages)| {
            let mut sorted_pages = pages.clone();
            sorted_pages.sort_by(|a, b| {
                if order_rules.get(a).map_or(false, |set| set.contains(b)) {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            });
            let half = (sorted_pages.len() / 2) as usize;
            sorted_pages.get(half).cloned()
        })
        .sum()
}

#[tokio::main]
async fn main() -> io::Result<()> {
    let source_file_name = "./d5/input.txt";

    match read_lists(source_file_name).await {
        Ok((orders, pages)) => {
            let first_result = first_task(&orders, &pages);
            println!("1st Task: sum of correct middle pages is: {}", first_result);

            let second_result = second_task(&orders, &pages);
            println!(
                "2nd Task: sum of corrected middle pages is: {}",
                second_result
            );
        }
        Err(e) => {
            eprintln!("Error reading source data: {}", e);
        }
    }

    Ok(())
}
