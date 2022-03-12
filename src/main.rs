use std::fs;

use parser::ScoreList;


pub mod parser;


fn main() {
    
    let mut db = ScoreList::from_file("C:\\Users\\seleneia\\Documents\\code\\RUST\\mcosudb\\target\\debug\\scores.db").unwrap();
    let j = serde_json::to_string(&db.beatmaps).unwrap();
    fs::write("db.json", j.as_bytes()).expect("Unable to write file");
    
    
}
