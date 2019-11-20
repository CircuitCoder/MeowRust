use crate::parser;
use std::fs;

#[test]
fn test_expr() {
    let dir = fs::read_dir("testcases/parsing/expr").unwrap();
    for file in dir {
        let path = file.unwrap().path();
        println!("testing {}", path.display());
        let content = fs::read_to_string(path).unwrap();
        let parsed = parser::expr::expr(&content);
        assert!(parsed.is_ok());
    }
}

#[test]
fn test_item() {
    let dir = fs::read_dir("testcases/parsing/item").unwrap();
    for file in dir {
        let path = file.unwrap().path();
        println!("testing {}", path.display());
        let content = fs::read_to_string(path).unwrap();
        let parsed = parser::item::item(&content);
        assert!(parsed.is_ok());
    }
}
