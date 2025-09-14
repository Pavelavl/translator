use crate::models::{Category, DataType, Identifier};
use std::collections::HashMap;

pub struct NameTable {
    identifiers: HashMap<String, Identifier>,
}

impl NameTable {
    pub fn new() -> Self {
        NameTable { identifiers: HashMap::new() }
    }

    pub fn add_or_update(&mut self, name: String, category: Category, data_type: DataType) {
        self.identifiers.insert(name.clone(), Identifier::new(name, category, data_type));
    }

    pub fn find_by_name(&self, name: &str) -> Option<&Identifier> {
        self.identifiers.get(name)
    }

    pub fn get_all_identifiers(&self) -> Vec<&Identifier> {
        self.identifiers.values().collect()
    }

    pub fn add(&mut self, name: String, category: Category, data_type: DataType) {
        self.add_or_update(name, category, data_type);
    }
}