use crate::models::{Category, DataType, Identifier};

pub struct NameTable {
    pub identifiers: Vec<Identifier>,
}

impl NameTable {
    pub fn new() -> Self {
        NameTable {
            identifiers: Vec::new(),
        }
    }

    pub fn add(&mut self, name: String, category: Category, data_type: DataType) -> Option<usize> {
        println!("[NameTable] Adding identifier: name='{}', category={:?}, data_type={:?}", name, category, data_type);
        if self.find_by_name(&name).is_none() {
            let id = self.identifiers.len();
            self.identifiers.push(Identifier {
                name: name.clone(),
                category,
                data_type,
                value: None,
            });
            println!("[NameTable] Added identifier '{}' at index {}", name, id);
            Some(id)
        } else {
            println!("[NameTable] Identifier '{}' already exists", name);
            None
        }
    }

    pub fn add_or_update(&mut self, name: String, category: Category, data_type: DataType, value: Option<i32>) {
        println!("[NameTable] Adding or updating identifier: name='{}', category={:?}, data_type={:?}, value={:?}", name, category, data_type, value);
        if let Some(ident) = self.identifiers.iter_mut().find(|i| i.name == name) {
            ident.category = category;
            ident.data_type = data_type;
            ident.value = value;
            println!("[NameTable] Updated existing identifier '{}'", name);
        } else {
            self.identifiers.push(Identifier {
                name,
                category,
                data_type,
                value,
            });
            println!("[NameTable] Added new identifier '{}'", self.identifiers.last().unwrap().name);
        }
    }

    pub fn find_by_name(&self, name: &str) -> Option<&Identifier> {
        let result = self.identifiers.iter().find(|i| i.name == name);
        println!("[NameTable] Searching for identifier '{}', found: {:?}", name, result.is_some());
        result
    }

    pub fn entries(&self) -> &Vec<Identifier> {
        &self.identifiers
    }
}