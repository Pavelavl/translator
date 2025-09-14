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
        if self.find_by_name(&name).is_none() {
            let id = self.identifiers.len();
            self.identifiers.push(Identifier {
                name,
                category,
                data_type,
                value: None,
            });
            Some(id)
        } else {
            None
        }
    }

    pub fn add_or_update(&mut self, name: String, category: Category, data_type: DataType, value: Option<i32>) {
        if let Some(ident) = self.identifiers.iter_mut().find(|i| i.name == name) {
            ident.category = category;
            ident.data_type = data_type;
            ident.value = value;
        } else {
            self.identifiers.push(Identifier {
                name,
                category,
                data_type,
                value,
            });
        }
    }

    pub fn find_by_name(&self, name: &str) -> Option<&Identifier> {
        self.identifiers.iter().find(|i| i.name == name)
    }

    pub fn get_all_identifiers(&self) -> &Vec<Identifier> {
        &self.identifiers
    }
}