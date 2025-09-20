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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_name_table() {
        let table = NameTable::new();
        assert_eq!(table.identifiers.len(), 0, "New NameTable should have no identifiers");
    }

    #[test]
    fn test_add_new_identifier() {
        let mut table = NameTable::new();
        let result = table.add(
            String::from("x"),
            Category::Var,
            DataType::Int,
        );
        assert_eq!(result, Some(0), "Adding new identifier should return index 0");
        assert_eq!(table.identifiers.len(), 1, "Table should contain one identifier");
        assert_eq!(table.identifiers[0].name, "x", "Identifier name should be 'x'");
        assert_eq!(table.identifiers[0].category, Category::Var, "Category should be Var");
        assert_eq!(table.identifiers[0].data_type, DataType::Int, "DataType should be Int");
        assert_eq!(table.identifiers[0].value, None, "Value should be None");
    }

    #[test]
    fn test_add_duplicate_identifier() {
        let mut table = NameTable::new();
        table.add(String::from("x"), Category::Var, DataType::Int);
        let result = table.add(String::from("x"), Category::Const, DataType::Bool);
        assert_eq!(result, None, "Adding duplicate identifier should return None");
        assert_eq!(table.identifiers.len(), 1, "Table should still contain only one identifier");
    }

    #[test]
    fn test_add_or_update_new_identifier() {
        let mut table = NameTable::new();
        table.add_or_update(
            String::from("y"),
            Category::Const,
            DataType::Bool,
            Some(42),
        );
        assert_eq!(table.identifiers.len(), 1, "Table should contain one identifier");
        assert_eq!(table.identifiers[0].name, "y", "Identifier name should be 'y'");
        assert_eq!(table.identifiers[0].category, Category::Const, "Category should be Const");
        assert_eq!(table.identifiers[0].data_type, DataType::Bool, "DataType should be Bool");
        assert_eq!(table.identifiers[0].value, Some(42), "Value should be Some(42)");
    }

    #[test]
    fn test_add_or_update_existing_identifier() {
        let mut table = NameTable::new();
        table.add(String::from("x"), Category::Var, DataType::Int);
        table.add_or_update(
            String::from("x"),
            Category::Const,
            DataType::Bool,
            Some(100),
        );
        assert_eq!(table.identifiers.len(), 1, "Table should still contain one identifier");
        assert_eq!(table.identifiers[0].name, "x", "Identifier name should be 'x'");
        assert_eq!(table.identifiers[0].category, Category::Const, "Category should be updated to Const");
        assert_eq!(table.identifiers[0].data_type, DataType::Bool, "DataType should be updated to Bool");
        assert_eq!(table.identifiers[0].value, Some(100), "Value should be updated to Some(100)");
    }

    #[test]
    fn test_find_by_name_existing() {
        let mut table = NameTable::new();
        table.add(String::from("z"), Category::Var, DataType::Int);
        let result = table.find_by_name("z");
        assert!(result.is_some(), "Should find existing identifier");
        assert_eq!(result.unwrap().name, "z", "Found identifier should have name 'z'");
    }

    #[test]
    fn test_find_by_name_non_existing() {
        let table = NameTable::new();
        let result = table.find_by_name("non_existent");
        assert!(result.is_none(), "Should not find non-existing identifier");
    }

    #[test]
    fn test_entries() {
        let mut table = NameTable::new();
        table.add(String::from("x"), Category::Var, DataType::Int);
        table.add(String::from("y"), Category::Const, DataType::Bool);
        let entries = table.entries();
        assert_eq!(entries.len(), 2, "Entries should contain two identifiers");
        assert_eq!(entries[0].name, "x", "First entry should be 'x'");
        assert_eq!(entries[1].name, "y", "Second entry should be 'y'");
    }
}
