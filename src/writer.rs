use std::fs::OpenOptions;
use std::io::{self, Write};

pub struct Writer;

impl Writer {
    pub fn new() -> Self {
        Writer
    }

    pub fn write_to_file(&self, path: &str, content: &str) -> io::Result<()> {
        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        f.write_all(content.as_bytes())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Read;

    #[test]
    fn test_write_to_file() {
        let writer = Writer::new();
        let path = "test_output.txt";
        let content = "Hello, world!";

        let result = writer.write_to_file(path, content);
        assert!(result.is_ok());

        let mut file = fs::File::open(path).unwrap();
        let mut read_content = String::new();
        file.read_to_string(&mut read_content).unwrap();
        assert_eq!(read_content, content);

        fs::remove_file(path).unwrap(); // Clean up
    }

    #[test]
    fn test_write_to_file_empty() {
        let writer = Writer::new();
        let path = "test_empty.txt";
        let content = "";

        let result = writer.write_to_file(path, content);
        assert!(result.is_ok());

        let mut file = fs::File::open(path).unwrap();
        let mut read_content = String::new();
        file.read_to_string(&mut read_content).unwrap();
        assert_eq!(read_content, "");

        fs::remove_file(path).unwrap(); // Clean up
    }
}