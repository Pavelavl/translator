use std::fs::File;
use std::io::{self, BufReader, Read};

#[derive(Default)]
pub struct ReaderInner {
    file_path: Option<String>,
    reader: Option<BufReader<File>>,
    current_char: Option<char>,
    line: usize,
    col: usize,
    eof: bool,
    string_content: Option<Vec<char>>,
    string_pos: usize,
}

impl ReaderInner {
    fn init_with_string(&mut self, content: &str) -> Result<(), String> {
        self.close();
        if content.is_empty() {
            self.current_char = None;
            self.eof = true;
            self.line = 1;
            self.col = 1;
            return Ok(());
        }
        let chars: Vec<char> = content.chars().collect();
        self.string_content = Some(chars);
        self.string_pos = 0;
        self.line = 1;
        self.col = 1;
        self.eof = false;
        self.reader = None;
        self.file_path = None;

        // Set current_char to the first character and adjust column
        if let Some(&ch) = self.string_content.as_ref().and_then(|chars| chars.get(0)) {
            self.current_char = Some(ch);
            match ch {
                '\t' => self.col += 4, // Tab increments column by 4
                _ => self.col += 1,    // Other characters increment column by 1
            }
        } else {
            self.current_char = None;
            self.eof = true;
        }
        Ok(())
    }

    fn read_next_char(&mut self) -> io::Result<()> {
        if let Some(ref chars) = self.string_content {
            if self.string_pos >= chars.len() {
                self.current_char = None;
                self.eof = true;
                return Ok(());
            }

            self.string_pos += 1;
            if self.string_pos >= chars.len() {
                self.current_char = None;
                self.eof = true;
                return Ok(());
            }

            let ch = chars[self.string_pos];
            match ch {
                '\r' => {
                    self.col += 1;
                    self.current_char = Some('\r');
                }
                '\n' => {
                    self.line += 1;
                    self.col = 1;
                    self.current_char = Some('\n');
                }
                '\t' => {
                    self.col += 4;
                    self.current_char = Some('\t');
                }
                _ => {
                    self.col += 1;
                    self.current_char = Some(ch);
                }
            }
        } else if let Some(r) = self.reader.as_mut() {
            let mut buf = [0u8; 1];
            match r.read(&mut buf)? {
                0 => {
                    self.current_char = None;
                    self.eof = true;
                }
                _ => {
                    let ch = buf[0] as char;
                    match ch {
                        '\r' => {
                            self.col += 1;
                            self.current_char = Some('\r');
                        }
                        '\n' => {
                            self.line += 1;
                            self.col = 1;
                            self.current_char = Some('\n');
                        }
                        '\t' => {
                            self.col += 4;
                            self.current_char = Some('\t');
                        }
                        _ => {
                            self.col += 1;
                            self.current_char = Some(ch);
                        }
                    }
                }
            }
        } else {
            self.current_char = None;
            self.eof = true;
        }
        Ok(())
    }

    fn close(&mut self) {
        self.reader = None;
        self.file_path = None;
        self.current_char = None;
        self.string_content = None;
        self.string_pos = 0;
        self.eof = true;
        self.line = 1;
        self.col = 1;
    }
}

pub struct Reader {
    inner: ReaderInner,
}

impl Reader {
    pub fn new() -> Self {
        Reader {
            inner: ReaderInner::default(),
        }
    }

    pub fn init_with_string(&mut self, content: &str) -> Result<(), String> {
        self.inner.init_with_string(content)
    }

    pub fn read_next_char(&mut self) -> io::Result<()> {
        self.inner.read_next_char()
    }

    pub fn close(&mut self) {
        self.inner.close()
    }

    pub fn current_char(&self) -> Option<char> {
        self.inner.current_char
    }

    pub fn line(&self) -> usize {
        self.inner.line
    }

    pub fn col(&self) -> usize {
        self.inner.col
    }

    pub fn is_eof(&self) -> bool {
        self.inner.eof
    }

    pub fn read_to_string(path: &str) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init_with_empty_string() {
        let mut reader = Reader::new();
        let result = reader.init_with_string("");
        assert!(result.is_ok());
        assert_eq!(reader.current_char(), None);
        assert!(reader.is_eof());
        assert_eq!(reader.line(), 1);
        assert_eq!(reader.col(), 1);
    }

    #[test]
    fn test_init_with_single_char() {
        let mut reader = Reader::new();
        let result = reader.init_with_string("a");
        assert!(result.is_ok());
        assert_eq!(reader.current_char(), Some('a'));
        assert!(!reader.is_eof());
        assert_eq!(reader.line(), 1);
        assert_eq!(reader.col(), 2);
    }

    #[test]
    fn test_read_next_char() {
        let mut reader = Reader::new();
        reader.init_with_string("ab").unwrap();
        assert_eq!(reader.current_char(), Some('a'));
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), Some('b'));
        assert_eq!(reader.col(), 3);
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), None);
        assert!(reader.is_eof());
    }

    #[test]
    fn test_newline_handling() {
        let mut reader = Reader::new();
        reader.init_with_string("a\nb").unwrap();
        assert_eq!(reader.current_char(), Some('a'));
        assert_eq!(reader.line(), 1);
        assert_eq!(reader.col(), 2);
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), Some('\n'));
        assert_eq!(reader.line(), 2);
        assert_eq!(reader.col(), 1);
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), Some('b'));
        assert_eq!(reader.line(), 2);
        assert_eq!(reader.col(), 2);
    }

    #[test]
    fn test_tab_handling() {
        let mut reader = Reader::new();
        reader.init_with_string("\ta").unwrap();
        assert_eq!(reader.current_char(), Some('\t'));
        assert_eq!(reader.col(), 5);
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), Some('a'));
        assert_eq!(reader.col(), 6);
    }

    #[test]
    fn test_carriage_return_handling() {
        let mut reader = Reader::new();
        reader.init_with_string("a\rb").unwrap();
        assert_eq!(reader.current_char(), Some('a'));
        assert_eq!(reader.col(), 2);
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), Some('\r'));
        assert_eq!(reader.col(), 3);
        reader.read_next_char().unwrap();
        assert_eq!(reader.current_char(), Some('b'));
        assert_eq!(reader.col(), 4);
    }

    #[test]
    fn test_close() {
        let mut reader = Reader::new();
        reader.init_with_string("abc").unwrap();
        reader.close();
        assert_eq!(reader.current_char(), None);
        assert!(reader.is_eof());
        assert_eq!(reader.line(), 1);
        assert_eq!(reader.col(), 1);
    }
}