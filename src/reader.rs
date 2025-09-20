use std::fs::{File, OpenOptions};
use std::io::{self, BufReader, Read, Write};
use std::sync::{Mutex, OnceLock};

static READER: OnceLock<Mutex<ReaderInner>> = OnceLock::new();

fn global_reader() -> &'static Mutex<ReaderInner> {
    READER.get_or_init(|| Mutex::new(ReaderInner::default()))
}

#[derive(Default)]
struct ReaderInner {
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
        self.col = 1; // Start at col 1 for the first character
        self.eof = false;
        self.reader = None;
        self.file_path = None;

        self.read_next_char().map_err(|e| e.to_string())?;
        Ok(())
    }

    fn read_next_char(&mut self) -> io::Result<()> {
        if let Some(ref chars) = self.string_content {
            if self.string_pos >= chars.len() {
                self.current_char = None;
                self.eof = true;
                return Ok(());
            }

            let ch = chars[self.string_pos];
            self.string_pos += 1;

            match ch {
                '\r' => {
                    // Skip \r and try next character
                    return self.read_next_char();
                }
                '\n' => {
                    self.line += 1;
                    self.col = 1;
                    self.current_char = None; // Don't treat newline as a character
                    return self.read_next_char(); // Advance to next non-whitespace
                }
                '\t' => {
                    self.col += 4; // Treat tab as 4 spaces
                    self.current_char = None;
                    return self.read_next_char(); // Skip tabs
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
                            return self.read_next_char();
                        }
                        '\n' => {
                            self.line += 1;
                            self.col = 1;
                            self.current_char = None;
                            return self.read_next_char();
                        }
                        '\t' => {
                            self.col += 4;
                            self.current_char = None;
                            return self.read_next_char();
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

    fn write_all(&self, path: &str, content: &str) -> io::Result<()> {
        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        f.write_all(content.as_bytes())
    }
}

pub struct Reader;

impl Reader {
    pub fn init_with_string(content: &str) -> Result<(), String> {
        global_reader().lock().unwrap().init_with_string(content)
    }

    pub fn read_next_char() -> io::Result<()> {
        global_reader().lock().unwrap().read_next_char()
    }

    pub fn close() {
        global_reader().lock().unwrap().close()
    }

    pub fn current_char() -> Option<char> {
        global_reader().lock().unwrap().current_char
    }

    pub fn line() -> usize {
        global_reader().lock().unwrap().line
    }

    pub fn col() -> usize {
        global_reader().lock().unwrap().col
    }

    pub fn is_eof() -> bool {
        global_reader().lock().unwrap().eof
    }

    pub fn read_to_string(path: &str) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    pub fn write_to_file(path: &str, content: &str) -> io::Result<()> {
        global_reader().lock().unwrap().write_all(path, content)
    }
}