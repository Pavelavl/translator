use crate::models::{Category, Lexems};
use crate::name_table::NameTable;
use crate::reader::Reader;
use std::collections::HashMap;

pub struct LexicalAnalyzer {
    pub keywords: HashMap<String, Lexems>,
    pub current_lexem: Lexems,
    pub current_name: String,
    pub name_table: NameTable,
    pub line: usize,
    pub col: usize,
}

impl LexicalAnalyzer {
    pub fn new() -> Self {
        println!("[LexicalAnalyzer] Initializing new LexicalAnalyzer");
        let mut lexer = Self {
            keywords: HashMap::new(),
            current_lexem: Lexems::None,
            current_name: String::new(),
            name_table: NameTable::new(),
            line: 1,
            col: 1,
        };
        lexer.init_keywords();
        println!("[LexicalAnalyzer] Initialization complete");
        lexer
    }

    pub fn init_keywords(&mut self) {
        println!("[LexicalAnalyzer] Initializing keywords");
        self.keywords = HashMap::from([
            ("int".to_string(), Lexems::Int),
            (".not.".to_string(), Lexems::Not),
            (".and.".to_string(), Lexems::And),
            (".or.".to_string(), Lexems::Or),
            (".xor.".to_string(), Lexems::Xor),
            ("boolean".to_string(), Lexems::Bool),
            ("bool".to_string(), Lexems::Bool),
            ("if".to_string(), Lexems::If),
            ("then".to_string(), Lexems::Then),
            ("elseif".to_string(), Lexems::ElseIf),
            ("else".to_string(), Lexems::Else),
            ("endif".to_string(), Lexems::EndIf),
            ("while".to_string(), Lexems::While),
            ("endwhile".to_string(), Lexems::EndWhile),
            ("print".to_string(), Lexems::Print),
            ("begin".to_string(), Lexems::Begin),
            ("end".to_string(), Lexems::End),
        ]);
        println!("[LexicalAnalyzer] Keywords initialized: {:?}", self.keywords.keys());
    }

    fn get_keyword(&self, name: &str) -> Lexems {
        let result = self.keywords.get(&name.to_lowercase()).cloned().unwrap_or(Lexems::Name);
        println!("[LexicalAnalyzer] Getting keyword for '{}', result: {:?}", name, result);
        result
    }

    fn parse_next_lexem(&mut self) {
        println!("[LexicalAnalyzer] Starting parse_next_lexem at line {}, col {}", self.line, self.col);
        while let Some(ch) = Reader::current_char() {
            if ch.is_whitespace() {
                if ch == '\n' {
                    println!("[LexicalAnalyzer] Found newline");
                    self.current_lexem = Lexems::NewLine;
                    self.current_name.clear();
                    self.line = Reader::line();
                    self.col = Reader::col();
                    let _ = Reader::read_next_char();
                    println!("[LexicalAnalyzer] Processed newline, new position: line {}, col {}", self.line, self.col);
                    return;
                }
                println!("[LexicalAnalyzer] Skipping whitespace '{}'", ch);
                let _ = Reader::read_next_char();
            } else {
                break;
            }
        }

        self.line = Reader::line();
        self.col = Reader::col();
        println!("[LexicalAnalyzer] Updated position: line {}, col {}", self.line, self.col);

        if Reader::is_eof() {
            println!("[LexicalAnalyzer] Reached EOF");
            self.current_lexem = Lexems::EOF;
            self.current_name.clear();
            return;
        }

        self.current_name.clear();
        let ch = match Reader::current_char() {
            Some(c) => {
                println!("[LexicalAnalyzer] Processing character '{}'", c);
                c
            }
            None => {
                println!("[LexicalAnalyzer] No character, setting EOF");
                self.current_lexem = Lexems::EOF;
                self.current_name.clear();
                return;
            }
        };

        if ch == '.' {
            println!("[LexicalAnalyzer] Found dot, checking for composite token");
            let mut temp_name = String::new();
            temp_name.push(ch);
            let _ = Reader::read_next_char();
            while let Some(next_ch) = Reader::current_char() {
                if next_ch.is_alphabetic() || next_ch == '.' {
                    temp_name.push(next_ch);
                    let _ = Reader::read_next_char();
                } else {
                    break;
                }
            }
            let keyword_lexem = self.get_keyword(&temp_name);
            if matches!(
                keyword_lexem,
                Lexems::Not | Lexems::And | Lexems::Or | Lexems::Xor
            ) {
                println!("[LexicalAnalyzer] Valid composite token '{}': {:?}", temp_name, keyword_lexem);
                self.current_lexem = keyword_lexem;
                self.current_name = temp_name;
                return;
            } else {
                println!("[LexicalAnalyzer] Invalid composite token '{}'", temp_name);
                self.current_lexem = Lexems::Error;
                self.current_name = format!("Invalid operator '{}'", temp_name);
                return;
            }
        }

        if ch.is_alphabetic() {
            println!("[LexicalAnalyzer] Parsing identifier starting with '{}'", ch);
            self.parse_identifier();
        } else if ch.is_digit(10) {
            println!("[LexicalAnalyzer] Parsing number starting with '{}'", ch);
            self.parse_number();
        } else {
            println!("[LexicalAnalyzer] Processing operator or symbol '{}'", ch);
            match ch {
                '+' => self.consume_simple(Lexems::Plus, '+'),
                '-' => self.consume_simple(Lexems::Minus, '-'),
                '*' => self.consume_simple(Lexems::Mul, '*'),
                '/' => self.consume_simple(Lexems::Div, '/'),
                ';' => self.consume_simple(Lexems::Semi, ';'),
                ':' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        println!("[LexicalAnalyzer] Found assignment operator ':='");
                        self.current_lexem = Lexems::Assign;
                        self.current_name = ":=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        println!("[LexicalAnalyzer] Found colon ':'");
                        self.current_lexem = Lexems::Colon;
                        self.current_name = ":".to_string();
                    }
                }
                '<' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        println!("[LexicalAnalyzer] Found less or equal '<='");
                        self.current_lexem = Lexems::LessOrEqual;
                        self.current_name = "<=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        println!("[LexicalAnalyzer] Found less '<'");
                        self.current_lexem = Lexems::Less;
                        self.current_name = "<".to_string();
                    }
                }
                '=' => self.consume_simple(Lexems::Equal, '='),
                '>' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        println!("[LexicalAnalyzer] Found greater or equal '>='");
                        self.current_lexem = Lexems::GreaterOrEqual;
                        self.current_name = ">=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        println!("[LexicalAnalyzer] Found greater '>'");
                        self.current_lexem = Lexems::Greater;
                        self.current_name = ">".to_string();
                    }
                }
                '!' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        println!("[LexicalAnalyzer] Found not equal '!='");
                        self.current_lexem = Lexems::NotEqual;
                        self.current_name = "!=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        println!("[LexicalAnalyzer] Invalid operator '!'");
                        self.current_lexem = Lexems::Error;
                        self.current_name = "!".to_string();
                    }
                }
                '(' => self.consume_simple(Lexems::LParen, '('),
                ')' => self.consume_simple(Lexems::RParen, ')'),
                ',' => self.consume_simple(Lexems::Comma, ','),
                _ => {
                    println!("[LexicalAnalyzer] Unknown character '{}', marking as error", ch);
                    self.current_lexem = Lexems::Error;
                    self.current_name = ch.to_string();
                    let _ = Reader::read_next_char();
                }
            }
        }
        println!("[LexicalAnalyzer] Completed parse_next_lexem: lexem={:?}, name='{}'", self.current_lexem, self.current_name);
    }

    fn consume_simple(&mut self, lex: Lexems, ch: char) {
        println!("[LexicalAnalyzer] Consuming simple token: lexem={:?}, char='{}'", lex, ch);
        self.current_lexem = lex;
        self.current_name = ch.to_string();
        let _ = Reader::read_next_char();
    }

    fn parse_identifier(&mut self) {
        println!("[LexicalAnalyzer] Starting parse_identifier");
        self.current_name.clear();
        while let Some(ch) = Reader::current_char() {
            if ch.is_alphabetic() {
                self.current_name.push(ch);
                let _ = Reader::read_next_char();
            } else {
                break;
            }
        }
        self.current_lexem = self.get_keyword(&self.current_name);
        println!("[LexicalAnalyzer] Parsed identifier: name='{}', lexem={:?}", self.current_name, self.current_lexem);
        if self.current_lexem == Lexems::Name {
            if self.name_table.find_by_name(&self.current_name).is_none() {
                let _ = self.name_table.add(
                    self.current_name.clone(),
                    Category::Var,
                    crate::models::DataType::None,
                );
                println!("[LexicalAnalyzer] Added new identifier '{}' to name table", self.current_name);
            } else {
                println!("[LexicalAnalyzer] Identifier '{}' already in name table", self.current_name);
            }
        }
    }

    fn parse_number(&mut self) {
        println!("[LexicalAnalyzer] Starting parse_number");
        self.current_name.clear();
        while let Some(ch) = Reader::current_char() {
            if ch.is_digit(10) {
                self.current_name.push(ch);
                let _ = Reader::read_next_char();
            } else {
                break;
            }
        }
        self.current_lexem = Lexems::Number;
        println!("[LexicalAnalyzer] Parsed number: value='{}'", self.current_name);
    }

    pub fn current_lexem(&self) -> Lexems {
        println!("[LexicalAnalyzer] Retrieving current lexem: {:?}", self.current_lexem);
        self.current_lexem.clone()
    }

    pub fn current_name(&self) -> String {
        println!("[LexicalAnalyzer] Retrieving current name: '{}'", self.current_name);
        self.current_name.clone()
    }

    pub fn advance(&mut self) {
        println!("[LexicalAnalyzer] Advancing to next lexem");
        self.parse_next_lexem();
        println!(
            "[Lexer] line: {}, col: {}, lexem: {:?}, name: '{}'",
            self.line, self.col, self.current_lexem, self.current_name
        );
    }
}