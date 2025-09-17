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
    pub fn init_keywords(&mut self) {
        self.keywords = HashMap::from([
            ("int".to_string(), Lexems::Int),
            (".NOT.".to_string(), Lexems::Not),
            (".AND.".to_string(), Lexems::And),
            (".OR.".to_string(), Lexems::Or),
            (".XOR.".to_string(), Lexems::Xor),
            ("Boolean".to_string(), Lexems::Bool),
            ("bool".to_string(), Lexems::Bool),
            ("if".to_string(), Lexems::If),
            ("then".to_string(), Lexems::Then),
            ("elseif".to_string(), Lexems::ElseIf),
            ("else".to_string(), Lexems::Else),
            ("endif".to_string(), Lexems::EndIf),
            ("while".to_string(), Lexems::While),
            ("endwhile".to_string(), Lexems::EndWhile),
            ("print".to_string(), Lexems::Print),
            ("Begin".to_string(), Lexems::Begin),
            ("End".to_string(), Lexems::End),
        ]);
    }

    fn get_keyword(&self, name: &str) -> Lexems {
        self.keywords
            .get(&name.to_lowercase())
            .cloned()
            .unwrap_or(Lexems::Name)
    }

    fn parse_next_lexem(&mut self) {
        // Пропуск пробелов
        while let Some(ch) = Reader::current_char() {
            if ch.is_whitespace() {
                if ch == '\n' {
                    self.current_lexem = Lexems::NewLine;
                    self.current_name.clear();
                    self.line = Reader::line();
                    self.col = Reader::col();
                    let _ = Reader::read_next_char();
                    return;
                }
                let _ = Reader::read_next_char();
            } else {
                break;
            }
        }

        // Обновляем текущую позицию
        self.line = Reader::line();
        self.col = Reader::col();

        if Reader::is_eof() {
            self.current_lexem = Lexems::EOF;
            return;
        }

        self.current_name.clear();

        let ch = match Reader::current_char() {
            Some(c) => c,
            None => {
                self.current_lexem = Lexems::EOF;
                return;
            }
        };

        if ch == '.' {
            // Проверка на составные токены .NOT., .AND., .OR., .XOR.
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
                self.current_lexem = keyword_lexem;
                self.current_name = temp_name;
                return;
            } else {
                self.current_lexem = Lexems::Error;
                self.current_name = format!("Invalid operator '{}'", temp_name);
                return;
            }
        }

        if ch.is_alphabetic() {
            self.parse_identifier();
        } else if ch.is_digit(10) {
            self.parse_number();
        } else {
            match ch {
                '+' => {
                    self.consume_simple(Lexems::Plus, '+');
                }
                '-' => {
                    self.consume_simple(Lexems::Minus, '-');
                }
                '*' => {
                    self.consume_simple(Lexems::Mul, '*');
                }
                '/' => {
                    self.consume_simple(Lexems::Div, '/');
                }
                ';' => {
                    self.consume_simple(Lexems::Semi, ';');
                }
                ':' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        self.current_lexem = Lexems::Assign;
                        self.current_name = ":=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        self.current_lexem = Lexems::Colon;
                        self.current_name = ":".to_string();
                    }
                }
                '<' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        self.current_lexem = Lexems::LessOrEqual;
                        self.current_name = "<=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        self.current_lexem = Lexems::Less;
                        self.current_name = "<".to_string();
                    }
                }
                '=' => {
                    self.consume_simple(Lexems::Equal, '=');
                }
                '>' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        self.current_lexem = Lexems::GreaterOrEqual;
                        self.current_name = ">=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        self.current_lexem = Lexems::Greater;
                        self.current_name = ">".to_string();
                    }
                }
                '!' => {
                    let _ = Reader::read_next_char();
                    if Reader::current_char() == Some('=') {
                        self.current_lexem = Lexems::NotEqual;
                        self.current_name = "!=".to_string();
                        let _ = Reader::read_next_char();
                    } else {
                        self.current_lexem = Lexems::Error;
                        self.current_name = "!".to_string();
                    }
                }
                '(' => {
                    self.consume_simple(Lexems::LParen, '(');
                }
                ')' => {
                    self.consume_simple(Lexems::RParen, ')');
                }
                ',' => {
                    self.consume_simple(Lexems::Comma, ',');
                }
                _ => {
                    self.current_lexem = Lexems::Error;
                    self.current_name = ch.to_string();
                    let _ = Reader::read_next_char();
                }
            }
        }
    }

    fn consume_simple(&mut self, lex: Lexems, ch: char) {
        self.current_lexem = lex;
        self.current_name = ch.to_string();
        let _ = Reader::read_next_char();
    }

    fn parse_identifier(&mut self) {
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
        if self.current_lexem == Lexems::Name {
            if self.name_table.find_by_name(&self.current_name).is_none() {
                let _ = self.name_table.add(
                    self.current_name.clone(),
                    Category::Var,
                    crate::models::DataType::None,
                );
            }
        }
    }

    fn parse_number(&mut self) {
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
    }

    pub fn current_lexem(&self) -> Lexems {
        self.current_lexem.clone()
    }

    pub fn current_name(&self) -> String {
        self.current_name.clone()
    }

    pub fn advance(&mut self) {
        self.parse_next_lexem();
        println!(
            "[Lexer] line: {}, col: {}, lexem: {:?}, name: '{}'",
            self.line, self.col, self.current_lexem, self.current_name
        );
    }
}
