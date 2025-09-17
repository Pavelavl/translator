use std::collections::HashMap;

use crate::code_generator::CodeGenerator;
use crate::lexical_analyzer::LexicalAnalyzer;
use crate::models::{Category, DataType, Lexems};
use crate::name_table::NameTable;
use crate::reader::Reader;

pub struct Error {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

pub struct SyntaxAnalyzer {
    pub lexer: LexicalAnalyzer,
    pub name_table: NameTable,
    pub errors: Vec<Error>,
    input_text: String,
    current_line: usize,
    instruction_count: usize,
}

impl SyntaxAnalyzer {
    pub fn new(input_text: String) -> Self {
        let mut lexer = LexicalAnalyzer {
            keywords: HashMap::new(),
            current_lexem: Lexems::None,
            current_name: String::new(),
            name_table: NameTable::new(),
            line: 1,
            col: 1,
        };
        lexer.init_keywords();

        Self {
            lexer,
            name_table: NameTable::new(),
            errors: Vec::new(),
            input_text,
            current_line: 0,
            instruction_count: 0,
        }
    }

    fn parse_expression(&mut self) -> DataType {
        // Начинаем с обработки арифметического выражения
        let mut t = self.parse_add_sub();
    
        // Проверяем наличие логических операторов (.AND., .OR., .XOR.)
        if matches!(
            self.lexer.current_lexem(),
            Lexems::And | Lexems::Or | Lexems::Xor
        ) {
            while matches!(
                self.lexer.current_lexem(),
                Lexems::And | Lexems::Or | Lexems::Xor
            ) {
                let op = self.lexer.current_lexem();
                self.lexer.advance();
                let rhs = self.parse_operand();
                if t != rhs || t != DataType::Bool {
                    self.report_error(format!("Type mismatch: expected Bool, found {:?}", rhs));
                    return DataType::None;
                }
                CodeGenerator::add_instruction("pop bx");
                CodeGenerator::add_instruction("pop ax");
                match op {
                    Lexems::And => {
                        CodeGenerator::add_instruction("and ax, bx");
                    }
                    Lexems::Or => {
                        CodeGenerator::add_instruction("or ax, bx");
                    }
                    Lexems::Xor => {
                        CodeGenerator::add_instruction("xor ax, bx");
                    }
                    _ => {}
                }
                CodeGenerator::add_instruction("push ax");
                t = DataType::Bool;
            }
        } else if self.lexer.current_lexem() == Lexems::Not {
            self.lexer.advance();
            let sub_t = self.parse_subexpression();
            if sub_t != DataType::Bool {
                self.report_error(format!("Expected Bool after .NOT., found {:?}", sub_t));
                return DataType::None;
            }
            CodeGenerator::add_instruction("pop ax");
            CodeGenerator::add_instruction("xor ax, 1");
            CodeGenerator::add_instruction("push ax");
            t = DataType::Bool;
        } else if matches!(
            self.lexer.current_lexem(),
            Lexems::Equal
                | Lexems::NotEqual
                | Lexems::Less
                | Lexems::LessOrEqual
                | Lexems::Greater
                | Lexems::GreaterOrEqual
        ) {
            t = self.parse_logical_expression();
        }
        t
    }

    fn parse_add_sub(&mut self) -> DataType {
        let t = self.parse_mul_div();
        while self.lexer.current_lexem() == Lexems::Plus
            || self.lexer.current_lexem() == Lexems::Minus
        {
            let op = self.lexer.current_lexem();
            self.lexer.advance();
            let rhs = self.parse_mul_div();
            if t != rhs && t != DataType::None && rhs != DataType::None {
                self.report_error(format!("Type mismatch: expected {:?}, found {:?}", t, rhs));
            }
            match op {
                Lexems::Plus => {
                    CodeGenerator::add_instruction("pop bx");
                    CodeGenerator::add_instruction("pop ax");
                    CodeGenerator::add_instruction("add ax, bx");
                    CodeGenerator::add_instruction("push ax");
                }
                Lexems::Minus => {
                    CodeGenerator::add_instruction("pop bx");
                    CodeGenerator::add_instruction("pop ax");
                    CodeGenerator::add_instruction("sub ax, bx");
                    CodeGenerator::add_instruction("push ax");
                }
                _ => {}
            }
        }
        t
    }

    fn parse_mul_div(&mut self) -> DataType {
        let t = self.parse_term();
        while self.lexer.current_lexem() == Lexems::Mul || self.lexer.current_lexem() == Lexems::Div
        {
            let op = self.lexer.current_lexem();
            self.lexer.advance();
            let rhs = self.parse_term();
            if t != rhs && t != DataType::None && rhs != DataType::None {
                self.report_error(format!("Type mismatch: expected {:?}, found {:?}", t, rhs));
            }
            match op {
                Lexems::Mul => {
                    CodeGenerator::add_instruction("pop bx");
                    CodeGenerator::add_instruction("pop ax");
                    CodeGenerator::add_instruction("mul bx");
                    CodeGenerator::add_instruction("push ax");
                }
                Lexems::Div => {
                    CodeGenerator::add_instruction("pop bx");
                    CodeGenerator::add_instruction("pop ax");
                    CodeGenerator::add_instruction("cwd");
                    CodeGenerator::add_instruction("div bx");
                    CodeGenerator::add_instruction("push ax");
                }
                _ => {}
            }
        }
        t
    }

    fn parse_term(&mut self) -> DataType {
        let lex = self.lexer.current_lexem();
        match lex {
            Lexems::Name => {
                let name = self.lexer.current_name().to_string();
                if let Some(ident) = self.name_table.find_by_name(&name) {
                    match ident.category {
                        Category::Var => {
                            CodeGenerator::add_instruction(&format!("mov ax, [{}]", name));
                            CodeGenerator::add_instruction("push ax");
                            self.lexer.advance();
                            ident.data_type.clone()
                        }
                        Category::Const => {
                            if let Some(value) = ident.value {
                                CodeGenerator::add_instruction(&format!("mov ax, {}", value));
                                CodeGenerator::add_instruction("push ax");
                                self.lexer.advance();
                                ident.data_type.clone()
                            } else {
                                self.report_error(format!("Constant '{}' has no value", name));
                                DataType::None
                            }
                        }
                    }
                } else {
                    self.report_error(format!("Undeclared identifier '{}'", name));
                    DataType::None
                }
            }
            Lexems::Number => {
                let value = self.lexer.current_name();
                CodeGenerator::add_instruction(&format!("mov ax, {}", value));
                CodeGenerator::add_instruction("push ax");
                self.lexer.advance();
                DataType::Int
            }
            _ => {
                if lex == Lexems::NewLine {
                    self.lexer.advance();
                    self.parse_term()
                } else if lex == Lexems::Error {
                    self.report_error("Unexpected token".to_string());
                    self.lexer.advance();
                    DataType::None
                } else if lex == Lexems::EOF {
                    DataType::None
                } else if lex == Lexems::Begin {
                    self.lexer.advance();
                    let t = self.parse_expression();
                    self.expect_lexem(Lexems::End);
                    t
                } else {
                    self.report_error("Unexpected token in expression".to_string());
                    self.lexer.advance();
                    DataType::None
                }
            }
        }
    }

    fn parse_assignment(&mut self) {
        if self.instruction_count > 0 && self.current_line == self.lexer.line {
            self.report_error("Only one assignment per line allowed".to_string());
            return;
        }
        self.instruction_count += 1;

        let name = self.lexer.current_name().to_string();
        if self.name_table.find_by_name(&name).is_none() {
            self.report_error(format!("Undeclared identifier '{}'", name));
            self.lexer.advance();
            return;
        }
        self.lexer.advance();
        if self.lexer.current_lexem() != Lexems::Equal
            && self.lexer.current_lexem() != Lexems::Assign
        {
            self.report_error("Expected '=' or ':=' after identifier".to_string());
            return;
        }
        self.lexer.advance();

        // Проверяем тип переменной
        let expected_type = self
            .name_table
            .find_by_name(&name)
            .map_or(DataType::None, |ident| ident.data_type.clone());

        // Проверяем, является ли выражение простым числом
        if self.lexer.current_lexem() == Lexems::Number {
            let value = self.lexer.current_name();
            let data_type = if expected_type == DataType::Bool {
                if value != "0" && value != "1" {
                    self.report_error(format!(
                        "Invalid constant '{}', expected 0 or 1 for Boolean",
                        value
                    ));
                    self.lexer.advance();
                    return;
                }
                DataType::Bool
            } else {
                DataType::Int
            };
            if expected_type != DataType::None && expected_type != data_type {
                self.report_error(format!(
                    "Type mismatch: variable '{}' expected {:?}, got {:?}",
                    name, expected_type, data_type
                ));
                return;
            }
            self.name_table
                .add_or_update(name.clone(), Category::Var, data_type, None);
            CodeGenerator::add_instruction(&format!("mov ax, {}", value));
            CodeGenerator::add_instruction(&format!("mov [{}], ax", name));
            self.lexer.advance();
        } else {
            // Для сложных выражений используем parse_expression
            let data_type = self.parse_expression();
            if expected_type != DataType::None && expected_type != data_type {
                self.report_error(format!(
                    "Type mismatch: variable '{}' expected {:?}, got {:?}",
                    name, expected_type, data_type
                ));
                return;
            }
            self.name_table
                .add_or_update(name.clone(), Category::Var, data_type, None);
            CodeGenerator::add_instruction("pop ax");
            CodeGenerator::add_instruction(&format!("mov [{}], ax", name));
        }

        if self.lexer.current_lexem() == Lexems::Semi {
            self.lexer.advance();
        }
    }

    fn report_error(&mut self, message: String) {
        eprintln!(
            "[ERROR] Line {}, Col {}: {} -- Token: {:?}, Name: '{}'",
            self.lexer.line,
            self.lexer.col,
            message,
            self.lexer.current_lexem(),
            self.lexer.current_name()
        );
        self.errors.push(Error {
            line: self.lexer.line,
            col: self.lexer.col,
            message,
        });
    }

    fn expect_lexem(&mut self, expected: Lexems) {
        if self.lexer.current_lexem() != expected {
            self.report_error(format!(
                "Expected {:?}, found {:?} ('{}')",
                expected,
                self.lexer.current_lexem(),
                self.lexer.current_name()
            ));
        }
        self.lexer.advance();
    }

    fn maybe_advance(&mut self) {
        while self.lexer.current_lexem() == Lexems::NewLine
            || self.lexer.current_lexem() == Lexems::Semi
            || self.lexer.current_lexem() == Lexems::EndWhile
        {
            self.lexer.advance();
        }
    }

    fn parse_print(&mut self) {
        if self.instruction_count > 0 && self.current_line == self.lexer.line {
            self.report_error("Only one instruction per line allowed".to_string());
            return;
        }
        self.instruction_count += 1;

        self.lexer.advance();
        if self.lexer.current_lexem() == Lexems::Name {
            let value = self.lexer.current_name().to_string();
            if let Some(ident) = self.name_table.find_by_name(&value) {
                if ident.data_type == DataType::Bool {
                    CodeGenerator::add_instruction(&format!("mov ax, [{}]", value));
                    CodeGenerator::add_instruction("CALL PRINT");
                } else {
                    CodeGenerator::add_instruction(&format!("mov ax, [{}]", value));
                    CodeGenerator::add_instruction("CALL PRINT_INT"); // Новая процедура для int
                }
                self.lexer.advance();
            } else {
                self.report_error(format!("Undeclared identifier '{}'", value));
                self.lexer.advance();
            }
        } else {
            self.report_error("Expected identifier after 'print'".to_string());
            self.lexer.advance();
        }
    }

    pub fn parse_instruction(&mut self) {
        self.maybe_advance();
        match self.lexer.current_lexem() {
            Lexems::Print => self.parse_print(),
            Lexems::Name => self.parse_assignment(),
            Lexems::Int | Lexems::Bool => self.parse_variable_declarations(),
            Lexems::Begin => {
                self.lexer.advance();
                self.parse_instructions(&[Lexems::End]);
                self.expect_lexem(Lexems::End);
            }
            Lexems::If => self.parse_if(),
            Lexems::While => self.parse_while(),
            Lexems::EOF => {}
            _ => {
                self.report_error(format!(
                    "Unexpected token in instruction: {:?} ('{}')",
                    self.lexer.current_lexem(),
                    self.lexer.current_name()
                ));
                self.lexer.advance();
            }
        }
    }

    fn parse_instructions(&mut self, end_tokens: &[Lexems]) {
        while !end_tokens.contains(&self.lexer.current_lexem())
            && self.lexer.current_lexem() != Lexems::EOF
        {
            self.parse_instruction();
        }
    }

    fn parse_variable_declarations(&mut self) {
        if self.instruction_count > 0 && self.current_line == self.lexer.line {
            self.report_error("Only one declaration per line allowed".to_string());
            return;
        }
        self.instruction_count += 1;

        let is_const = self.lexer.current_name().to_lowercase().ends_with("_const");
        let data_type = match self.lexer.current_lexem() {
            Lexems::Int => DataType::Int,
            Lexems::Bool => DataType::Bool,
            _ => {
                self.report_error("Expected 'int' or 'bool'".to_string());
                self.lexer.advance();
                return;
            }
        };
        self.lexer.advance();
        if self.lexer.current_lexem() != Lexems::Name {
            self.report_error("Expected variable name".to_string());
            return;
        }
        let mut names = vec![self.lexer.current_name().to_string()];
        self.lexer.advance();
        while self.lexer.current_lexem() == Lexems::Comma {
            self.lexer.advance();
            if self.lexer.current_lexem() != Lexems::Name {
                self.report_error("Expected variable name after comma".to_string());
                return;
            }
            names.push(self.lexer.current_name().to_string());
            self.lexer.advance();
        }
        if self.lexer.current_lexem() == Lexems::Colon {
            self.lexer.advance();
            for name in names {
                self.name_table.add_or_update(
                    name.clone(),
                    if is_const {
                        Category::Const
                    } else {
                        Category::Var
                    },
                    data_type.clone(),
                    None,
                );
            }
        } else if self.lexer.current_lexem() == Lexems::Assign {
            self.lexer.advance();
            let expr_type = self.parse_expression();
            let value = if is_const && self.lexer.current_lexem() == Lexems::Number {
                if data_type == DataType::Bool {
                    let value = self.lexer.current_name();
                    if value != "0" && value != "1" {
                        self.report_error(format!(
                            "Invalid constant '{}', expected 0 or 1 for Boolean",
                            value
                        ));
                        return;
                    }
                }
                self.lexer.current_name().parse::<i32>().ok()
            } else {
                None
            };
            if expr_type != data_type && expr_type != DataType::None {
                self.report_error(format!(
                    "Type mismatch: expected {:?}, found {:?}",
                    data_type, expr_type
                ));
                return;
            }
            self.name_table.add_or_update(
                names[0].clone(),
                if is_const {
                    Category::Const
                } else {
                    Category::Var
                },
                data_type,
                value,
            );
            if !is_const {
                CodeGenerator::add_instruction("pop ax");
                CodeGenerator::add_instruction(&format!("mov [{}], ax", names[0]));
            }
            if self.lexer.current_lexem() == Lexems::Semi {
                self.lexer.advance();
            } else if self.lexer.current_lexem() != Lexems::NewLine
                && self.lexer.current_lexem() != Lexems::EOF
            {
                self.report_error("Expected ';' or newline after variable declaration".to_string());
            }
        } else {
            self.report_error("Expected ',' or ':=' or ':' after variable name".to_string());
        }
    }

    pub fn compile(&mut self) {
        CodeGenerator::clear();
        if let Err(e) = Reader::init_with_string(&self.input_text) {
            self.report_error(format!("Reader init error: {}", e));
            return;
        }
        self.lexer.advance();

        // Первый проход: сбор объявлений
        while self.lexer.current_lexem() == Lexems::Int
            || self.lexer.current_lexem() == Lexems::Bool
        {
            self.parse_variable_declarations_no_code();
            self.maybe_advance();
        }
        // Проверка вычислений
        if self.lexer.current_lexem() == Lexems::Begin {
            self.lexer.advance();
            self.skip_block(&[Lexems::End]);
            if self.lexer.current_lexem() == Lexems::End {
                self.lexer.advance();
            }
        } else {
            self.report_error("Expected 'Begin' for computations".to_string());
        }
        // Проверка print
        self.maybe_advance();
        if self.lexer.current_lexem() == Lexems::Print {
            self.parse_instruction_no_code();
        } else {
            self.report_error("Expected 'print' statement".to_string());
        }
        self.maybe_advance();
        if self.lexer.current_lexem() != Lexems::EOF {
            self.report_error("Unexpected tokens after program".to_string());
        }
        Reader::close();

        // Генерация кода
        CodeGenerator::declare_data_segment();
        CodeGenerator::declare_variables(&self.name_table);
        CodeGenerator::add_instruction("PRINT_BUF DB ' ' DUP(10)");
        CodeGenerator::add_instruction("BUFEND    DB '$'");
        CodeGenerator::add_instruction("data ends");
        CodeGenerator::add_instruction("stk segment stack");
        CodeGenerator::add_instruction("db 256 dup ('?')");
        CodeGenerator::add_instruction("stk ends");
        CodeGenerator::add_instruction("code segment para public 'code'");
        CodeGenerator::add_instruction("main proc");
        CodeGenerator::add_instruction("assume cs:code,ds:data,ss:stk");
        CodeGenerator::add_instruction("mov ax,data");
        CodeGenerator::add_instruction("mov ds,ax");

        if let Err(e) = Reader::init_with_string(&self.input_text) {
            self.report_error(format!("Reader init error: {}", e));
            return;
        }
        self.lexer.advance();
        self.current_line = 0;
        self.instruction_count = 0;

        // Второй проход: генерация кода
        while self.lexer.current_lexem() == Lexems::Int
            || self.lexer.current_lexem() == Lexems::Bool
        {
            self.parse_variable_declarations();
            self.maybe_advance();
        }
        if self.lexer.current_lexem() == Lexems::Begin {
            self.parse_computations();
        } else {
            self.report_error("Expected 'Begin' for computations".to_string());
        }
        self.maybe_advance();
        if self.lexer.current_lexem() == Lexems::Print {
            self.parse_print();
        } else {
            self.report_error("Expected 'print' statement".to_string());
        }
        self.maybe_advance();
        if self.lexer.current_lexem() != Lexems::EOF {
            self.report_error("Unexpected tokens after program".to_string());
        }
        Reader::close();

        CodeGenerator::add_instruction("mov ax,4c00h");
        CodeGenerator::add_instruction("int 21h");
        CodeGenerator::add_instruction("main endp");
        CodeGenerator::declare_print_procedure();
        CodeGenerator::add_instruction("code ends");
        CodeGenerator::add_instruction("end main");
    }

    fn parse_instruction_no_code(&mut self) {
        self.maybe_advance();
        match self.lexer.current_lexem() {
            Lexems::Print => {
                self.lexer.advance();
                if self.lexer.current_lexem() == Lexems::Name
                    || self.lexer.current_lexem() == Lexems::Number
                {
                    self.lexer.advance();
                }
                if self.lexer.current_lexem() == Lexems::Semi {
                    self.lexer.advance();
                }
            }
            Lexems::Name => {
                let name = self.lexer.current_name().to_string();
                self.lexer.advance();
                if self.lexer.current_lexem() == Lexems::Assign
                    || self.lexer.current_lexem() == Lexems::Equal
                {
                    if let Some(ident) = self.name_table.find_by_name(&name) {
                        if ident.category == Category::Const {
                            self.report_error(format!("Cannot assign to constant '{}'", name));
                            self.lexer.advance();
                            self.skip_expression();
                            if self.lexer.current_lexem() == Lexems::Semi {
                                self.lexer.advance();
                            }
                            return;
                        }
                    }
                    self.lexer.advance();
                    self.skip_expression();
                    if self.lexer.current_lexem() == Lexems::Semi {
                        self.lexer.advance();
                    }
                    if self.name_table.find_by_name(&name).is_none() {
                        self.name_table
                            .add_or_update(name, Category::Var, DataType::Int, None);
                    }
                } else {
                    self.report_error(format!("Expected ':=' or '=' after identifier '{}'", name));
                }
            }
            Lexems::Int | Lexems::Bool => self.parse_variable_declarations_no_code(),
            Lexems::If => self.skip_if_no_code(),
            Lexems::While => self.skip_while_no_code(),
            Lexems::Begin => {
                self.lexer.advance();
                self.skip_block(&[Lexems::End]);
                if self.lexer.current_lexem() == Lexems::End {
                    self.lexer.advance();
                }
            }
            Lexems::EOF => {}
            _ => {
                self.report_error(format!(
                    "Unexpected token in instruction: {:?} ('{}')",
                    self.lexer.current_lexem(),
                    self.lexer.current_name()
                ));
                self.lexer.advance();
            }
        }
    }

    fn parse_variable_declarations_no_code(&mut self) {
        let is_const = self.lexer.current_name().to_lowercase().ends_with("_const");
        let data_type = match self.lexer.current_lexem() {
            Lexems::Int => DataType::Int,
            Lexems::Bool => DataType::Bool,
            _ => {
                self.report_error("Expected 'int' or 'bool'".to_string());
                self.lexer.advance();
                return;
            }
        };
        self.lexer.advance();
        if self.lexer.current_lexem() != Lexems::Name {
            self.report_error("Expected variable name".to_string());
            return;
        }
        let mut names = vec![self.lexer.current_name().to_string()];
        self.lexer.advance();
        while self.lexer.current_lexem() == Lexems::Comma {
            self.lexer.advance();
            if self.lexer.current_lexem() != Lexems::Name {
                self.report_error("Expected variable name after comma".to_string());
                return;
            }
            names.push(self.lexer.current_name().to_string());
            self.lexer.advance();
        }
        if self.lexer.current_lexem() == Lexems::Colon {
            self.lexer.advance();
            for name in names {
                self.name_table.add_or_update(
                    name,
                    if is_const {
                        Category::Const
                    } else {
                        Category::Var
                    },
                    data_type.clone(),
                    None,
                );
            }
        } else if self.lexer.current_lexem() == Lexems::Assign {
            self.lexer.advance();
            let value = if is_const && self.lexer.current_lexem() == Lexems::Number {
                if data_type == DataType::Bool {
                    let value = self.lexer.current_name();
                    if value != "0" && value != "1" {
                        self.report_error(format!(
                            "Invalid constant '{}', expected 0 or 1 for Boolean",
                            value
                        ));
                        self.lexer.advance();
                        return;
                    }
                }
                let num = self.lexer.current_name().parse::<i32>().ok();
                self.lexer.advance();
                num
            } else {
                self.skip_expression();
                None
            };
            self.name_table.add_or_update(
                names[0].clone(),
                if is_const {
                    Category::Const
                } else {
                    Category::Var
                },
                data_type,
                value,
            );
            if self.lexer.current_lexem() == Lexems::Semi {
                self.lexer.advance();
            }
        } else {
            self.report_error("Expected ',' or ':=' after variable name".to_string());
        }
    }

    fn parse_computations(&mut self) {
        if self.instruction_count > 0 && self.current_line == self.lexer.line {
            self.report_error("Only one instruction per line allowed".to_string());
            return;
        }
        self.instruction_count += 1;

        self.expect_lexem(Lexems::Begin);
        while self.lexer.current_lexem() != Lexems::End && self.lexer.current_lexem() != Lexems::EOF
        {
            self.parse_instruction();
        }
        self.expect_lexem(Lexems::End);
    }

    fn skip_expression(&mut self) {
        while self.lexer.current_lexem() != Lexems::Semi
            && self.lexer.current_lexem() != Lexems::EOF
            && self.lexer.current_lexem() != Lexems::Then
            && self.lexer.current_lexem() != Lexems::EndWhile
        {
            self.lexer.advance();
        }
    }

    fn skip_block(&mut self, end_tokens: &[Lexems]) {
        while !end_tokens.contains(&self.lexer.current_lexem())
            && self.lexer.current_lexem() != Lexems::EOF
        {
            self.parse_instruction_no_code();
        }
    }

    fn skip_if_no_code(&mut self) {
        self.lexer.advance();
        self.skip_expression();
        if self.lexer.current_lexem() == Lexems::Then {
            self.lexer.advance();
        }
        self.skip_block(&[Lexems::ElseIf, Lexems::Else, Lexems::EndIf]);
        while self.lexer.current_lexem() == Lexems::ElseIf {
            self.lexer.advance();
            self.skip_expression();
            if self.lexer.current_lexem() == Lexems::Then {
                self.lexer.advance();
            }
            self.skip_block(&[Lexems::ElseIf, Lexems::Else, Lexems::EndIf]);
        }
        if self.lexer.current_lexem() == Lexems::Else {
            self.lexer.advance();
            self.skip_block(&[Lexems::EndIf]);
        }
        if self.lexer.current_lexem() == Lexems::EndIf {
            self.lexer.advance();
        }
    }

    fn skip_while_no_code(&mut self) {
        self.lexer.advance();
        self.skip_expression();
        self.skip_block(&[Lexems::EndWhile]);
        if self.lexer.current_lexem() == Lexems::EndWhile {
            self.lexer.advance();
        }
    }

    fn parse_subexpression(&mut self) -> DataType {
        let mut t;
        if self.lexer.current_lexem() == Lexems::LParen {
            self.lexer.advance();
            t = self.parse_expression();
            self.expect_lexem(Lexems::RParen);
        } else {
            t = self.parse_operand();
        }
    
        while matches!(
            self.lexer.current_lexem(),
            Lexems::And | Lexems::Or | Lexems::Xor
        ) {
            let op = self.lexer.current_lexem();
            self.lexer.advance();
            let rhs = self.parse_operand();
            if t != rhs || t != DataType::Bool {
                self.report_error(format!("Type mismatch: expected Bool, found {:?}", rhs));
                return DataType::None;
            }
            CodeGenerator::add_instruction("pop bx");
            CodeGenerator::add_instruction("pop ax");
            match op {
                Lexems::And => {
                    CodeGenerator::add_instruction("and ax, bx");
                }
                Lexems::Or => {
                    CodeGenerator::add_instruction("or ax, bx");
                }
                Lexems::Xor => {
                    CodeGenerator::add_instruction("xor ax, bx");
                }
                _ => {}
            }
            CodeGenerator::add_instruction("push ax");
            t = DataType::Bool;
        }
        t
    }

    fn parse_operand(&mut self) -> DataType {
        match self.lexer.current_lexem() {
            Lexems::Name => {
                let name = self.lexer.current_name().to_string();
                if let Some(ident) = self.name_table.find_by_name(&name) {
                    CodeGenerator::add_instruction(&format!("mov ax, [{}]", name));
                    CodeGenerator::add_instruction("push ax");
                    self.lexer.advance();
                    ident.data_type.clone()
                } else {
                    self.report_error(format!("Undeclared identifier '{}'", name));
                    self.lexer.advance();
                    DataType::None
                }
            }
            Lexems::Number => {
                let value = self.lexer.current_name();
                if value != "0" && value != "1" {
                    self.report_error(format!(
                        "Invalid constant '{}', expected 0 or 1 for Boolean",
                        value
                    ));
                    CodeGenerator::add_instruction(&format!("mov ax, {}", value));
                    CodeGenerator::add_instruction("push ax");
                    self.lexer.advance();
                    DataType::Int
                } else {
                    // Для Boolean проверяем тип переменной в контексте
                    let data_type = if self
                        .name_table
                        .find_by_name(&self.lexer.current_name())
                        .map_or(false, |ident| ident.data_type == DataType::Bool)
                    {
                        DataType::Bool
                    } else {
                        DataType::Int
                    };
                    CodeGenerator::add_instruction(&format!("mov ax, {}", value));
                    CodeGenerator::add_instruction("push ax");
                    self.lexer.advance();
                    data_type
                }
            }
            _ => {
                self.report_error("Expected identifier or constant".to_string());
                self.lexer.advance();
                DataType::None
            }
        }
    }

    fn parse_logical_expression(&mut self) -> DataType {
        let t = self.parse_add_sub();
        match self.lexer.current_lexem() {
            Lexems::Equal
            | Lexems::NotEqual
            | Lexems::Less
            | Lexems::LessOrEqual
            | Lexems::Greater
            | Lexems::GreaterOrEqual => {
                let op = self.lexer.current_lexem();
                self.lexer.advance();
                let rhs = self.parse_add_sub();
                if t != rhs && t != DataType::None && rhs != DataType::None {
                    self.report_error(format!("Type mismatch: expected {:?}, found {:?}", t, rhs));
                }
                let true_label = CodeGenerator::new_label("true");
                let done_label = CodeGenerator::new_label("done");
                CodeGenerator::add_instruction("pop bx");
                CodeGenerator::add_instruction("pop ax");
                CodeGenerator::add_instruction("cmp ax, bx");
                match op {
                    Lexems::Equal => {
                        CodeGenerator::add_instruction(&format!("je {}", true_label));
                    }
                    Lexems::NotEqual => {
                        CodeGenerator::add_instruction(&format!("jne {}", true_label));
                    }
                    Lexems::Less => {
                        CodeGenerator::add_instruction(&format!("jl {}", true_label));
                    }
                    Lexems::LessOrEqual => {
                        CodeGenerator::add_instruction(&format!("jle {}", true_label));
                    }
                    Lexems::Greater => {
                        CodeGenerator::add_instruction(&format!("jg {}", true_label));
                    }
                    Lexems::GreaterOrEqual => {
                        CodeGenerator::add_instruction(&format!("jge {}", true_label));
                    }
                    _ => {}
                }
                CodeGenerator::add_instruction("mov ax, 0");
                CodeGenerator::add_instruction(&format!("jmp {}", done_label));
                CodeGenerator::add_instruction(&format!("{}:", true_label));
                CodeGenerator::add_instruction("mov ax, 1");
                CodeGenerator::add_instruction(&format!("{}:", done_label));
                CodeGenerator::add_instruction("push ax");
                DataType::Bool
            }
            _ => t,
        }
    }

    fn parse_if(&mut self) {
        self.expect_lexem(Lexems::If);
        let expr_type = self.parse_expression();
        if expr_type != DataType::Bool {
            self.report_error(format!(
                "Expected boolean expression in 'if', got {:?}",
                expr_type
            ));
        }
        let else_label = CodeGenerator::new_label("else");
        let endif_label = CodeGenerator::new_label("endif");
        CodeGenerator::add_instruction("pop ax");
        CodeGenerator::add_instruction("cmp ax, 0");
        CodeGenerator::add_instruction(&format!("je {}", else_label));
        self.expect_lexem(Lexems::Then);
        self.parse_instructions(&[Lexems::Else, Lexems::ElseIf, Lexems::EndIf]);
        CodeGenerator::add_instruction(&format!("jmp {}", endif_label));
        CodeGenerator::add_instruction(&format!("{}:", else_label));
        while self.lexer.current_lexem() == Lexems::ElseIf {
            self.lexer.advance();
            let expr_type = self.parse_expression();
            if expr_type != DataType::Bool {
                self.report_error(format!(
                    "Expected boolean expression in 'elseif', got {:?}",
                    expr_type
                ));
            }
            let next_else_label = CodeGenerator::new_label("else");
            CodeGenerator::add_instruction("pop ax");
            CodeGenerator::add_instruction("cmp ax, 0");
            CodeGenerator::add_instruction(&format!("je {}", next_else_label));
            self.expect_lexem(Lexems::Then);
            self.parse_instructions(&[Lexems::Else, Lexems::ElseIf, Lexems::EndIf]);
            CodeGenerator::add_instruction(&format!("jmp {}", endif_label));
            CodeGenerator::add_instruction(&format!("{}:", next_else_label));
        }
        if self.lexer.current_lexem() == Lexems::Else {
            self.lexer.advance();
            self.parse_instructions(&[Lexems::EndIf]);
        }
        self.expect_lexem(Lexems::EndIf);
        CodeGenerator::add_instruction(&format!("{}:", endif_label));
    }

    fn parse_while(&mut self) {
        self.expect_lexem(Lexems::While);
        let start_label = CodeGenerator::new_label("while_start");
        let end_label = CodeGenerator::new_label("while_end");
        CodeGenerator::add_instruction(&format!("{}:", start_label));
        let expr_type = self.parse_expression();
        if expr_type != DataType::Bool {
            self.report_error(format!(
                "Expected boolean expression in 'while', got {:?}",
                expr_type
            ));
        }
        CodeGenerator::add_instruction("pop ax");
        CodeGenerator::add_instruction("cmp ax, 0");
        CodeGenerator::add_instruction(&format!("je {}", end_label));
        self.parse_instructions(&[Lexems::EndWhile]);
        CodeGenerator::add_instruction(&format!("jmp {}", start_label));
        self.expect_lexem(Lexems::EndWhile);
        CodeGenerator::add_instruction(&format!("{}:", end_label));
        self.maybe_advance();
    }
}
