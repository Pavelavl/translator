use crate::lexical_analyzer::LexicalAnalyzer;
use crate::models::{BinOp, Category, DataType, ExprKind, Expression, Lexems, Program, Statement, UnaryOp};
use crate::name_table::NameTable;

#[derive(Debug, Clone)]
pub struct Error {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

pub struct SyntaxAnalyzer {
    pub lexer: LexicalAnalyzer,
    pub name_table: NameTable,
    pub errors: Vec<Error>,
    pub current_line: usize,
    pub instruction_count: usize,
}

impl SyntaxAnalyzer {
    pub fn new(mut lexer: LexicalAnalyzer) -> Self {
        println!("[SyntaxAnalyzer] Initializing new SyntaxAnalyzer");
        lexer.init_keywords();
        let analyzer = Self {
            lexer,
            name_table: NameTable::new(),
            errors: Vec::new(),
            current_line: 0,
            instruction_count: 0,
        };
        println!("[SyntaxAnalyzer] Initialization complete");
        analyzer
    }

    fn can_be_bool(&self, expr: &Expression) -> bool {
        let result = match &expr.kind {
            ExprKind::Literal(v) => *v == 0 || *v == 1,
            ExprKind::Variable(n) => self
                .name_table
                .find_by_name(n)
                .map(|i| i.data_type == DataType::Bool)
                .unwrap_or(false),
            _ => expr.typ == DataType::Bool,
        };
        println!("[SyntaxAnalyzer] Checking if expression can be bool: expr={:?}, result={}", expr, result);
        result
    }

    fn parse_expression(&mut self) -> Expression {
        println!("[SyntaxAnalyzer] Starting parse_expression");
        let expr = self.parse_logical();
        println!("[SyntaxAnalyzer] Completed parse_expression: {:?}", expr);
        expr
    }

    fn parse_logical(&mut self) -> Expression {
        println!("[SyntaxAnalyzer] Starting parse_logical");
        let left = self.parse_comparison();
        if matches!(
            self.lexer.current_lexem(),
            Lexems::And | Lexems::Or | Lexems::Xor
        ) {
            let mut expr = left;
            while matches!(
                self.lexer.current_lexem(),
                Lexems::And | Lexems::Or | Lexems::Xor
            ) {
                let op = match self.lexer.current_lexem() {
                    Lexems::And => BinOp::And,
                    Lexems::Or => BinOp::Or,
                    Lexems::Xor => BinOp::Xor,
                    _ => unreachable!(),
                };
                println!("[SyntaxAnalyzer] Parsing logical operator: {:?}", op);
                self.lexer.advance();
                let right = self.parse_comparison();
                if !self.can_be_bool(&expr) || !self.can_be_bool(&right) {
                    self.report_error(format!("Type mismatch: expected Bool operands for logical operator"));
                }
                expr = Expression {
                    kind: ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    typ: DataType::Bool,
                };
            }
            println!("[SyntaxAnalyzer] Completed parse_logical: {:?}", expr);
            expr
        } else {
            println!("[SyntaxAnalyzer] No logical operator, returning: {:?}", left);
            left
        }
    }

    fn parse_comparison(&mut self) -> Expression {
        println!("[SyntaxAnalyzer] Starting parse_comparison");
        let left = self.parse_add_sub();
        if matches!(
            self.lexer.current_lexem(),
            Lexems::Equal
                | Lexems::NotEqual
                | Lexems::Less
                | Lexems::LessOrEqual
                | Lexems::Greater
                | Lexems::GreaterOrEqual
        ) {
            let op = match self.lexer.current_lexem() {
                Lexems::Equal => BinOp::Eq,
                Lexems::NotEqual => BinOp::Ne,
                Lexems::Less => BinOp::Lt,
                Lexems::LessOrEqual => BinOp::Le,
                Lexems::Greater => BinOp::Gt,
                Lexems::GreaterOrEqual => BinOp::Ge,
                _ => unreachable!(),
            };
            println!("[SyntaxAnalyzer] Parsing comparison operator: {:?}", op);
            self.lexer.advance();
            let right = self.parse_add_sub();
            let typ = if left.typ != right.typ || left.typ == DataType::None || right.typ == DataType::None {
                self.report_error(format!(
                    "Type mismatch in comparison: expected {:?}, found {:?}",
                    left.typ, right.typ
                ));
                DataType::None
            } else {
                DataType::Bool
            };
            let expr = Expression {
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                typ,
            };
            println!("[SyntaxAnalyzer] Completed parse_comparison: {:?}", expr);
            expr
        } else {
            println!("[SyntaxAnalyzer] No comparison operator, returning: {:?}", left);
            left
        }
    }

    fn parse_add_sub(&mut self) -> Expression {
        println!("[SyntaxAnalyzer] Starting parse_add_sub");
        let mut left = self.parse_mul_div();
        while matches!(self.lexer.current_lexem(), Lexems::Plus | Lexems::Minus) {
            let op = match self.lexer.current_lexem() {
                Lexems::Plus => BinOp::Add,
                Lexems::Minus => BinOp::Sub,
                _ => unreachable!(),
            };
            println!("[SyntaxAnalyzer] Parsing add/sub operator: {:?}", op);
            self.lexer.advance();
            let right = self.parse_mul_div();
            let typ = if left.typ != right.typ || left.typ == DataType::None || right.typ == DataType::None {
                self.report_error(format!(
                    "Type mismatch in add/sub: expected {:?}, found {:?}",
                    left.typ, right.typ
                ));
                DataType::None
            } else {
                left.typ.clone()
            };
            left = Expression {
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                typ,
            };
        }
        println!("[SyntaxAnalyzer] Completed parse_add_sub: {:?}", left);
        left
    }

    fn parse_mul_div(&mut self) -> Expression {
        println!("[SyntaxAnalyzer] Starting parse_mul_div");
        let mut left = self.parse_term();
        while matches!(self.lexer.current_lexem(), Lexems::Mul | Lexems::Div) {
            let op = match self.lexer.current_lexem() {
                Lexems::Mul => BinOp::Mul,
                Lexems::Div => BinOp::Div,
                _ => unreachable!(),
            };
            println!("[SyntaxAnalyzer] Parsing mul/div operator: {:?}", op);
            self.lexer.advance();
            let right = self.parse_term();
            let typ = if left.typ != right.typ || left.typ == DataType::None || right.typ == DataType::None {
                self.report_error(format!(
                    "Type mismatch in mul/div: expected {:?}, found {:?}",
                    left.typ, right.typ
                ));
                DataType::None
            } else {
                left.typ.clone()
            };
            left = Expression {
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                typ,
            };
        }
        println!("[SyntaxAnalyzer] Completed parse_mul_div: {:?}", left);
        left
    }

    fn parse_term(&mut self) -> Expression {
        println!("[SyntaxAnalyzer] Starting parse_term");
        if self.lexer.current_lexem() == Lexems::Not {
            println!("[SyntaxAnalyzer] Parsing unary .NOT.");
            self.lexer.advance();
            let expr = self.parse_term();
            let typ = if expr.typ != DataType::Bool {
                self.report_error(format!("Expected Bool after .NOT., found {:?}", expr.typ));
                DataType::None
            } else {
                DataType::Bool
            };
            let result = Expression {
                kind: ExprKind::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                },
                typ,
            };
            println!("[SyntaxAnalyzer] Completed parse_term (unary): {:?}", result);
            result
        } else if self.lexer.current_lexem() == Lexems::LParen {
            println!("[SyntaxAnalyzer] Parsing parenthesized expression");
            self.lexer.advance();
            let expr = self.parse_expression();
            self.expect_lexem(Lexems::RParen);
            println!("[SyntaxAnalyzer] Completed parse_term (parens): {:?}", expr);
            expr
        } else if self.lexer.current_lexem() == Lexems::Begin {
            println!("[SyntaxAnalyzer] Parsing begin-end expression");
            self.lexer.advance();
            let expr = self.parse_expression();
            self.expect_lexem(Lexems::End);
            println!("[SyntaxAnalyzer] Completed parse_term (begin-end): {:?}", expr);
            expr
        } else if self.lexer.current_lexem() == Lexems::Name {
            let name = self.lexer.current_name();
            println!("[SyntaxAnalyzer] Parsing identifier: '{}'", name);
            if let Some(ident) = self.name_table.find_by_name(&name) {
                let typ = ident.data_type.clone();
                self.lexer.advance();
                let expr = Expression {
                    kind: ExprKind::Variable(name),
                    typ,
                };
                println!("[SyntaxAnalyzer] Completed parse_term (variable): {:?}", expr);
                expr
            } else {
                self.report_error(format!("Undeclared identifier '{}'", name));
                self.lexer.advance();
                let expr = Expression {
                    kind: ExprKind::Literal(0),
                    typ: DataType::None,
                };
                println!("[SyntaxAnalyzer] Completed parse_term (undeclared): {:?}", expr);
                expr
            }
        } else if self.lexer.current_lexem() == Lexems::Number {
            let value_str = self.lexer.current_name();
            let value = value_str.parse::<i32>().unwrap_or(0);
            println!("[SyntaxAnalyzer] Parsing number: '{}'", value_str);
            self.lexer.advance();
            let expr = Expression {
                kind: ExprKind::Literal(value),
                typ: DataType::Int,
            };
            println!("[SyntaxAnalyzer] Completed parse_term (number): {:?}", expr);
            expr
        } else {
            self.report_error("Unexpected token in expression".to_string());
            self.lexer.advance();
            let expr = Expression {
                kind: ExprKind::Literal(0),
                typ: DataType::None,
            };
            println!("[SyntaxAnalyzer] Completed parse_term (error): {:?}", expr);
            expr
        }
    }

    fn parse_assignment(&mut self) -> Vec<Statement> {
        println!("[SyntaxAnalyzer] Starting parse_assignment at line {}", self.lexer.line);
        if self.instruction_count > 0 && self.current_line == self.lexer.line {
            self.report_error("Only one assignment per line allowed".to_string());
            self.lexer.advance();
            println!("[SyntaxAnalyzer] Aborted parse_assignment due to multiple assignments");
            return vec![];
        }
        self.instruction_count += 1;

        let name = self.lexer.current_name();
        println!("[SyntaxAnalyzer] Parsing assignment to identifier: '{}'", name);
        if self.name_table.find_by_name(&name).is_none() {
            self.report_error(format!("Undeclared identifier '{}'", name));
            self.lexer.advance();
            println!("[SyntaxAnalyzer] Aborted parse_assignment due to undeclared identifier");
            return vec![];
        }
        self.lexer.advance();
        if !matches!(self.lexer.current_lexem(), Lexems::Equal | Lexems::Assign) {
            self.report_error("Expected '=' or ':=' after identifier".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_assignment due to missing assignment operator");
            return vec![];
        }
        self.lexer.advance();

        let expected_type = self
            .name_table
            .find_by_name(&name)
            .map(|i| i.data_type.clone())
            .unwrap_or(DataType::None);
        println!("[SyntaxAnalyzer] Expected type for '{}': {:?}", name, expected_type);

        let expr = self.parse_expression();
        let mut typ_mismatch = false;
        if expected_type == DataType::Bool {
            if !self.can_be_bool(&expr) {
                self.report_error(format!(
                    "Type mismatch: variable '{}' expected Bool, got invalid value for Bool",
                    name
                ));
                typ_mismatch = true;
            }
        } else if expected_type != expr.typ {
            self.report_error(format!(
                "Type mismatch: variable '{}' expected {:?}, got {:?}",
                name, expected_type, expr.typ
            ));
            typ_mismatch = true;
        }
        if typ_mismatch {
            println!("[SyntaxAnalyzer] Aborted parse_assignment due to type mismatch");
            return vec![];
        }

        if self.lexer.current_lexem() == Lexems::Semi {
            self.lexer.advance();
        }

        let stmt = vec![Statement::Assign { name, expr }];
        println!("[SyntaxAnalyzer] Completed parse_assignment: {:?}", stmt);
        stmt
    }

    pub fn report_error(&mut self, message: String) {
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
        println!("[SyntaxAnalyzer] Expecting lexem: {:?}", expected);
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

    pub fn maybe_advance(&mut self) {
        println!("[SyntaxAnalyzer] Starting maybe_advance");
        while matches!(self.lexer.current_lexem(), Lexems::NewLine | Lexems::Semi) {
            println!("[SyntaxAnalyzer] Skipping token: {:?}", self.lexer.current_lexem());
            self.lexer.advance();
        }
        println!("[SyntaxAnalyzer] Completed maybe_advance, current lexem: {:?}", self.lexer.current_lexem());
    }

    fn parse_print(&mut self) -> Vec<Statement> {
        println!("[SyntaxAnalyzer] Starting parse_print");
        if self.instruction_count > 0 && self.current_line == self.lexer.line {
            self.report_error("Only one instruction per line allowed".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_print due to multiple instructions");
            return vec![];
        }
        self.instruction_count += 1;

        self.lexer.advance();
        if self.lexer.current_lexem() == Lexems::Name {
            let var = self.lexer.current_name();
            println!("[SyntaxAnalyzer] Parsing print for variable: '{}'", var);
            if self.name_table.find_by_name(&var).is_some() {
                self.lexer.advance();
                let stmt = vec![Statement::Print { var }];
                println!("[SyntaxAnalyzer] Completed parse_print: {:?}", stmt);
                stmt
            } else {
                self.report_error(format!("Undeclared identifier '{}'", var));
                self.lexer.advance();
                println!("[SyntaxAnalyzer] Aborted parse_print due to undeclared identifier");
                vec![]
            }
        } else {
            self.report_error("Expected identifier after 'print'".to_string());
            self.lexer.advance();
            println!("[SyntaxAnalyzer] Aborted parse_print due to missing identifier");
            vec![]
        }
    }

    fn parse_statement(&mut self) -> Vec<Statement> {
        println!("[SyntaxAnalyzer] Starting parse_statement at line {}", self.lexer.line);
        self.maybe_advance();
        let result = match self.lexer.current_lexem() {
            Lexems::Print => {
                println!("[SyntaxAnalyzer] Parsing print statement");
                self.parse_print()
            }
            Lexems::Name => {
                println!("[SyntaxAnalyzer] Parsing assignment statement");
                self.parse_assignment()
            }
            Lexems::Int | Lexems::Bool => {
                println!("[SyntaxAnalyzer] Parsing variable declaration");
                self.parse_variable_declarations()
            }
            Lexems::Begin => {
                println!("[SyntaxAnalyzer] Parsing block");
                vec![self.parse_block()]
            }
            Lexems::If => {
                println!("[SyntaxAnalyzer] Parsing if statement");
                vec![self.parse_if()]
            }
            Lexems::While => {
                println!("[SyntaxAnalyzer] Parsing while statement");
                vec![self.parse_while()]
            }
            Lexems::End => {
                println!("[SyntaxAnalyzer] Encountered End token, stopping statement parsing");
                vec![] // Просто завершаем парсинг оператора, не добавляя его
            }
            Lexems::EOF => {
                println!("[SyntaxAnalyzer] Reached EOF");
                vec![]
            }
            _ => {
                self.report_error(format!(
                    "Unexpected token in instruction: {:?} ('{}')",
                    self.lexer.current_lexem(),
                    self.lexer.current_name()
                ));
                self.lexer.advance();
                println!("[SyntaxAnalyzer] Aborted parse_statement due to unexpected token");
                vec![]
            }
        };
        println!("[SyntaxAnalyzer] Completed parse_statement: {:?}", result);
        result
    }

    fn parse_statements(&mut self, end_tokens: &[Lexems]) -> Vec<Statement> {
        println!("[SyntaxAnalyzer] Starting parse_statements, end_tokens: {:?}", end_tokens);
        let mut stmts = vec![];
        while !end_tokens.contains(&self.lexer.current_lexem())
            && self.lexer.current_lexem() != Lexems::EOF
        {
            stmts.extend(self.parse_statement());
        }
        println!("[SyntaxAnalyzer] Completed parse_statements, {} statements", stmts.len());
        stmts
    }

    fn parse_variable_declarations(&mut self) -> Vec<Statement> {
        println!("[SyntaxAnalyzer] Starting parse_variable_declarations at line {}", self.lexer.line);
        if self.instruction_count > 0 && self.lexer.line == self.current_line {
            self.report_error("Only one declaration per line allowed".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to multiple declarations");
            return vec![];
        }
        self.instruction_count += 1;

        let is_const = self.lexer.current_name().to_lowercase().ends_with("_const");
        let typ = match self.lexer.current_lexem() {
            Lexems::Int => DataType::Int,
            Lexems::Bool => DataType::Bool,
            _ => {
                self.report_error("Expected 'int' or 'bool'".to_string());
                self.lexer.advance();
                println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to invalid type");
                return vec![];
            }
        };
        println!("[SyntaxAnalyzer] Parsing variable declaration: type={:?}, is_const={}", typ, is_const);
        self.lexer.advance();
        if self.lexer.current_lexem() != Lexems::Name {
            self.report_error("Expected variable name".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to missing variable name");
            return vec![];
        }
        let mut names = vec![self.lexer.current_name()];
        self.lexer.advance();
        while self.lexer.current_lexem() == Lexems::Comma {
            self.lexer.advance();
            if self.lexer.current_lexem() != Lexems::Name {
                self.report_error("Expected variable name after comma".to_string());
                println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to missing variable name after comma");
                return vec![];
            }
            names.push(self.lexer.current_name());
            self.lexer.advance();
        }
        let init: Option<Expression>;
        if self.lexer.current_lexem() == Lexems::Colon {
            self.lexer.advance();
            init = None;
            for name in &names {
                self.name_table.add_or_update(
                    name.clone(),
                    if is_const { Category::Const } else { Category::Var },
                    typ.clone(),
                    None,
                );
            }
        } else if self.lexer.current_lexem() == Lexems::Assign {
            self.lexer.advance();
            if names.len() > 1 {
                self.report_error("Multiple variables cannot have initializer".to_string());
                println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to multiple initializers");
                return vec![];
            }
            if is_const {
                if self.lexer.current_lexem() == Lexems::Number {
                    let value_str = self.lexer.current_name();
                    if let Ok(v) = value_str.parse::<i32>() {
                        if typ == DataType::Bool && v != 0 && v != 1 {
                            self.report_error(format!(
                                "Invalid constant '{}', expected 0 or 1 for Boolean",
                                value_str
                            ));
                            println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to invalid boolean constant");
                            return vec![];
                        }
                        init = Some(Expression {
                            kind: ExprKind::Literal(v),
                            typ: typ.clone(),
                        });
                        self.lexer.advance();
                        self.name_table.add_or_update(
                            names[0].clone(),
                            Category::Const,
                            typ.clone(),
                            Some(v),
                        );
                    } else {
                        self.report_error("Invalid number".to_string());
                        println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to invalid number");
                        return vec![];
                    }
                } else {
                    self.report_error("Constant must be initialized with a number literal".to_string());
                    self.skip_expression();
                    println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to non-numeric constant initializer");
                    return vec![];
                }
            } else {
                let expr = self.parse_expression();
                if expr.typ != typ {
                    self.report_error(format!(
                        "Type mismatch: expected {:?}, found {:?}",
                        typ, expr.typ
                    ));
                    println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to type mismatch");
                    return vec![];
                }
                init = Some(expr);
                self.name_table.add_or_update(
                    names[0].clone(),
                    Category::Var,
                    typ.clone(),
                    None,
                );
            }
        } else if self.lexer.current_lexem() == Lexems::Semi {
            self.lexer.advance();
            init = None;
            for name in &names {
                self.name_table.add_or_update(
                    name.clone(),
                    if is_const { Category::Const } else { Category::Var },
                    typ.clone(),
                    None,
                );
            }
        } else {
            self.report_error("Expected ':', ':=', or ';' after variable name(s)".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_variable_declarations due to unexpected token");
            return vec![];
        }
        let stmt = vec![Statement::VarDecl {
            typ,
            is_const,
            names,
            init,
        }];
        println!("[SyntaxAnalyzer] Completed parse_variable_declarations: {:?}", stmt);
        stmt
    }

    pub fn parse_program(&mut self) -> Program {
        println!("[SyntaxAnalyzer] Starting parse_program");
        let mut stmts = vec![];
        self.current_line = self.lexer.line;

        // Парсим объявления переменных
        while matches!(self.lexer.current_lexem(), Lexems::Int | Lexems::Bool) {
            println!("[SyntaxAnalyzer] Parsing variable declarations");
            stmts.extend(self.parse_statement());
            self.current_line = self.lexer.line;
        }

        // Парсим основной блок программы
        if self.lexer.current_lexem() == Lexems::Begin {
            println!("[SyntaxAnalyzer] Parsing computation block");
            stmts.extend(self.parse_statement());
        } else {
            self.report_error("Expected 'Begin' for computations".to_string());
            println!("[SyntaxAnalyzer] Missing 'Begin' for computation block");
        }

        // Парсим необязательные операторы print
        self.maybe_advance();
        while self.lexer.current_lexem() == Lexems::Print {
            println!("[SyntaxAnalyzer] Parsing print statement");
            stmts.extend(self.parse_statement());
            self.maybe_advance();
        }

        // Проверяем, что после программы нет лишних токенов
        if self.lexer.current_lexem() != Lexems::EOF {
            self.report_error("Unexpected tokens after program".to_string());
            println!("[SyntaxAnalyzer] Found unexpected tokens after program");
        }

        let program = Program { stmts };
        println!("[SyntaxAnalyzer] Completed parse_program: {} statements", program.stmts.len());
        program
    }

    pub fn parse_variable_declarations_no_code(&mut self) {
        println!("[SyntaxAnalyzer] Starting parse_variable_declarations_no_code");
        let is_const = self.lexer.current_name().to_lowercase().ends_with("_const");
        let data_type = match self.lexer.current_lexem() {
            Lexems::Int => DataType::Int,
            Lexems::Bool => DataType::Bool,
            _ => {
                self.report_error("Expected 'int' or 'bool'".to_string());
                self.lexer.advance();
                println!("[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to invalid type");
                return;
            }
        };
        println!("[SyntaxAnalyzer] Declaration type: {:?}", data_type);
        self.lexer.advance();
        if self.lexer.current_lexem() != Lexems::Name {
            self.report_error("Expected variable name".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to missing variable name");
            return;
        }
        let mut names = vec![self.lexer.current_name()];
        self.lexer.advance();
        while self.lexer.current_lexem() == Lexems::Comma {
            self.lexer.advance();
            if self.lexer.current_lexem() != Lexems::Name {
                self.report_error("Expected variable name after comma".to_string());
                println!("[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to missing variable name after comma");
                return;
            }
            names.push(self.lexer.current_name());
            self.lexer.advance();
        }
        let mut value = None;
        if self.lexer.current_lexem() == Lexems::Colon {
            self.lexer.advance();
            if self.lexer.current_lexem() == Lexems::Assign {
                self.report_error("Unexpected ':=' after ':' in declaration".to_string());
                self.lexer.advance();
                self.skip_expression();
                println!("[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to unexpected ':='");
                return;
            }
        } else if self.lexer.current_lexem() == Lexems::Assign {
            self.lexer.advance();
            if is_const {
                if self.lexer.current_lexem() == Lexems::Number {
                    let value_str = self.lexer.current_name();
                    if let Ok(num) = value_str.parse::<i32>() {
                        if data_type == DataType::Bool && num != 0 && num != 1 {
                            self.report_error(format!(
                                "Invalid constant '{}', expected 0 or 1 for Boolean",
                                value_str
                            ));
                        } else {
                            value = Some(num);
                        }
                        self.lexer.advance();
                    } else {
                        self.report_error("Invalid number".to_string());
                    }
                } else {
                    self.report_error("Constant must be initialized with a number literal".to_string());
                    self.skip_expression();
                    println!("[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to non-numeric constant initializer");
                    return;
                }
            } else {
                self.skip_expression();
            }
        } else if self.lexer.current_lexem() == Lexems::Semi {
            self.lexer.advance();
        } else {
            self.report_error("Expected ':', ':=', or ';' after variable name(s)".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to unexpected token");
            return;
        }
        for name in names.iter() {
            self.name_table.add_or_update(
                name.clone(),
                if is_const { Category::Const } else { Category::Var },
                data_type.clone(),
                if name == &names[0] { value } else { None },
            );
        }
        println!("[SyntaxAnalyzer] Completed parse_variable_declarations_no_code: names={:?}, value={:?}", names, value);
    }

    fn skip_expression(&mut self) {
        println!("[SyntaxAnalyzer] Starting skip_expression");
        while self.lexer.current_lexem() != Lexems::Semi
            && self.lexer.current_lexem() != Lexems::EOF
            && self.lexer.current_lexem() != Lexems::Then
            && self.lexer.current_lexem() != Lexems::EndWhile
        {
            self.lexer.advance();
        }
        println!("[SyntaxAnalyzer] Completed skip_expression, current lexem: {:?}", self.lexer.current_lexem());
    }

    fn parse_block(&mut self) -> Statement {
        println!("[SyntaxAnalyzer] Starting parse_block");
        self.lexer.advance();
        let stmts = self.parse_statements(&[Lexems::End]);
        self.expect_lexem(Lexems::End);
        let block = Statement::Block { stmts };
        println!("[SyntaxAnalyzer] Completed parse_block: {:?}", block);
        block
    }

    fn parse_if(&mut self) -> Statement {
        println!("[SyntaxAnalyzer] Starting parse_if");
        self.lexer.advance();
        let cond = self.parse_expression();
        if cond.typ != DataType::Bool {
            self.report_error(format!(
                "Expected boolean expression in 'if', got {:?}",
                cond.typ
            ));
        }
        self.expect_lexem(Lexems::Then);
        let then = self.parse_statements(&[Lexems::ElseIf, Lexems::Else, Lexems::EndIf]);
        let mut elseifs = vec![];
        while self.lexer.current_lexem() == Lexems::ElseIf {
            self.lexer.advance();
            let elseif_cond = self.parse_expression();
            if elseif_cond.typ != DataType::Bool {
                self.report_error(format!(
                    "Expected boolean expression in 'elseif', got {:?}",
                    elseif_cond.typ
                ));
            }
            self.expect_lexem(Lexems::Then);
            let elseif_stmts = self.parse_statements(&[Lexems::ElseIf, Lexems::Else, Lexems::EndIf]);
            elseifs.push((elseif_cond, elseif_stmts));
        }
        let els = if self.lexer.current_lexem() == Lexems::Else {
            self.lexer.advance();
            Some(self.parse_statements(&[Lexems::EndIf]))
        } else {
            None
        };
        self.expect_lexem(Lexems::EndIf);
        let stmt = Statement::If {
            cond,
            then,
            elseifs,
            els,
        };
        println!("[SyntaxAnalyzer] Completed parse_if: {:?}", stmt);
        stmt
    }

    fn parse_while(&mut self) -> Statement {
        println!("[SyntaxAnalyzer] Starting parse_while");
        self.lexer.advance();
        let cond = self.parse_expression();
        if cond.typ != DataType::Bool {
            self.report_error(format!(
                "Expected boolean expression in 'while', got {:?}",
                cond.typ
            ));
        }
        let body = self.parse_statements(&[Lexems::EndWhile]);
        self.expect_lexem(Lexems::EndWhile);
        let stmt = Statement::While { cond, body };
        println!("[SyntaxAnalyzer] Completed parse_while: {:?}", stmt);
        stmt
    }
}