use crate::lexical_analyzer::LexicalAnalyzerTrait;
use crate::models::{
    BinOp, Category, DataType, ExprKind, Expression, Lexems, Program, Statement, UnaryOp,
};
use crate::name_table::NameTable;

#[derive(Debug, Clone)]
pub struct Error {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

pub struct SyntaxAnalyzer<L: LexicalAnalyzerTrait> {
    pub lexer: L,
    pub name_table: NameTable,
    pub errors: Vec<Error>,
    pub current_line: usize,
    pub instruction_count: usize,
}

impl<L: LexicalAnalyzerTrait> SyntaxAnalyzer<L> {
    pub fn new(mut lexer: L) -> Self {
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
        println!(
            "[SyntaxAnalyzer] Checking if expression can be bool: expr={:?}, result={}",
            expr, result
        );
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
                    self.report_error(format!(
                        "Type mismatch: expected Bool operands for logical operator"
                    ));
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
            println!(
                "[SyntaxAnalyzer] No logical operator, returning: {:?}",
                left
            );
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
            let typ = if left.typ != right.typ
                || left.typ == DataType::None
                || right.typ == DataType::None
            {
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
            println!(
                "[SyntaxAnalyzer] No comparison operator, returning: {:?}",
                left
            );
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
            let typ = if left.typ != right.typ
                || left.typ == DataType::None
                || right.typ == DataType::None
            {
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
            let typ = if left.typ != right.typ
                || left.typ == DataType::None
                || right.typ == DataType::None
            {
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
            println!(
                "[SyntaxAnalyzer] Completed parse_term (unary): {:?}",
                result
            );
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
            println!(
                "[SyntaxAnalyzer] Completed parse_term (begin-end): {:?}",
                expr
            );
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
                println!(
                    "[SyntaxAnalyzer] Completed parse_term (variable): {:?}",
                    expr
                );
                expr
            } else {
                self.report_error(format!("Undeclared identifier '{}'", name));
                self.lexer.advance();
                let expr = Expression {
                    kind: ExprKind::Literal(0),
                    typ: DataType::None,
                };
                println!(
                    "[SyntaxAnalyzer] Completed parse_term (undeclared): {:?}",
                    expr
                );
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
        println!(
            "[SyntaxAnalyzer] Starting parse_assignment at line {}",
            self.lexer.line()
        );
        let name = self.lexer.current_name();
        println!(
            "[SyntaxAnalyzer] Parsing assignment to identifier: '{}'",
            name
        );
        if self.instruction_count > 0 && self.current_line == self.lexer.line() {
            self.report_error("Only one assignment per line allowed".to_string());
            self.lexer.advance();
            println!("[SyntaxAnalyzer] Aborted parse_assignment due to multiple assignments");
            return vec![];
        }
        self.instruction_count += 1; // Increment instruction_count for this assignment
        if self.name_table.find_by_name(&name).is_none() {
            self.report_error(format!("Undeclared identifier '{}'", name));
            self.lexer.advance();
            println!("[SyntaxAnalyzer] Aborted parse_assignment due to undeclared identifier");
            return vec![];
        }
        self.lexer.advance();
        if !matches!(self.lexer.current_lexem(), Lexems::Equal | Lexems::Assign) {
            self.report_error("Expected '=' or ':=' after identifier".to_string());
            println!(
                "[SyntaxAnalyzer] Aborted parse_assignment due to missing assignment operator"
            );
            return vec![];
        }
        self.lexer.advance();
    
        let expected_type = self
            .name_table
            .find_by_name(&name)
            .map(|i| i.data_type.clone())
            .unwrap_or(DataType::None);
        println!(
            "[SyntaxAnalyzer] Expected type for '{}': {:?}",
            name, expected_type
        );
    
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
            self.lexer.line(),
            self.lexer.col(),
            message,
            self.lexer.current_lexem(),
            self.lexer.current_name()
        );
        self.errors.push(Error {
            line: self.lexer.line(),
            col: self.lexer.col(),
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
            println!(
                "[SyntaxAnalyzer] Skipping token: {:?}",
                self.lexer.current_lexem()
            );
            self.lexer.advance();
        }
        println!(
            "[SyntaxAnalyzer] Completed maybe_advance, current lexem: {:?}",
            self.lexer.current_lexem()
        );
    }

    fn parse_print(&mut self) -> Vec<Statement> {
        println!("[SyntaxAnalyzer] Starting parse_print");
        if self.instruction_count > 0 && self.current_line == self.lexer.line() {
            self.report_error("Only one instruction per line allowed".to_string());
            println!("[SyntaxAnalyzer] Aborted parse_print due to multiple instructions");
            self.lexer.advance();
            return vec![];
        }
        self.instruction_count += 1;

        self.lexer.advance();
        match self.lexer.current_lexem() {
            Lexems::Name => {
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
            }
            Lexems::Number => {
                let value_str = self.lexer.current_name();
                let value = value_str.parse::<i32>().unwrap_or(0);
                println!("[SyntaxAnalyzer] Parsing print for number: '{}'", value_str);
                self.lexer.advance();
                let temp_var = format!("_temp_{}", self.instruction_count);
                // Add the temporary variable to the NameTable
                self.name_table.add_or_update(
                    temp_var.clone(),
                    Category::Const,
                    DataType::Int,
                    Some(value),
                );
                let stmt = vec![
                    Statement::VarDecl {
                        typ: DataType::Int,
                        is_const: true,
                        names: vec![temp_var.clone()],
                        init: Some(Expression {
                            kind: ExprKind::Literal(value),
                            typ: DataType::Int,
                        }),
                    },
                    Statement::Print { var: temp_var },
                ];
                println!("[SyntaxAnalyzer] Completed parse_print: {:?}", stmt);
                stmt
            }
            _ => {
                self.report_error("Expected identifier or number after 'print'".to_string());
                self.lexer.advance();
                println!("[SyntaxAnalyzer] Aborted parse_print due to missing identifier");
                vec![]
            }
        }
    }

    fn parse_statement(&mut self) -> Vec<Statement> {
        println!(
            "[SyntaxAnalyzer] Starting parse_statement at line {}",
            self.lexer.line()
        );
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
                self.lexer.advance();
                vec![]
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
        // Reset instruction_count after each statement to allow multiple statements per line
        self.instruction_count = 0;
        self.current_line = self.lexer.line();
        println!("[SyntaxAnalyzer] Completed parse_statement: {:?}", result);
        result
    }

    fn parse_statements(&mut self, end_tokens: &[Lexems]) -> Vec<Statement> {
        println!(
            "[SyntaxAnalyzer] Starting parse_statements, end_tokens: {:?}",
            end_tokens
        );
        let mut stmts = vec![];
        while !end_tokens.contains(&self.lexer.current_lexem())
            && self.lexer.current_lexem() != Lexems::EOF
        {
            let new_stmts = self.parse_statement();
            stmts.extend(new_stmts);
            self.maybe_advance(); // Ensure we advance past newlines/semicolons after each statement
        }
        println!(
            "[SyntaxAnalyzer] Completed parse_statements, {} statements",
            stmts.len()
        );
        stmts
    }

    fn parse_variable_declarations(&mut self) -> Vec<Statement> {
        println!(
            "[SyntaxAnalyzer] Starting parse_variable_declarations at line {}",
            self.lexer.line()
        );
        if self.instruction_count > 0 && self.lexer.line() == self.current_line {
            self.report_error("Only one declaration per line allowed".to_string());
            println!(
                "[SyntaxAnalyzer] Aborted parse_variable_declarations due to multiple declarations"
            );
            return vec![];
        }
        self.instruction_count += 1;

        let typ = match self.lexer.current_lexem() {
            Lexems::Int => DataType::Int,
            Lexems::Bool => DataType::Bool,
            _ => {
                self.report_error("Expected 'int' or 'bool'".to_string());
                self.lexer.advance();
                println!(
                    "[SyntaxAnalyzer] Aborted parse_variable_declarations due to invalid type"
                );
                return vec![];
            }
        };
        self.lexer.advance();

        // Check for "const" keyword after type
        let mut is_const = false;
        if matches!(self.lexer.current_lexem(), Lexems::Name)
            && self.lexer.current_name().to_lowercase() == "const"
        {
            is_const = true;
            self.lexer.advance();
        }

        println!(
            "[SyntaxAnalyzer] Parsing variable declaration: type={:?}, is_const={}",
            typ, is_const
        );

        if self.lexer.current_lexem() != Lexems::Name {
            self.report_error("Expected variable name".to_string());
            println!(
                "[SyntaxAnalyzer] Aborted parse_variable_declarations due to missing variable name"
            );
            return vec![];
        }
        let mut names = vec![self.lexer.current_name()];
        self.lexer.advance();
        while self.lexer.current_lexem() == Lexems::Comma {
            self.lexer.advance();
            if self.lexer.current_lexem() != Lexems::Name {
                self.report_error("Expected variable name after comma".to_string());
                println!(
                    "[SyntaxAnalyzer] Aborted parse_variable_declarations due to missing variable name after comma"
                );
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
                    if is_const {
                        Category::Const
                    } else {
                        Category::Var
                    },
                    typ.clone(),
                    None,
                );
            }
        } else if self.lexer.current_lexem() == Lexems::Assign {
            self.lexer.advance();
            if names.len() > 1 {
                self.report_error("Multiple variables cannot have initializer".to_string());
                println!(
                    "[SyntaxAnalyzer] Aborted parse_variable_declarations due to multiple initializers"
                );
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
                            println!(
                                "[SyntaxAnalyzer] Aborted parse_variable_declarations due to invalid boolean constant"
                            );
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
                        println!(
                            "[SyntaxAnalyzer] Aborted parse_variable_declarations due to invalid number"
                        );
                        return vec![];
                    }
                } else {
                    self.report_error(
                        "Constant must be initialized with a number literal".to_string(),
                    );
                    self.skip_expression();
                    println!(
                        "[SyntaxAnalyzer] Aborted parse_variable_declarations due to non-numeric constant initializer"
                    );
                    return vec![];
                }
            } else {
                let expr = self.parse_expression();
                if expr.typ != typ {
                    self.report_error(format!(
                        "Type mismatch: expected {:?}, found {:?}",
                        typ, expr.typ
                    ));
                    println!(
                        "[SyntaxAnalyzer] Aborted parse_variable_declarations due to type mismatch"
                    );
                    return vec![];
                }
                init = Some(expr);
                self.name_table
                    .add_or_update(names[0].clone(), Category::Var, typ.clone(), None);
            }
        } else if self.lexer.current_lexem() == Lexems::Semi {
            self.lexer.advance();
            init = None;
            for name in &names {
                self.name_table.add_or_update(
                    name.clone(),
                    if is_const {
                        Category::Const
                    } else {
                        Category::Var
                    },
                    typ.clone(),
                    None,
                );
            }
        } else {
            self.report_error("Expected ':', ':=', or ';' after variable name(s)".to_string());
            println!(
                "[SyntaxAnalyzer] Aborted parse_variable_declarations due to unexpected token"
            );
            return vec![];
        }
        let stmt = vec![Statement::VarDecl {
            typ,
            is_const,
            names,
            init,
        }];
        println!(
            "[SyntaxAnalyzer] Completed parse_variable_declarations: {:?}",
            stmt
        );
        stmt
    }

    pub fn parse_program(&mut self) -> Program {
        println!("[SyntaxAnalyzer] Starting parse_program");
        let mut stmts = vec![];
        self.current_line = self.lexer.line();
        self.instruction_count = 0; // Initialize instruction_count

        // Parse variable declarations
        while matches!(self.lexer.current_lexem(), Lexems::Int | Lexems::Bool) {
            println!("[SyntaxAnalyzer] Parsing variable declarations");
            stmts.extend(self.parse_statement());
            self.current_line = self.lexer.line();
            self.instruction_count = 0; // Reset instruction_count after each statement
        }

        // Parse main computation block if present
        if self.lexer.current_lexem() == Lexems::Begin {
            println!("[SyntaxAnalyzer] Parsing computation block");
            stmts.extend(self.parse_statement());
            self.current_line = self.lexer.line();
            self.instruction_count = 0; // Reset instruction_count after block
        } else if stmts.is_empty() && self.lexer.current_lexem() == Lexems::EOF {
            // Report error for empty program
            self.report_error("Expected 'Begin' for computations".to_string());
            println!("[SyntaxAnalyzer] Missing 'Begin' for computation block");
        }

        // Parse additional statements (e.g., print) until EOF
        self.maybe_advance();
        while self.lexer.current_lexem() != Lexems::EOF {
            println!("[SyntaxAnalyzer] Parsing additional statement");
            if self.current_line != self.lexer.line() {
                self.instruction_count = 0;
                self.current_line = self.lexer.line();
            }
            stmts.extend(self.parse_statement());
            self.maybe_advance();
        }

        let program = Program { stmts };
        println!(
            "[SyntaxAnalyzer] Completed parse_program: {} statements",
            program.stmts.len()
        );
        program
    }

    pub fn parse_variable_declarations_no_code(&mut self) {
        println!("[SyntaxAnalyzer] Starting parse_variable_declarations_no_code");
        let data_type = match self.lexer.current_lexem() {
            Lexems::Int => DataType::Int,
            Lexems::Bool => DataType::Bool,
            _ => {
                self.report_error("Expected 'int' or 'bool'".to_string());
                self.lexer.advance();
                println!(
                    "[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to invalid type"
                );
                return;
            }
        };
        println!("[SyntaxAnalyzer] Declaration type: {:?}", data_type);
        self.lexer.advance();

        // Check for "const" keyword after type
        let mut is_const = false;
        if matches!(self.lexer.current_lexem(), Lexems::Name)
            && self.lexer.current_name().to_lowercase() == "const"
        {
            is_const = true;
            self.lexer.advance();
        }

        if self.lexer.current_lexem() != Lexems::Name {
            self.report_error("Expected variable name".to_string());
            println!(
                "[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to missing variable name"
            );
            return;
        }
        let mut names = vec![self.lexer.current_name()];
        self.lexer.advance();
        while self.lexer.current_lexem() == Lexems::Comma {
            self.lexer.advance();
            if self.lexer.current_lexem() != Lexems::Name {
                self.report_error("Expected variable name after comma".to_string());
                println!(
                    "[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to missing variable name after comma"
                );
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
                println!(
                    "[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to unexpected ':='"
                );
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
                    self.report_error(
                        "Constant must be initialized with a number literal".to_string(),
                    );
                    self.skip_expression();
                    println!(
                        "[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to non-numeric constant initializer"
                    );
                    return;
                }
            } else {
                self.skip_expression();
            }
        } else if self.lexer.current_lexem() == Lexems::Semi {
            self.lexer.advance();
        } else {
            self.report_error("Expected ':', ':=', or ';' after variable name(s)".to_string());
            println!(
                "[SyntaxAnalyzer] Aborted parse_variable_declarations_no_code due to unexpected token"
            );
            return;
        }
        for name in names.iter() {
            self.name_table.add_or_update(
                name.clone(),
                if is_const {
                    Category::Const
                } else {
                    Category::Var
                },
                data_type.clone(),
                if name == &names[0] { value } else { None },
            );
        }
        println!(
            "[SyntaxAnalyzer] Completed parse_variable_declarations_no_code: names={:?}, value={:?}",
            names, value
        );
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
        println!(
            "[SyntaxAnalyzer] Completed skip_expression, current lexem: {:?}",
            self.lexer.current_lexem()
        );
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
            let elseif_stmts =
                self.parse_statements(&[Lexems::ElseIf, Lexems::Else, Lexems::EndIf]);
            elseifs.push((elseif_cond, elseif_stmts));
        }

        let els = if self.lexer.current_lexem() == Lexems::Else {
            self.lexer.advance();
            Some(self.parse_statements(&[Lexems::EndIf]))
        } else {
            None
        };

        self.expect_lexem(Lexems::EndIf);
        println!(
            "[SyntaxAnalyzer] After EndIf, current lexem: {:?}",
            self.lexer.current_lexem()
        );
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
        println!(
            "[SyntaxAnalyzer] After EndWhile, current lexem: {:?}",
            self.lexer.current_lexem()
        );
        let stmt = Statement::While { cond, body };
        println!("[SyntaxAnalyzer] Completed parse_while: {:?}", stmt);
        stmt
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::Mutex;

    use super::*;
    use crate::models::{
        BinOp, Category, DataType, ExprKind, Expression, Lexems, Statement, UnaryOp,
    };
    use mockall::mock;
    use mockall::predicate::*;

    // Mock для LexicalAnalyzer
    mock! {
        pub LexicalAnalyzer {
            fn init_keywords(&mut self);
            fn advance(&mut self);
            fn current_lexem(&self) -> Lexems;
            fn current_name(&self) -> String;
            fn line(&self) -> usize;
            fn col(&self) -> usize;
        }
    }

    impl LexicalAnalyzerTrait for MockLexicalAnalyzer {
        fn init_keywords(&mut self) {
            self.init_keywords();
        }

        fn advance(&mut self) {
            self.advance();
        }

        fn current_lexem(&self) -> Lexems {
            self.current_lexem()
        }

        fn current_name(&self) -> String {
            self.current_name()
        }

        fn line(&self) -> usize {
            self.line()
        }

        fn col(&self) -> usize {
            self.col()
        }
    }

    // Вспомогательная функция для создания SyntaxAnalyzer с мок-лексером
    fn setup_analyzer(
        tokens: Vec<(Lexems, String, usize, usize)>,
    ) -> SyntaxAnalyzer<MockLexicalAnalyzer> {
        let mut mock = MockLexicalAnalyzer::new();
        // Оборачиваем tokens в Arc<Mutex<Vec<_>>> для потокобезопасного разделения
        let tokens = Arc::new(Mutex::new(tokens));
        // Счетчик вызовов для отслеживания текущей позиции
        let call_count = Arc::new(Mutex::new(0));

        // Клонируем Arc для каждого замыкания
        let _tokens_clone = tokens.clone();
        let call_count_clone = call_count.clone();
        mock.expect_advance().returning(move || {
            let mut count = call_count_clone.lock().unwrap();
            *count += 1;
        });

        let tokens_clone = tokens.clone();
        let call_count_clone = call_count.clone();
        mock.expect_current_lexem().returning(move || {
            let tokens = tokens_clone.lock().unwrap();
            let count = *call_count_clone.lock().unwrap();
            tokens
                .get(count)
                .map(|(lexem, _, _, _)| lexem)
                .unwrap_or(&Lexems::EOF)
                .clone()
        });

        let tokens_clone = tokens.clone();
        let call_count_clone = call_count.clone();
        mock.expect_current_name().returning(move || {
            let tokens = tokens_clone.lock().unwrap();
            let count = *call_count_clone.lock().unwrap();
            tokens
                .get(count)
                .map(|(_, name, _, _)| name.clone())
                .unwrap_or_default()
        });

        let tokens_clone = tokens.clone();
        let call_count_clone = call_count.clone();
        mock.expect_line().returning(move || {
            let tokens = tokens_clone.lock().unwrap();
            let count = *call_count_clone.lock().unwrap();
            tokens.get(count).map(|(_, _, line, _)| *line).unwrap_or(1)
        });

        let tokens_clone = tokens.clone();
        let call_count_clone = call_count.clone();
        mock.expect_col().returning(move || {
            let tokens = tokens_clone.lock().unwrap();
            let count = *call_count_clone.lock().unwrap();
            tokens.get(count).map(|(_, _, _, col)| *col).unwrap_or(1)
        });

        mock.expect_init_keywords().return_once(|| {});

        SyntaxAnalyzer::new(mock)
    }

    #[test]
    fn test_parse_program_unexpected_tokens_after_end() {
        let tokens = vec![
            (Lexems::Begin, "Begin".to_string(), 1, 1),
            (Lexems::End, "End".to_string(), 2, 1),
            (Lexems::Number, "42".to_string(), 3, 1),
            (Lexems::EOF, String::new(), 3, 3),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let program = analyzer.parse_program();
        assert_eq!(program.stmts, vec![Statement::Block { stmts: vec![] }]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Unexpected token in instruction: Number ('42')"
        );
    }

    #[test]
    fn test_new_analyzer() {
        let tokens = vec![(Lexems::EOF, String::new(), 1, 1)];
        let analyzer = setup_analyzer(tokens);
        assert!(analyzer.errors.is_empty());
        assert_eq!(analyzer.current_line, 0);
        assert_eq!(analyzer.instruction_count, 0);
        assert!(analyzer.name_table.entries().len() == 0);
    }

    #[test]
    fn test_can_be_bool_literal() {
        let analyzer = setup_analyzer(vec![(Lexems::EOF, String::new(), 1, 1)]);
        let expr = Expression {
            kind: ExprKind::Literal(0),
            typ: DataType::Int,
        };
        assert!(analyzer.can_be_bool(&expr));
        let expr = Expression {
            kind: ExprKind::Literal(2),
            typ: DataType::Int,
        };
        assert!(!analyzer.can_be_bool(&expr));
    }

    #[test]
    fn test_can_be_bool_variable() {
        let mut analyzer = setup_analyzer(vec![(Lexems::EOF, String::new(), 1, 1)]);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        let expr = Expression {
            kind: ExprKind::Variable("x".to_string()),
            typ: DataType::Bool,
        };
        assert!(analyzer.can_be_bool(&expr));
        let expr = Expression {
            kind: ExprKind::Variable("y".to_string()),
            typ: DataType::Int,
        };
        assert!(!analyzer.can_be_bool(&expr));
    }

    #[test]
    fn test_can_be_bool_binary() {
        let analyzer = setup_analyzer(vec![(Lexems::EOF, String::new(), 1, 1)]);
        let expr = Expression {
            kind: ExprKind::Binary {
                op: BinOp::And,
                left: Box::new(Expression {
                    kind: ExprKind::Literal(1),
                    typ: DataType::Bool,
                }),
                right: Box::new(Expression {
                    kind: ExprKind::Literal(0),
                    typ: DataType::Bool,
                }),
            },
            typ: DataType::Bool,
        };
        assert!(analyzer.can_be_bool(&expr));
        let expr = Expression {
            kind: ExprKind::Binary {
                op: BinOp::Add,
                left: Box::new(Expression {
                    kind: ExprKind::Literal(1),
                    typ: DataType::Int,
                }),
                right: Box::new(Expression {
                    kind: ExprKind::Literal(2),
                    typ: DataType::Int,
                }),
            },
            typ: DataType::Int,
        };
        assert!(!analyzer.can_be_bool(&expr));
    }

    #[test]
    fn test_parse_expression_number() {
        let tokens = vec![
            (Lexems::Number, "42".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 3),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_variable() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 2),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Variable("x".to_string()),
                typ: DataType::Int,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_undeclared_variable() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 2),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Literal(0),
                typ: DataType::None,
            }
        );
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Undeclared identifier 'x'");
    }

    #[test]
    fn test_parse_expression_binary_add() {
        let tokens = vec![
            (Lexems::Number, "1".to_string(), 1, 1),
            (Lexems::Plus, "+".to_string(), 1, 3),
            (Lexems::Number, "2".to_string(), 1, 5),
            (Lexems::EOF, String::new(), 1, 7),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Binary {
                    op: BinOp::Add,
                    left: Box::new(Expression {
                        kind: ExprKind::Literal(1),
                        typ: DataType::Int,
                    }),
                    right: Box::new(Expression {
                        kind: ExprKind::Literal(2),
                        typ: DataType::Int,
                    }),
                },
                typ: DataType::Int,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_type_mismatch_add() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Plus, "+".to_string(), 1, 3),
            (Lexems::Name, "y".to_string(), 1, 5),
            (Lexems::EOF, String::new(), 1, 7),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        analyzer
            .name_table
            .add_or_update("y".to_string(), Category::Var, DataType::Bool, None);
        let expr = analyzer.parse_expression();
        assert_eq!(expr.typ, DataType::None);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Type mismatch in add/sub: expected Int, found Bool"
        );
    }

    #[test]
    fn test_parse_expression_logical_and() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::And, "and".to_string(), 1, 3),
            (Lexems::Name, "y".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        analyzer
            .name_table
            .add_or_update("y".to_string(), Category::Var, DataType::Bool, None);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Binary {
                    op: BinOp::And,
                    left: Box::new(Expression {
                        kind: ExprKind::Variable("x".to_string()),
                        typ: DataType::Bool,
                    }),
                    right: Box::new(Expression {
                        kind: ExprKind::Variable("y".to_string()),
                        typ: DataType::Bool,
                    }),
                },
                typ: DataType::Bool,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_logical_type_mismatch() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::And, "and".to_string(), 1, 3),
            (Lexems::Name, "y".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        analyzer
            .name_table
            .add_or_update("y".to_string(), Category::Var, DataType::Bool, None);
        let expr = analyzer.parse_expression();
        assert_eq!(expr.typ, DataType::Bool);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Type mismatch: expected Bool operands for logical operator"
        );
    }

    #[test]
    fn test_parse_expression_unary_not() {
        let tokens = vec![
            (Lexems::Not, ".NOT.".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(Expression {
                        kind: ExprKind::Variable("x".to_string()),
                        typ: DataType::Bool,
                    }),
                },
                typ: DataType::Bool,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_unary_not_type_mismatch() {
        let tokens = vec![
            (Lexems::Not, ".NOT.".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let expr = analyzer.parse_expression();
        assert_eq!(expr.typ, DataType::None);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected Bool after .NOT., found Int"
        );
    }

    #[test]
    fn test_parse_expression_parens() {
        let tokens = vec![
            (Lexems::LParen, "(".to_string(), 1, 1),
            (Lexems::Number, "42".to_string(), 1, 2),
            (Lexems::RParen, ")".to_string(), 1, 4),
            (Lexems::EOF, String::new(), 1, 5),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_missing_rparen() {
        let tokens = vec![
            (Lexems::LParen, "(".to_string(), 1, 1),
            (Lexems::Number, "42".to_string(), 1, 2),
            (Lexems::EOF, String::new(), 1, 4),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            }
        );
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected RParen, found EOF ('')"
        );
    }

    #[test]
    fn test_parse_expression_begin_end() {
        let tokens = vec![
            (Lexems::Begin, "Begin".to_string(), 1, 1),
            (Lexems::Number, "42".to_string(), 1, 7),
            (Lexems::End, "End".to_string(), 1, 9),
            (Lexems::EOF, String::new(), 1, 12),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_expression_unexpected_token() {
        let tokens = vec![
            (Lexems::Semi, ";".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 2),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let expr = analyzer.parse_expression();
        assert_eq!(
            expr,
            Expression {
                kind: ExprKind::Literal(0),
                typ: DataType::None,
            }
        );
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Unexpected token in expression");
    }

    #[test]
    fn test_parse_assignment_valid() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Number, "42".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmts = analyzer.parse_assignment();
        assert_eq!(
            stmts,
            vec![Statement::Assign {
                name: "x".to_string(),
                expr: Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                },
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_assignment_undeclared_variable() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Number, "42".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_assignment();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Undeclared identifier 'x'");
    }

    #[test]
    fn test_parse_assignment_missing_assign() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Number, "42".to_string(), 1, 3),
            (Lexems::Semi, ";".to_string(), 1, 5),
            (Lexems::EOF, String::new(), 1, 6),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmts = analyzer.parse_assignment();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected '=' or ':=' after identifier"
        );
    }

    #[test]
    fn test_parse_assignment_type_mismatch() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Name, "y".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        analyzer
            .name_table
            .add_or_update("y".to_string(), Category::Var, DataType::Bool, None);
        let stmts = analyzer.parse_assignment();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Type mismatch: variable 'x' expected Int, got Bool"
        );
    }

    #[test]
    fn test_parse_assignment_bool_invalid() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Number, "2".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        let stmts = analyzer.parse_assignment();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Type mismatch: variable 'x' expected Bool, got invalid value for Bool"
        );
    }

    #[test]
    fn test_parse_assignment_multiple_per_line() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Number, "42".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        analyzer.instruction_count = 1;
        analyzer.current_line = 1;
        let stmts = analyzer.parse_assignment();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Only one assignment per line allowed"
        );
    }

    #[test]
    fn test_report_error() {
        let tokens = vec![(Lexems::EOF, String::new(), 1, 1)];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.report_error("Test error".to_string());
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Test error");
        assert_eq!(analyzer.errors[0].line, 1);
        assert_eq!(analyzer.errors[0].col, 1);
    }

    #[test]
    fn test_expect_lexem_success() {
        let tokens = vec![
            (Lexems::Semi, ";".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 2),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.expect_lexem(Lexems::Semi);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_expect_lexem_failure() {
        let tokens = vec![
            (Lexems::Number, "42".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 3),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.expect_lexem(Lexems::Semi);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected Semi, found Number ('42')"
        );
    }

    #[test]
    fn test_maybe_advance() {
        let tokens = vec![
            (Lexems::NewLine, "\n".to_string(), 1, 1),
            (Lexems::Semi, ";".to_string(), 2, 1),
            (Lexems::Number, "42".to_string(), 2, 2),
            (Lexems::EOF, String::new(), 2, 4),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.maybe_advance();
        assert_eq!(analyzer.lexer.current_lexem(), Lexems::Number);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_print_valid() {
        let tokens = vec![
            (Lexems::Print, "print".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmts = analyzer.parse_print();
        assert_eq!(
            stmts,
            vec![Statement::Print {
                var: "x".to_string()
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_print_undeclared_variable() {
        let tokens = vec![
            (Lexems::Print, "print".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_print();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Undeclared identifier 'x'");
    }

    #[test]
    fn test_parse_print_number_literal() {
        let tokens = vec![
            (Lexems::Print, "print".to_string(), 1, 1),
            (Lexems::Number, "42".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_print();
        assert_eq!(
            stmts,
            vec![
                Statement::VarDecl {
                    typ: DataType::Int,
                    is_const: true,
                    names: vec!["_temp_1".to_string()],
                    init: Some(Expression {
                        kind: ExprKind::Literal(42),
                        typ: DataType::Int,
                    }),
                },
                Statement::Print {
                    var: "_temp_1".to_string(),
                },
            ]
        );
        assert_eq!(analyzer.errors.len(), 0, "No errors should be reported");
        assert!(
            analyzer
                .name_table
                .find_by_name("_temp_1")
                .map(|ident| ident.category == Category::Const
                    && ident.data_type == DataType::Int
                    && ident.value == Some(42))
                .unwrap_or(false),
            "Temporary variable '_temp_1' should be in NameTable"
        );
    }

    #[test]
    fn test_parse_print_multiple_per_line() {
        let tokens = vec![
            (Lexems::Print, "print".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        analyzer.instruction_count = 1;
        analyzer.current_line = 1;
        let stmts = analyzer.parse_print();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Only one instruction per line allowed"
        );
    }

    #[test]
    fn test_parse_statement_print() {
        let tokens = vec![
            (Lexems::Print, "print".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmts = analyzer.parse_statement();
        assert_eq!(
            stmts,
            vec![Statement::Print {
                var: "x".to_string()
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_assignment() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Number, "42".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::EOF, String::new(), 1, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmts = analyzer.parse_statement();
        assert_eq!(
            stmts,
            vec![Statement::Assign {
                name: "x".to_string(),
                expr: Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                },
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_variable_declaration() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Semi, ";".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 8),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_statement();
        assert_eq!(
            stmts,
            vec![Statement::VarDecl {
                typ: DataType::Int,
                is_const: false,
                names: vec!["x".to_string()],
                init: None,
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_block() {
        let tokens = vec![
            (Lexems::Begin, "Begin".to_string(), 1, 1),
            (Lexems::End, "End".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 10),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_statement();
        assert_eq!(stmts, vec![Statement::Block { stmts: vec![] }]);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_if() {
        let tokens = vec![
            (Lexems::If, "if".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 4),
            (Lexems::Then, "then".to_string(), 1, 6),
            (Lexems::EndIf, "endif".to_string(), 1, 11),
            (Lexems::EOF, String::new(), 1, 16),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        let stmts = analyzer.parse_statement();
        assert_eq!(
            stmts,
            vec![Statement::If {
                cond: Expression {
                    kind: ExprKind::Variable("x".to_string()),
                    typ: DataType::Bool,
                },
                then: vec![],
                elseifs: vec![],
                els: None,
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_while() {
        let tokens = vec![
            (Lexems::While, "while".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EndWhile, "endwhile".to_string(), 1, 9),
            (Lexems::EOF, String::new(), 1, 17),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        let stmts = analyzer.parse_statement();
        assert_eq!(
            stmts,
            vec![Statement::While {
                cond: Expression {
                    kind: ExprKind::Variable("x".to_string()),
                    typ: DataType::Bool,
                },
                body: vec![],
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_end() {
        let tokens = vec![
            (Lexems::End, "End".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 4),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_statement();
        assert_eq!(stmts, vec![]);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_eof() {
        let tokens = vec![(Lexems::EOF, String::new(), 1, 1)];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_statement();
        assert_eq!(stmts, vec![]);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statement_unexpected_token() {
        let tokens = vec![
            (Lexems::Plus, "+".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 2),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_statement();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Unexpected token in instruction: Plus ('+')"
        );
    }

    #[test]
    fn test_parse_statements_empty() {
        let tokens = vec![
            (Lexems::End, "End".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 4),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_statements(&[Lexems::End]);
        assert_eq!(stmts, vec![]);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_statements_with_statement() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Assign, ":=".to_string(), 1, 3),
            (Lexems::Number, "42".to_string(), 1, 6),
            (Lexems::Semi, ";".to_string(), 1, 8),
            (Lexems::End, "End".to_string(), 1, 9),
            (Lexems::EOF, String::new(), 1, 12),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmts = analyzer.parse_statements(&[Lexems::End]);
        assert_eq!(
            stmts,
            vec![Statement::Assign {
                name: "x".to_string(),
                expr: Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                },
            }]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_variable_declarations_single() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Semi, ";".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 8),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(
            stmts,
            vec![Statement::VarDecl {
                typ: DataType::Int,
                is_const: false,
                names: vec!["x".to_string()],
                init: None,
            }]
        );
        assert!(analyzer.errors.is_empty());
        assert!(analyzer.name_table.find_by_name("x").is_some());
    }

    #[test]
    fn test_parse_variable_declarations_multiple() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Comma, ",".to_string(), 1, 7),
            (Lexems::Name, "y".to_string(), 1, 9),
            (Lexems::Semi, ";".to_string(), 1, 11),
            (Lexems::EOF, String::new(), 1, 12),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(
            stmts,
            vec![Statement::VarDecl {
                typ: DataType::Int,
                is_const: false,
                names: vec!["x".to_string(), "y".to_string()],
                init: None,
            }]
        );
        assert!(analyzer.errors.is_empty());
        assert!(analyzer.name_table.find_by_name("x").is_some());
        assert!(analyzer.name_table.find_by_name("y").is_some());
    }

    #[test]
    fn test_parse_variable_declarations_const() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "const".to_string(), 1, 5), // "const" as Name token
            (Lexems::Name, "x".to_string(), 1, 11),
            (Lexems::Assign, ":=".to_string(), 1, 13),
            (Lexems::Number, "42".to_string(), 1, 16),
            (Lexems::Semi, ";".to_string(), 1, 18),
            (Lexems::EOF, String::new(), 1, 19),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(
            stmts,
            vec![Statement::VarDecl {
                typ: DataType::Int,
                is_const: true,
                names: vec!["x".to_string()],
                init: Some(Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                }),
            }]
        );
        assert!(analyzer.errors.is_empty());
        let ident = analyzer.name_table.find_by_name("x").unwrap();
        assert_eq!(ident.category, Category::Const);
        assert_eq!(ident.value, Some(42));
    }

    #[test]
    fn test_parse_variable_declarations_invalid_type() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Name, "y".to_string(), 1, 3),
            (Lexems::Semi, ";".to_string(), 1, 5),
            (Lexems::EOF, String::new(), 1, 6),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Expected 'int' or 'bool'");
    }

    #[test]
    fn test_parse_variable_declarations_missing_name() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Semi, ";".to_string(), 1, 5),
            (Lexems::EOF, String::new(), 1, 6),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Expected variable name");
    }

    #[test]
    fn test_parse_variable_declarations_multiple_init_error() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Comma, ",".to_string(), 1, 7),
            (Lexems::Name, "y".to_string(), 1, 9),
            (Lexems::Assign, ":=".to_string(), 1, 11),
            (Lexems::Number, "42".to_string(), 1, 14),
            (Lexems::Semi, ";".to_string(), 1, 16),
            (Lexems::EOF, String::new(), 1, 17),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Multiple variables cannot have initializer"
        );
    }

    #[test]
    fn test_parse_variable_declarations_invalid_bool_const() {
        let tokens = vec![
            (Lexems::Bool, "bool".to_string(), 1, 1),
            (Lexems::Name, "const".to_string(), 1, 6), // "const" as Name token
            (Lexems::Name, "x".to_string(), 1, 12),
            (Lexems::Assign, ":=".to_string(), 1, 14),
            (Lexems::Number, "2".to_string(), 1, 17),
            (Lexems::Semi, ";".to_string(), 1, 19),
            (Lexems::EOF, String::new(), 1, 20),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Invalid constant '2', expected 0 or 1 for Boolean"
        );
    }

    #[test]
    fn test_parse_variable_declarations_non_numeric_const() {
        let tokens = vec![
            (Lexems::Bool, "bool".to_string(), 1, 1),
            (Lexems::Name, "const".to_string(), 1, 6), // "const" as Name token
            (Lexems::Name, "x".to_string(), 1, 12),
            (Lexems::Assign, ":=".to_string(), 1, 14),
            (Lexems::Name, "y".to_string(), 1, 17),
            (Lexems::Semi, ";".to_string(), 1, 19),
            (Lexems::EOF, String::new(), 1, 20),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Constant must be initialized with a number literal"
        );
    }

    #[test]
    fn test_parse_variable_declarations_multiple_per_line() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Semi, ";".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 8),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.instruction_count = 1;
        analyzer.current_line = 1;
        let stmts = analyzer.parse_variable_declarations();
        assert_eq!(stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Only one declaration per line allowed"
        );
    }

    #[test]
    fn test_parse_program_empty() {
        let tokens = vec![(Lexems::EOF, String::new(), 1, 1)];
        let mut analyzer = setup_analyzer(tokens);
        let program = analyzer.parse_program();
        assert_eq!(program.stmts, vec![]);
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected 'Begin' for computations"
        );
    }

    #[test]
    fn test_parse_program_full() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Semi, ";".to_string(), 1, 7),
            (Lexems::Begin, "Begin".to_string(), 2, 1),
            (Lexems::Name, "x".to_string(), 3, 1),
            (Lexems::Assign, ":=".to_string(), 3, 3),
            (Lexems::Number, "42".to_string(), 3, 6),
            (Lexems::Semi, ";".to_string(), 3, 8),
            (Lexems::End, "End".to_string(), 4, 1),
            (Lexems::Print, "print".to_string(), 5, 1),
            (Lexems::Name, "x".to_string(), 5, 7),
            (Lexems::EOF, String::new(), 5, 9),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let program = analyzer.parse_program();
        assert_eq!(
            program.stmts,
            vec![
                Statement::VarDecl {
                    typ: DataType::Int,
                    is_const: false,
                    names: vec!["x".to_string()],
                    init: None,
                },
                Statement::Block {
                    stmts: vec![Statement::Assign {
                        name: "x".to_string(),
                        expr: Expression {
                            kind: ExprKind::Literal(42),
                            typ: DataType::Int,
                        },
                    }],
                },
                Statement::Print {
                    var: "x".to_string(),
                },
            ]
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_variable_declarations_no_code_single() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Semi, ";".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 8),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.parse_variable_declarations_no_code();
        assert!(analyzer.errors.is_empty());
        assert!(analyzer.name_table.find_by_name("x").is_some());
    }

    #[test]
    fn test_parse_variable_declarations_no_code_const() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "const".to_string(), 1, 5), // "const" as Name token
            (Lexems::Name, "x".to_string(), 1, 11),
            (Lexems::Assign, ":=".to_string(), 1, 13),
            (Lexems::Number, "42".to_string(), 1, 16),
            (Lexems::Semi, ";".to_string(), 1, 18),
            (Lexems::EOF, String::new(), 1, 19),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.parse_variable_declarations_no_code();
        assert!(analyzer.errors.is_empty());
        let ident = analyzer.name_table.find_by_name("x").unwrap();
        assert_eq!(ident.category, Category::Const);
        assert_eq!(ident.value, Some(42));
    }

    #[test]
    fn test_parse_variable_declarations_no_code_invalid_type() {
        let tokens = vec![
            (Lexems::Name, "x".to_string(), 1, 1),
            (Lexems::Semi, ";".to_string(), 1, 3),
            (Lexems::EOF, String::new(), 1, 4),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.parse_variable_declarations_no_code();
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Expected 'int' or 'bool'");
    }

    #[test]
    fn test_parse_variable_declarations_no_code_unexpected_assign() {
        let tokens = vec![
            (Lexems::Int, "int".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 5),
            (Lexems::Colon, ":".to_string(), 1, 7),
            (Lexems::Assign, ":=".to_string(), 1, 9),
            (Lexems::Number, "42".to_string(), 1, 12),
            (Lexems::Semi, ";".to_string(), 1, 14),
            (Lexems::EOF, String::new(), 1, 15),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.parse_variable_declarations_no_code();
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Unexpected ':=' after ':' in declaration"
        );
    }

    #[test]
    fn test_skip_expression() {
        let tokens = vec![
            (Lexems::Number, "42".to_string(), 1, 1),
            (Lexems::Plus, "+".to_string(), 1, 3),
            (Lexems::Number, "10".to_string(), 1, 5),
            (Lexems::Semi, ";".to_string(), 1, 7),
            (Lexems::EOF, String::new(), 1, 8),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.skip_expression();
        assert_eq!(analyzer.lexer.current_lexem(), Lexems::Semi);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_skip_expression_until_endwhile() {
        let tokens = vec![
            (Lexems::Number, "42".to_string(), 1, 1),
            (Lexems::Plus, "+".to_string(), 1, 3),
            (Lexems::EndWhile, "endwhile".to_string(), 1, 5),
            (Lexems::EOF, String::new(), 1, 13),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer.skip_expression();
        assert_eq!(analyzer.lexer.current_lexem(), Lexems::EndWhile);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_block() {
        let tokens = vec![
            (Lexems::Begin, "Begin".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 2, 1),
            (Lexems::Assign, ":=".to_string(), 2, 3),
            (Lexems::Number, "42".to_string(), 2, 6),
            (Lexems::Semi, ";".to_string(), 2, 8),
            (Lexems::End, "End".to_string(), 3, 1),
            (Lexems::EOF, String::new(), 3, 4),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmt = analyzer.parse_block();
        assert_eq!(
            stmt,
            Statement::Block {
                stmts: vec![Statement::Assign {
                    name: "x".to_string(),
                    expr: Expression {
                        kind: ExprKind::Literal(42),
                        typ: DataType::Int,
                    },
                }],
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_block_missing_end() {
        let tokens = vec![
            (Lexems::Begin, "Begin".to_string(), 1, 1),
            (Lexems::EOF, String::new(), 1, 7),
        ];
        let mut analyzer = setup_analyzer(tokens);
        let stmt = analyzer.parse_block();
        assert_eq!(stmt, Statement::Block { stmts: vec![] });
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(analyzer.errors[0].message, "Expected End, found EOF ('')");
    }

    #[test]
    fn test_parse_if_with_else() {
        let tokens = vec![
            (Lexems::If, "if".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 4),
            (Lexems::Then, "then".to_string(), 1, 6),
            (Lexems::Name, "x".to_string(), 2, 1),
            (Lexems::Assign, ":=".to_string(), 2, 3),
            (Lexems::Number, "42".to_string(), 2, 6),
            (Lexems::Semi, ";".to_string(), 2, 8),
            (Lexems::Else, "else".to_string(), 3, 1),
            (Lexems::Name, "x".to_string(), 4, 1),
            (Lexems::Assign, ":=".to_string(), 4, 3),
            (Lexems::Number, "0".to_string(), 4, 6),
            (Lexems::Semi, ";".to_string(), 4, 8),
            (Lexems::EndIf, "endif".to_string(), 5, 1),
            (Lexems::EOF, String::new(), 5, 6),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let stmt = analyzer.parse_if();
        assert_eq!(
            stmt,
            Statement::If {
                cond: Expression {
                    kind: ExprKind::Variable("x".to_string()),
                    typ: DataType::Int,
                },
                then: vec![Statement::Assign {
                    name: "x".to_string(),
                    expr: Expression {
                        kind: ExprKind::Literal(42),
                        typ: DataType::Int,
                    },
                }],
                elseifs: vec![],
                els: Some(vec![Statement::Assign {
                    name: "x".to_string(),
                    expr: Expression {
                        kind: ExprKind::Literal(0),
                        typ: DataType::Int,
                    },
                }]),
            }
        );
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected boolean expression in 'if', got Int"
        );
    }

    #[test]
    fn test_parse_if_with_elseif() {
        let tokens = vec![
            (Lexems::If, "if".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 4),
            (Lexems::Then, "then".to_string(), 1, 6),
            (Lexems::ElseIf, "elseif".to_string(), 2, 1),
            (Lexems::Name, "y".to_string(), 2, 8),
            (Lexems::Then, "then".to_string(), 2, 10),
            (Lexems::EndIf, "endif".to_string(), 3, 1),
            (Lexems::EOF, String::new(), 3, 6),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Bool, None);
        analyzer
            .name_table
            .add_or_update("y".to_string(), Category::Var, DataType::Bool, None);
        let stmt = analyzer.parse_if();
        assert_eq!(
            stmt,
            Statement::If {
                cond: Expression {
                    kind: ExprKind::Variable("x".to_string()),
                    typ: DataType::Bool,
                },
                then: vec![],
                elseifs: vec![(
                    Expression {
                        kind: ExprKind::Variable("y".to_string()),
                        typ: DataType::Bool,
                    },
                    vec![]
                )],
                els: None,
            }
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn test_parse_if_invalid_condition() {
        let tokens = vec![
            (Lexems::If, "if".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 4),
            (Lexems::Then, "then".to_string(), 1, 6),
            (Lexems::EndIf, "endif".to_string(), 2, 1),
            (Lexems::EOF, String::new(), 2, 6),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let _stmt = analyzer.parse_if();
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected boolean expression in 'if', got Int"
        );
    }

    #[test]
    fn test_parse_while_invalid_condition() {
        let tokens = vec![
            (Lexems::While, "while".to_string(), 1, 1),
            (Lexems::Name, "x".to_string(), 1, 7),
            (Lexems::EndWhile, "endwhile".to_string(), 1, 9),
            (Lexems::EOF, String::new(), 1, 17),
        ];
        let mut analyzer = setup_analyzer(tokens);
        analyzer
            .name_table
            .add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        let _stmt = analyzer.parse_while();
        assert_eq!(analyzer.errors.len(), 1);
        assert_eq!(
            analyzer.errors[0].message,
            "Expected boolean expression in 'while', got Int"
        );
    }
}
