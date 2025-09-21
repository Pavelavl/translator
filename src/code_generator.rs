use crate::models::{BinOp, Category, DataType, ExprKind, Expression, Program, Statement, UnaryOp};
use crate::name_table::NameTable;

pub struct CodeGenerator {
    code: Vec<String>,
    label_counter: usize,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            code: Vec::new(),
            label_counter: 0,
        }
    }

    pub fn add_instruction(&mut self, instruction: &str) {
        println!("[CodeGenerator] Adding instruction: '{}'", instruction);
        self.code.push(instruction.to_string());
    }

    pub fn get_code(&self) -> Vec<String> {
        println!("[CodeGenerator] Retrieving generated code, {} instructions", self.code.len());
        self.code.clone()
    }

    pub fn clear(&mut self) {
        println!("[CodeGenerator] Clearing code and label counter");
        self.code.clear();
        self.label_counter = 0;
    }

    pub fn generate(&mut self, program: &Program, name_table: &NameTable) {
        println!("[CodeGenerator] Starting code generation for program with {} statements", program.stmts.len());
        self.add_instruction("data segment para public \"data\"");
        self.declare_variables(name_table);
        self.add_instruction("PRINT_BUF DB ' ' DUP(10)");
        self.add_instruction("BUFEND    DB '$'");
        self.add_instruction("data ends");
        self.add_instruction("stk segment stack");
        self.add_instruction("db 256 dup ('?')");
        self.add_instruction("stk ends");
        self.add_instruction("code segment para public 'code'");
        self.add_instruction("main proc");
        self.add_instruction("assume cs:code,ds:data,ss:stk");
        self.add_instruction("mov ax,data");
        self.add_instruction("mov ds,ax");

        for st in &program.stmts {
            println!("[CodeGenerator] Generating code for statement: {:?}", st);
            self.gen_statement(st, name_table);
        }

        self.add_instruction("mov ax,4c00h");
        self.add_instruction("int 21h");
        self.add_instruction("main endp");
        self.declare_print_procedure();
        self.add_instruction("code ends");
        self.add_instruction("end main");
        println!("[CodeGenerator] Code generation complete");
    }

    fn gen_statement(&mut self, st: &Statement, name_table: &NameTable) {
        println!("[CodeGenerator] Processing statement: {:?}", st);
        match st {
            Statement::VarDecl {
                typ: _,
                is_const,
                names,
                init,
            } => {
                println!("[CodeGenerator] Generating variable declaration: names={:?}, is_const={}, init={:?}", names, is_const, init);
                if !*is_const {
                    if let Some(expr) = init {
                        if let ExprKind::Literal(v) = &expr.kind {
                            println!("[CodeGenerator] Generating literal initialization: value={}", v);
                            self.add_instruction(&format!("mov ax, {}", v));
                            self.add_instruction(&format!("mov [{}], ax", names[0]));
                        } else {
                            self.gen_expression(expr);
                            self.add_instruction("pop ax");
                            self.add_instruction(&format!("mov [{}], ax", names[0]));
                        }
                    }
                }
            }
            Statement::Assign { name, expr } => {
                println!("[CodeGenerator] Generating assignment: name='{}', expr={:?}", name, expr);
                if let ExprKind::Literal(v) = &expr.kind {
                    println!("[CodeGenerator] Generating literal assignment: value={}", v);
                    self.add_instruction(&format!("mov ax, {}", v));
                    self.add_instruction(&format!("mov [{}], ax", name));
                } else {
                    self.gen_expression(expr);
                    self.add_instruction("pop ax");
                    self.add_instruction(&format!("mov [{}], ax", name));
                }
            }
            Statement::Print { var } => {
                println!("[CodeGenerator] Generating print: var='{}'", var);
                let typ = name_table
                    .find_by_name(var)
                    .map(|i| i.data_type.clone())
                    .unwrap_or(DataType::None);
                println!("[CodeGenerator] Variable '{}' type: {:?}", var, typ);
                self.add_instruction(&format!("mov ax, [{}]", var));
                if typ == DataType::Bool {
                    self.add_instruction("CALL PRINT");
                } else {
                    self.add_instruction("CALL PRINT_INT");
                }
            }
            Statement::If {
                cond,
                then,
                elseifs,
                els,
            } => {
                println!("[CodeGenerator] Generating if statement: cond={:?}, then_len={}, elseifs_len={}, els={:?}", cond, then.len(), elseifs.len(), els);
                // Optimize for literal condition
                if let ExprKind::Literal(v) = &cond.kind {
                    println!("[CodeGenerator] Generating literal condition: value={}", v);
                    self.add_instruction(&format!("mov ax, {}", v));
                    self.add_instruction("cmp ax, 0");
                } else {
                    self.gen_expression(cond);
                    self.add_instruction("pop ax");
                    self.add_instruction("cmp ax, 0");
                }
                let else_label = self.new_label("else");
                self.add_instruction(&format!("je {}", else_label));
                for s in then {
                    self.gen_statement(s, name_table);
                }
                let endif_label = self.new_label("endif");
                self.add_instruction(&format!("jmp {}", endif_label));
                let mut current_else = else_label;
                for (c, stmts) in elseifs {
                    self.add_instruction(&format!("{}:", current_else));
                    // Optimize for literal condition in elseif
                    if let ExprKind::Literal(v) = &c.kind {
                        println!("[CodeGenerator] Generating literal elseif condition: value={}", v);
                        self.add_instruction(&format!("mov ax, {}", v));
                        self.add_instruction("cmp ax, 0");
                    } else {
                        self.gen_expression(c);
                        self.add_instruction("pop ax");
                        self.add_instruction("cmp ax, 0");
                    }
                    current_else = self.new_label("else");
                    self.add_instruction(&format!("je {}", current_else));
                    for s in stmts {
                        self.gen_statement(s, name_table);
                    }
                    self.add_instruction(&format!("jmp {}", endif_label));
                }
                self.add_instruction(&format!("{}:", current_else));
                if let Some(e) = els {
                    for s in e {
                        self.gen_statement(s, name_table);
                    }
                }
                self.add_instruction(&format!("{}:", endif_label));
            }
            Statement::While { cond, body } => {
                println!("[CodeGenerator] Generating while loop: cond={:?}, body_len={}", cond, body.len());
                let start_label = self.new_label("while_start");
                self.add_instruction(&format!("{}:", start_label));
                // Optimize for literal condition
                if let ExprKind::Literal(v) = &cond.kind {
                    println!("[CodeGenerator] Generating literal while condition: value={}", v);
                    self.add_instruction(&format!("mov ax, {}", v));
                    self.add_instruction("cmp ax, 0");
                } else {
                    self.gen_expression(cond);
                    self.add_instruction("pop ax");
                    self.add_instruction("cmp ax, 0");
                }
                let end_label = self.new_label("while_end");
                self.add_instruction(&format!("je {}", end_label));
                for s in body {
                    self.gen_statement(s, name_table);
                }
                self.add_instruction(&format!("jmp {}", start_label));
                self.add_instruction(&format!("{}:", end_label));
            }
            Statement::Block { stmts } => {
                println!("[CodeGenerator] Generating block with {} statements", stmts.len());
                for s in stmts {
                    self.gen_statement(s, name_table);
                }
            }
        }
    }

    fn gen_expression(&mut self, expr: &Expression) {
        println!("[CodeGenerator] Generating expression: {:?}", expr);
        match &expr.kind {
            ExprKind::Literal(v) => {
                println!("[CodeGenerator] Generating literal: value={}", v);
                self.add_instruction(&format!("mov ax, {}", v));
                self.add_instruction("push ax");
            }
            ExprKind::Variable(n) => {
                println!("[CodeGenerator] Generating variable: name='{}'", n);
                self.add_instruction(&format!("mov ax, [{}]", n));
                self.add_instruction("push ax");
            }
            ExprKind::Binary { op, left, right } => {
                println!("[CodeGenerator] Generating binary operation: op={:?}, left={:?}, right={:?}", op, left, right);
                self.gen_expression(left);
                self.gen_expression(right);
                self.add_instruction("pop bx");
                self.add_instruction("pop ax");
                match op {
                    BinOp::Add => self.add_instruction("add ax, bx"),
                    BinOp::Sub => self.add_instruction("sub ax, bx"),
                    BinOp::Mul => self.add_instruction("mul bx"),
                    BinOp::Div => {
                        self.add_instruction("cwd");
                        self.add_instruction("div bx");
                    }
                    BinOp::And => self.add_instruction("and ax, bx"),
                    BinOp::Or => self.add_instruction("or ax, bx"),
                    BinOp::Xor => self.add_instruction("xor ax, bx"),
                    _ => {
                        self.add_instruction("cmp ax, bx");
                        let true_label = self.new_label("true");
                        let done_label = self.new_label("done");
                        let jump_inst = match op {
                            BinOp::Eq => "je",
                            BinOp::Ne => "jne",
                            BinOp::Lt => "jl",
                            BinOp::Le => "jle",
                            BinOp::Gt => "jg",
                            BinOp::Ge => "jge",
                            _ => unreachable!(),
                        };
                        self.add_instruction(&format!("{} {}", jump_inst, true_label));
                        self.add_instruction("mov ax, 0");
                        self.add_instruction(&format!("jmp {}", done_label));
                        self.add_instruction(&format!("{}:", true_label));
                        self.add_instruction("mov ax, 1");
                        self.add_instruction(&format!("{}:", done_label));
                    }
                }
                self.add_instruction("push ax");
            }
            ExprKind::Unary { op, expr } => {
                println!("[CodeGenerator] Generating unary operation: op={:?}, expr={:?}", op, expr);
                self.gen_expression(expr);
                self.add_instruction("pop ax");
                match op {
                    UnaryOp::Not => self.add_instruction("xor ax, 1"),
                }
                self.add_instruction("push ax");
            }
        }
    }

    pub fn declare_variables(&mut self, name_table: &NameTable) {
        println!("[CodeGenerator] Declaring variables from name table");
        for ident in name_table.entries() {
            if ident.category == Category::Var {
                println!("[CodeGenerator] Declaring variable: {} dw 0", ident.name);
                self.add_instruction(&format!("{} dw 0", ident.name));
            } else if ident.category == Category::Const {
                let value = ident.value.unwrap_or(0); // Default to 0 if no value
                println!("[CodeGenerator] Declaring constant: {} dw {}", ident.name, value);
                self.add_instruction(&format!("{} dw {}", ident.name, value));
            }
        }
    }

    pub fn declare_print_procedure(&mut self) {
        println!("[CodeGenerator] Declaring print procedures");
        self.add_instruction("PRINT PROC NEAR");
        self.add_instruction("push cx");
        self.add_instruction("push dx");
        self.add_instruction("push di");
        self.add_instruction("cmp ax, 0");
        self.add_instruction("jne print_one");
        self.add_instruction("mov byte ptr [PRINT_BUF], '0'");
        self.add_instruction("mov byte ptr [PRINT_BUF+1], '$'");
        self.add_instruction("jmp print_output");
        self.add_instruction("print_one:");
        self.add_instruction("mov byte ptr [PRINT_BUF], '1'");
        self.add_instruction("mov byte ptr [PRINT_BUF+1], '$'");
        self.add_instruction("print_output:");
        self.add_instruction("lea dx, PRINT_BUF");
        self.add_instruction("mov ah, 09h");
        self.add_instruction("int 21h");
        self.add_instruction("mov dl, 0Dh");
        self.add_instruction("mov ah, 02h");
        self.add_instruction("int 21h");
        self.add_instruction("mov dl, 0Ah");
        self.add_instruction("mov ah, 02h");
        self.add_instruction("int 21h");
        self.add_instruction("pop di");
        self.add_instruction("pop dx");
        self.add_instruction("pop cx");
        self.add_instruction("RET");
        self.add_instruction("PRINT ENDP");

        self.add_instruction("PRINT_INT PROC NEAR");
        self.add_instruction("push ax");
        self.add_instruction("push bx");
        self.add_instruction("push cx");
        self.add_instruction("push dx");
        self.add_instruction("push di");
        self.add_instruction("mov bx, 10");
        self.add_instruction("lea di, PRINT_BUF + 9");
        self.add_instruction("mov byte ptr [di], '$'");
        self.add_instruction("cmp ax, 0");
        self.add_instruction("jge convert_loop");
        self.add_instruction("mov byte ptr [di-1], '-'");
        self.add_instruction("dec di");
        self.add_instruction("neg ax");
        self.add_instruction("convert_loop:");
        self.add_instruction("xor dx, dx");
        self.add_instruction("div bx");
        self.add_instruction("add dl, '0'");
        self.add_instruction("dec di");
        self.add_instruction("mov [di], dl");
        self.add_instruction("cmp ax, 0");
        self.add_instruction("jne convert_loop");
        self.add_instruction("mov dx, di");
        self.add_instruction("mov ah, 09h");
        self.add_instruction("int 21h");
        self.add_instruction("mov dl, 0Dh");
        self.add_instruction("mov ah, 02h");
        self.add_instruction("int 21h");
        self.add_instruction("mov dl, 0Ah");
        self.add_instruction("mov ah, 02h");
        self.add_instruction("int 21h");
        self.add_instruction("pop di");
        self.add_instruction("pop dx");
        self.add_instruction("pop cx");
        self.add_instruction("pop bx");
        self.add_instruction("pop ax");
        self.add_instruction("RET");
        self.add_instruction("PRINT_INT ENDP");
    }

    pub fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        let label = format!("{}_{}", prefix, self.label_counter);
        println!("[CodeGenerator] Generated new label: {}", label);
        label
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{BinOp, Category, DataType, ExprKind, Expression, Program, Statement, UnaryOp};
    use crate::name_table::NameTable;

    // Helper function to create a NameTable with a single variable
    fn setup_name_table(name: &str, category: Category, data_type: DataType, value: Option<i32>) -> NameTable {
        let mut name_table = NameTable::new();
        name_table.add_or_update(name.to_string(), category, data_type, value);
        name_table
    }

    // Helper function to create a program with a single statement
    fn setup_program(stmt: Statement) -> Program {
        Program { stmts: vec![stmt] }
    }

    #[test]
    fn test_add_instruction() {
        let mut generator = CodeGenerator::new();
        generator.add_instruction("mov ax, bx");
        let code = generator.get_code();
        assert_eq!(code, vec!["mov ax, bx"]);
    }

    #[test]
    fn test_clear() {
        let mut generator = CodeGenerator::new();
        generator.add_instruction("mov ax, bx");
        generator.clear();
        let code = generator.get_code();
        assert!(code.is_empty());
        assert_eq!(generator.label_counter, 0);
    }

    #[test]
    fn test_new_label() {
        let mut generator = CodeGenerator::new();
        let label1 = generator.new_label("test");
        let label2 = generator.new_label("test");
        assert_eq!(label1, "test_1");
        assert_eq!(label2, "test_2");
    }

    #[test]
    fn test_declare_variables() {
        let mut generator = CodeGenerator::new();
        let mut name_table = NameTable::new();
        name_table.add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        name_table.add_or_update("y".to_string(), Category::Const, DataType::Int, Some(42));
        generator.declare_variables(&name_table);
        let code = generator.get_code();
        assert_eq!(code, vec!["x dw 0", "y dw 42"]);
    }

    #[test]
    fn test_generate_empty_program() {
        let mut generator = CodeGenerator::new();
        let program = Program { stmts: vec![] };
        let name_table = NameTable::new();
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for empty program");
    }

    #[test]
    fn test_generate_variable_declaration_without_init() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::VarDecl {
            typ: DataType::Int,
            is_const: false,
            names: vec!["x".to_string()],
            init: None,
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for variable declaration without init");
    }

    #[test]
    fn test_generate_variable_declaration_with_init() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::VarDecl {
            typ: DataType::Int,
            is_const: false,
            names: vec!["x".to_string()],
            init: Some(Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            }),
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 42",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for variable declaration with init");
    }

    #[test]
    fn test_generate_constant_declaration() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::VarDecl {
            typ: DataType::Int,
            is_const: true,
            names: vec!["x".to_string()],
            init: Some(Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            }),
        });
        let name_table = setup_name_table("x", Category::Const, DataType::Int, Some(42));
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 42",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for constant declaration");
    }

    #[test]
    fn test_generate_assignment() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Assign {
            name: "x".to_string(),
            expr: Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            },
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 42",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for assignment");
    }

    #[test]
    fn test_generate_print_int() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Print {
            var: "x".to_string(),
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, [x]",
            "CALL PRINT_INT",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for print int");
    }

    #[test]
    fn test_generate_print_bool() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Print {
            var: "x".to_string(),
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Bool, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, [x]",
            "CALL PRINT",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for print bool");
    }

    #[test]
    fn test_generate_if_statement() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::If {
            cond: Expression {
                kind: ExprKind::Literal(1),
                typ: DataType::Bool,
            },
            then: vec![Statement::Assign {
                name: "x".to_string(),
                expr: Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                },
            }],
            elseifs: vec![],
            els: None,
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 1",
            "cmp ax, 0",
            "je else_1",
            "mov ax, 42",
            "mov [x], ax",
            "jmp endif_2",
            "else_1:",
            "endif_2:",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for if statement");
    }

    #[test]
    fn test_generate_if_else_statement() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::If {
            cond: Expression {
                kind: ExprKind::Literal(1),
                typ: DataType::Bool,
            },
            then: vec![],
            elseifs: vec![],
            els: Some(vec![Statement::Assign {
                name: "x".to_string(),
                expr: Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                },
            }]),
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 1",
            "cmp ax, 0",
            "je else_1",
            "jmp endif_2",
            "else_1:",
            "mov ax, 42",
            "mov [x], ax",
            "endif_2:",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for if-else statement");
    }

    #[test]
    fn test_generate_if_elseif_statement() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::If {
            cond: Expression {
                kind: ExprKind::Literal(0),
                typ: DataType::Bool,
            },
            then: vec![],
            elseifs: vec![(
                Expression {
                    kind: ExprKind::Literal(1),
                    typ: DataType::Bool,
                },
                vec![Statement::Assign {
                    name: "x".to_string(),
                    expr: Expression {
                        kind: ExprKind::Literal(42),
                        typ: DataType::Int,
                    },
                }],
            )],
            els: None,
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 0",
            "cmp ax, 0",
            "je else_1",
            "jmp endif_2",
            "else_1:",
            "mov ax, 1",
            "cmp ax, 0",
            "je else_3",
            "mov ax, 42",
            "mov [x], ax",
            "jmp endif_2",
            "else_3:",
            "endif_2:",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for if-elseif statement");
    }

    #[test]
    fn test_generate_while_statement() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::While {
            cond: Expression {
                kind: ExprKind::Literal(1),
                typ: DataType::Bool,
            },
            body: vec![Statement::Assign {
                name: "x".to_string(),
                expr: Expression {
                    kind: ExprKind::Literal(42),
                    typ: DataType::Int,
                },
            }],
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "while_start_1:",
            "mov ax, 1",
            "cmp ax, 0",
            "je while_end_2",
            "mov ax, 42",
            "mov [x], ax",
            "jmp while_start_1",
            "while_end_2:",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for while statement");
    }

    #[test]
    fn test_generate_block_statement() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Block {
            stmts: vec![
                Statement::Assign {
                    name: "x".to_string(),
                    expr: Expression {
                        kind: ExprKind::Literal(42),
                        typ: DataType::Int,
                    },
                },
                Statement::Assign {
                    name: "y".to_string(),
                    expr: Expression {
                        kind: ExprKind::Literal(100),
                        typ: DataType::Int,
                    },
                },
            ],
        });
        let mut name_table = NameTable::new();
        name_table.add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        name_table.add_or_update("y".to_string(), Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "y dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 42",
            "mov [x], ax",
            "mov ax, 100",
            "mov [y], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for block statement");
    }

    #[test]
    fn test_generate_expression_literal() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Assign {
            name: "x".to_string(),
            expr: Expression {
                kind: ExprKind::Literal(42),
                typ: DataType::Int,
            },
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 42",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for expression literal");
    }

    #[test]
    fn test_generate_expression_variable() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Assign {
            name: "x".to_string(),
            expr: Expression {
                kind: ExprKind::Variable("y".to_string()),
                typ: DataType::Int,
            },
        });
        let mut name_table = NameTable::new();
        name_table.add_or_update("x".to_string(), Category::Var, DataType::Int, None);
        name_table.add_or_update("y".to_string(), Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "y dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, [y]",
            "push ax",
            "pop ax",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for expression variable");
    }

    #[test]
    fn test_generate_expression_binary_add() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Assign {
            name: "x".to_string(),
            expr: Expression {
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
            },
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Int, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 1",
            "push ax",
            "mov ax, 2",
            "push ax",
            "pop bx",
            "pop ax",
            "add ax, bx",
            "push ax",
            "pop ax",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for binary add");
    }

    #[test]
    fn test_generate_expression_binary_comparison() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Assign {
            name: "x".to_string(),
            expr: Expression {
                kind: ExprKind::Binary {
                    op: BinOp::Eq,
                    left: Box::new(Expression {
                        kind: ExprKind::Literal(1),
                        typ: DataType::Int,
                    }),
                    right: Box::new(Expression {
                        kind: ExprKind::Literal(2),
                        typ: DataType::Int,
                    }),
                },
                typ: DataType::Bool,
            },
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Bool, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 1",
            "push ax",
            "mov ax, 2",
            "push ax",
            "pop bx",
            "pop ax",
            "cmp ax, bx",
            "je true_1",
            "mov ax, 0",
            "jmp done_2",
            "true_1:",
            "mov ax, 1",
            "done_2:",
            "push ax",
            "pop ax",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for binary comparison");
    }

    #[test]
    fn test_generate_expression_unary_not() {
        let mut generator = CodeGenerator::new();
        let program = setup_program(Statement::Assign {
            name: "x".to_string(),
            expr: Expression {
                kind: ExprKind::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(Expression {
                        kind: ExprKind::Literal(1),
                        typ: DataType::Bool,
                    }),
                },
                typ: DataType::Bool,
            },
        });
        let name_table = setup_name_table("x", Category::Var, DataType::Bool, None);
        generator.generate(&program, &name_table);
        let code = generator.get_code();
        let expected = vec![
            "data segment para public \"data\"",
            "x dw 0",
            "PRINT_BUF DB ' ' DUP(10)",
            "BUFEND    DB '$'",
            "data ends",
            "stk segment stack",
            "db 256 dup ('?')",
            "stk ends",
            "code segment para public 'code'",
            "main proc",
            "assume cs:code,ds:data,ss:stk",
            "mov ax,data",
            "mov ds,ax",
            "mov ax, 1",
            "push ax",
            "pop ax",
            "xor ax, 1",
            "push ax",
            "pop ax",
            "mov [x], ax",
            "mov ax,4c00h",
            "int 21h",
            "main endp",
            "PRINT PROC NEAR",
            "push cx",
            "push dx",
            "push di",
            "cmp ax, 0",
            "jne print_one",
            "mov byte ptr [PRINT_BUF], '0'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "jmp print_output",
            "print_one:",
            "mov byte ptr [PRINT_BUF], '1'",
            "mov byte ptr [PRINT_BUF+1], '$'",
            "print_output:",
            "lea dx, PRINT_BUF",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "RET",
            "PRINT ENDP",
            "PRINT_INT PROC NEAR",
            "push ax",
            "push bx",
            "push cx",
            "push dx",
            "push di",
            "mov bx, 10",
            "lea di, PRINT_BUF + 9",
            "mov byte ptr [di], '$'",
            "cmp ax, 0",
            "jge convert_loop",
            "mov byte ptr [di-1], '-'",
            "dec di",
            "neg ax",
            "convert_loop:",
            "xor dx, dx",
            "div bx",
            "add dl, '0'",
            "dec di",
            "mov [di], dl",
            "cmp ax, 0",
            "jne convert_loop",
            "mov dx, di",
            "mov ah, 09h",
            "int 21h",
            "mov dl, 0Dh",
            "mov ah, 02h",
            "int 21h",
            "mov dl, 0Ah",
            "mov ah, 02h",
            "int 21h",
            "pop di",
            "pop dx",
            "pop cx",
            "pop bx",
            "pop ax",
            "RET",
            "PRINT_INT ENDP",
            "code ends",
            "end main",
        ];
        assert_eq!(code, expected, "Generated code does not match expected for unary not");
    }
}