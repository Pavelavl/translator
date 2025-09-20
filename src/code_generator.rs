use std::sync::Mutex;

use crate::models::{BinOp, Category, DataType, ExprKind, Expression, Program, Statement, UnaryOp};
use crate::name_table::NameTable;

pub struct CodeGenerator;

lazy_static::lazy_static! {
    static ref CODE: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref LABEL_COUNTER: Mutex<usize> = Mutex::new(0);
}

impl CodeGenerator {
    pub fn add_instruction(instruction: &str) {
        println!("[CodeGenerator] Adding instruction: '{}'", instruction);
        CODE.lock().unwrap().push(instruction.to_string());
    }

    pub fn get_code() -> Vec<String> {
        let code = CODE.lock().unwrap().clone();
        println!("[CodeGenerator] Retrieving generated code, {} instructions", code.len());
        code
    }

    pub fn clear() {
        println!("[CodeGenerator] Clearing code and label counter");
        CODE.lock().unwrap().clear();
        *LABEL_COUNTER.lock().unwrap() = 0;
    }

    pub fn generate(program: &Program, name_table: &NameTable) {
        println!("[CodeGenerator] Starting code generation for program with {} statements", program.stmts.len());
        Self::add_instruction("data segment para public \"data\"");
        Self::declare_variables(name_table);
        Self::add_instruction("PRINT_BUF DB ' ' DUP(10)");
        Self::add_instruction("BUFEND    DB '$'");
        Self::add_instruction("data ends");
        Self::add_instruction("stk segment stack");
        Self::add_instruction("db 256 dup ('?')");
        Self::add_instruction("stk ends");
        Self::add_instruction("code segment para public 'code'");
        Self::add_instruction("main proc");
        Self::add_instruction("assume cs:code,ds:data,ss:stk");
        Self::add_instruction("mov ax,data");
        Self::add_instruction("mov ds,ax");

        for st in &program.stmts {
            println!("[CodeGenerator] Generating code for statement: {:?}", st);
            Self::gen_statement(st, name_table);
        }

        Self::add_instruction("mov ax,4c00h");
        Self::add_instruction("int 21h");
        Self::add_instruction("main endp");
        Self::declare_print_procedure();
        Self::add_instruction("code ends");
        Self::add_instruction("end main");
        println!("[CodeGenerator] Code generation complete");
    }

    fn gen_statement(st: &Statement, name_table: &NameTable) {
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
                        Self::gen_expression(expr);
                        Self::add_instruction("pop ax");
                        Self::add_instruction(&format!("mov [{}], ax", names[0]));
                    }
                }
            }
            Statement::Assign { name, expr } => {
                println!("[CodeGenerator] Generating assignment: name='{}', expr={:?}", name, expr);
                Self::gen_expression(expr);
                Self::add_instruction("pop ax");
                Self::add_instruction(&format!("mov [{}], ax", name));
            }
            Statement::Print { var } => {
                println!("[CodeGenerator] Generating print: var='{}'", var);
                let typ = name_table
                    .find_by_name(var)
                    .map(|i| i.data_type.clone())
                    .unwrap_or(DataType::None);
                println!("[CodeGenerator] Variable '{}' type: {:?}", var, typ);
                Self::add_instruction(&format!("mov ax, [{}]", var));
                if typ == DataType::Bool {
                    Self::add_instruction("CALL PRINT");
                } else {
                    Self::add_instruction("CALL PRINT_INT");
                }
            }
            Statement::If {
                cond,
                then,
                elseifs,
                els,
            } => {
                println!("[CodeGenerator] Generating if statement: cond={:?}, then_len={}, elseifs_len={}, els={:?}", cond, then.len(), elseifs.len(), els);
                Self::gen_expression(cond);
                Self::add_instruction("pop ax");
                Self::add_instruction("cmp ax, 0");
                let else_label = Self::new_label("else");
                Self::add_instruction(&format!("je {}", else_label));
                for s in then {
                    Self::gen_statement(s, name_table);
                }
                let endif_label = Self::new_label("endif");
                Self::add_instruction(&format!("jmp {}", endif_label));
                let mut current_else = else_label;
                for (c, stmts) in elseifs {
                    Self::add_instruction(&format!("{}:", current_else));
                    Self::gen_expression(c);
                    Self::add_instruction("pop ax");
                    Self::add_instruction("cmp ax, 0");
                    current_else = Self::new_label("else");
                    Self::add_instruction(&format!("je {}", current_else));
                    for s in stmts {
                        Self::gen_statement(s, name_table);
                    }
                    Self::add_instruction(&format!("jmp {}", endif_label));
                }
                Self::add_instruction(&format!("{}:", current_else));
                if let Some(e) = els {
                    for s in e {
                        Self::gen_statement(s, name_table);
                    }
                }
                Self::add_instruction(&format!("{}:", endif_label));
            }
            Statement::While { cond, body } => {
                println!("[CodeGenerator] Generating while loop: cond={:?}, body_len={}", cond, body.len());
                let start_label = Self::new_label("while_start");
                Self::add_instruction(&format!("{}:", start_label));
                Self::gen_expression(cond);
                Self::add_instruction("pop ax");
                Self::add_instruction("cmp ax, 0");
                let end_label = Self::new_label("while_end");
                Self::add_instruction(&format!("je {}", end_label));
                for s in body {
                    Self::gen_statement(s, name_table);
                }
                Self::add_instruction(&format!("jmp {}", start_label));
                Self::add_instruction(&format!("{}:", end_label));
            }
            Statement::Block { stmts } => {
                println!("[CodeGenerator] Generating block with {} statements", stmts.len());
                for s in stmts {
                    Self::gen_statement(s, name_table);
                }
            }
        }
    }

    fn gen_expression(expr: &Expression) {
        println!("[CodeGenerator] Generating expression: {:?}", expr);
        match &expr.kind {
            ExprKind::Literal(v) => {
                println!("[CodeGenerator] Generating literal: value={}", v);
                Self::add_instruction(&format!("mov ax, {}", v));
                Self::add_instruction("push ax");
            }
            ExprKind::Variable(n) => {
                println!("[CodeGenerator] Generating variable: name='{}'", n);
                Self::add_instruction(&format!("mov ax, [{}]", n));
                Self::add_instruction("push ax");
            }
            ExprKind::Binary { op, left, right } => {
                println!("[CodeGenerator] Generating binary operation: op={:?}, left={:?}, right={:?}", op, left, right);
                Self::gen_expression(left);
                Self::gen_expression(right);
                Self::add_instruction("pop bx");
                Self::add_instruction("pop ax");
                match op {
                    BinOp::Add => Self::add_instruction("add ax, bx"),
                    BinOp::Sub => Self::add_instruction("sub ax, bx"),
                    BinOp::Mul => Self::add_instruction("mul bx"),
                    BinOp::Div => {
                        Self::add_instruction("cwd");
                        Self::add_instruction("div bx");
                    }
                    BinOp::And => Self::add_instruction("and ax, bx"),
                    BinOp::Or => Self::add_instruction("or ax, bx"),
                    BinOp::Xor => Self::add_instruction("xor ax, bx"),
                    _ => {
                        Self::add_instruction("cmp ax, bx");
                        let true_label = Self::new_label("true");
                        let done_label = Self::new_label("done");
                        let jump_inst = match op {
                            BinOp::Eq => "je",
                            BinOp::Ne => "jne",
                            BinOp::Lt => "jl",
                            BinOp::Le => "jle",
                            BinOp::Gt => "jg",
                            BinOp::Ge => "jge",
                            _ => unreachable!(),
                        };
                        Self::add_instruction(&format!("{} {}", jump_inst, true_label));
                        Self::add_instruction("mov ax, 0");
                        Self::add_instruction(&format!("jmp {}", done_label));
                        Self::add_instruction(&format!("{}:", true_label));
                        Self::add_instruction("mov ax, 1");
                        Self::add_instruction(&format!("{}:", done_label));
                    }
                }
                Self::add_instruction("push ax");
            }
            ExprKind::Unary { op, expr } => {
                println!("[CodeGenerator] Generating unary operation: op={:?}, expr={:?}", op, expr);
                Self::gen_expression(expr);
                Self::add_instruction("pop ax");
                match op {
                    UnaryOp::Not => Self::add_instruction("xor ax, 1"),
                }
                Self::add_instruction("push ax");
            }
        }
    }

    pub fn declare_variables(name_table: &NameTable) {
        println!("[CodeGenerator] Declaring variables from name table");
        for ident in name_table.entries() {
            if ident.category == Category::Var {
                println!("[CodeGenerator] Declaring variable: {} dw 0", ident.name);
                Self::add_instruction(&format!("{} dw 0", ident.name));
            } else if ident.category == Category::Const && ident.value.is_some() {
                let value = ident.value.unwrap();
                println!("[CodeGenerator] Declaring constant: {} dw {}", ident.name, value);
                Self::add_instruction(&format!("{} dw {}", ident.name, value));
            }
        }
    }

    pub fn declare_print_procedure() {
        println!("[CodeGenerator] Declaring print procedures");
        Self::add_instruction("PRINT PROC NEAR");
        Self::add_instruction("push cx");
        Self::add_instruction("push dx");
        Self::add_instruction("push di");
        Self::add_instruction("cmp ax, 0");
        Self::add_instruction("jne print_one");
        Self::add_instruction("mov byte ptr [PRINT_BUF], '0'");
        Self::add_instruction("mov byte ptr [PRINT_BUF+1], '$'");
        Self::add_instruction("jmp print_output");
        Self::add_instruction("print_one:");
        Self::add_instruction("mov byte ptr [PRINT_BUF], '1'");
        Self::add_instruction("mov byte ptr [PRINT_BUF+1], '$'");
        Self::add_instruction("print_output:");
        Self::add_instruction("lea dx, PRINT_BUF");
        Self::add_instruction("mov ah, 09h");
        Self::add_instruction("int 21h");
        Self::add_instruction("mov dl, 0Dh");
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("mov dl, 0Ah");
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("pop di");
        Self::add_instruction("pop dx");
        Self::add_instruction("pop cx");
        Self::add_instruction("RET");
        Self::add_instruction("PRINT ENDP");

        Self::add_instruction("PRINT_INT PROC NEAR");
        Self::add_instruction("push ax");
        Self::add_instruction("push bx");
        Self::add_instruction("push cx");
        Self::add_instruction("push dx");
        Self::add_instruction("push di");
        Self::add_instruction("mov bx, 10");
        Self::add_instruction("lea di, PRINT_BUF + 9");
        Self::add_instruction("mov byte ptr [di], '$'");
        Self::add_instruction("cmp ax, 0");
        Self::add_instruction("jge convert_loop");
        Self::add_instruction("mov byte ptr [di-1], '-'");
        Self::add_instruction("dec di");
        Self::add_instruction("neg ax");
        Self::add_instruction("convert_loop:");
        Self::add_instruction("xor dx, dx");
        Self::add_instruction("div bx");
        Self::add_instruction("add dl, '0'");
        Self::add_instruction("dec di");
        Self::add_instruction("mov [di], dl");
        Self::add_instruction("cmp ax, 0");
        Self::add_instruction("jne convert_loop");
        Self::add_instruction("mov dx, di");
        Self::add_instruction("mov ah, 09h");
        Self::add_instruction("int 21h");
        Self::add_instruction("mov dl, 0Dh");
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("mov dl, 0Ah");
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("pop di");
        Self::add_instruction("pop dx");
        Self::add_instruction("pop cx");
        Self::add_instruction("pop bx");
        Self::add_instruction("pop ax");
        Self::add_instruction("RET");
        Self::add_instruction("PRINT_INT ENDP");
    }

    pub fn new_label(prefix: &str) -> String {
        let mut counter = LABEL_COUNTER.lock().unwrap();
        *counter += 1;
        let label = format!("{}_{}", prefix, *counter);
        println!("[CodeGenerator] Generated new label: {}", label);
        label
    }
}