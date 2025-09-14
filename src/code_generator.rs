use std::sync::Mutex;

use crate::name_table::NameTable;

pub struct CodeGenerator;

lazy_static::lazy_static! {
    static ref CODE: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref LABEL_COUNTER: Mutex<usize> = Mutex::new(0);
}

impl CodeGenerator {
    pub fn add_instruction(instruction: &str) {
        CODE.lock().unwrap().push(instruction.to_string());
    }

    pub fn get_code() -> Vec<String> {
        CODE.lock().unwrap().clone()
    }

    pub fn clear() {
        CODE.lock().unwrap().clear()
    }

    pub fn declare_data_segment() {
        Self::add_instruction("data segment para public \"data\"");
    }

    pub fn declare_variables(name_table: &NameTable) {
        for ident in name_table.get_all_identifiers() {
            if ident.category == crate::models::Category::Var {
                Self::add_instruction(&format!("{}  dw    1", ident.name));
            }
        }
    }

    pub fn declare_print_procedure() {
        Self::add_instruction("PRINT PROC NEAR");
        Self::add_instruction("push cx");
        Self::add_instruction("push dx");
        Self::add_instruction("push di");
        Self::add_instruction("cmp ax, 0");
        Self::add_instruction("jne not_zero");
        Self::add_instruction("mov byte ptr [PRINT_BUF], '0'");
        Self::add_instruction("lea dx, PRINT_BUF");
        Self::add_instruction("mov ah, 09h");
        Self::add_instruction("int 21h");
        Self::add_instruction("jmp print_newline");
        Self::add_instruction("not_zero:");
        Self::add_instruction("MOV   CX, 10");
        Self::add_instruction("MOV   DI, BUFEND - PRINT_BUF");
        Self::add_instruction("PRINT_LOOP:");
        Self::add_instruction("MOV   DX, 0");
        Self::add_instruction("DIV   CX");
        Self::add_instruction("ADD   DL, '0'");
        Self::add_instruction("MOV   [PRINT_BUF + DI - 1], DL");
        Self::add_instruction("DEC   DI");
        Self::add_instruction("CMP   AL, 0");
        Self::add_instruction("JNE   PRINT_LOOP");
        Self::add_instruction("LEA   DX, PRINT_BUF");
        Self::add_instruction("ADD   DX, DI");
        Self::add_instruction("MOV   AH, 09H");
        Self::add_instruction("INT   21H");
        Self::add_instruction("print_newline:");
        Self::add_instruction("mov dl, 0Dh"); // CR
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("mov dl, 0Ah"); // LF
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("print_end:");
        Self::add_instruction("pop di");
        Self::add_instruction("pop dx");
        Self::add_instruction("pop cx");
        Self::add_instruction("RET");
        Self::add_instruction("PRINT ENDP");
    }

    pub fn new_label(prefix: &str) -> String {
        let mut counter = LABEL_COUNTER.lock().unwrap();
        *counter += 1;
        format!("{}_{}", prefix, *counter)
    }
}