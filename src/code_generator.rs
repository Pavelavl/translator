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
                Self::add_instruction(&format!("{}  dw    0", ident.name));
            }
        }
    }

    pub fn declare_print_procedure() {
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
    
        // Исправленная процедура PRINT_INT
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
        Self::add_instruction("mov dl, 0Dh"); // CR
        Self::add_instruction("mov ah, 02h");
        Self::add_instruction("int 21h");
        Self::add_instruction("mov dl, 0Ah"); // LF
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
        format!("{}_{}", prefix, *counter)
    }
}