use crate::code_generator::CodeGenerator;
use crate::lexical_analyzer::LexicalAnalyzer;
use crate::models::Lexems;
use crate::reader::Reader;
use crate::syntax_analyzer::{Error, SyntaxAnalyzer};

pub struct Compiler {
    pub analyzer: SyntaxAnalyzer<LexicalAnalyzer>,
    pub code_generator: CodeGenerator,
}

impl Compiler {
    pub fn new(input_text: &str) -> Self {
        let mut r = Reader::new();
        if let Err(e) = r.init_with_string(input_text) {
            println!("[Compiler] Init failed: {}", e);
            r.close();
        }

        let code_generator = CodeGenerator::new();
        let mut lexer = LexicalAnalyzer::new(r);
        lexer.advance(); // Advance to first token after Reader initialization
        let analyzer = SyntaxAnalyzer::new(lexer);

        return Self {
            analyzer: analyzer,
            code_generator: code_generator,
        };
    }

    pub fn compile(&mut self) -> Result<String, Vec<Error>> {
        println!("[Compiler] Starting compilation");
        self.code_generator.clear();
        println!("[Compiler] Collecting declarations");
        self.collect_declarations();
        if !self.analyzer.errors.is_empty() {
            println!(
                "[Compiler] Compilation failed with {} errors",
                self.analyzer.errors.len()
            );
            return Err(self.analyzer.errors.clone());
        }

        println!("[Compiler] Parsing program to build AST");
        let program = self.analyzer.parse_program();
        if !self.analyzer.errors.is_empty() {
            println!(
                "[Compiler] Parsing failed with {} errors",
                self.analyzer.errors.len()
            );
            return Err(self.analyzer.errors.clone());
        }

        println!("[Compiler] Generating assembly code");
        self.code_generator
            .generate(&program, &self.analyzer.name_table);
        let code = self.code_generator.get_code().join("\n");
        println!(
            "[Compiler] Compilation complete, generated code length: {}",
            code.len()
        );
        Ok(code)
    }

    fn collect_declarations(&mut self) {
        println!("[Compiler] Starting collect_declarations");
        while matches!(
            self.analyzer.lexer.current_lexem(),
            Lexems::Int | Lexems::Bool
        ) {
            println!("[Compiler] Processing variable declaration");
            self.analyzer.parse_variable_declarations_no_code();
            self.analyzer.maybe_advance();
        }
        println!("[Compiler] Completed collect_declarations");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::Category;

    #[test]
    fn test_compiler_new() {
        let input = "int x; begin x = 5; end; print x;";
        let compiler = Compiler::new(input);

        assert!(compiler.analyzer.errors.is_empty());
        assert!(compiler.code_generator.get_code().is_empty());
    }

    #[test]
    fn test_compiler_compile_success() {
        let input = "int x;\nbegin;\nx = 5;\nend;\nprint x;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 5"));
        assert!(code.contains("mov [x], ax"));
        assert!(code.contains("CALL PRINT_INT"));
    }

    #[test]
    fn test_compiler_compile_with_errors() {
        let input = "int x; begin x = y; end;"; // y is not declared
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_err());

        let errors = result.err().unwrap();
        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("Undeclared identifier"));
    }

    #[test]
    fn test_compiler_compile_only_declarations() {
        let input = "int x, y; bool z; begin end;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("x dw 0"));
        assert!(code.contains("y dw 0"));
        assert!(code.contains("z dw 0"));
    }

    #[test]
    fn test_compiler_compile_arithmetic() {
        let input = "int x, y, result; begin x = 10; y = 20; result = x + y; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(
            code.contains("mov ax, 10"),
            "Expected 'mov ax, 10' in generated code"
        );
        assert!(
            code.contains("mov ax, 20"),
            "Expected 'mov ax, 20' in generated code"
        );
        assert!(
            code.contains("add ax, bx"),
            "Expected 'add ax, bx' in generated code"
        );
        assert!(
            code.contains("mov [result], ax"),
            "Expected 'mov [result], ax' in generated code"
        );
        assert!(
            code.contains("CALL PRINT_INT"),
            "Expected 'CALL PRINT_INT' in generated code"
        );
    }

    #[test]
    fn test_compiler_compile_logical_operations() {
        let input = "bool x, y, result; begin x = 1; y = 0; result = x .and. y; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 1"));
        assert!(code.contains("mov ax, 0"));
        assert!(code.contains("and ax, bx"));
        assert!(code.contains("mov [result], ax"));
        assert!(code.contains("CALL PRINT"));
    }

    #[test]
    fn test_compiler_compile_comparison() {
        let input =
            "int x, y; bool result; begin x = 10; y = 20; result = x < y; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 10"));
        assert!(code.contains("mov ax, 20"));
        assert!(code.contains("cmp ax, bx"));
        assert!(code.contains("jl true_"));
        assert!(code.contains("mov [result], ax"));
        assert!(code.contains("CALL PRINT"));
    }

    #[test]
    fn test_compiler_compile_if_statement() {
        let input = "int x; bool result; begin x = 10; if x > 5 then result = 1; else result = 0; endif; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 10"));
        assert!(code.contains("cmp ax, bx"));
        assert!(code.contains("je else_"));
        assert!(code.contains("mov ax, 1"));
        assert!(code.contains("mov ax, 0"));
        assert!(code.contains("mov [result], ax"));
        assert!(code.contains("CALL PRINT"));
    }

    #[test]
    fn test_compiler_compile_while_loop() {
        let input = "int x; begin x = 0; while x < 10 x = x + 1; endwhile; end; print x;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 0"));
        assert!(code.contains("while_start_"));
        assert!(code.contains("cmp ax, bx"));
        assert!(code.contains("je while_end_"));
        assert!(code.contains("add ax, bx"));
        assert!(code.contains("mov [x], ax"));
        assert!(code.contains("CALL PRINT_INT"));
    }

    #[test]
    fn test_compiler_compile_unary_not() {
        let input = "bool x, result; begin x = 1; result = .not. x; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 1"));
        assert!(code.contains("xor ax, 1"));
        assert!(code.contains("mov [result], ax"));
        assert!(code.contains("CALL PRINT"));
    }

    #[test]
    fn test_compiler_collect_declarations() {
        let input = "int x, y; bool z; begin end;";
        let mut compiler = Compiler::new(input);

        compiler.collect_declarations();

        // Check that variables are in the name table
        assert!(compiler.analyzer.name_table.find_by_name("x").is_some());
        assert!(compiler.analyzer.name_table.find_by_name("y").is_some());
        assert!(compiler.analyzer.name_table.find_by_name("z").is_some());
    }

    #[test]
    fn test_compiler_collect_declarations_with_const() {
        let input = "int const x := 42; bool y; begin end;";
        let mut compiler = Compiler::new(input);

        compiler.collect_declarations();

        // Check that constant is in the name table
        let x = compiler.analyzer.name_table.find_by_name("x").unwrap();
        assert_eq!(x.category, Category::Const);
        assert_eq!(x.value, Some(42));
    }

    #[test]
    fn test_compiler_collect_declarations_empty() {
        let input = "begin end;";
        let mut compiler = Compiler::new(input);

        compiler.collect_declarations();

        // Should not have any errors
        assert!(compiler.analyzer.errors.is_empty());
    }

    #[test]
    fn test_compiler_collect_declarations_with_errors() {
        let input = "int x, ;"; // Invalid syntax
        let mut compiler = Compiler::new(input);

        compiler.collect_declarations();

        // Should have errors
        assert!(!compiler.analyzer.errors.is_empty());
        assert!(
            compiler.analyzer.errors[0]
                .message
                .contains("Expected variable name after comma")
        );
    }

    #[test]
    fn test_compiler_compile_multiple_prints() {
        let input = "int x, y; begin x = 5; y = 10; end; print x; print y;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 5"));
        assert!(code.contains("mov ax, 10"));
        assert_eq!(code.matches("CALL PRINT_INT").count(), 2);
    }

    #[test]
    fn test_compiler_compile_nested_blocks() {
        let input = "int x; begin begin x = 5; end; begin x = 10; end; end; print x;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 5"));
        assert!(code.contains("mov ax, 10"));
        assert!(code.contains("mov [x], ax"));
        assert!(code.contains("CALL PRINT_INT"));
    }

    #[test]
    fn test_compiler_compile_complex_expression() {
        let input = "int x, y, z, result; begin x = 5; y = 10; z = 15; result = (x + y) * z / 2; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 5"));
        assert!(code.contains("mov bx, 10"));
        assert!(code.contains("add ax, bx"));
        assert!(code.contains("mov ax, 15"));
        assert!(code.contains("mul bx"));
        assert!(code.contains("mov ax, 2"));
        assert!(code.contains("div bx"));
        assert!(code.contains("mov [result], ax"));
        assert!(code.contains("CALL PRINT_INT"));
    }

    #[test]
    fn test_compiler_compile_boolean_expressions() {
        let input = "bool a, b, c, result; begin a = 1; b = 0; c = 1; result = (a .and. b) .or. c; end; print result;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 1")); // For a = 1 or c = 1
        assert!(code.contains("mov ax, 0")); // For b = 0
        assert!(code.contains("and ax, bx")); // For a .and. b
        assert!(code.contains("or ax, bx")); // For (a .and. b) .or. c
        assert!(code.contains("mov [result], ax")); // For result = ...
        assert!(code.contains("CALL PRINT")); // For print result
    }

    #[test]
    fn test_compiler_compile_with_whitespace() {
        let input =
            "   int   x   ;   \n  begin   \n x   =   5   ;   \n  end   ;   \n  print   x   ;   ";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 5"));
        assert!(code.contains("mov [x], ax"));
        assert!(code.contains("CALL PRINT_INT"));
    }

    #[test]
    fn test_compiler_compile_print_after_block() {
        let input = "int x; begin; x = 5; end; print x;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_ok());

        let code = result.unwrap();
        assert!(code.contains("mov ax, 5"));
        assert!(code.contains("mov [x], ax"));
        assert!(code.contains("CALL PRINT_INT"));
        assert!(compiler.analyzer.errors.is_empty());
    }

    #[test]
    fn test_compiler_compile_invalid_token_after_end() {
        let input = "begin; end; 42;";
        let mut compiler = Compiler::new(input);

        let result = compiler.compile();
        assert!(result.is_err());

        let errors = result.err().unwrap();
        assert_eq!(errors.len(), 2);
        assert_eq!(
            errors[0].message,
            "Unexpected token in instruction: Number ('42')"
        );
    }
}
