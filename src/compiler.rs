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
            println!("[Compiler] Compilation failed with {} errors", self.analyzer.errors.len());
            return Err(self.analyzer.errors.clone());
        }

        println!("[Compiler] Parsing program to build AST");
        let program = self.analyzer.parse_program();
        if !self.analyzer.errors.is_empty() {
            println!("[Compiler] Parsing failed with {} errors", self.analyzer.errors.len());
            return Err(self.analyzer.errors.clone());
        }

        println!("[Compiler] Generating assembly code");
        self.code_generator.generate(&program, &self.analyzer.name_table);
        let code = self.code_generator.get_code().join("\n");
        println!("[Compiler] Compilation complete, generated code length: {}", code.len());
        Ok(code)
    }

    fn collect_declarations(&mut self) {
        println!("[Compiler] Starting collect_declarations");
        while matches!(self.analyzer.lexer.current_lexem(), Lexems::Int | Lexems::Bool) {
            println!("[Compiler] Processing variable declaration");
            self.analyzer.parse_variable_declarations_no_code();
            self.analyzer.maybe_advance();
        }
        println!("[Compiler] Completed collect_declarations");
    }
}