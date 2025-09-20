use crate::code_generator::CodeGenerator;
use crate::lexical_analyzer::LexicalAnalyzer;
use crate::models::Lexems;
use crate::syntax_analyzer::{Error, SyntaxAnalyzer};

pub struct Compiler {
    pub analyzer: SyntaxAnalyzer<LexicalAnalyzer>,
}

impl Compiler {
    pub fn new(analyzer: SyntaxAnalyzer<LexicalAnalyzer>) -> Self {
        println!("[Compiler] Initializing new Compiler");
        Self { analyzer }
    }

    pub fn compile(&mut self) -> Result<String, Vec<Error>> {
        println!("[Compiler] Starting compilation");
        CodeGenerator::clear();
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
        CodeGenerator::generate(&program, &self.analyzer.name_table);
        let code = CodeGenerator::get_code().join("\n");
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