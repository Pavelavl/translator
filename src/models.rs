#[derive(Debug, Clone, PartialEq)]
pub enum Lexems {
    None,
    Name,
    Number,
    Plus,
    Minus,
    Mul,
    Div,
    Assign,
    Semi,
    NewLine,
    Begin,
    End,
    EOF,
    Let,
    Const,
    Error,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    If,
    Then,
    ElseIf,
    Else,
    EndIf,
    While,
    EndWhile,
    Print,
}

pub struct Keyword {
    pub word: &'static str,
    pub lex: Lexems,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Category {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    None,
    Int,
    Bool,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub category: Category,
    pub data_type: DataType,
}

impl Identifier {
    pub fn new(name: String, category: Category, data_type: DataType) -> Self {
        Identifier {
            name,
            category,
            data_type,
        }
    }
}
