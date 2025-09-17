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
    Error,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Not,
    And,
    Or,
    Xor,
    If,
    Then,
    ElseIf,
    Else,
    EndIf,
    While,
    EndWhile,
    Print,
    Int,
    Bool,
    LParen,
    RParen,
    Comma,
    Colon,
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
    pub value: Option<i32>,
}
