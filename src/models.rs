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

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(i32),
    Variable(String),
    Binary {
        op: BinOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExprKind,
    pub typ: DataType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VarDecl {
        typ: DataType,
        is_const: bool,
        names: Vec<String>,
        init: Option<Expression>,
    },
    Assign {
        name: String,
        expr: Expression,
    },
    Print {
        var: String,
    },
    If {
        cond: Expression,
        then: Vec<Statement>,
        elseifs: Vec<(Expression, Vec<Statement>)>,
        els: Option<Vec<Statement>>,
    },
    While {
        cond: Expression,
        body: Vec<Statement>,
    },
    Block {
        stmts: Vec<Statement>,
    },
}

pub struct Program {
    pub(crate) stmts: Vec<Statement>,
}
