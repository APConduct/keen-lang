#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    TypeDef(TypeDef),
    VariableDecl(VariableDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<Type>,
    pub mutability: Option<Mutability>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionBody {
    Block(Vec<Statement>),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    Alias {
        name: String,
        is_distinct: bool,
        underlying_type: Type,
    },
    Product {
        name: String,
        fields: Vec<ProductField>,
    },
    Union {
        name: String,
        variants: Vec<UnionVariant>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mutability {
    Immutable,
    Live,
    Keep,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl {
    pub name: String,
    pub mutability: Mutability,
    pub type_annotation: Option<Type>,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(String),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Product {
        name: String,
        fields: Vec<ProductField>,
    },
    Union {
        name: String,
        variants: Vec<UnionVariant>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProductField {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionVariant {
    pub name: String,
    pub fields: Vec<ProductField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VariableDecl(VariableDecl),
    DestructuringDecl { pattern: Pattern, value: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Call {
        function: Box<Expression>,
        args: Vec<Expression>,
    },
    Case {
        expr: Box<Expression>,
        arms: Vec<CaseArm>,
    },
    When {
        expr: Box<Expression>,
        arms: Vec<WhenArm>,
    },
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    List {
        elements: Vec<Expression>,
    },
    Map {
        pairs: Vec<(Expression, Expression)>,
    },
    MethodCall {
        object: Box<Expression>,
        method: String,
        args: Vec<Expression>,
    },
    Constructor {
        name: String,
        args: Vec<ConstructorArg>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorArg {
    pub name: Option<String>,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseArm {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenArm {
    pub condition: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Identifier(String),
    Wildcard,
    Constructor { name: String, args: Vec<Pattern> },
}
