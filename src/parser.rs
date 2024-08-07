use std::{iter::Peekable, vec::IntoIter};

use crate::lexer::{Keyword, LexerLiteral, LexerToken, LexerType, Symbol};

type Identifier = String;
type CodeBlock = Vec<Statement>;
type Parameter = (ASTType, Identifier);

#[derive(Debug)]
pub enum TopLevel {
    Function(ASTType, Identifier, Option<Vec<Parameter>>, CodeBlock),
    VariableDefinition(ASTType, Identifier, Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionCall(Identifier, Option<Vec<Expression>>),
    VariableAssignment(VariableAssignment),
    Conditional(Conditional),
    CodeBlock(CodeBlock),
    Return,
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub enum VariableAssignment {
    Definition(ASTType, Identifier, Option<Expression>),
    Assignment(Identifier, Expression),
}

#[derive(Debug, Clone)]
pub enum Conditional {
    IfStatement(Vec<(Expression, CodeBlock)>),
    ForLoop(
        Option<VariableAssignment>,
        Option<Expression>,
        Option<VariableAssignment>,
        CodeBlock,
    ),
    WhileLoop(Expression, CodeBlock),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Literal(ASTLiteral),
    Variable(Identifier),
    FunctionCall(Identifier, Option<Vec<Expression>>),
    Grouping(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negation,
    LogicalNegation,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    NEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    And,
    Or,

    BitwiseAnd,
    BitwiseOr,
}

#[derive(Debug, Clone)]
pub enum ASTLiteral {
    String(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    Null,
}

#[derive(Debug, Clone)]
pub enum ASTType {
    Int,
    Bool,
    Float,
    Char,
    Void,
    Double,
    Long,
    Short,
}

struct ParserIterator {
    iter: Peekable<IntoIter<LexerToken>>,
}

impl ParserIterator {
    fn new(tokens: Peekable<IntoIter<LexerToken>>) -> Self {
        ParserIterator { iter: tokens }
    }

    fn peek(&mut self) -> Option<LexerToken> {
        self.iter.peek().cloned()
    }
}

impl Iterator for ParserIterator {
    type Item = LexerToken;

    fn next(&mut self) -> Option<LexerToken> {
        self.iter.next()
    }
}

pub fn parse(tokens: Vec<LexerToken>) -> Result<Vec<TopLevel>, String> {
    let mut iterator = ParserIterator::new(tokens.into_iter().peekable());
    parse_top_level(&mut iterator)
}

fn parse_top_level(iterator: &mut ParserIterator) -> Result<Vec<TopLevel>, String> {
    let mut top_level: Vec<TopLevel> = Vec::new();

    loop {
        if let Some(LexerToken::EOF) = iterator.peek() {
            break Ok(top_level);
        }
        let x = parse_top_level_element(iterator)?;
        top_level.push(x);
    }
}

fn parse_top_level_element(iterator: &mut ParserIterator) -> Result<TopLevel, String> {
    let token = iterator
        .next()
        .ok_or("Missing token at start of top level element".to_string())?;

    let keyword = match token {
        LexerToken::Keyword(keyword) => keyword,
        _ => return Err(format!("Expected keyword found '{:?}'", token)),
    };

    let ast_type = match keyword {
        Keyword::Type(LexerType::Int) => ASTType::Int,
        Keyword::Type(LexerType::Float) => ASTType::Float,
        Keyword::Type(LexerType::Bool) => ASTType::Bool,
        Keyword::Type(LexerType::Char) => ASTType::Char,
        Keyword::Type(LexerType::Void) => ASTType::Void,
        Keyword::Type(LexerType::Double) => ASTType::Double,
        Keyword::Type(LexerType::Long) => ASTType::Long,
        Keyword::Type(LexerType::Short) => ASTType::Short,
        _ => return Err(format!("Invalid return type '{:?}'", keyword)),
    };

    let token = iterator.next().ok_or("Missing identifier".to_string())?;
    let identifier = match token {
        LexerToken::Identifier(identifier) => identifier,
        _ => return Err(format!("Expected identifier found '{:?}'", token)),
    };

    let token = iterator.peek().ok_or("Missing identifier".to_string())?;
    match token {
        LexerToken::Symbol(Symbol::Semicolon) => match iterator.peek() {
            Some(LexerToken::Symbol(Symbol::Semicolon)) => {
                iterator.next();
                Ok(TopLevel::VariableDefinition(ast_type, identifier, None))
            }
            token => Err(format!("Expected semicolon, found {:?}", token).to_string()),
        },
        LexerToken::Symbol(Symbol::Equal) => {
            iterator.next();
            let expression = parse_expression(iterator, 0)?;

            match iterator.peek() {
                Some(LexerToken::Symbol(Symbol::Semicolon)) => {
                    iterator.next();
                    Ok(TopLevel::VariableDefinition(
                        ast_type,
                        identifier,
                        Some(expression),
                    ))
                }
                token => Err(format!("Expected semicolon, found {:?}", token).to_string()),
            }
        }
        LexerToken::Symbol(Symbol::LBracket) => {
            parse_function_definition(iterator, keyword, identifier)
        }
        _ => Err(format!("Invalid top level element found {:?}", token)),
    }
}

fn parse_function_definition(
    iterator: &mut ParserIterator,
    return_type: Keyword,
    function_name: String,
) -> Result<TopLevel, String> {
    let return_type = match return_type {
        Keyword::Type(LexerType::Int) => ASTType::Int,
        Keyword::Type(LexerType::Float) => ASTType::Float,
        Keyword::Type(LexerType::Bool) => ASTType::Bool,
        Keyword::Type(LexerType::Char) => ASTType::Char,
        Keyword::Type(LexerType::Void) => ASTType::Void,
        Keyword::Type(LexerType::Double) => ASTType::Double,
        Keyword::Type(LexerType::Long) => ASTType::Long,
        Keyword::Type(LexerType::Short) => ASTType::Short,
        _ => return Err(format!("Invalid return type '{:?}'", return_type)),
    };

    let token = iterator.next().ok_or("Missing token".to_string())?;
    if token != LexerToken::Symbol(Symbol::LBracket) {
        return Err("Expected LBracket".to_string());
    }

    let token = iterator.peek().ok_or("Missing token".to_string())?;
    if token == LexerToken::Symbol(Symbol::RBracket) {
        iterator.next();
        return Ok(TopLevel::Function(
            return_type,
            function_name,
            None,
            parse_code_block(iterator)?,
        ));
    }

    let mut parameters: Vec<Parameter> = Vec::new();
    loop {
        let token = iterator.next().ok_or("Missing token".to_string())?;
        let parameter_type = match token {
            LexerToken::Keyword(Keyword::Type(LexerType::Int)) => ASTType::Int,
            LexerToken::Keyword(Keyword::Type(LexerType::Float)) => ASTType::Float,
            LexerToken::Keyword(Keyword::Type(LexerType::Bool)) => ASTType::Bool,
            LexerToken::Keyword(Keyword::Type(LexerType::Char)) => ASTType::Char,
            LexerToken::Keyword(Keyword::Type(LexerType::Void)) => ASTType::Void,
            LexerToken::Keyword(Keyword::Type(LexerType::Double)) => ASTType::Double,
            LexerToken::Keyword(Keyword::Type(LexerType::Long)) => ASTType::Long,
            LexerToken::Keyword(Keyword::Type(LexerType::Short)) => ASTType::Short,
            _ => return Err(format!("Expect type found '{:?}'", token)),
        };

        let token = iterator.next().ok_or("Missing token".to_string())?;
        let identifier = if let LexerToken::Identifier(identifier) = token {
            identifier
        } else {
            return Err(format!("Expected identifier found '{:?}", token));
        };

        parameters.push((parameter_type, identifier));

        let token = iterator.next().ok_or("Missing token".to_string())?;
        if token == LexerToken::Symbol(Symbol::RBracket) {
            return Ok(TopLevel::Function(
                return_type,
                function_name,
                Some(parameters),
                parse_code_block(iterator)?,
            ));
        } else if token != LexerToken::Symbol(Symbol::Comma) {
            return Err(format!("Expected ',' found '{:?}'", token));
        }
    }
}

fn parse_code_block(iterator: &mut ParserIterator) -> Result<CodeBlock, String> {
    let token = iterator.next().ok_or("Missing token".to_string())?;
    if token != LexerToken::Symbol(Symbol::LBrace) {
        return Err("Invalid Code Block".to_string());
    }

    let mut statements: CodeBlock = Vec::new();
    loop {
        let token = iterator.peek().ok_or("Missing token".to_string())?;
        if token == LexerToken::Symbol(Symbol::RBrace) {
            iterator.next();
            return Ok(statements);
        }
        let statement = parse_statement(iterator)?;
        statements.push(statement)
    }
}

fn parse_statement(iterator: &mut ParserIterator) -> Result<Statement, String> {
    let token = iterator
        .peek()
        .ok_or("Missing token at start of statement".to_string())?;

    let statement = match token {
        LexerToken::Identifier(identifier) => parse_identifier(iterator, identifier)?,
        LexerToken::Keyword(keyword) => parse_keyword(iterator, keyword)?,
        LexerToken::Symbol(Symbol::LBrace) => Statement::CodeBlock(parse_code_block(iterator)?),
        token => return Err(format!("Invalid token {:?} at start of statement", token)),
    };

    if let Statement::Conditional(_) | Statement::CodeBlock(_) = statement {
        Ok(statement)
    } else {
        match iterator.peek() {
            Some(LexerToken::Symbol(Symbol::Semicolon)) => {
                iterator.next();
                Ok(statement)
            }
            token => Err(format!("Expected semicolon, found {:?}", token).to_string()),
        }
    }
}

fn parse_identifier(
    iterator: &mut ParserIterator,
    identifier: String,
) -> Result<Statement, String> {
    iterator.next();
    let token = iterator.next().ok_or("Missing token".to_string())?;
    match token {
        LexerToken::Symbol(Symbol::LBracket) => Ok(Statement::FunctionCall(
            identifier,
            parse_function_call_arguements(iterator)?,
        )),
        LexerToken::Symbol(Symbol::Equal) => Ok(Statement::VariableAssignment(
            VariableAssignment::Assignment(identifier, parse_expression(iterator, 0)?),
        )),
        token => Err(format!(
            "Invalid symbol after {:?}, found {:?}",
            identifier, token
        )),
    }
}

fn parse_keyword(iterator: &mut ParserIterator, keyword: Keyword) -> Result<Statement, String> {
    if let Ok(statement) = parse_break_continue(iterator, keyword.clone()) {
        return Ok(statement);
    }

    if let Ok(statement) = parse_variable_definition_declaration(iterator, keyword.clone()) {
        return Ok(statement);
    }

    match keyword {
        Keyword::If => Ok(Statement::Conditional(Conditional::IfStatement(
            parse_if_statement(iterator)?,
        ))),
        Keyword::For => Ok(Statement::Conditional(parse_for_loop(iterator)?)),
        Keyword::While => {
            iterator.next();
            Ok(Statement::Conditional(Conditional::WhileLoop(
                parse_grouping(iterator)?,
                parse_code_block(iterator)?,
            )))
        }
        _ => Err(format!("Invalid keyword here {:?}", keyword)),
    }
}

fn parse_break_continue(
    iterator: &mut ParserIterator,
    keyword: Keyword,
) -> Result<Statement, String> {
    let keyword = match keyword {
        Keyword::Break => Ok(Statement::Break),
        Keyword::Return => Ok(Statement::Return),
        Keyword::Continue => Ok(Statement::Continue),
        _ => Err(format!("Invalid keyword '{:?}'", keyword)),
    };

    if let Ok(_) = keyword {
        iterator.next();
    }
    keyword
}

fn parse_variable_definition_declaration(
    iterator: &mut ParserIterator,
    keyword: Keyword,
) -> Result<Statement, String> {
    let variable_type = match keyword {
        Keyword::Type(LexerType::Int) => ASTType::Int,
        Keyword::Type(LexerType::Float) => ASTType::Float,
        Keyword::Type(LexerType::Bool) => ASTType::Bool,
        Keyword::Type(LexerType::Char) => ASTType::Char,
        token => return Err(format!("Expected type in declaration, found {:?}", token)),
    };

    iterator.next();
    let variable_name = match iterator
        .next()
        .ok_or("Expected variable name in assignment".to_string())?
    {
        LexerToken::Identifier(identifer) => identifer,
        token => {
            return Err(format!(
                "Expected variable name in assignment, found {:?}",
                token
            ))
        }
    };

    match iterator
        .peek()
        .ok_or("Expected semicolon or equal sign".to_string())?
    {
        LexerToken::Symbol(Symbol::Semicolon) => Ok(Statement::VariableAssignment(
            VariableAssignment::Definition(variable_type, variable_name, None),
        )),
        LexerToken::Symbol(Symbol::Equal) => {
            iterator.next();
            let expression = parse_expression(iterator, 0)?;
            Ok(Statement::VariableAssignment(
                VariableAssignment::Definition(variable_type, variable_name, Some(expression)),
            ))
        }
        token => Err(format!(
            "Expected semicolon or equal sign, found {:?}",
            token
        )),
    }
}

fn parse_if_statement(
    iterator: &mut ParserIterator,
) -> Result<Vec<(Expression, CodeBlock)>, String> {
    iterator.next();
    let mut if_statement: Vec<(Expression, CodeBlock)> = Vec::new();

    let token = iterator.peek().ok_or("Missing token".to_string())?;
    if token != LexerToken::Symbol(Symbol::LBracket) {
        return Err(format!("Expected '(', found {:?}", token));
    }

    let condition = parse_grouping(iterator)?;
    let code_block = parse_code_block(iterator)?;

    if_statement.push((condition, code_block));

    loop {
        let token = iterator.peek().ok_or("Missing token".to_string())?;
        if token != LexerToken::Keyword(Keyword::Else) {
            return Ok(if_statement);
        }
        iterator.next();

        let token = iterator.next().ok_or("Missing token".to_string())?;
        if token != LexerToken::Keyword(Keyword::If) {
            return Err(format!("Expected 'if', found {:?}", token));
        }

        let token = iterator.peek().ok_or("Missing token".to_string())?;
        if token != LexerToken::Symbol(Symbol::LBracket) {
            return Err(format!("Expected '(', found {:?}", token));
        }

        let condition = parse_grouping(iterator)?;
        let code_block = parse_code_block(iterator)?;

        if_statement.push((condition, code_block));
    }
}

fn parse_for_loop(iterator: &mut ParserIterator) -> Result<Conditional, String> {
    iterator.next();
    let token = iterator.next().ok_or("Missing token".to_string())?;
    if token != LexerToken::Symbol(Symbol::LBracket) {
        return Err(format!("Expected '(' found {:?}", token));
    }

    let token = iterator.peek().ok_or("Missing token".to_string())?;
    let initial_statement = if token == LexerToken::Symbol(Symbol::Semicolon) {
        iterator.next();
        None
    } else {
        if let Statement::VariableAssignment(initial_statement) = parse_statement(iterator)? {
            Some(initial_statement)
        } else {
            return Err("Expected variable definition or assignment".to_string());
        }
    };

    let token = iterator.peek().ok_or("Missing token".to_string())?;
    let exit_condition = if token == LexerToken::Symbol(Symbol::Semicolon) {
        None
    } else {
        Some(parse_expression(iterator, 0)?)
    };

    iterator.next();

    let token = iterator.peek().ok_or("Missing token".to_string())?;
    let continuous_expression = if token == LexerToken::Symbol(Symbol::RBracket) {
        None
    } else {
        let identifier = match iterator.next() {
            Some(LexerToken::Identifier(identifier)) => identifier,
            token => return Err(format!("Expected identifier found {:?}", token)),
        };
        iterator.next();

        Some(VariableAssignment::Assignment(
            identifier,
            parse_expression(iterator, 0)?,
        ))
    };
    iterator.next();

    let code_block = parse_code_block(iterator)?;

    Ok(Conditional::ForLoop(
        initial_statement,
        exit_condition,
        continuous_expression,
        code_block,
    ))
}

fn parse_function_call_arguements(
    iterator: &mut ParserIterator,
) -> Result<Option<Vec<Expression>>, String> {
    let token = iterator.peek().ok_or("Missing token".to_string())?;
    if token == LexerToken::Symbol(Symbol::RBracket) {
        iterator.next();
        return Ok(None);
    }

    let mut arguements: Vec<Expression> = Vec::new();

    loop {
        arguements.push(parse_expression(iterator, 0)?);

        let token = iterator.next().ok_or("Missing token".to_string())?;
        if token == LexerToken::Symbol(Symbol::RBracket) {
            return Ok(Some(arguements));
        } else if token != LexerToken::Symbol(Symbol::Comma) {
            return Err(format!("Expected ',' found '{:?}'", token));
        }
    }
}

fn parse_expression(
    iterator: &mut ParserIterator,
    previous_precedence: i32,
) -> Result<Expression, String> {
    let mut left = parse_primary_expression(iterator)?;
    let mut operator = iterator.peek().ok_or("Expected operator".to_string())?;
    if let LexerToken::Symbol(Symbol::Semicolon | Symbol::RBracket | Symbol::Comma) = operator {
        return Ok(left);
    }

    while get_precedence(&operator)? > previous_precedence {
        iterator.next();
        let right = parse_expression(iterator, get_precedence(&operator)?)?;

        left = Expression::Binary(
            Box::new(left),
            convert_to_ast_token(&operator)?,
            Box::new(right),
        );

        operator = iterator.peek().ok_or("Expected operator".to_string())?;
        if let LexerToken::Symbol(Symbol::Semicolon | Symbol::RBracket | Symbol::Comma) = operator {
            return Ok(left);
        }
    }
    Ok(left)
}

fn get_precedence(operator: &LexerToken) -> Result<i32, String> {
    match operator {
        LexerToken::Symbol(Symbol::Plus) => Ok(10),
        LexerToken::Symbol(Symbol::Minus) => Ok(10),
        LexerToken::Symbol(Symbol::Star) => Ok(11),
        LexerToken::Symbol(Symbol::Slash) => Ok(11),
        LexerToken::Symbol(Symbol::DoubleEqual) => Ok(7),
        LexerToken::Symbol(Symbol::NotEqual) => Ok(7),
        LexerToken::Symbol(Symbol::LAngleBracket) => Ok(8),
        LexerToken::Symbol(Symbol::LessThanOrEqual) => Ok(8),
        LexerToken::Symbol(Symbol::RAngleBracket) => Ok(8),
        LexerToken::Symbol(Symbol::GreaterThanOrEqual) => Ok(8),
        LexerToken::Symbol(Symbol::And) => Ok(6),
        LexerToken::Symbol(Symbol::Or) => Ok(5),
        LexerToken::Symbol(Symbol::Equal) => Ok(4),
        LexerToken::Symbol(Symbol::BitwiseAnd) => Ok(3),
        LexerToken::Symbol(Symbol::BitwiseOr) => Ok(2),
        LexerToken::Symbol(Symbol::BitwiseNot) => Ok(1),
        token => Err(format!("Expected operator, found {:?}", token).to_string()),
    }
}

fn convert_to_ast_token(operator: &LexerToken) -> Result<BinaryOperator, String> {
    match operator {
        LexerToken::Symbol(Symbol::Plus) => Ok(BinaryOperator::Add),
        LexerToken::Symbol(Symbol::Minus) => Ok(BinaryOperator::Sub),
        LexerToken::Symbol(Symbol::Star) => Ok(BinaryOperator::Mul),
        LexerToken::Symbol(Symbol::Slash) => Ok(BinaryOperator::Div),
        LexerToken::Symbol(Symbol::DoubleEqual) => Ok(BinaryOperator::Eq),
        LexerToken::Symbol(Symbol::NotEqual) => Ok(BinaryOperator::NEq),
        LexerToken::Symbol(Symbol::LAngleBracket) => Ok(BinaryOperator::Lt),
        LexerToken::Symbol(Symbol::LessThanOrEqual) => Ok(BinaryOperator::LtEq),
        LexerToken::Symbol(Symbol::RAngleBracket) => Ok(BinaryOperator::Gt),
        LexerToken::Symbol(Symbol::GreaterThanOrEqual) => Ok(BinaryOperator::GtEq),
        LexerToken::Symbol(Symbol::And) => Ok(BinaryOperator::And),
        LexerToken::Symbol(Symbol::Or) => Ok(BinaryOperator::Or),
        LexerToken::Symbol(Symbol::BitwiseAnd) => Ok(BinaryOperator::BitwiseAnd),
        LexerToken::Symbol(Symbol::BitwiseOr) => Ok(BinaryOperator::BitwiseOr),
        token => Err(format!("Expected operator, found {:?}", token).to_string()),
    }
}

fn parse_primary_expression(iterator: &mut ParserIterator) -> Result<Expression, String> {
    if let Ok(expression) = parse_literal(iterator) {
        iterator.next();
        Ok(Expression::Literal(expression))
    } else if let Ok(expression) = parse_unary_expresssion(iterator) {
        Ok(expression)
    } else if let Ok(expression) = parse_expression_identifier(iterator) {
        Ok(expression)
    } else if let Ok(expression) = parse_grouping(iterator) {
        Ok(expression)
    } else {
        Err("Invalid primary expression".to_string())
    }
}

fn parse_literal(iterator: &mut ParserIterator) -> Result<ASTLiteral, String> {
    match iterator
        .peek()
        .ok_or("Expected primary expression".to_string())?
    {
        LexerToken::Literal(literal) => match literal {
            LexerLiteral::Int(value) => Ok(ASTLiteral::Int(value)),
            LexerLiteral::Float(value) => Ok(ASTLiteral::Float(value)),
            LexerLiteral::String(value) => Ok(ASTLiteral::String(value.to_string())),
            LexerLiteral::Bool(value) => Ok(ASTLiteral::Bool(value)),
            LexerLiteral::Char(value) => Ok(ASTLiteral::Char(value)),
        },
        LexerToken::Keyword(Keyword::Type(LexerType::Null)) => Ok(ASTLiteral::Null),

        token => return Err(format!("Invalid primary expression, {:?}", token).to_string()),
    }
}

fn parse_unary_expresssion(iterator: &mut ParserIterator) -> Result<Expression, String> {
    match iterator
        .peek()
        .ok_or("Expected primary expression".to_string())?
    {
        LexerToken::Symbol(Symbol::Minus) => {
            iterator.next();
            Ok(Expression::Unary(
                UnaryOperator::Negation,
                Box::new(parse_primary_expression(iterator)?),
            ))
        }
        LexerToken::Symbol(Symbol::Bang) => {
            iterator.next();
            Ok(Expression::Unary(
                UnaryOperator::LogicalNegation,
                Box::new(parse_primary_expression(iterator)?),
            ))
        }
        LexerToken::Symbol(Symbol::BitwiseNot) => {
            iterator.next();
            Ok(Expression::Unary(
                UnaryOperator::BitwiseNot,
                Box::new(parse_primary_expression(iterator)?),
            ))
        }
        token => Err(format!("Expected unary expression, found '{:?}'", token)),
    }
}

fn parse_expression_identifier(iterator: &mut ParserIterator) -> Result<Expression, String> {
    match iterator
        .peek()
        .ok_or("Expected primary expression".to_string())?
    {
        LexerToken::Identifier(identifier) => {
            let token = iterator
                .next()
                .ok_or("Missing token at start of top level element".to_string())?;

            if token == LexerToken::Symbol(Symbol::LBracket) {
                Ok(Expression::FunctionCall(
                    identifier,
                    parse_function_call_arguements(iterator)?,
                ))
            } else {
                Ok(Expression::Variable(identifier.to_string()))
            }
        }
        token => Err(format!("Invalid expression identifier, found {:?}", token)),
    }
}

fn parse_grouping(iterator: &mut ParserIterator) -> Result<Expression, String> {
    match iterator.peek().ok_or("Expected LBracket".to_string()) {
        Ok(LexerToken::Symbol(Symbol::LBracket)) => (),
        Ok(token) => return Err(format!("Expected {:?}", token).to_string()),
        Err(_) => return Err("Expected LBracket".to_string()),
    }
    iterator.next();
    let expression = parse_expression(iterator, 0)?;
    iterator.next();
    Ok(Expression::Grouping(Box::new(expression)))
}
