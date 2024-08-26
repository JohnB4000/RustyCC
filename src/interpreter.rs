use std::collections::HashMap;

use crate::parser::{
    ASTLiteral, BinaryOperator, Conditional, Expression, Statement, TopLevel, UnaryOperator,
    VariableAssignment,
};

#[derive(Debug, Clone)]
enum EvaluatedExpression {
    Int(i32),
    Bool(bool),
    String(String),
    Float(f32),
    Char(char),
    DefaultValue,
    Null,
    ReturnValue(Box<Self>),
    Break,
    Continue,
}

impl EvaluatedExpression {
    fn negate(self) -> Result<Self, String> {
        match self {
            Self::Int(value) => Ok(Self::Int(-value)),
            Self::Float(value) => Ok(Self::Float(-value)),
            _ => Err("Cannot negate expression".to_string()),
        }
    }

    fn logical_negate(&self) -> Result<Self, String> {
        match self {
            Self::Bool(value) => Ok(Self::Bool(!value)),
            Self::Int(value) => Ok(Self::Bool(!(*value > 0))),
            _ => Err("Cannot logically negate expression".to_string()),
        }
    }

    fn bitwise_not(&self) -> Result<EvaluatedExpression, String> {
        match self {
            Self::Int(left) => Ok(Self::Int(!left)),
            _ => Err("Cannot bitwise not expression".to_string()),
        }
    }

    fn add(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left + right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Float(*left as f32 + right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Float(left + right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Float(left + right)),
            _ => Err("Cannot add expression".to_string()),
        }
    }

    fn sub(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left - right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Float(*left as f32 - right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Float(left - right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Float(left - right)),
            _ => Err("Cannot subtract expression".to_string()),
        }
    }

    fn mul(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left * right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Float(*left as f32 * right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Float(left * right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Float(left * right)),
            _ => Err("Cannot multiply expression".to_string()),
        }
    }

    fn div(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) if right != 0 => Ok(Self::Int(left / right)),
            (Self::Int(left), Self::Float(right)) if right != 0.0 => {
                Ok(Self::Float(*left as f32 * right))
            }
            (Self::Float(left), Self::Int(right)) if right != 0 => {
                Ok(Self::Float(left / right as f32))
            }
            (Self::Float(left), Self::Float(right)) if right != 0.0 => {
                Ok(Self::Float(left / right))
            }
            _ => Err("Cannot divide expression".to_string()),
        }
    }

    fn eq(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left == right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Bool(*left as f32 == right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Bool(*left == right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Bool(*left == right)),
            (Self::Char(left), Self::Char(right)) => Ok(Self::Bool(*left == right)),
            _ => Err("Cannot apply equals to expressions".to_string()),
        }
    }

    fn neq(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left != right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Bool(*left as f32 != right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Bool(*left != right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Bool(*left != right)),
            (Self::Char(left), Self::Char(right)) => Ok(Self::Bool(*left != right)),
            _ => Err("Cannot apply not equal to expressions".to_string()),
        }
    }

    fn gt(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left > right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Bool(*left as f32 > right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Bool(*left > right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Bool(*left > right)),
            (Self::Char(left), Self::Char(right)) => Ok(Self::Bool(*left > right)),
            _ => Err("Cannot apply greater than to expression".to_string()),
        }
    }

    fn lt(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left < right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Bool((*left as f32) < right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Bool(*left < right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Bool(*left < right)),
            (Self::Char(left), Self::Char(right)) => Ok(Self::Bool(*left < right)),
            _ => Err("Cannot apply less than to expression".to_string()),
        }
    }

    fn gteq(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left >= right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Bool((*left as f32) >= right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Bool(*left >= right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Bool(*left >= right)),
            (Self::Char(left), Self::Char(right)) => Ok(Self::Bool(*left >= right)),
            _ => Err("Cannot apply greater than or equal to expressions".to_string()),
        }
    }

    fn lteq(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left <= right)),
            (Self::Int(left), Self::Float(right)) => Ok(Self::Bool((*left as f32) <= right)),
            (Self::Float(left), Self::Int(right)) => Ok(Self::Bool(*left <= right as f32)),
            (Self::Float(left), Self::Float(right)) => Ok(Self::Bool(*left <= right)),
            (Self::Char(left), Self::Char(right)) => Ok(Self::Bool(*left <= right)),
            _ => Err("Cannot apply less than or equal to expressions".to_string()),
        }
    }

    fn and(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(*left && right)),
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left > 0 && right > 0)),
            _ => Err("Cannot logically negate expression".to_string()),
        }
    }

    fn or(&self, right: Self) -> Result<Self, String> {
        match (self, right) {
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(*left || right)),
            (Self::Int(left), Self::Int(right)) => Ok(Self::Bool(*left > 0 || right > 0)),
            _ => Err("Cannot logically negate expression".to_string()),
        }
    }

    fn bitwise_and(&self, right: EvaluatedExpression) -> Result<EvaluatedExpression, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left & right)),
            _ => Err("Cannot bitwise and expression".to_string()),
        }
    }

    fn bitwise_or(&self, right: EvaluatedExpression) -> Result<EvaluatedExpression, String> {
        match (self, right) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left | right)),
            _ => Err("Cannot bitwise or expression".to_string()),
        }
    }
}

#[derive(Debug)]
struct FuncEnv {
    env: HashMap<String, (Option<Vec<String>>, Vec<Statement>)>,
}

impl FuncEnv {
    fn new() -> Self {
        FuncEnv {
            env: HashMap::new(),
        }
    }

    fn register_function(
        &mut self,
        identifier: String,
        parameters: Option<Vec<String>>,
        code_block: Vec<Statement>,
    ) {
        self.env.insert(identifier, (parameters, code_block));
    }

    fn evaluate_function_call(
        &mut self,
        identifier: &String,
        arguements: &Option<Vec<Expression>>,
        variable_env: &mut VarEnv,
    ) -> Result<EvaluatedExpression, String> {
        let evaluated_arguements = match arguements {
            Some(arguements) => Some(
                arguements
                    .iter()
                    .map(|expression| evaluate_expression(expression, variable_env, self))
                    .collect::<Result<Vec<EvaluatedExpression>, String>>()?,
            ),
            None => None,
        };

        let (parameters, code_block) = match self.env.get(identifier) {
            Some(function) => function,
            None => return Err(format!("Function '{:?}' not defined", identifier)),
        };

        let mut variable_env = VarEnv::new();
        variable_env.add_env();

        match (parameters, evaluated_arguements) {
            (Some(parameters), Some(arguements)) => {
                for (parameter, arguement) in parameters.iter().zip(arguements.iter()) {
                    if let EvaluatedExpression::DefaultValue = arguement {
                        return Err("Failure while trying to interpret arguements".to_string());
                    }
                    variable_env.define_variable(parameter, Some(arguement.clone()))?;
                }
            }
            _ => (),
        }

        let return_value = evaluate_code_block(&code_block.to_vec(), &mut variable_env, self)?;

        variable_env.remove_env();

        match return_value {
            EvaluatedExpression::ReturnValue(return_value) => Ok(*return_value),
            EvaluatedExpression::Break => return Err("Invalid break statement in".to_string()),
            EvaluatedExpression::Continue => {
                return Err("Invalid continue statement in".to_string())
            }
            _ => Ok(EvaluatedExpression::Null),
        }
    }
}

#[derive(Debug)]
struct VarEnv {
    env: Vec<HashMap<String, EvaluatedExpression>>,
}

impl VarEnv {
    fn new() -> Self {
        VarEnv { env: Vec::new() }
    }

    fn add_env(&mut self) {
        self.env.push(HashMap::new());
    }

    fn remove_env(&mut self) {
        self.env.pop();
    }

    fn define_variable(
        &mut self,
        identifier: &String,
        inital_value: Option<EvaluatedExpression>,
    ) -> Result<EvaluatedExpression, String> {
        let variable_values = match self.env.last_mut() {
            Some(env) => env,
            None => return Err("No envs defined".to_string()),
        };

        if variable_values.contains_key(identifier) {
            return Err(format!("Variable already defined: {:?}", identifier));
        }

        variable_values.insert(
            identifier.to_string(),
            inital_value.unwrap_or(EvaluatedExpression::DefaultValue),
        );

        Ok(EvaluatedExpression::Null)
    }

    fn assign_variable(
        &mut self,
        identifier: &String,
        value: EvaluatedExpression,
    ) -> Result<EvaluatedExpression, String> {
        for env in self.env.iter_mut() {
            if env.contains_key(identifier) {
                env.insert(identifier.to_string(), value);
                return Ok(EvaluatedExpression::Null);
            }
        }
        Err(format!("Variable {:?} doesn't exist", identifier))
    }

    fn get_variable(&mut self, identifier: &String) -> Result<EvaluatedExpression, String> {
        for env in &self.env {
            if let Some(value) = env.get(identifier) {
                return Ok(value.to_owned());
            }
        }
        Err(format!("Variable {:?} doesn't exist", identifier))
    }
}

pub fn interpret(ast: Vec<TopLevel>) -> Result<i32, String> {
    let mut variable_env = VarEnv::new();
    variable_env.add_env();
    let mut function_env = FuncEnv::new();

    for top_level in ast {
        match top_level {
            TopLevel::Function(_, identifier, parameters, code_block) => {
                let parameters = match parameters {
                    Some(parameters) => Some(
                        parameters
                            .iter()
                            .map(|(_, identifier)| identifier.to_string())
                            .collect(),
                    ),
                    None => None,
                };
                function_env.register_function(identifier, parameters, code_block)
            }
            TopLevel::VariableDefinition(_, identifier, expression) => {
                let expression = match expression {
                    Some(expression) => {
                        match evaluate_expression(&expression, &mut variable_env, &mut function_env)
                        {
                            Ok(expression) => Some(expression),
                            Err(error) => return Err(error),
                        }
                    }
                    None => None,
                };
                variable_env.define_variable(&identifier, expression)?;
            }
        }
    }

    match function_env.evaluate_function_call(&"main".to_string(), &None, &mut variable_env) {
        Ok(EvaluatedExpression::Int(exit_code)) if exit_code >= 0 => Ok(exit_code),
        Err(error) => Err(error),
        _ => Ok(0),
    }
}

fn evaluate_code_block(
    code_block: &Vec<Statement>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    variable_env.add_env();

    for statement in code_block {
        let return_value = match statement {
            Statement::FunctionCall(identifier, arguements) => {
                evaluate_function_call(identifier, arguements, variable_env, function_env)?
            }
            Statement::VariableAssignment(variable_assignment) => {
                evaluate_variable_assignment(&variable_assignment, variable_env, function_env)?
            }
            Statement::Conditional(conditional) => {
                evaluate_conditional(&conditional, variable_env, function_env)?
            }
            Statement::CodeBlock(code_block) => {
                return evaluate_code_block(code_block, variable_env, function_env);
            }
            Statement::Return(return_value) => EvaluatedExpression::ReturnValue(Box::new(
                evaluate_expression(return_value, variable_env, function_env)?,
            )),
            Statement::Break => return Ok(EvaluatedExpression::Break),
            Statement::Continue => return Ok(EvaluatedExpression::Continue),
        };
        match return_value {
            EvaluatedExpression::ReturnValue(_)
            | EvaluatedExpression::Break
            | EvaluatedExpression::Continue => {
                variable_env.remove_env();
                return Ok(return_value);
            }
            _ => (),
        }
    }

    variable_env.remove_env();
    Ok(EvaluatedExpression::Null)
}

fn evaluate_function_call(
    identifier: &String,
    arguements: &Option<Vec<Expression>>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    match identifier.as_str() {
        "printf" => evaluate_print_statement(arguements, variable_env, function_env),
        _ => function_env.evaluate_function_call(identifier, arguements, variable_env),
    }
}

fn evaluate_print_statement(
    arguements: &Option<Vec<Expression>>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    let evaluated_arguements = match arguements {
        Some(arguements) => arguements
            .iter()
            .map(|expression| evaluate_expression(expression, variable_env, function_env))
            .collect::<Result<Vec<EvaluatedExpression>, String>>(),
        None => return Ok(EvaluatedExpression::Null),
    }?;

    if evaluated_arguements.len() == 0 {
        return Ok(EvaluatedExpression::Null);
    }

    match evaluated_arguements.first() {
        Some(EvaluatedExpression::Int(value)) => print!("{}", value),
        Some(EvaluatedExpression::Bool(value)) => print!("{}", value),
        Some(EvaluatedExpression::String(value)) => print!("{}", value),
        Some(EvaluatedExpression::Float(value)) => print!("{}", value),
        Some(EvaluatedExpression::Char(value)) => print!("{}", value),
        _ => (),
    }

    for index in 1..evaluated_arguements.len() {
        match evaluated_arguements.get(index) {
            Some(EvaluatedExpression::Int(value)) => print!(", {}", value),
            Some(EvaluatedExpression::Bool(value)) => print!(", {}", value),
            Some(EvaluatedExpression::String(value)) => print!(", {}", value),
            Some(EvaluatedExpression::Float(value)) => print!(", {}", value),
            Some(EvaluatedExpression::Char(value)) => print!(", {}", value),
            _ => (),
        }
    }

    println!();

    Ok(EvaluatedExpression::Null)
}

fn evaluate_variable_assignment(
    variable_assignment: &VariableAssignment,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    match variable_assignment {
        VariableAssignment::Definition(_, identifier, expression) => {
            match expression
                .as_ref()
                .map(|expression| evaluate_expression(&expression, variable_env, function_env))
                .transpose()
            {
                Ok(expression) => variable_env.define_variable(identifier, expression),
                Err(error) => Err(error),
            }
        }

        VariableAssignment::Assignment(identifier, expression) => {
            let expression = match evaluate_expression(expression, variable_env, function_env) {
                Ok(expression) => expression,
                Err(error) => return Err(error),
            };
            variable_env.assign_variable(identifier, expression)
        }
    }
}

fn evaluate_conditional(
    conditional: &Conditional,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    match conditional {
        Conditional::IfStatement(if_statement) => {
            evaluate_if_statement(if_statement, variable_env, function_env)
        }
        Conditional::ForLoop(
            initial_statement,
            exit_condition,
            continuous_expression,
            code_block,
        ) => evaluate_for_loop(
            initial_statement,
            exit_condition,
            continuous_expression,
            code_block,
            variable_env,
            function_env,
        ),
        Conditional::WhileLoop(condition, code_block) => {
            evaluate_while_loop(condition, code_block, variable_env, function_env)
        }
    }
}

fn evaluate_if_statement(
    if_statement: &Vec<(Option<Expression>, Vec<Statement>)>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    for (condition, code_block) in if_statement {
        if let Some(condition) = condition {
            match evaluate_expression(condition, variable_env, function_env)? {
                EvaluatedExpression::Bool(true) => {
                    return evaluate_code_block(code_block, variable_env, function_env)
                }
                EvaluatedExpression::Int(value) if value > 0 => {
                    return evaluate_code_block(code_block, variable_env, function_env)
                }
                EvaluatedExpression::Bool(false) => continue,
                EvaluatedExpression::Int(value) if value <= 0 => {
                    return evaluate_code_block(code_block, variable_env, function_env)
                }
                _ => return Err("Invalid condition".to_string()),
            }
        }
        return evaluate_code_block(code_block, variable_env, function_env);
    }

    Ok(EvaluatedExpression::Null)
}

fn evaluate_for_loop(
    initial_statement: &Option<VariableAssignment>,
    exit_condition: &Option<Expression>,
    continuous_expression: &Option<VariableAssignment>,
    code_block: &Vec<Statement>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    if let Some(statement) = initial_statement {
        evaluate_variable_assignment(statement, variable_env, function_env)?;
    }

    loop {
        if let Some(condition) = exit_condition {
            match evaluate_expression(condition, variable_env, function_env)? {
                EvaluatedExpression::Bool(true) => (),
                EvaluatedExpression::Bool(false) => return Ok(EvaluatedExpression::Null),
                EvaluatedExpression::Int(value) if value > 0 => (),
                EvaluatedExpression::Int(value) if value <= 0 => {
                    return Ok(EvaluatedExpression::Null)
                }
                _ => return Err("Invalid exit condition".to_string()),
            }
        }

        match evaluate_code_block(code_block, variable_env, function_env)? {
            EvaluatedExpression::ReturnValue(value) => {
                return Ok(EvaluatedExpression::ReturnValue(value))
            }
            EvaluatedExpression::Break => return Ok(EvaluatedExpression::Null),
            EvaluatedExpression::Continue => (),
            _ => (),
        }

        if let Some(statement) = continuous_expression {
            evaluate_variable_assignment(statement, variable_env, function_env)?;
        }
    }
}

fn evaluate_while_loop(
    condition: &Expression,
    code_block: &Vec<Statement>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    loop {
        match evaluate_expression(condition, variable_env, function_env)? {
            EvaluatedExpression::Bool(true) => (),
            EvaluatedExpression::Bool(false) => return Ok(EvaluatedExpression::Null),
            EvaluatedExpression::Int(value) if value > 0 => (),
            EvaluatedExpression::Int(value) if value <= 0 => return Ok(EvaluatedExpression::Null),

            _ => return Err("Invalid exit condition".to_string()),
        }

        match evaluate_code_block(code_block, variable_env, function_env) {
            Ok(EvaluatedExpression::ReturnValue(return_value)) => {
                return Ok(EvaluatedExpression::ReturnValue(return_value));
            }
            Ok(EvaluatedExpression::Break) => return Ok(EvaluatedExpression::Null),
            Ok(EvaluatedExpression::Continue) => (),
            Err(error) => return Err(error),
            _ => (),
        }
    }
}

fn evaluate_expression(
    expression: &Expression,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    match expression {
        Expression::Unary(operator, right) => {
            evaluate_unary_expression(operator, right, variable_env, function_env)
        }
        Expression::Binary(left, operator, right) => {
            evaluate_binary_expression(left, operator, right, variable_env, function_env)
        }
        Expression::Literal(literal) => Ok(evaluate_literal(literal)),
        Expression::Variable(identifier) => variable_env.get_variable(identifier),
        Expression::FunctionCall(identifier, arguements) => {
            evaluate_function_call(identifier, arguements, variable_env, function_env)
        }
        Expression::Grouping(group) => evaluate_expression(&*group, variable_env, function_env),
    }
}

fn evaluate_unary_expression(
    operator: &UnaryOperator,
    right: &Box<Expression>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    let expression = evaluate_expression(right, variable_env, function_env)?;
    match operator {
        UnaryOperator::Negation => expression.negate(),
        UnaryOperator::LogicalNegation => expression.logical_negate(),
        UnaryOperator::BitwiseNot => expression.bitwise_not(),
    }
}

fn evaluate_binary_expression(
    left: &Box<Expression>,
    operator: &BinaryOperator,
    right: &Box<Expression>,
    variable_env: &mut VarEnv,
    function_env: &mut FuncEnv,
) -> Result<EvaluatedExpression, String> {
    let left = evaluate_expression(left, variable_env, function_env)?;
    let right = evaluate_expression(right, variable_env, function_env)?;

    match operator {
        BinaryOperator::Add => left.add(right),
        BinaryOperator::Sub => left.sub(right),
        BinaryOperator::Mul => left.mul(right),
        BinaryOperator::Div => left.div(right),
        BinaryOperator::Eq => left.eq(right),
        BinaryOperator::NEq => left.neq(right),
        BinaryOperator::Gt => left.gt(right),
        BinaryOperator::Lt => left.lt(right),
        BinaryOperator::GtEq => left.gteq(right),
        BinaryOperator::LtEq => left.lteq(right),
        BinaryOperator::And => left.and(right),
        BinaryOperator::Or => left.or(right),
        BinaryOperator::BitwiseAnd => left.bitwise_and(right),
        BinaryOperator::BitwiseOr => left.bitwise_or(right),
    }
}

fn evaluate_literal(literal: &ASTLiteral) -> EvaluatedExpression {
    match literal {
        ASTLiteral::String(value) => EvaluatedExpression::String(value.to_string()),
        ASTLiteral::Int(value) => EvaluatedExpression::Int(*value),
        ASTLiteral::Float(value) => EvaluatedExpression::Float(*value),
        ASTLiteral::Bool(value) => EvaluatedExpression::Bool(*value),
        ASTLiteral::Char(value) => EvaluatedExpression::Char(*value),
        ASTLiteral::Null => EvaluatedExpression::Null,
    }
}
