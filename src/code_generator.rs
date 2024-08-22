use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Write},
};

use crate::parser::{
    ASTLiteral, ASTType, BinaryOperator, Conditional, Expression, Statement, TopLevel,
    UnaryOperator, VariableAssignment,
};

struct LocalSymbolsTable {
    local_variables: Vec<HashMap<String, String>>,
    current_index: usize,
}

impl LocalSymbolsTable {
    fn new() -> Self {
        LocalSymbolsTable {
            local_variables: vec![HashMap::new()],
            current_index: 0,
        }
    }

    fn add_env(&mut self) {
        self.local_variables.push(HashMap::new())
    }

    fn remove_env(&mut self) {
        self.local_variables.pop();
    }

    fn insert(&mut self, key: &String, value: String) -> Option<String> {
        if let Some(env) = self.local_variables.last_mut() {
            return env.insert(key.to_string(), value);
        }
        None
    }

    fn get(&self, key: &String) -> Option<String> {
        for env in &self.local_variables {
            if let Some(value) = env.get(key) {
                return Some(value.to_string());
            }
        }
        None
    }

    fn register_local_variable(&mut self, identifier: &String, size: usize) {
        self.current_index += size;
        self.insert(identifier, format!("rbp-{}", self.current_index));
    }
}

struct RegisterTracker {
    registers: Vec<String>,
    availibility: Vec<bool>,
}

impl RegisterTracker {
    fn new() -> Self {
        RegisterTracker {
            registers: vec!["r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            availibility: vec![true; 8],
        }
    }

    fn claim_register(&mut self) -> Result<usize, String> {
        for index in 0..self.registers.len() {
            if self.availibility[index] {
                self.availibility[index] = false;
                return Ok(index);
            }
        }

        Err("No availiable register".to_string())
    }

    fn release_register(&mut self, register_index: usize) {
        self.availibility[register_index] = true;
    }

    fn loopup_register(&self, register_index: usize) -> String {
        self.registers[register_index].clone()
    }
}

pub fn generate_code(ast: Vec<TopLevel>) -> Result<(), String> {
    let file = match File::create("out.asm") {
        Ok(file) => file,
        Err(_) => todo!(),
    };
    let mut file = BufWriter::new(file);

    let (global_variables, functions) = extract_top_level(ast);

    generate_preamble(&mut file, global_variables);

    generate_code_from_ast(&mut file, functions);

    Ok(())
}

fn write_vector(file: &mut BufWriter<File>, vector: Vec<&[u8]>) {
    for line in vector {
        file.write_all(line);
    }
}

fn extract_top_level(ast: Vec<TopLevel>) -> (Vec<TopLevel>, Vec<TopLevel>) {
    let mut global_variables = Vec::new();
    let mut functions = Vec::new();

    for top_level in ast {
        match top_level {
            TopLevel::Function(_, _, _, _) => functions.push(top_level),
            TopLevel::VariableDefinition(_, _, _) => global_variables.push(top_level),
        }
    }

    (global_variables, functions)
}

fn generate_preamble(file: &mut BufWriter<File>, global_variables: Vec<TopLevel>) {
    let mut data_section: Vec<&[u8]> = vec![
        "section .data\n",
        "\tint_format db \"%d\", 10, 0\n",
        "\tstring_format db \"%s\", 10, 0\n",
        "\tchar_format db \"%c\", 10, 0\n\n",
    ]
    .iter()
    .map(|s| s.as_bytes())
    .collect();

    let mut bss_section = vec!["section .bss\n".to_string()];

    let text_section: Vec<String> = vec![
        "section .text\n",
        "\tglobal _start\n",
        "\textern printf\n\n",
        "_start:\n",
        "\tcall main\n",
        "\tmov eax, 60\n",
        "\tmov edi, 0\n",
        "\tsyscall\n",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    for variable in global_variables {
        match variable {
            TopLevel::Function(_, _, _, _) => todo!("Should never be reached"),
            TopLevel::VariableDefinition(_, identifier, value) => match value {
                Some(value) => {
                    todo!("Implement defined global variables")
                    //data_section.push(&format!("\t{} dd {}\n", identifier, value).to_string())
                }
                None => bss_section.push(format!("\t{} resd 1\n", identifier).to_string()),
            },
        }
    }

    // TODO: Global defined variables

    write_vector(file, data_section);
    file.write_all("\n".as_bytes());
    write_vector(file, bss_section.iter().map(|s| s.as_bytes()).collect());
    file.write_all("\n".as_bytes());
    write_vector(file, text_section.iter().map(|s| s.as_bytes()).collect());
}

fn generate_code_from_ast(file: &mut BufWriter<File>, ast: Vec<TopLevel>) {
    let mut register_tracker = RegisterTracker::new();

    for top_level in ast {
        match top_level {
            TopLevel::Function(return_type, identifier, parameters, code_block) => {
                generate_function(
                    file,
                    &mut register_tracker,
                    &return_type,
                    &identifier,
                    &parameters,
                    &code_block,
                )
            }
            TopLevel::VariableDefinition(variable_type, identifier, value) => {
                todo!("Should never get called")
            }
        }
    }
}

fn generate_function(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    return_type: &ASTType,
    identifier: &String,
    parameters: &Option<Vec<(ASTType, String)>>,
    code_block: &Vec<Statement>,
) {
    let local_variable_size = calc_stack_size(Some(&parameters), &code_block);

    let mut symbols_table = LocalSymbolsTable::new();

    file.write_all(format!("\n{}:\n", identifier).as_bytes());

    if identifier != "main" {
        write_vector(
            file,
            vec![
                "\tpush rbp\n".as_bytes(),
                "\tpush r12\n".as_bytes(),
                "\tpush r13\n".as_bytes(),
                "\tpush r14\n".as_bytes(),
                "\tpush r15\n\n".as_bytes(),
            ],
        );
    } else {
        file.write_all("\tpush rbp\n".as_bytes());
    }

    file.write_all("\tmov rbp, rsp\n\n".as_bytes());
    if local_variable_size > 0 {
        file.write_all(format!("\tsub rsp, {}\n\n", local_variable_size).as_bytes());
    }

    // TODO: Deal with parameters - move from reg to stack
    // RDI, RSI, RDX, RCX, R8, R9, rest on stack

    generate_code_block(file, register_tracker, &mut symbols_table, code_block);

    // TODO: Deal with return

    file.write_all("\n\tmov rsp, rbp\n\n".as_bytes());
    if identifier != "main" {
        write_vector(
            file,
            vec![
                "\tpop r15\n".as_bytes(),
                "\tpop r14\n".as_bytes(),
                "\tpop r13\n".as_bytes(),
                "\tpop r12\n".as_bytes(),
                "\tpop rbp\n\n".as_bytes(),
            ],
        );
    } else {
        file.write_all("\tpop rbp\n".as_bytes());
    }

    file.write_all("\tret\n".as_bytes());
}

fn calc_stack_size(
    parameters: Option<&Option<Vec<(ASTType, String)>>>,
    code_block: &Vec<Statement>,
) -> i32 {
    let mut total = 0;

    // TODO: Introduce different sized types, everything assumed integer 4 bytes

    for statement in code_block {
        match statement {
            Statement::VariableAssignment(VariableAssignment::Definition(_, _, _)) => total += 4,
            Statement::Conditional(conditional) => match conditional {
                Conditional::IfStatement(if_statement) => {
                    for (_, code_block) in if_statement {
                        total += calc_stack_size(None, code_block)
                    }
                }
                Conditional::ForLoop(_, _, _, code_block) => {
                    total += calc_stack_size(None, code_block)
                }
                Conditional::WhileLoop(_, code_block) => total += calc_stack_size(None, code_block),
            },
            Statement::CodeBlock(code_block) => total += calc_stack_size(None, code_block),
            _ => continue,
        }
    }

    if let Some(Some(parameters)) = parameters {
        for parameter in parameters {
            total += 4;
        }
    }

    total
}

fn generate_code_block(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    code_block: &Vec<Statement>,
) {
    for statement in code_block {
        match statement {
            Statement::FunctionCall(identifier, arguements) => generate_function_call(
                file,
                register_tracker,
                symbols_table,
                identifier,
                arguements,
            ),
            Statement::VariableAssignment(variable_assignment) => {
                generate_variable_definition_assignment(
                    file,
                    register_tracker,
                    symbols_table,
                    variable_assignment,
                )
            }
            Statement::Conditional(_) => todo!("Conditional"),
            Statement::CodeBlock(code_block) => {
                symbols_table.add_env();
                generate_code_block(file, register_tracker, symbols_table, code_block);
                symbols_table.remove_env();
            }
            Statement::Return(_) => todo!("Return value"),
            Statement::Break => todo!("Break"),
            Statement::Continue => todo!("Continue"),
        }
    }
}

fn generate_function_call(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
    arguements: &Option<Vec<Expression>>,
) {
    let arguement_registers = vec!["edi", "esi", "edx", "ecx", "r8d", "r9d"];

    if identifier == "printf" {
        file.write_all("\tmov edi, int_format\n".as_bytes());
    }

    if let Some(arguements) = arguements {
        for index in 0..arguements.len() {
            file.write_all("\n".as_bytes());
            let register_index =
                generate_expression(file, register_tracker, symbols_table, &arguements[index]);
            file.write_all(
                format!(
                    "\tmov {}, {}\n",
                    arguement_registers[if identifier == "printf" {
                        index + 1
                    } else {
                        index
                    }],
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
            );
            register_tracker.release_register(register_index);
        }
    }

    // ISSUE: Problem may arise when using r8 and r9 for paramters if being used for operations
    // Potential solve: if r8 and r9 are being used move them to the stack and record that in
    // register_tracker

    write_vector(
        file,
        vec![
            "\n\tpush r10\n".as_bytes(),
            "\tpush r11\n\n".as_bytes(),
            "\tand rsp, 0xFFFFFFFFFFFFFFF0\n".as_bytes(),
            format!("\tcall {}\n\n", identifier).as_bytes(),
            "\tpop r11\n".as_bytes(),
            "\tpop r10\n".as_bytes(),
        ],
    );
}

fn generate_variable_definition_assignment(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    variable_assignment: &VariableAssignment,
) {
    match variable_assignment {
        VariableAssignment::Definition(_, identifier, initial_expression) => {
            generate_variable_definition(
                file,
                register_tracker,
                symbols_table,
                identifier,
                initial_expression,
            )
        }
        VariableAssignment::Assignment(identifier, expression) => generate_variable_assignment(
            file,
            register_tracker,
            symbols_table,
            identifier,
            expression,
        ),
    }
}

fn generate_variable_definition(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
    initial_expression: &Option<Expression>,
) {
    symbols_table.register_local_variable(&identifier, 4);

    if let Some(expression) = initial_expression {
        generate_variable_assignment(
            file,
            register_tracker,
            symbols_table,
            identifier,
            expression,
        );
    }
}

fn generate_variable_assignment(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
    expression: &Expression,
) {
    let register_index = generate_expression(file, register_tracker, symbols_table, expression);

    let pointer = match symbols_table.get(identifier) {
        Some(pointer) => pointer,
        None => todo!("Implement error handling"),
    };

    file.write_all(
        format!(
            "\tmov [{}], {}\n\n",
            pointer,
            register_tracker.loopup_register(register_index)
        )
        .as_bytes(),
    );
    register_tracker.release_register(register_index);
}

fn generate_expression(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    expression: &Expression,
) -> usize {
    match expression {
        Expression::Unary(operator, left) => {
            generate_unary_expression(file, register_tracker, symbols_table, operator, left)
        }
        Expression::Binary(left, operator, right) => {
            generate_binary_expression(file, register_tracker, symbols_table, left, operator, right)
        }
        Expression::Literal(literal) => {
            generate_literal(file, register_tracker, symbols_table, literal)
        }
        Expression::Variable(identifier) => {
            let register_index = match register_tracker.claim_register() {
                Ok(register_index) => register_index,
                Err(_) => todo!("Add error handling"),
            };

            let pointer = match symbols_table.get(identifier) {
                Some(pointer) => pointer,
                None => todo!("Add error handling"),
            };

            file.write_all(
                format!(
                    "\tmov {}, [{}]\n",
                    register_tracker.loopup_register(register_index),
                    pointer,
                )
                .as_bytes(),
            );
            return register_index;
        }
        Expression::FunctionCall(identifier, arguements) => {
            generate_function_call(
                file,
                register_tracker,
                symbols_table,
                identifier,
                arguements,
            );

            let register_index = match register_tracker.claim_register() {
                Ok(register_index) => register_index,
                Err(_) => todo!("Implement error handling"),
            };

            file.write_all(
                format!(
                    "\nmov {}, rax",
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
            );
            register_index
        }
        Expression::Grouping(expression) => {
            generate_expression(file, register_tracker, symbols_table, expression)
        }
    }
}

fn generate_unary_expression(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    operator: &UnaryOperator,
    left: &Expression,
) -> usize {
    let register_index = generate_expression(file, register_tracker, symbols_table, left);
    match operator {
        UnaryOperator::Negation => {
            file.write_all(
                format!(
                    "\tneg {}\n",
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
            );
        }
        UnaryOperator::LogicalNegation => write_vector(
            file,
            vec![
                format!(
                    "\ttest {}, {}\n",
                    register_tracker.loopup_register(register_index),
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
                "\tsetz al\n".as_bytes(),
                format!(
                    "\tmovzx {}, al\n",
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
            ],
        ),
        UnaryOperator::BitwiseNot => {
            file.write_all(
                format!(
                    "\tnot {}\n",
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
            );
        }
    }
    register_index
}

fn generate_binary_expression(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    left: &Expression,
    operator: &BinaryOperator,
    right: &Expression,
) -> usize {
    let left_index = generate_expression(file, register_tracker, symbols_table, left);
    let right_index = generate_expression(file, register_tracker, symbols_table, right);

    match operator {
        BinaryOperator::Add => {
            file.write_all(
                format!(
                    "\tadd {}, {}\n",
                    register_tracker.loopup_register(left_index),
                    register_tracker.loopup_register(right_index)
                )
                .as_bytes(),
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Sub => {
            file.write_all(
                format!(
                    "\tsub {}, {}\n",
                    register_tracker.loopup_register(left_index),
                    register_tracker.loopup_register(right_index)
                )
                .as_bytes(),
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Mul => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tmov eax, {}\n",
                        register_tracker.loopup_register(left_index)
                    )
                    .as_bytes(),
                    "\txor edx, edx\n".as_bytes(),
                    format!("\tmul {}\n", register_tracker.loopup_register(right_index)).as_bytes(),
                    format!(
                        "\tmov {}, eax\n",
                        register_tracker.loopup_register(left_index)
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Div => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tmov eax, {}\n",
                        register_tracker.loopup_register(left_index)
                    )
                    .as_bytes(),
                    "\txor edx, edx\n".as_bytes(),
                    format!("\tdiv {}\n", register_tracker.loopup_register(right_index)).as_bytes(),
                    format!(
                        "\tmov {}, eax\n",
                        register_tracker.loopup_register(left_index)
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Eq => todo!(),
        BinaryOperator::NEq => todo!(),
        BinaryOperator::Gt => todo!(),
        BinaryOperator::Lt => todo!(),
        BinaryOperator::GtEq => todo!(),
        BinaryOperator::LtEq => todo!(),
        BinaryOperator::And => todo!(),
        BinaryOperator::Or => todo!(),
        BinaryOperator::BitwiseAnd => todo!(),
        BinaryOperator::BitwiseOr => todo!(),
    }
}

fn generate_literal(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    symbols_table: &mut LocalSymbolsTable,
    literal: &ASTLiteral,
) -> usize {
    match literal {
        ASTLiteral::Int(value) => generate_load_int_literal(file, register_tracker, *value),
        _ => todo!("Implement other literals"),
    }
}

fn generate_load_int_literal(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    value: i32,
) -> usize {
    let register_index = match register_tracker.claim_register() {
        Ok(register_index) => register_index,
        Err(_) => todo!("Implement error handling"),
    };

    file.write_all(
        format!(
            "\tmov {}, {}\n",
            register_tracker.loopup_register(register_index),
            value.to_string()
        )
        .as_bytes(),
    );

    register_index
}
