use std::{
    char,
    collections::HashMap,
    fs::File,
    io::{BufWriter, Write},
};

use crate::{
    interpreter::{evaluate_expression, FuncEnv, VarEnv},
    parser::{
        ASTLiteral, ASTType, BinaryOperator, Conditional, Expression, Statement, TopLevel,
        UnaryOperator, VariableAssignment,
    },
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

    fn decrease_pointer(&mut self, amount: usize) {
        self.current_index += amount
    }

    fn increase_pointer(&mut self, amount: usize) {
        self.current_index -= amount;
    }

    fn get_pointer(&self) -> usize {
        self.current_index
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

struct LabelTracker {
    current_label: Vec<u32>,
}

impl LabelTracker {
    fn new() -> Self {
        let mut label_tracker = LabelTracker {
            current_label: Vec::new(),
        };

        label_tracker
    }

    fn add_label(&mut self) -> String {
        for index in 0..self.current_label.len() {
            self.current_label[index] += 1;
            if self.current_label[index] <= 122 {
                return self.get_current_label();
            }
            self.current_label[index] = 65
        }
        self.current_label.push(65);
        self.get_current_label()
    }

    fn get_current_label(&self) -> String {
        self.current_label
            .iter()
            .map(|i| char::from_u32(i.clone()).unwrap())
            .collect::<Vec<char>>()
            .iter()
            .collect()
    }
}

pub fn generate_code(ast: Vec<TopLevel>) -> Result<(), String> {
    let file = match File::create("out.asm") {
        Ok(file) => file,
        Err(_) => todo!(),
    };
    let mut file = BufWriter::new(file);

    let (global_variables, functions) = extract_top_level(ast);

    let mut global_variables = generate_preamble(&mut file, global_variables);

    generate_code_from_ast(&mut file, &mut global_variables, functions);

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

fn generate_preamble(file: &mut BufWriter<File>, global_variables: Vec<TopLevel>) -> Vec<String> {
    let mut data_section: Vec<String> = vec![
        "section .data\n",
        "\tint_format db \"%d\", 10, 0\n",
        "\tstring_format db \"%s\", 10, 0\n",
        "\tchar_format db \"%c\", 10, 0\n\n",
    ]
    .iter()
    .map(|s| s.to_string())
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

    let mut global_variables_list = Vec::new();

    let mut variable_env = VarEnv::new();
    variable_env.add_env();
    let mut function_env = FuncEnv::new();

    for variable in global_variables {
        match variable {
            TopLevel::Function(_, _, _, _) => todo!("Should never be reached"),
            TopLevel::VariableDefinition(_, identifier, value) => {
                match value {
                    Some(expression) => {
                        let final_value = match evaluate_expression(
                            &expression,
                            &mut variable_env,
                            &mut function_env,
                        ) {
                            Ok(value) => value,
                            Err(error) => {
                                println!("Add error handling {:?}", error);
                                todo!();
                            }
                        };
                        variable_env.define_variable(&identifier, Some(final_value));

                        let final_value = 2;
                        data_section
                            .push(format!("\t{} dd {}\n", identifier, final_value).to_string())
                    }
                    None => bss_section.push(format!("\t{} resd 1\n", identifier).to_string()),
                };
                global_variables_list.push(identifier);
            }
        }
    }

    // TODO: Global defined variables

    write_vector(file, data_section.iter().map(|s| s.as_bytes()).collect());
    file.write_all("\n".as_bytes());
    write_vector(file, bss_section.iter().map(|s| s.as_bytes()).collect());
    file.write_all("\n".as_bytes());
    write_vector(file, text_section.iter().map(|s| s.as_bytes()).collect());

    global_variables_list
}

fn generate_code_from_ast(
    file: &mut BufWriter<File>,
    global_variables: &mut Vec<String>,
    ast: Vec<TopLevel>,
) {
    let mut register_tracker = RegisterTracker::new();
    let mut label_tracker = LabelTracker::new();

    for top_level in ast {
        match top_level {
            TopLevel::Function(return_type, identifier, parameters, code_block) => {
                generate_function(
                    file,
                    &mut register_tracker,
                    &mut label_tracker,
                    global_variables,
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
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
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

    let parameter_registers = vec!["edi", "esi", "edx", "ecx", "r8d", "r9d"];
    if let Some(parameters) = parameters {
        for index in 0..parameters.len() {
            let (_, identifier) = &parameters[index];
            symbols_table.register_local_variable(identifier, 4);

            let pointer = match symbols_table.get(identifier) {
                Some(pointer) => pointer,
                None => todo!("Implement error handling"),
            };

            file.write_all(
                format!("\tmov [{}], {}\n\n", pointer, parameter_registers[index]).as_bytes(),
            );
        }
    }

    generate_code_block(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        &mut symbols_table,
        code_block,
    );

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
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    code_block: &Vec<Statement>,
) {
    for statement in code_block {
        match statement {
            Statement::FunctionCall(identifier, arguements) => generate_function_call(
                file,
                register_tracker,
                label_tracker,
                global_variables,
                symbols_table,
                identifier,
                arguements,
            ),
            Statement::VariableAssignment(variable_assignment) => {
                generate_variable_definition_assignment(
                    file,
                    register_tracker,
                    label_tracker,
                    global_variables,
                    symbols_table,
                    variable_assignment,
                )
            }
            Statement::Conditional(conditional) => generate_conditional(
                file,
                register_tracker,
                label_tracker,
                global_variables,
                symbols_table,
                conditional,
            ),
            Statement::CodeBlock(code_block) => {
                symbols_table.add_env();
                generate_code_block(
                    file,
                    register_tracker,
                    label_tracker,
                    global_variables,
                    symbols_table,
                    code_block,
                );
                symbols_table.remove_env();
            }
            Statement::Return(expression) => {
                let register_index = generate_expression(
                    file,
                    register_tracker,
                    label_tracker,
                    global_variables,
                    symbols_table,
                    expression,
                );
                write_vector(
                    file,
                    vec![
                        format!(
                            "\tmov eax, {}\n",
                            register_tracker.loopup_register(register_index)
                        )
                        .as_bytes(),
                        "\n\tmov rsp, rbp\n\n".as_bytes(),
                        "\tpop r15\n".as_bytes(),
                        "\tpop r14\n".as_bytes(),
                        "\tpop r13\n".as_bytes(),
                        "\tpop r12\n".as_bytes(),
                        "\tpop rbp\n\n".as_bytes(),
                        "\tret\n".as_bytes(),
                    ],
                );

                // ISSUE: If in main its the wrong return boilerplate no need to pop them off
                //
                // ISSUE: Will double generate the return boilerplate
            }
            Statement::Break => todo!("Break"),
            Statement::Continue => todo!("Continue"),
        }
    }
}

fn generate_function_call(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
    arguements: &Option<Vec<Expression>>,
) {
    let arguement_registers = vec!["edi", "esi", "edx", "ecx", "r8d", "r9d"];
    let mut arguement_pointers = Vec::new();
    let mut parameter_size = 0;

    if let Some(arguements) = arguements {
        for index in 0..arguements.len() {
            let register_index = generate_expression(
                file,
                register_tracker,
                label_tracker,
                global_variables,
                symbols_table,
                &arguements[index],
            );
            symbols_table.decrease_pointer(4);
            parameter_size += 4;
            write_vector(
                file,
                vec![
                    "\n\tsub rsp, 4\n".as_bytes(),
                    format!(
                        "\tmov [rbp-{}], {}\n\n",
                        symbols_table.get_pointer(),
                        register_tracker.loopup_register(register_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(register_index);
            arguement_pointers.push(symbols_table.get_pointer());
        }

        if identifier == "printf" {
            file.write_all("\tmov edi, int_format\n".as_bytes());
        }
        for index in 0..arguements.len() {
            file.write_all(
                format!(
                    "\tmov {}, [rbp-{}]\n",
                    arguement_registers[if identifier == "printf" {
                        arguements.len() - index - 1 + 1
                    } else {
                        arguements.len() - index - 1
                    }],
                    arguement_pointers[arguements.len() - index - 1]
                )
                .as_bytes(),
            );
        }
    }

    symbols_table.increase_pointer(parameter_size);
    file.write_all(format!("\tadd rsp, {}\n", parameter_size).as_bytes());

    // ISSUE: Problem may arise when using r8 and r9 for paramters if being used for operations
    // Potential solve: if r8 and r9 are being used move them to the stack and record that in
    // register_tracker

    write_vector(
        file,
        vec![
            "\tsub rsp, 1\n\n".as_bytes(),
            "\tand rsp, 0xFFFFFFFFFFFFFFF0\n".as_bytes(),
            format!("\tcall {}\n\n", identifier).as_bytes(),
            "\tadd rsp, 1\n".as_bytes(),
        ],
    );
}

fn generate_variable_definition_assignment(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    variable_assignment: &VariableAssignment,
) {
    match variable_assignment {
        VariableAssignment::Definition(_, identifier, initial_expression) => {
            generate_variable_definition(
                file,
                register_tracker,
                label_tracker,
                global_variables,
                symbols_table,
                identifier,
                initial_expression,
            )
        }
        VariableAssignment::Assignment(identifier, expression) => generate_variable_assignment(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            identifier,
            expression,
        ),
    }
}

fn generate_variable_definition(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
    initial_expression: &Option<Expression>,
) {
    symbols_table.register_local_variable(&identifier, 4);

    if let Some(expression) = initial_expression {
        generate_variable_assignment(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            identifier,
            expression,
        );
    }
}

fn generate_variable_assignment(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
    expression: &Expression,
) {
    let register_index = generate_expression(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        expression,
    );

    let pointer = match symbols_table.get(identifier) {
        Some(pointer) => pointer,
        None => {
            if global_variables.contains(identifier) {
                identifier.to_string()
            } else {
                todo!("Add error handling")
            }
        }
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

fn generate_conditional(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    conditional: &Conditional,
) {
    match conditional {
        Conditional::IfStatement(if_statement) => generate_if_statement(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            if_statement,
        ),
        Conditional::ForLoop(
            initial_statement,
            exit_condition,
            continuous_expression,
            code_block,
        ) => generate_for_loop(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            initial_statement,
            exit_condition,
            continuous_expression,
            code_block,
        ),
        Conditional::WhileLoop(condition, code_block) => generate_while_loop(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            condition,
            code_block,
        ),
    }
}

fn generate_if_statement(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    if_statement: &Vec<(Option<Expression>, Vec<Statement>)>,
) {
    let mut labels = Vec::new();
    for index in 0..if_statement.len() {
        labels.push(label_tracker.add_label());
    }
    labels.push(label_tracker.add_label());

    for index in 0..if_statement.len() {
        let (expression, code_block) = &if_statement[index];
        file.write_all(format!(".{}:\n", labels[index]).as_bytes());

        if let Some(expression) = expression {
            let register_index = generate_expression(
                file,
                register_tracker,
                label_tracker,
                global_variables,
                symbols_table,
                expression,
            );

            write_vector(
                file,
                vec![
                    format!(
                        "\n\tcmp {}, 0\n",
                        register_tracker.loopup_register(register_index)
                    )
                    .as_bytes(),
                    format!("\tjle .{}\n\n", labels[index + 1]).as_bytes(),
                ],
            );
            register_tracker.release_register(register_index);
        }
        generate_code_block(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            code_block,
        );

        file.write_all(format!("\tjmp .{}\n", labels.last().unwrap()).as_bytes());
    }
    file.write_all(format!(".{}:\n", labels.last().unwrap()).as_bytes());
}

fn generate_for_loop(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    initial_statement: &Option<VariableAssignment>,
    exit_condition: &Option<Expression>,
    continuous_expression: &Option<VariableAssignment>,
    code_block: &Vec<Statement>,
) {
    let start_label = label_tracker.add_label();
    let end_label = label_tracker.add_label();

    if let Some(initial_statement) = initial_statement {
        generate_variable_definition_assignment(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            initial_statement,
        );
    }

    file.write_all(format!(".{}:\n", start_label).as_bytes());

    if let Some(exit_condition) = exit_condition {
        let register_index = generate_expression(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            exit_condition,
        );

        write_vector(
            file,
            vec![
                format!(
                    "\n\tcmp {}, 0\n",
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
                format!("\tjle .{}\n\n", end_label).as_bytes(),
            ],
        );
        register_tracker.release_register(register_index);
    }

    generate_code_block(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        code_block,
    );

    if let Some(continuous_expression) = continuous_expression {
        generate_variable_definition_assignment(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            continuous_expression,
        );
    }

    write_vector(
        file,
        vec![
            format!("\tjmp .{}\n", start_label).as_bytes(),
            format!(".{}:\n", end_label).as_bytes(),
        ],
    );
}

fn generate_while_loop(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    condition: &Expression,
    code_block: &Vec<Statement>,
) {
    let start_label = label_tracker.add_label();
    let end_label = label_tracker.add_label();

    file.write_all(format!(".{}:\n", start_label).as_bytes());

    let register_index = generate_expression(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        condition,
    );

    write_vector(
        file,
        vec![
            format!(
                "\n\tcmp {}, 0\n",
                register_tracker.loopup_register(register_index)
            )
            .as_bytes(),
            format!("\tjle .{}\n\n", end_label).as_bytes(),
        ],
    );
    register_tracker.release_register(register_index);

    generate_code_block(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        code_block,
    );

    write_vector(
        file,
        vec![
            format!("\tjmp .{}\n", start_label).as_bytes(),
            format!(".{}:\n", end_label).as_bytes(),
        ],
    );
}

fn generate_expression(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    expression: &Expression,
) -> usize {
    match expression {
        Expression::Unary(operator, left) => generate_unary_expression(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            operator,
            left,
        ),
        Expression::Binary(left, operator, right) => generate_binary_expression(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            left,
            operator,
            right,
        ),
        Expression::Literal(literal) => {
            generate_literal(file, register_tracker, symbols_table, literal)
        }
        Expression::Variable(identifier) => load_variable(
            file,
            register_tracker,
            global_variables,
            symbols_table,
            identifier,
        ),
        Expression::FunctionCall(identifier, arguements) => {
            generate_function_call(
                file,
                register_tracker,
                label_tracker,
                global_variables,
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
                    "\tmov {}, eax\n",
                    register_tracker.loopup_register(register_index)
                )
                .as_bytes(),
            );
            register_index
        }
        Expression::Grouping(expression) => generate_expression(
            file,
            register_tracker,
            label_tracker,
            global_variables,
            symbols_table,
            expression,
        ),
    }
}

fn generate_unary_expression(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    operator: &UnaryOperator,
    left: &Expression,
) -> usize {
    let register_index = generate_expression(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        left,
    );
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
    label_tracker: &mut LabelTracker,
    global_variables: &mut Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    left: &Expression,
    operator: &BinaryOperator,
    right: &Expression,
) -> usize {
    let left_index = generate_expression(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        left,
    );
    let right_index = generate_expression(
        file,
        register_tracker,
        label_tracker,
        global_variables,
        symbols_table,
        right,
    );

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
        BinaryOperator::Eq => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tcmp {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsete al\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::NEq => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tcmp {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetne al\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Gt => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tcmp {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetg al\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Lt => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tcmp {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetl al\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::GtEq => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tcmp {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetge al\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::LtEq => {
            write_vector(
                file,
                vec![
                    format!(
                        "\tcmp {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetle al\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::And => {
            write_vector(
                file,
                vec![
                    format!(
                        "\ttest {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(left_index)
                    )
                    .as_bytes(),
                    "\tsetnz al\n".as_bytes(),
                    format!(
                        "\ttest {}, {}\n",
                        register_tracker.loopup_register(right_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetnz bl\n".as_bytes(),
                    "\tand al, bl\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::Or => {
            write_vector(
                file,
                vec![
                    format!(
                        "\ttest {}, {}\n",
                        register_tracker.loopup_register(left_index),
                        register_tracker.loopup_register(left_index)
                    )
                    .as_bytes(),
                    "\tsetnz al\n".as_bytes(),
                    format!(
                        "\ttest {}, {}\n",
                        register_tracker.loopup_register(right_index),
                        register_tracker.loopup_register(right_index)
                    )
                    .as_bytes(),
                    "\tsetnz bl\n".as_bytes(),
                    "\tor al, bl\n".as_bytes(),
                    format!(
                        "\tmovzx {}, al\n",
                        register_tracker.loopup_register(left_index),
                    )
                    .as_bytes(),
                ],
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::BitwiseAnd => {
            file.write_all(
                format!(
                    "\tand {}, {}\n",
                    register_tracker.loopup_register(left_index),
                    register_tracker.loopup_register(right_index)
                )
                .as_bytes(),
            );
            register_tracker.release_register(right_index);
            left_index
        }
        BinaryOperator::BitwiseOr => {
            file.write_all(
                format!(
                    "\tor {}, {}\n",
                    register_tracker.loopup_register(left_index),
                    register_tracker.loopup_register(right_index)
                )
                .as_bytes(),
            );
            register_tracker.release_register(right_index);
            left_index
        }
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

fn load_variable(
    file: &mut BufWriter<File>,
    register_tracker: &mut RegisterTracker,
    global_variables: &Vec<String>,
    symbols_table: &mut LocalSymbolsTable,
    identifier: &String,
) -> usize {
    let register_index = match register_tracker.claim_register() {
        Ok(register_index) => register_index,
        Err(_) => todo!("Add error handling"),
    };

    let pointer = match symbols_table.get(identifier) {
        Some(pointer) => pointer,
        None => {
            if global_variables.contains(identifier) {
                identifier.to_string()
            } else {
                todo!("Add error handling")
            }
        }
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
