use std::{
    fs::File,
    io::{BufWriter, Write},
    process::ExitCode,
};

use crate::parser::{ASTType, Conditional, Expression, Statement, TopLevel, VariableAssignment};

struct Symbol {}

struct SymbolsTable {
    global_variables: Vec<Symbol>,
    local_variables: Vec<Symbol>,
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
        "\tglobal _start\n\n",
        "_start:\n",
        "\tcall main\n",
        "\tmov rax, 60\n",
        "\tmov rdi, 0\n",
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

fn generate_postamble(file: &mut BufWriter<File>) {}

fn generate_code_from_ast(file: &mut BufWriter<File>, ast: Vec<TopLevel>) {
    for top_level in ast {
        match top_level {
            TopLevel::Function(return_type, identifier, parameters, code_block) => {
                generate_function(file, return_type, identifier, parameters, code_block)
            }
            TopLevel::VariableDefinition(variable_type, identifier, value) => {
                todo!("Should never get called")
            }
        }
    }
}

fn generate_function(
    file: &mut BufWriter<File>,
    return_type: ASTType,
    identifier: String,
    parameters: Option<Vec<(ASTType, String)>>,
    code_block: Vec<Statement>,
) {
    let local_variable_size = calc_stack_size(Some(&parameters), &code_block);

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
    }

    file.write_all("\tmov rbp, rsp\n\n".as_bytes());
    if local_variable_size > 0 {
        file.write_all(format!("\tsub rsp, {}\n\n", local_variable_size).as_bytes());
    }

    // TODO: Deal with parameters - move from reg to stack
    // RDI, RSI, RDX, RCX, R8, R9, rest on stack

    generate_code_block(file, code_block);

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

fn generate_code_block(file: &mut BufWriter<File>, code_block: Vec<Statement>) {
    for statement in code_block {
        match statement {
            Statement::FunctionCall(identifier, arguements) => {
                generate_function_call(file, identifier, arguements)
            }
            Statement::VariableAssignment(variable_assignment) => {
                generate_variable_assignment(file, variable_assignment)
            }
            Statement::Conditional(_) => todo!("Conditional"),
            Statement::CodeBlock(code_block) => generate_code_block(file, code_block),
            Statement::Return(_) => todo!("Return value"),
            Statement::Break => todo!("Break"),
            Statement::Continue => todo!("Continue"),
        }
    }
}

fn generate_function_call(
    file: &mut BufWriter<File>,
    identifier: String,
    arguements: Option<Vec<Expression>>,
) {
    write_vector(
        file,
        vec![
            "\tpush r8\n".as_bytes(),
            "\tpush r9\n".as_bytes(),
            "\tpush r10\n".as_bytes(),
            "\tpush r11\n\n".as_bytes(),
            format!("\tcall {}\n\n", identifier).as_bytes(),
            "\tpop r11\n".as_bytes(),
            "\tpop r10\n".as_bytes(),
            "\tpop r9\n".as_bytes(),
            "\tpop r8\n\n".as_bytes(),
        ],
    );
}

fn generate_variable_assignment(
    file: &mut BufWriter<File>,
    variable_assignment: VariableAssignment,
) {
    match variable_assignment {
        VariableAssignment::Definition(_, identifier, initial_expression) => todo!(),
        VariableAssignment::Assignment(identifier, expression) => todo!(),
    }
}
