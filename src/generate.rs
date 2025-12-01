use crate::bf_ir::{BfIR, IRValue};

const FILE_HEAD: &'static str = "#include <stdio.h>\n#include <stdint.h>\n\nint ptr = 15000;\nuint8_t buffer[30000];\n\nint main() {\n";

pub fn generate_code(irs: &Vec<BfIR>) -> String {
    fn _g(irs: &Vec<BfIR>, depth: usize) -> String {
        let mut code = String::new();
        let indent = "    ".repeat(depth + 1);
        for ir in irs {
            match ir {
                BfIR::IncrementValue(offset, value) => {
                    code += &match value {
                        IRValue::Const(value) => {
                            format!("{indent}buffer[ptr{offset}] += {value};\n")
                        }
                        IRValue::Expr(expr) => match (expr.source, expr.op, expr.value) {
                            (source, None, None) => {
                                format!("{indent}buffer[ptr{offset}] += buffer[ptr{source}];\n")
                            }
                            (source, Some(op), Some(value)) => format!(
                                "{indent}buffer[ptr{offset}] += buffer[ptr{source}] {op} {value};\n"
                            ),
                            _ => panic!(""),
                        },
                    }
                }
                BfIR::DecrementValue(offset, value) => {
                    code += &match value {
                        IRValue::Const(value) => {
                            format!("{indent}buffer[ptr{offset}] -= {value};\n")
                        }
                        IRValue::Expr(expr) => match (expr.source, expr.op, expr.value) {
                            (source, None, None) => {
                                format!("{indent}buffer[ptr{offset}] -= buffer[ptr{source}];\n")
                            }
                            (source, Some(op), Some(value)) => format!(
                                "{indent}buffer[ptr{offset}] -= buffer[ptr{source}] {op} {value};\n"
                            ),
                            _ => panic!(""),
                        },
                    }
                }
                BfIR::IncrementPointer(value) => {
                    code += &format!("{indent}ptr += {};\n", value);
                }
                BfIR::DecrementPointer(value) => {
                    code += &format!("{indent}ptr -= {};\n", value);
                }
                BfIR::Input(offset) => {
                    code += &format!("{indent}buffer[ptr{offset}] = fgetc(stdin);\n");
                }
                BfIR::Output(offset) => {
                    code += &format!("{indent}fputc(buffer[ptr{offset}], stdout);\n");
                }
                BfIR::ForLoop(irs, condition) => {
                    code += &format!(
                        "{indent}for (; buffer[ptr] != 0; buffer[ptr] += {condition}) {{\n"
                    );
                    code += &_g(irs, depth + 1);
                    code += &format!("{indent}}}\n");
                }
                BfIR::WhileLoop(irs) => {
                    code += &format!("{indent}while (buffer[ptr] != 0) {{\n");
                    code += &_g(irs, depth + 1);
                    code += &format!("{indent}}}\n");
                }
                BfIR::DeadLoop(irs) => {
                    code += &format!("{indent}while (1) {{\n");
                    code += &_g(irs, depth + 1);
                    code += &format!("{indent}}}\n");
                }
                BfIR::SetValue(offset, value) => {
                    code += &format!("{indent}buffer[ptr{offset}] = {value};\n");
                }
            }
        }
        code
    }
    String::from(FILE_HEAD) + &_g(irs, 0) + "    return 0;\n}\n"
}
