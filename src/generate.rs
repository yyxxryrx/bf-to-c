use crate::bf_ir::{BfIR, IRValue};

const C_FILE_HEAD: &'static str = "#include <stdio.h>\n#include <stdint.h>\n\nint ptr = 15000;\nuint8_t buffer[30000];\n\nint main() {\n";
const PY_FILE_HEAD: &'static str = "import sys\n\nbuffer = [0] * 30000\nptr = 15000\n";
const LLVM_FILE_HEAD: &'static str = "declare i32 @putchar(i32)\ndeclare i32 @getchar()\n\n@buffer = global [30000 x i8] zeroinitializer\n\ndefine i32 @main() {\n    %ptr = alloca i64\n    store i64 15000, %ptr\n";

pub fn generate_c_code(irs: &Vec<BfIR>) -> String {
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
    String::from(C_FILE_HEAD) + &_g(irs, 0) + "    return 0;\n}\n"
}

pub fn generate_py_code(irs: &Vec<BfIR>) -> String {
    fn _g(irs: &Vec<BfIR>, depth: usize) -> String {
        let mut code = String::new();
        let indent = "    ".repeat(depth);
        for ir in irs {
            match ir {
                BfIR::IncrementValue(offset, value) => match value {
                    IRValue::Const(value) => {
                        code += &format!(
                            "{indent}buffer[ptr{offset}] = (buffer[ptr{offset}] + {value}) & 0xFF\n"
                        );
                    }
                    IRValue::Expr(expr) => match (expr.source, expr.op, expr.value) {
                        (src, Some(op), Some(val)) => {
                            code += &format!(
                                "{indent}buffer[ptr{offset}] = (buffer[ptr{offset}] + buffer[ptr{src}] {op} {val}) & 0xFF\n"
                            );
                        }
                        (src, None, None) => {
                            code += &format!(
                                "{indent}buffer[ptr{offset}] = (buffer[ptr{offset}] + buffer[ptr{src}]) & 0xFF"
                            );
                        }
                        _ => panic!(),
                    },
                },
                BfIR::DecrementValue(offset, value) => match value {
                    IRValue::Const(value) => {
                        code += &format!(
                            "{indent}buffer[ptr{offset}] = (buffer[ptr{offset}] - {value}) & 0xFF\n"
                        );
                    }
                    IRValue::Expr(expr) => match (expr.source, expr.op, expr.value) {
                        (src, Some(op), Some(val)) => {
                            code += &format!(
                                "{indent}buffer[ptr{offset}] = (buffer[ptr{offset}] - (buffer[ptr{src}] {op} {val})) & 0xFF\n"
                            );
                        }
                        (src, None, None) => {
                            code += &format!(
                                "{indent}buffer[ptr{offset}] = (buffer[ptr{offset}] - buffer[ptr{src}]) & 0xFF\n"
                            );
                        }
                        _ => panic!(),
                    },
                },
                BfIR::SetValue(offset, value) => {
                    code += &format!("{indent}buffer[ptr{offset}] = {value} & 0xFF\n")
                }
                BfIR::IncrementPointer(val) => code += &format!("{indent}ptr += {val}\n"),
                BfIR::DecrementPointer(val) => code += &format!("{indent}ptr -= {val}\n"),
                BfIR::DeadLoop(body) => {
                    code += &format!("{indent}while True:\n");
                    code += &_g(body, depth + 1);
                }
                BfIR::ForLoop(body, val) => {
                    code += &format!("{indent}while buffer[ptr] != 0:\n");
                    code += &_g(body, depth + 1);
                    code += &format!("{indent}    buffer[ptr] = (buffer[ptr] + {val}) & 0xFF\n")
                }
                BfIR::WhileLoop(body) => {
                    code += &format!("{indent}while buffer[ptr] != 0:\n");
                    code += &_g(body, depth + 1);
                }
                BfIR::Output(offset) => {
                    code +=
                        &format!("{indent}print(chr(buffer[ptr{offset}]), end=\"\", flush=True)\n")
                }
                BfIR::Input(offset) => {
                    code += &format!("{indent}buffer[ptr{offset}] = ord(sys.stdin.read(1))\n");
                }
            }
        }
        code
    }
    String::from(PY_FILE_HEAD) + &_g(irs, 0)
}

pub fn generate_llvm_ir(irs: &Vec<BfIR>) -> String {
    macro_rules! new_ir {
        {var: $var:ident, code: $code:ident, cur: $cur:ident, void! $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?} => {
            $code += "    ";
            $code += &{
                $(let $last = format!("%bf{}", $var);)?
                format!($smt)
            };
            $code += "\n";
            $(let $global = format!("%bf{}", $var);)?
        };
        {var: $var:ident, code: $code:ident, cur: $cur:ident $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?} => {
            $code += "    ";
            $code += &{
                $(let $last = format!("%bf{}", $var);)?
                $var += 1;
                let $cur = format!("%bf{}", $var);
                format!($smt)
            };
            $code += "\n";
            $(let $global = format!("%bf{}", $var);)?
        };
        {@$val_index:ident, $cur:ident $( $code:ident += $(void$void:tt)? $(#$last:ident)? $smt:literal $( => $global:ident)?),*$(,)?} => {
            $(
                new_ir!(var: $val_index, code: $code, cur: $cur $(, void$void)? $(, last: $last)?, smt: $smt $(, global: $global)?);
            )*
        };
    }
    fn _g(irs: &Vec<BfIR>, mut var_index: usize) -> String {
        let mut code = String::new();
        for ir in irs {
            match ir {
                BfIR::IncrementValue(offset, value) => match value {
                    IRValue::Const(value) => {
                        // 先将其转换
                        let value = *value as u8;
                        if let Some((cmd, val)) = offset.to_ir() {
                            new_ir! {
                                // 指定索引(var_index) 和 每行IR的虚拟寄存器名称(_cur)
                                @var_index, _cur
                                // 计算指针偏移
                                code += "{_cur} = {cmd} i64 {val}, %ptr",
                                // 获取指向的元素的指针，并将其虚拟寄存器保存为当前的局部变量(e_ptr)，以供后续使用
                                // #last 是获取上一句的虚拟寄存器名称并保存到 last 这个变量名里，语法：#<var_name>
                                // => e_ptr 是保存这一行的虚拟寄存器名称到局部变量，以供后续使用，语法：=> <var_name>
                                code += #last "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {last}" => e_ptr,
                                // 获取指针指向的数值
                                code += "{_cur} = load i8, ptr {e_ptr}",
                                // 计算更新后的值
                                code += #last "{_cur} = add i8 {value}, {last}",
                                // 保存更新后的值
                                // void! 表示这一行没有返回值，不会增加索引，同样无法引用 _cur，但可以引用局部变量以及上一行的寄存器
                                code += void! #last "store i8 {last}, ptr {e_ptr}"
                            }
                        } else {
                            new_ir! {
                                @var_index, _cur
                                // 没有偏移，直接拿就行
                                code += "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 %ptr" => e_ptr,
                                code += "{_cur} = load i8, ptr {e_ptr}",
                                code += #last "{_cur} = add i8 {value}, {last}",
                                code += void! #last "store i8 {last}, ptr {e_ptr}"
                            }
                        }
                    }
                    _ => {
                        todo!("Expr 情况的分支")
                    }
                },
                _ => {
                    todo!("其他的转换")
                }
            }
        }
        code
    }
    String::from(LLVM_FILE_HEAD) + &_g(irs, 0) + "}\n"
}
