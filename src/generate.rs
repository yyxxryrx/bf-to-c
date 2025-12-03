use crate::bf_ir::{BfIR, IRValue, PtrOffset};

const C_FILE_HEAD: &'static str = "#include <stdio.h>\n#include <stdint.h>\n\nint ptr = 15000;\nuint8_t buffer[30000];\n\nint main() {\n";
const PY_FILE_HEAD: &'static str = "import sys\n\nbuffer = [0] * 30000\nptr = 15000\n";
const LLVM_FILE_HEAD: &'static str = "declare i32 @putchar(i32)\ndeclare i32 @getchar()\n\n@buffer = global [30000 x i8] zeroinitializer\n\ndefine i32 @main() {\n    %ptr = alloca i64\n    store i64 15000, ptr %ptr\n";

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
                    if body.is_empty() {
                        code += &format!("{indent}    pass\n");
                    } else {
                        code += &_g(body, depth + 1);
                    }
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
    /// Macro for generating LLVM IR instructions
    ///
    /// This macro provides several different ways to generate LLVM IR instructions:
    ///
    /// ## Syntax
    ///
    /// ### Single instruction generation form
    /// ```ignore
    /// new_ir! {var: $var:ident, code: $code:ident, cur: $cur:ident $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?}
    /// ```
    /// or
    /// ```ignore
    /// new_ir! {var: $var:ident, code: $code:ident, cur: $cur:ident, void! $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?}
    /// ```
    ///
    /// ### Batch instruction generation form
    /// ```ignore
    /// new_ir! {@$val_index:ident, $cur:ident $($code:ident += $(void$void:tt)? $(#$last:ident)? $smt:literal $( => $global:ident)?),*$(,)?}
    /// ```
    ///
    /// ## Parameter Description
    ///
    /// - `var: $var:ident` - A mutable variable used to track register numbers
    /// - `code: $code:ident` - A string variable that stores the generated code
    /// - `cur: $cur:ident` - A variable name for the current instruction's register
    /// - `void!` - A marker indicating that the instruction has no return value and does not increment the register number
    /// - `last: $last:ident` - Optional parameter referencing the previous instruction's register
    /// - `smt: $smt:literal` - The format string for the LLVM IR instruction
    /// - `global: $global:ident` - Optional parameter saving the current register name to a global variable
    /// - `#$last` - In batch form, references the previous instruction's register name
    /// - `=> $global` - In batch form, saves the current register name to the specified variable
    ///
    /// ## Usage Examples
    ///
    /// ```ignore
    /// // Generate an instruction with a return value
    /// new_ir! {var: index, code: code_str, cur: current_reg, smt: "{current_reg} = add i8 1, 2"}
    ///
    /// // Generate an instruction without a return value
    /// new_ir! {var: index, code: code_str, cur: current_reg, void!, smt: "store i8 0, ptr %ptr"}
    ///
    /// // Generate instructions in batch
    /// new_ir! {@index, _cur code_str += "{_cur} = load i64, ptr %ptr", code_str += void! "store ptr %ptr"}
    /// ```
    macro_rules! new_ir {
        // Generate LLVM IR instruction without return value (such as store)
        {*var: $var:ident, code: $code:ident $(, cur: $cur:ident)?, void! $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?} => {
            $code += "    ";
            $code += &{
                $(let $last = format!("%bf{}", $var);)?
                format!($smt)
            };
            $code += "\n";
            $(let $global = format!("%bf{}", $var);)?
        };
        // Generate LLVM IR instruction with return value (such as load, add, etc.)
        {*var: $var:ident, code: $code:ident, cur: $cur:ident $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?} => {
            $code += "    ";
            $code += &{
                $(let $last = format!("%bf{}", $var);)?
                *$var += 1;
                let $cur = format!("%bf{}", $var);
                format!($smt)
            };
            $code += "\n";
            $(let $global = format!("%bf{}", $var);)?
        };
        // Generate LLVM IR instruction without return value (such as store)
        {var: $var:ident, code: $code:ident $(, cur: $cur:ident)?, void! $(,last: $last:ident)?, smt: $smt:literal $(,global: $global:ident)?} => {
            $code += "    ";
            $code += &{
                $(let $last = format!("%bf{}", $var);)?
                format!($smt)
            };
            $code += "\n";
            $(let $global = format!("%bf{}", $var);)?
        };
        // Generate LLVM IR instruction with return value (such as load, add, etc.)
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
        // Generate LLVM IR instructions in batch
        {@*$val_index:ident, void! $( $code:ident += $(#$last:ident)? $smt:literal $( => $global:ident)?),*$(,)?} => {
            $(
                new_ir!(*var: $val_index, code: $code ,void! $(, last: $last)?, smt: $smt $(, global: $global)?);
            )*
        };
        {@*$val_index:ident, $cur:ident $( $code:ident += $(void$void:tt)? $(#$last:ident)? $smt:literal $( => $global:ident)?),*$(,)?} => {
            $(
                new_ir!(*var: $val_index, code: $code, cur: $cur $(, void$void)? $(, last: $last)?, smt: $smt $(, global: $global)?);
            )*
        };
        {@$val_index:ident, void! $( $code:ident += $(#$last:ident)? $smt:literal $( => $global:ident)?),*$(,)?} => {
            $(
                new_ir!(var: $val_index, code: $code ,void! $(, last: $last)?, smt: $smt $(, global: $global)?);
            )*
        };
        // Generate LLVM IR instructions in batch
        {@$val_index:ident, $cur:ident $( $code:ident += $(void$void:tt)? $(#$last:ident)? $smt:literal $( => $global:ident)?),*$(,)?} => {
            $(
                new_ir!(var: $val_index, code: $code, cur: $cur $(, void$void)? $(, last: $last)?, smt: $smt $(, global: $global)?);
            )*
        };
    }

    /// 根据偏移自动获取当前指针的位置，之后直接通过 `#<var_name>` 语法获取就可以了
    fn get_ptr(offset: &PtrOffset, var_index: &mut usize, code: &mut String) -> String {
        let mut index = *var_index;
        let mut c = String::new();
        let ptr = if let Some((cmd, val)) = offset.to_ir() {
            new_ir! {
                @index, _cur
                // 读取指针的值
                c += "{_cur} = load i64, ptr %ptr",
                // 计算指针偏移
                c += #ptr "{_cur} = {cmd} i64 {ptr}, {val}" => ptr,
            }
            ptr
        } else {
            new_ir! {
                @index, _cur
                c += "{_cur} = load i64, ptr %ptr" => ptr
            }
            ptr
        };
        *code += &c;
        *var_index = index;
        ptr
    }

    fn get_element_ptr(offset: &PtrOffset, var_index: &mut usize, code: &mut String) -> String {
        get_ptr(offset, var_index, code);
        let mut index = *var_index;
        let mut c = String::new();
        new_ir! {
            @index, _cur
            c += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
        }
        *code += &c;
        *var_index = index;
        e_ptr
    }

    fn _g(irs: &Vec<BfIR>, var_index: &mut usize, loop_index: &mut usize) -> String {
        let mut code = String::new();
        for ir in irs {
            match ir {
                BfIR::IncrementValue(offset, value) => match value {
                    IRValue::Const(value) => {
                        // 先将其转换
                        let value = *value as u8;
                        get_ptr(offset, var_index, &mut code);
                        new_ir! {
                            // 指定索引(var_index) 和 每行IR的虚拟寄存器名称(_cur)
                            @*var_index, _cur
                            // 获取指向的元素的指针，并将其虚拟寄存器保存为当前的局部变量(e_ptr)，以供后续使用
                            // #<var_name> 是获取上一句的虚拟寄存器名称并保存到 var_name 这个变量名里，语法：#<var_name>
                            // 这里上面运行了 `get_ptr` 所以上一句的虚拟寄存器存放的就是指针的值
                            // => e_ptr 是保存这一行的虚拟寄存器名称到局部变量，以供后续使用，语法：=> <var_name>
                            code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
                            // 获取指针指向的数值
                            code += "{_cur} = load i8, ptr {e_ptr}",
                            // 计算更新后的值
                            code += #last "{_cur} = add i8 {last}, {value}",
                            // 保存更新后的值
                            // void! 表示这一行没有返回值，不会增加索引，同样无法引用 _cur，但可以引用局部变量以及上一行的寄存器
                            code += void! #last "store i8 {last}, ptr {e_ptr}"
                        }
                    }
                    IRValue::Expr(expr) => {
                        // 获取表达式依赖的元素的指针值
                        get_ptr(&expr.source, var_index, &mut code);
                        match (expr.op, expr.value) {
                            // 有计算的部分
                            (Some(op), Some(val)) => {
                                let cmd = op.to_ir();
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到表达式依赖的值的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}",
                                    // 取出表达式依赖的值
                                    code += #e_ptr "{_cur} = load i8, ptr {e_ptr}",
                                    // 计算表达式结果
                                    code += #last "{_cur} = {cmd} i8 {val}, {last}" => val,
                                }
                                // 获取目标项元素的指针值
                                get_ptr(offset, var_index, &mut code);
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到目标元素的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
                                    // 拿到目标元素的当前值
                                    code += "{_cur} = load i8, ptr {e_ptr}",
                                    // 将当前值与之前表达式计算出来的值相加
                                    code += #last "{_cur} = add i8 {last}, {val}",
                                    // 将结果保存回去
                                    code += void! #val "store i8 {val}, ptr {e_ptr}"
                                }
                            }
                            // 没有计算的部分
                            (None, None) => {
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到表达式依赖的值的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}",
                                    // 取出表达式依赖的值，这里不需要计算，直接保存就行
                                    code += #e_ptr "{_cur} = load i8, ptr {e_ptr}" => val,
                                }
                                // 获取目标项元素的指针值
                                get_ptr(offset, var_index, &mut code);
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到目标元素的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
                                    // 拿到目标元素的当前值
                                    code += "{_cur} = load i8, ptr {e_ptr}",
                                    // 将当前值与之前表达式计算出来的值相加
                                    code += #last "{_cur} = add i8 {last}, {val}",
                                    // 将结果保存回去
                                    code += void! #val "store i8 {val}, ptr {e_ptr}"
                                }
                            }
                            _ => {}
                        }
                    }
                },
                BfIR::DecrementValue(offset, value) => match value {
                    IRValue::Const(value) => {
                        // 先将其转换
                        let value = *value as u8;
                        get_ptr(offset, var_index, &mut code);
                        new_ir! {
                            // 指定索引(var_index) 和 每行IR的虚拟寄存器名称(_cur)
                            @*var_index, _cur
                            // 获取指向的元素的指针，并将其虚拟寄存器保存为当前的局部变量(e_ptr)，以供后续使用
                            // #<var_name> 是获取上一句的虚拟寄存器名称并保存到 var_name 这个变量名里，语法：#<var_name>
                            // 这里上面运行了 `get_ptr` 所以上一句的虚拟寄存器存放的就是指针的值
                            // => e_ptr 是保存这一行的虚拟寄存器名称到局部变量，以供后续使用，语法：=> <var_name>
                            code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
                            // 获取指针指向的数值
                            code += "{_cur} = load i8, ptr {e_ptr}",
                            // 计算更新后的值
                            code += #last "{_cur} = sub i8 {last}, {value}",
                            // 保存更新后的值
                            // void! 表示这一行没有返回值，不会增加索引，同样无法引用 _cur，但可以引用局部变量以及上一行的寄存器
                            code += void! #last "store i8 {last}, ptr {e_ptr}"
                        }
                    }
                    IRValue::Expr(expr) => {
                        // 获取表达式依赖的元素的指针值
                        get_ptr(&expr.source, var_index, &mut code);
                        match (expr.op, expr.value) {
                            // 有计算的部分
                            (Some(op), Some(val)) => {
                                let cmd = op.to_ir();
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到表达式依赖的值的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}",
                                    // 取出表达式依赖的值
                                    code += #e_ptr "{_cur} = load i8, ptr {e_ptr}",
                                    // 计算表达式结果
                                    code += #last "{_cur} = {cmd} i8 {val}, {last}" => val,
                                }
                                // 获取目标项元素的指针值
                                get_ptr(offset, var_index, &mut code);
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到目标元素的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
                                    // 拿到目标元素的当前值
                                    code += "{_cur} = load i8, ptr {e_ptr}",
                                    // 将当前值与之前表达式计算出来的值相减
                                    code += #last "{_cur} = sub i8 {last}, {val}",
                                    // 将结果保存回去
                                    code += void! #val "store i8 {val}, ptr {e_ptr}"
                                }
                            }
                            // 没有计算的部分
                            (None, None) => {
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到表达式依赖的值的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}",
                                    // 取出表达式依赖的值，这里不需要计算，直接保存就行
                                    code += #e_ptr "{_cur} = load i8, ptr {e_ptr}" => val,
                                }
                                // 获取目标项元素的指针值
                                get_ptr(offset, var_index, &mut code);
                                new_ir! {
                                    @*var_index, _cur
                                    // 拿到目标元素的指针
                                    code += #ptr "{_cur} = getelementptr [30000 x i8], ptr @buffer, i64 0, i64 {ptr}" => e_ptr,
                                    // 拿到目标元素的当前值
                                    code += "{_cur} = load i8, ptr {e_ptr}",
                                    // 将当前值与之前表达式计算出来的值相减
                                    code += #last "{_cur} = sub i8 {val}, {last}",
                                    // 将结果保存回去
                                    code += void! #val "store i8 {val}, ptr {e_ptr}"
                                }
                            }
                            _ => {}
                        }
                    }
                },
                BfIR::SetValue(offset, value) => {
                    let value = *value as u8;
                    get_element_ptr(offset, var_index, &mut code);
                    new_ir! {
                        @var_index, _cur
                        code += void! #e_ptr "store i8 {value}, ptr {e_ptr}"
                    }
                }
                BfIR::IncrementPointer(val) => {
                    new_ir! {
                        @*var_index, _cur
                        code += "{_cur} = load i64, ptr %ptr",
                        code += #ptr_value "{_cur} = add i64 {ptr_value}, {val}",
                        code += void! #new_value "store i64 {new_value}, ptr %ptr",
                    }
                }
                BfIR::DecrementPointer(val) => {
                    new_ir! {
                        @*var_index, _cur
                        code += "{_cur} = load i64, ptr %ptr",
                        code += #ptr_value "{_cur} = sub i64 {ptr_value}, {val}",
                        code += void! #new_value "store i64 {new_value}, ptr %ptr",
                    }
                }
                BfIR::Output(offset) => {
                    get_element_ptr(offset, var_index, &mut code);
                    new_ir! {
                        @*var_index, _cur
                        code += #e_ptr "{_cur} = load i8, ptr {e_ptr}",
                        code += #val "{_cur} = zext i8 {val} to i32",
                        code += void! #chr "call i32 @putchar(i32 {chr})"
                    }
                }
                BfIR::Input(offset) => {
                    let e_ptr = get_element_ptr(offset, var_index, &mut code);
                    new_ir! {
                        @*var_index, _cur
                        code += "{_cur} = call @getchar()",
                        code += #val "{_cur} = trunc i32 {val} to i8",
                        code += void! #val "store i8 {val}, ptr {e_ptr}"
                    }
                }
                BfIR::DeadLoop(body) => {
                    *loop_index += 1;
                    let cur_loop_index = *loop_index;
                    code += &format!("start{cur_loop_index}:\n");
                    code += &_g(body, var_index, loop_index);
                    new_ir! {
                        @*var_index, void!
                        code += "br label %start{cur_loop_index}"
                    }
                }
                BfIR::WhileLoop(body) => {
                    *loop_index += 1;
                    let cur_loop_index = *loop_index;
                    get_element_ptr(&PtrOffset::None, var_index, &mut code);
                    new_ir! {
                        @*var_index, _cur
                        code += #ptr "{_cur} = load i8, ptr {ptr}",
                        code += #val "{_cur} = icmp eq i8 {val}, 0",
                        code += void! #ret "br i1 {ret}, label %end{cur_loop_index}, label %start{cur_loop_index}"
                    }
                    code += &format!("start{cur_loop_index}:\n");
                    code += &_g(body, var_index, loop_index);
                    get_element_ptr(&PtrOffset::None, var_index, &mut code);
                    new_ir! {
                        @*var_index, _cur
                        code += #ptr "{_cur} = load i8, ptr {ptr}",
                        code += #val "{_cur} = icmp eq i8 {val}, 0",
                        code += void! #ret "br i1 {ret}, label %end{cur_loop_index}, label %start{cur_loop_index}"
                    }
                    code += &format!("end{cur_loop_index}:\n");
                }
                BfIR::ForLoop(body, val) => {
                    *loop_index += 1;
                    let cur_loop_index = *loop_index;
                    get_element_ptr(&PtrOffset::None, var_index, &mut code);
                    new_ir! {
                        @*var_index, _cur
                        code += #ptr "{_cur} = load i8, ptr {ptr}",
                        code += #val "{_cur} = icmp eq i8 {val}, 0",
                        code += void! #ret "br i1 {ret}, label %end{cur_loop_index}, label %start{cur_loop_index}"
                    }
                    code += &format!("start{cur_loop_index}:\n");
                    code += &_g(body, var_index, loop_index);
                    let cmd = if *val >= 0 {
                        "add"
                    } else {
                        "sub"
                    };
                    let val = (*val).abs();
                    let e_ptr = get_element_ptr(&PtrOffset::None, var_index, &mut code);
                    new_ir! {
                        @*var_index, _cur
                        code += "{_cur} = load i8, ptr {e_ptr}",
                        code += #v "{_cur} = {cmd} i8 {v}, {val}" => val,
                        code += void! "store i8 {val}, ptr {e_ptr}",
                        code += "{_cur} = icmp eq i8 {val}, 0",
                        code += void! #ret "br i1 {ret}, label %end{cur_loop_index}, label %start{cur_loop_index}"
                    }
                    code += &format!("end{cur_loop_index}:\n");
                }
            }
        }
        code
    }
    String::from(LLVM_FILE_HEAD) + &_g(irs, &mut 0, &mut 0) + "    ret i32 0\n}\n"
}
