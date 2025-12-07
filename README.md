# bf-to-c

一个将 Brainfuck 语言编译成多种目标语言的编译器，支持输出 C、Python 和 LLVM IR 代码。

## 功能特点

- 将 Brainfuck 代码编译为可执行的 C 代码
- 支持多种输出格式：C、Python、LLVM IR
- 提供多级优化选项
- 命令行界面友好

## 安装

确保你已经安装了 Rust 工具链，然后克隆本项目并构建：

```bash
git clone <repository-url>
cd bf-to-c
cargo build --release
```

## 使用方法

基本用法：

```bash
bf-to-c <input.bf> [-o output] [-L language] [-O optimization_level]
```

参数说明：
- `input.bf`: 输入的 Brainfuck 源代码文件（使用 `-` 表示从标准输入读取）
- `-o, --output`: 输出文件路径（可选，默认基于输入文件名生成）
- `-L, --language`: 目标语言，可选 `c`、`python`、`llvm`（默认为 `c`）
- `-O, --optimization`: 优化级别，0-3 级（默认为 1）
- `-c, --cv`: 编译器版本（默认为 0）

示例：

```bash
# 编译为 C 代码
bf-to-c hello.bf -o hello.c

# 编译为 Python 代码并启用高级优化
bf-to-c hello.bf -c 1 -L python -O3 -o hello.py

# 从标准输入读取并输出到标准输出
echo "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.---------.>+.>." | bf-to-c - -L c -O 2
```

## 技术架构

该项目采用模块化设计，主要包含以下组件：

- **词法分析器** ([lexer.rs](src/lexer.rs)): 将 Brainfuck 源码转换为标记流
- **抽象语法树** ([ast.rs](src/ast.rs)): 构建程序的 AST 表示
- **中间表示** ([bf_ir.rs](src/bf_ir.rs)): 生成优化的中间表示形式
- **代码生成器** ([generate.rs](src/generate.rs)): 将 IR 转换为目标语言代码
- **命令行接口** ([cli.rs](src/cli.rs)): 处理命令行参数和用户交互

## 优化级别

- **0级**: 无优化，直接线性翻译
- **1级**: 基本的指令合并优化
- **2级**: 循环优化
- **3级**: 特定于操作的深度优化

## 支持的目标语言

1. **C语言**: 生成标准 C 代码，可使用 GCC 或 Clang 编译
2. **Python**: 生成可以直接运行的 Python 脚本
3. **LLVM IR**: 生成 LLVM 中间表示，可用于进一步编译优化

## 示例

以下是一个简单的 "Hello World" Brainfuck 程序：

```brainfuck
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.---------.>+.>.
```

使用本工具编译后可以生成对应的目标语言代码。
