use crate::bf_ir::BfIR;
use crate::generate::{generate_c_code, generate_llvm_ir, generate_py_code};
use clap::builder::PossibleValue;
use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[derive(Debug, Parser)]
pub struct Cli {
    #[clap(help = "your bf code file")]
    pub input: PathBuf,
    #[clap(short, long, help = "The out C file path")]
    pub output: Option<PathBuf>,
    #[clap(
        short,
        long,
        help = "Will be used compiler version",
        default_value = "0"
    )]
    pub cv: usize,
    #[clap(short = 'O', help = "Optimize level", default_value = "1")]
    pub opt: usize,
    #[clap(short = 'L', long, help = "The target language", default_value = "c")]
    pub language: Language,
}

#[derive(Debug, Clone)]
pub enum Language {
    C,
    Python,
    LLVM,
}

impl ValueEnum for Language {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::C, Self::Python, Self::LLVM]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            Self::C => Some(PossibleValue::new("c")),
            Self::Python => Some(PossibleValue::new("python")),
            Self::LLVM => Some(PossibleValue::new("llvm")),
        }
    }
}

impl Language {
    pub fn generate_code(&self, irs: &Vec<BfIR>) -> String {
        match self {
            Self::C => generate_c_code(irs),
            Self::Python => generate_py_code(irs),
            Self::LLVM => generate_llvm_ir(irs),
        }
    }
}
