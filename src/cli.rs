use clap::Parser;
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
}
