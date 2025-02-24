use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// A command line to-do app written in Rust
#[derive(Debug, Parser)]
#[command(name = "Rusty Journal", version, about, long_about)]
pub struct CommandLineArgs {
    #[command(subcommand)]
    pub action: Action,
    /// Use a different journal file.
    #[arg(short, long)]
    pub journal_file: Option<PathBuf>,
}

#[derive(Debug, Subcommand)]
pub enum Action {
    /// Write tasks to the journal file.
    Add {
        /// The task description text.
        title: String,
    },
    /// Remove an entry from the journal file by position.
    Done { number: usize },
    /// List all tasks in the journal file.
    List,
}
