mod cli;
mod todo;

use anyhow::anyhow;
use clap::Parser;
use cli::CommandLineArgs;
use home::home_dir;
use std::path::PathBuf;
use todo::{add_matter, done_matter, list_matters, TodoMatter};

fn main() -> anyhow::Result<()> {
    let CommandLineArgs {
        action,
        journal_file,
    } = CommandLineArgs::parse();
    let journal_path = journal_file
        .or_else(default_journal_file)
        .ok_or(anyhow!("Failed to find journal file."))?;
    match action {
        cli::Action::Add { title } => add_matter(journal_path, TodoMatter::new(title)),
        cli::Action::Done { number } => done_matter(journal_path, number),
        cli::Action::List => list_matters(journal_path),
    }?;
    Ok(())
}

fn default_journal_file() -> Option<PathBuf> {
    home_dir().map(|mut path| {
        path.push(".rusty-journal.json");
        path
    })
}
