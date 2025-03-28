use tomado::adapters::cli::{self, CommandLineArgs, Parser};
use tomado::adapters::workspace::WorkSpace;
use tomado::usecases::{add_matter, done_matter, edit_matter, list_matters, view_matter};

fn main() -> anyhow::Result<()> {
    let CommandLineArgs { action } = CommandLineArgs::parse();
    let workspace = WorkSpace::setup(env!("CARGO_PKG_NAME"))?;
    let config = workspace.load_config()?;
    let journal_path = &workspace.journal_path;
    match action {
        cli::Action::Add {
            title,
            is_set_detail,
            priority,
            due,
        } => add_matter(journal_path, title, is_set_detail, priority, due),
        cli::Action::Done { number } => done_matter(journal_path, number),
        cli::Action::Edit {
            number,
            title,
            is_set_detail,
            priority,
            due,
            done,
        } => edit_matter(
            &config,
            journal_path,
            number,
            title,
            is_set_detail,
            priority,
            due,
            done,
        ),
        cli::Action::List => list_matters(journal_path),
        cli::Action::Today => todo!(),
        cli::Action::View { number } => view_matter(journal_path, number),
        cli::Action::Trash { number: _ } => todo!(),
        cli::Action::Config => todo!(),
    }?;
    Ok(())
}
