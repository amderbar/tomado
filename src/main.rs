mod adapters;

use adapters::{
    cli::{self, CommandLineArgs, Parser},
    input::{from_stdin, with_edit_tempfile},
    outputs::{DetailedDisplay, OneLineDisplay},
    workspace::WorkSpace,
};
use tomado::ports::TodoRepository as _;
use tomado::usecases::{
    add_matter, done_matter, edit_matter, list_matters, trash_matter, view_matter,
};

fn main() -> anyhow::Result<()> {
    let CommandLineArgs { action } = CommandLineArgs::parse();
    let workspace = WorkSpace::setup(env!("CARGO_PKG_NAME"))?;
    let config = workspace.load_config()?;
    let todo_repo = &workspace.get_todo_repository()?;
    match action {
        cli::Action::Add {
            title,
            is_set_detail,
            priority,
            due,
        } => {
            let detail = if is_set_detail {
                let detail = from_stdin("-- Please input the detail of the task. --")?;
                Some(detail)
            } else {
                None
            };
            add_matter(todo_repo, title, detail, priority, due)
        }
        cli::Action::Done { number } => done_matter(todo_repo, number),
        cli::Action::Edit {
            number,
            title,
            is_set_detail,
            priority,
            due,
            done,
        } => {
            let matter = todo_repo.find(number)?;

            let detail = if is_set_detail {
                let new_detail = with_edit_tempfile(
                    &format!("TODO_{}_DETAIL_EDITTING_", matter.number),
                    &matter.contents.detail,
                    &config,
                )?;
                Some(new_detail)
            } else {
                None
            };
            edit_matter(todo_repo, matter, title, detail, priority, due, done)
        }
        cli::Action::List => {
            let list = list_matters(todo_repo)?;
            for mttr in list {
                println!("{}", Into::<OneLineDisplay>::into(mttr));
            }
            Ok(())
        }
        cli::Action::Today => todo!(),
        cli::Action::View { number } => view_matter(todo_repo, number)
            .map(|mttr| println!("{}", Into::<DetailedDisplay>::into(mttr))),
        cli::Action::Trash { number } => trash_matter(todo_repo, number),
        cli::Action::Config => todo!(),
    }?;
    Ok(())
}
