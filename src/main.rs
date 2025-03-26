mod cli;
mod config;
mod todo;

use std::{
    env,
    fs::create_dir_all,
    io,
    path::{Path, PathBuf},
};

use anyhow::anyhow;
use cli::{CommandLineArgs, Parser};
use config::Config;
use directories::ProjectDirs;
use todo::{add_matter, done_matter, edit_matter, list_matters, view_matter};

fn main() -> anyhow::Result<()> {
    let CommandLineArgs { action } = CommandLineArgs::parse();
    let workspace = WorkSpace::new(env!("CARGO_PKG_NAME"))?;
    workspace.setup()?;
    let config = Config::new(&workspace.config_path)?;
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

struct WorkSpace {
    project_dirs: ProjectDirs,
    config_path: PathBuf,
    journal_path: PathBuf,
}

impl WorkSpace {
    fn new(package_name: &str) -> anyhow::Result<Self> {
        let project_dirs = ProjectDirs::from("com.github", "amderbar", package_name)
            .ok_or(anyhow!("Failed to get project directories."))?;
        let journal_path = project_dirs.data_dir().join("journal.json");
        let config_path = project_dirs.config_dir().join("config.toml");
        Ok(Self {
            project_dirs,
            config_path,
            journal_path,
        })
    }

    fn setup(&self) -> anyhow::Result<()> {
        let project_dirs = &self.project_dirs;
        self.setup_dir(project_dirs.data_dir())?;
        self.setup_dir(project_dirs.config_dir())?;
        Ok(())
    }

    fn setup_dir(&self, dir_path: &Path) -> anyhow::Result<()> {
        match create_dir_all(dir_path) {
            Ok(_) => Ok(()),
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(()),
            Err(e) => Err(anyhow!(e)),
        }
    }
}
