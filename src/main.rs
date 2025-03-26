mod cli;
mod todo;

use std::{
    env,
    fs::create_dir_all,
    io,
    path::{Path, PathBuf},
};

use anyhow::anyhow;
use cli::{CommandLineArgs, Parser};
use directories::ProjectDirs;
use todo::{add_matter, done_matter, list_matters, TodoMatter};

fn main() -> anyhow::Result<()> {
    let CommandLineArgs { action } = CommandLineArgs::parse();
    let workspace = WorkSpace::new(env!("CARGO_PKG_NAME"))?;
    workspace.setup()?;
    let config = Config::new(&workspace.config_path)?;
    let journal_path = &workspace.journal_path;
    match action {
        cli::Action::Add { title } => add_matter(journal_path, TodoMatter::new(title)),
        cli::Action::Done { number } => done_matter(journal_path, number),
        cli::Action::Edit { number: _ } => todo!(),
        cli::Action::List => list_matters(journal_path),
        cli::Action::Today => todo!(),
        cli::Action::View { number: _ } => todo!(),
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

struct Config {
    editor: String,
}

impl Config {
    fn new(_config_path: &PathBuf) -> anyhow::Result<Self> {
        // TODO: Read the config file
        let editor = env::var("VISUAL")
            .or(env::var("EDITOR"))
            .unwrap_or(Self::default_editor());
        Ok(Self { editor })
    }

    #[cfg(target_family = "windows")]
    fn default_editor() -> String {
        "notepad".to_string()
    }

    #[cfg(target_family = "unix")]
    fn default_editor() -> String {
        "/usr/bin/editor".to_string()
    }
}
