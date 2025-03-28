use std::{
    fs::create_dir_all,
    io,
    path::{Path, PathBuf},
};

use anyhow::anyhow;
use directories::ProjectDirs;

use super::config::Config;

pub struct WorkSpace {
    project_dirs: ProjectDirs,
    config_path: PathBuf,
    pub journal_path: PathBuf,
}

impl WorkSpace {
    fn new(project_dirs: ProjectDirs) -> Self {
        let journal_path = project_dirs.data_dir().join("journal.json");
        let config_path = project_dirs.config_dir().join("config.toml");
        Self {
            project_dirs,
            config_path,
            journal_path,
        }
    }

    pub fn setup(package_name: &str) -> anyhow::Result<Self> {
        let project_dirs = ProjectDirs::from("com.github", "amderbar", package_name)
            .ok_or(anyhow!("Failed to get project directories."))?;
        let ws = WorkSpace::new(project_dirs);
        setup_dir(&ws.project_dirs.data_dir())?;
        setup_dir(&ws.project_dirs.config_dir())?;
        Ok(ws)
    }

    pub fn load_config(&self) -> anyhow::Result<Config> {
        Config::new(&self.config_path)
    }
}

fn setup_dir(dir_path: &Path) -> anyhow::Result<()> {
    match create_dir_all(dir_path) {
        Ok(_) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(()),
        Err(e) => Err(anyhow!(e)),
    }
}
