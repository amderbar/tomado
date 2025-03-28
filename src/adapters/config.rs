use std::{env, path::PathBuf};

pub struct Config {
    pub editor: String,
}

impl Config {
    pub fn new(_config_path: &PathBuf) -> anyhow::Result<Self> {
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
