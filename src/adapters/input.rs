use std::{fs, io::{self, IsTerminal, Read, Write}, process::Command};

use tempfile::NamedTempFile;

use super::config::Config;

pub fn with_edit_tempfile(prefix: &str, initail_contents: &str, config: &Config) -> io::Result<String> {
    let mut tempfile = NamedTempFile::with_prefix(prefix)?;
    write!(tempfile, "{}", initail_contents)?;
    let temp_path = tempfile.path();

    let status = Command::new(&config.editor).arg(temp_path).status()?;
    if !status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Editor did not exit successfully",
        ));
    }
    // エディタで編集された内容が最初に作ったハンドルには反映されないことがあるので、パスを指定して読む
    let new_contents = fs::read_to_string(&temp_path)?;
    Ok(new_contents)
}

pub fn from_stdin(msg: &str) -> io::Result<String> {
    if io::stdin().is_terminal() {
        println!("{}", msg);
    }
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    Ok(input)
}
