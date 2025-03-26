use core::fmt;
use std::{
    fs::{self, File, OpenOptions},
    io::{self, Error, ErrorKind, Read, Seek, SeekFrom, Write},
    path::Path,
    process::Command,
};

use crate::config::Config;
use chrono::{
    serde::{ts_seconds, ts_seconds_option},
    DateTime, Local, Utc,
};
use serde::{Deserialize, Serialize};
use tempfile::NamedTempFile;

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct TodoMatter {
    pub number: usize,
    pub title: String,
    pub detail: String,
    pub priority: Option<i8>,
    pub done: bool,

    #[serde(with = "ts_seconds_option")]
    pub due: Option<DateTime<Utc>>,

    #[serde(with = "ts_seconds")]
    pub created_at: DateTime<Utc>,

    #[serde(with = "ts_seconds_option")]
    pub updated_at: Option<DateTime<Utc>>,
}

impl TodoMatter {
    pub fn new(number: usize, title: String) -> Self {
        Self {
            number,
            title,
            priority: None,
            due: None,
            done: false,
            detail: String::new(),
            created_at: Utc::now(),
            updated_at: None,
        }
    }

    pub fn set_title(self, title: String) -> Self {
        Self { title, ..self }
    }

    pub fn set_priority(self, priority: i8) -> Self {
        Self {
            priority: Some(priority),
            ..self
        }
    }

    pub fn set_due(self, due: DateTime<Utc>) -> Self {
        Self {
            due: Some(due),
            ..self
        }
    }

    pub fn set_done(self) -> Self {
        Self { done: true, ..self }
    }

    pub fn set_undone(self) -> Self {
        Self {
            done: false,
            ..self
        }
    }

    pub fn set_detail(self, detail: String) -> Self {
        Self { detail, ..self }
    }

    pub fn set_updated_at(self) -> Self {
        Self {
            updated_at: Some(Utc::now()),
            ..self
        }
    }
}

impl fmt::Display for TodoMatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let created_at = self.created_at.with_timezone(&Local).format("%F %H:%M");
        let done_mark = if self.done { "x" } else { " " };
        write!(f, "[{}] {:<50} [{}]", done_mark, self.title, created_at)
    }
}

pub fn add_matter(
    journal_path: &Path,
    title: String,
    is_set_detail: bool,
    priority: Option<i8>,
    due: Option<DateTime<Utc>>,
) -> io::Result<()> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(journal_path)?;

    let mut matters = collect_matters(&file)?;

    let number = matters.len() + 1;
    let mut new_matter = TodoMatter::new(number, title);
    if let Some(p) = priority {
        new_matter = new_matter.set_priority(p);
    }
    if let Some(due) = due {
        new_matter = new_matter.set_due(due);
    }
    if is_set_detail {
        let mut detail = String::new();
        println!("Please input the detail of the task.");
        io::stdin().read_to_string(&mut detail)?;
        new_matter = new_matter.set_detail(detail);
    }

    matters.push(new_matter);
    serde_json::to_writer(file, &matters)?;

    Ok(())
}

pub fn edit_matter(
    config: &Config,
    journal_path: &Path,
    number: usize,
    title: Option<String>,
    is_set_detail: bool,
    priority: Option<i8>,
    due: Option<DateTime<Utc>>,
    done: bool,
) -> io::Result<()> {
    // Open the file.
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(journal_path)?;

    let mut matters = collect_matters(&file)?;

    match matters.get(number - 1) {
        Some(matter) => {
            let mut new_matter = matter.clone();
            if let Some(title) = title {
                new_matter = new_matter.set_title(title);
            }
            if let Some(p) = priority {
                new_matter = new_matter.set_priority(p);
            }
            if let Some(due) = due {
                new_matter = new_matter.set_due(due);
            }
            if done {
                new_matter = new_matter.set_done();
            }
            if is_set_detail {
                let new_detail = edit_matter_detail(config, &new_matter)?;
                new_matter = new_matter.set_detail(new_detail);
            }
            if &new_matter == matter {
                return Ok(());
            }
            matters[number - 1] = new_matter.set_updated_at();
        }
        None => return Err(Error::new(ErrorKind::InvalidInput, "Invalid ToDo Number")),
    }

    // Write the modified task list back into the file.
    file.set_len(0)?;
    serde_json::to_writer(file, &matters)?;
    Ok(())
}

fn edit_matter_detail(config: &Config, matter: &TodoMatter) -> io::Result<String> {
    let temp_predix = format!("TODO_{}_DETAIL_EDITTING_", matter.number);
    let mut tempfile = NamedTempFile::with_prefix(temp_predix)?;
    write!(tempfile, "{}", matter.detail)?;
    let temp_path = tempfile.path();

    let status = Command::new(&config.editor).arg(temp_path).status()?;
    if !status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Editor did not exit successfully",
        ));
    }
    // エディタで編集された内容が最初に作ったハンドルには反映されないことがあるので、パスを指定して読む
    let new_detail = fs::read_to_string(&temp_path)?;
    Ok(new_detail)
}

pub fn done_matter(journal_path: &Path, number: usize) -> io::Result<()> {
    // Open the file.
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(journal_path)?;

    let mut matters = collect_matters(&file)?;

    match matters.get(number - 1) {
        Some(matter) => {
            if !matter.done {
                let new_matter = matter.clone().set_done().set_updated_at();
                matters[number - 1] = new_matter;
            }
        }
        None => return Err(Error::new(ErrorKind::InvalidInput, "Invalid ToDo Number")),
    }

    // Write the modified task list back into the file.
    file.set_len(0)?;
    serde_json::to_writer(file, &matters)?;
    Ok(())
}

pub fn list_matters(journal_path: &Path) -> io::Result<()> {
    // Open the file.
    let file = OpenOptions::new().read(true).open(journal_path)?;

    let matters = collect_matters(&file)?;

    if matters.is_empty() {
        println!("Task list is empty.");
    } else {
        for (i, matter) in matters.iter().enumerate() {
            println!("{}: {}", i + 1, matter);
        }
    }
    Ok(())
}

pub fn view_matter(journal_path: &Path, number: usize) -> io::Result<()> {
    // Open the file.
    let file = OpenOptions::new().read(true).open(journal_path)?;

    let matters = collect_matters(&file)?;

    if number == 0 || number > matters.len() {
        return Err(Error::new(ErrorKind::InvalidInput, "Invalid ToDo Number"));
    }

    let matter = &matters[number - 1];
    println!("{}: {}", number, matter);
    if let Some(p) = matter.priority {
        println!("Priority: {}", p);
    }
    if let Some(due) = matter.due {
        println!("Due: {}", due.with_timezone(&Local));
    }
    println!("--");
    println!("{}", matter.detail);
    println!("Created At: {}", matter.created_at.with_timezone(&Local));
    if let Some(updated_at) = matter.updated_at {
        println!("Updated At: {}", updated_at.with_timezone(&Local));
    }

    Ok(())
}

fn collect_matters(mut file: &File) -> io::Result<Vec<TodoMatter>> {
    // Rewind the file before.
    file.seek(SeekFrom::Start(0))?;

    let matters = match serde_json::from_reader(file) {
        Ok(matters) => matters,
        Err(e) if e.is_eof() => Vec::new(),
        Err(e) => Err(e)?,
    };

    // Rewind the file after.
    file.seek(SeekFrom::Start(0))?;
    Ok(matters)
}
