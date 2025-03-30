use std::{
    fs,
    io::{self, Error, ErrorKind, IsTerminal, Read, Write},
    process::Command,
};

use chrono::{DateTime, Local, Utc};
use tempfile::NamedTempFile;

use crate::{
    adapters::config::Config,
    entities::todo_matter::{TodoMatter, TodoMatterContents},
    ports::TodoRepository,
};

pub fn add_matter(
    repo: &impl TodoRepository,
    title: String,
    is_set_detail: bool,
    priority: Option<i8>,
    due: Option<DateTime<Utc>>,
) -> io::Result<()> {
    let detail = if is_set_detail {
        if io::stdin().is_terminal() {
            println!("-- Please input the detail of the task. --");
        }
        let mut detail = String::new();
        io::stdin().read_to_string(&mut detail)?;
        Some(detail)
    } else {
        None
    };

    let contents = TodoMatterContents::new(title)
        .set_priority(priority)
        .set_due(due)
        .set_detail(detail);

    repo.register(contents)
}

pub fn edit_matter(
    config: &Config,
    repo: &impl TodoRepository,
    number: usize,
    title: Option<String>,
    is_set_detail: bool,
    priority: Option<i8>,
    due: Option<DateTime<Utc>>,
    done: bool,
) -> io::Result<()> {
    let matter = repo.find(number)?;

    let new_detail = if is_set_detail {
        let new_detail = edit_matter_detail(config, &matter)?;
        Some(new_detail)
    } else {
        None
    };

    let contents = matter
        .contents
        .clone()
        .set_title(title)
        .set_priority(priority)
        .set_due(due)
        .set_done(if done { Some(()) } else { None })
        .set_detail(new_detail);

    repo.update(number, contents)
}

fn edit_matter_detail(config: &Config, matter: &TodoMatter) -> io::Result<String> {
    let temp_predix = format!("TODO_{}_DETAIL_EDITTING_", matter.number);
    let mut tempfile = NamedTempFile::with_prefix(temp_predix)?;
    write!(tempfile, "{}", matter.contents.detail)?;
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

pub fn done_matter(repo: &impl TodoRepository, number: usize) -> io::Result<()> {
    let matter = repo.find(number)?;
    if matter.is_done() {
        return Ok(());
    }

    let contents = matter.contents.clone().set_done(Some(()));
    repo.update(number, contents)
}

pub fn list_matters(repo: &impl TodoRepository) -> io::Result<()> {
    let matters = repo.list()?;

    if matters.is_empty() {
        println!("Task list is empty.");
    } else {
        for (i, matter) in matters.iter().enumerate() {
            println!("{}: {}", i + 1, matter);
        }
    }
    Ok(())
}

pub fn view_matter(repo: &impl TodoRepository, number: usize) -> io::Result<()> {
    let matter = repo.find(number)?;

    println!("{}: {}", number, matter);
    if let Some(p) = matter.contents.priority {
        println!("Priority: {}", p);
    }
    if let Some(due) = matter.contents.due {
        println!("Due: {}", due.with_timezone(&Local));
    }
    println!("--");
    println!("{}", matter.contents.detail);
    println!("Created At: {}", matter.created_at.with_timezone(&Local));
    if let Some(updated_at) = matter.updated_at {
        println!("Updated At: {}", updated_at.with_timezone(&Local));
    }

    Ok(())
}
