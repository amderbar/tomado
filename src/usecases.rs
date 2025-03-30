use std::io;

use chrono::{DateTime, Local, Utc};

use crate::{
    entities::todo_matter::{TodoMatter, TodoMatterContents},
    ports::TodoRepository,
};

pub fn add_matter(
    repo: &impl TodoRepository,
    title: String,
    detail: Option<String>,
    priority: Option<i8>,
    due: Option<DateTime<Utc>>,
) -> io::Result<()> {
    let contents = TodoMatterContents::new(title)
        .set_priority(priority)
        .set_due(due)
        .set_detail(detail);

    repo.register(contents)
}

pub fn edit_matter(
    repo: &impl TodoRepository,
    matter: TodoMatter,
    title: Option<String>,
    detail: Option<String>,
    priority: Option<i8>,
    due: Option<DateTime<Utc>>,
    done: bool,
) -> io::Result<()> {
    let contents = matter
        .contents
        .clone()
        .set_title(title)
        .set_priority(priority)
        .set_due(due)
        .set_done(if done { Some(()) } else { None })
        .set_detail(detail);

    repo.update(matter.number, contents)
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
