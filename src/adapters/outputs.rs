use core::fmt;

use chrono::Local;
use tomado::entities::todo_matter::TodoMatter;

#[derive(Debug)]
pub struct OneLineDisplay(TodoMatter);

impl From<TodoMatter> for OneLineDisplay {
    fn from(value: TodoMatter) -> Self {
        Self(value)
    }
}

impl fmt::Display for OneLineDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let matter = &self.0;
        let contents = &matter.contents;
        let created_at = matter.created_at.with_timezone(&Local).format("%F");
        let done_mark = if contents.done { "x" } else { " " };
        let priority = contents
            .priority
            .map(|p| format!("({}) ", p))
            .unwrap_or("".to_string());
        let due = contents.due
            .map(|d| format!("due:{} ", d.with_timezone(&Local).format("%F")))
            .unwrap_or("".to_string());
        write!(
            f,
            "{:>2}[{}] {}{}{:<50} [{}]",
            matter.number, done_mark, priority, due, contents.title, created_at
        )
    }
}

pub struct DetailedDisplay(TodoMatter);

impl From<TodoMatter> for DetailedDisplay {
    fn from(value: TodoMatter) -> Self {
        Self(value)
    }
}

impl fmt::Display for DetailedDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let matter = &self.0;
        let contents = &matter.contents;
        let done_mark = if contents.done { "x" } else { " " };
        writeln!(f, "{:>2}[{}] {:<50}", matter.number, done_mark, contents.title)?;
        if let Some(p) = contents.priority {
            writeln!(f, "Priority: {}", p)?;
        }
        if let Some(due) = contents.due {
            writeln!(f, "Due: {}", due.with_timezone(&Local).format("%F"))?;
        }
        writeln!(f, "--")?;
        writeln!(f, "{}", contents.detail)?;
        writeln!(f, "Created At: {}", matter.created_at.with_timezone(&Local))?;
        if let Some(updated_at) = matter.updated_at {
            writeln!(f, "Updated At: {}", updated_at.with_timezone(&Local))?;
        }
        Ok(())
    }
}
