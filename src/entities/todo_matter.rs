use core::fmt;

use chrono::{
    serde::{ts_seconds, ts_seconds_option},
    DateTime, Local, Utc,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct TodoMatter {
    pub number: usize,
    pub contents: TodoMatterContents,

    #[serde(with = "ts_seconds")]
    pub created_at: DateTime<Utc>,

    #[serde(with = "ts_seconds_option")]
    pub updated_at: Option<DateTime<Utc>>,
}

impl TodoMatter {
    pub fn new(number: usize, contents: TodoMatterContents) -> Self {
        Self {
            number,
            contents,
            created_at: Utc::now(),
            updated_at: None,
        }
    }

    pub fn update(self, contents: TodoMatterContents) -> Self {
        if contents == self.contents {
            self
        } else {
            Self {
                contents,
                updated_at: Some(Utc::now()),
                ..self
            }
        }
    }

    pub fn is_done(&self) -> bool {
        self.contents.done
    }
}

impl fmt::Display for TodoMatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let created_at = self.created_at.with_timezone(&Local).format("%F %H:%M");
        let done_mark = if self.contents.done { "x" } else { " " };
        write!(
            f,
            "[{}] {:<50} [{}]",
            done_mark, self.contents.title, created_at
        )
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct TodoMatterContents {
    pub title: String,
    pub detail: String,
    pub priority: Option<i8>,
    pub done: bool,
    pub due: Option<DateTime<Utc>>,
}

impl TodoMatterContents {
    pub fn new(title: String) -> Self {
        Self {
            title,
            priority: None,
            due: None,
            done: false,
            detail: String::new(),
        }
    }

    pub fn set_title(self, renewal: Option<String>) -> Self {
        match renewal {
            Some(title) => Self { title, ..self },
            None => self,
        }
    }

    pub fn set_priority(self, renewal: Option<i8>) -> Self {
        match renewal {
            Some(priority) => Self {
                priority: Some(priority),
                ..self
            },
            None => self,
        }
    }

    pub fn set_due(self, renewal: Option<DateTime<Utc>>) -> Self {
        match renewal {
            Some(due) => Self {
                due: Some(due),
                ..self
            },
            None => self,
        }
    }

    pub fn set_done(self, renewal: Option<()>) -> Self {
        match renewal {
            Some(_) => Self { done: true, ..self },
            None => self,
        }
    }

    pub fn set_detail(self, renewal: Option<String>) -> Self {
        match renewal {
            Some(detail) => Self { detail, ..self },
            None => self,
        }
    }
}
