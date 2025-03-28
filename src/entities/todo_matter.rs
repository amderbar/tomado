use core::fmt;

use chrono::{
    serde::{ts_seconds, ts_seconds_option},
    DateTime, Local, Utc,
};
use serde::{Deserialize, Serialize};

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
