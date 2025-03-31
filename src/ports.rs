use std::io;

use crate::entities::todo_matter::{TodoMatter, TodoMatterContents};

pub trait TodoRepository {
    fn list(&self) -> io::Result<Vec<TodoMatter>>;
    fn find(&self, number: usize) -> io::Result<TodoMatter>;
    fn register(&self, matter_contents: TodoMatterContents) -> io::Result<()>;
    fn update(&self, number: usize, matter_contents: TodoMatterContents) -> io::Result<()>;
    fn delete(&self, number: usize) -> io::Result<()>;
}

pub trait HasTodoRepository {
    type Repository: TodoRepository;
    fn repository(&self) -> Self::Repository;
}
