use std::{
    fs::{File, OpenOptions},
    io::{self, Error, ErrorKind, Seek, SeekFrom},
    path::Path,
};

use crate::{
    entities::todo_matter::{TodoMatter, TodoMatterContents},
    ports::TodoRepository,
};

pub struct TodoRepositoryFile {
    file: File,
}

impl TodoRepositoryFile {
    pub fn new(journal_path: &Path) -> io::Result<Self> {
        let file = File::create_new(journal_path).or_else(|e| {
            if e.kind() == io::ErrorKind::AlreadyExists {
                OpenOptions::new().read(true).write(true).open(journal_path)
            } else {
                Err(e)
            }
        })?;
        Ok(Self { file })
    }

    fn read_all(&self) -> io::Result<Vec<TodoMatter>> {
        collect_matters(&self.file)
    }
}

impl TodoRepository for TodoRepositoryFile {
    fn list(&self) -> io::Result<Vec<TodoMatter>> {
        self.read_all()
    }

    fn find(&self, number: usize) -> io::Result<TodoMatter> {
        let matters = self.list()?;
        matters.get(number - 1).map(|m| m.clone()).ok_or(Error::new(
            ErrorKind::NotFound,
            format!("Todo {} is not found", number),
        ))
    }

    fn register(&self, matter_contents: TodoMatterContents) -> io::Result<()> {
        let mut matters = self.list()?;
        let number = matters.len() + 1;
        matters.push(TodoMatter::new(number, matter_contents));
        serde_json::to_writer(&self.file, &matters)?;
        Ok(())
    }

    fn update(&self, number: usize, matter_contents: TodoMatterContents) -> io::Result<()> {
        let mut matters = self.list()?;
        let matter = self.find(number)?;
        matters[number - 1] = matter.clone().update(matter_contents);
        serde_json::to_writer(&self.file, &matters)?;
        Ok(())
    }
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
