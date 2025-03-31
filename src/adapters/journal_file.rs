use std::{
    fs::{File, OpenOptions},
    io::{self, Error, ErrorKind, Seek, SeekFrom},
    path::Path,
};

use serde::{Deserialize, Serialize};
use tomado::{
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

    fn read_schema(&self) -> io::Result<JournalSchema> {
        read_journal(&self.file)
    }

    fn save_schema(&self, schema: &JournalSchema) -> io::Result<()> {
        write_journal(&self.file, schema)
    }
}

impl TodoRepository for TodoRepositoryFile {
    fn list(&self) -> io::Result<Vec<TodoMatter>> {
        let schema = self.read_schema()?;
        Ok(schema.matters)
    }

    fn find(&self, number: usize) -> io::Result<TodoMatter> {
        let schema = self.read_schema()?;
        let matter = schema.find(number)?;
        Ok(matter.clone())
    }

    fn register(&self, matter_contents: TodoMatterContents) -> io::Result<()> {
        let mut schema = self.read_schema()?;
        schema.insert(matter_contents)?;
        self.save_schema(&schema)
    }

    fn update(&self, number: usize, matter_contents: TodoMatterContents) -> io::Result<()> {
        let mut schema = self.read_schema()?;
        schema.update(number, matter_contents)?;
        self.save_schema(&schema)
    }

    fn delete(&self, number: usize) -> io::Result<()> {
        let mut schema = self.read_schema()?;
        schema.delete(number)?;
        self.save_schema(&schema)
    }
}

fn read_journal(mut file: &File) -> io::Result<JournalSchema> {
    // Rewind the file before.
    file.seek(SeekFrom::Start(0))?;

    let matters = match serde_json::from_reader(file) {
        Ok(matters) => matters,
        Err(e) if e.is_eof() => JournalSchema::default(),
        Err(e) => Err(e)?,
    };

    // Rewind the file after.
    file.seek(SeekFrom::Start(0))?;
    Ok(matters)
}

fn write_journal(file: &File, schema: &JournalSchema) -> io::Result<()> {
    file.set_len(0)?;
    serde_json::to_writer(file, &schema)?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Default)]
struct JournalSchema {
    seq: JournalSequence,
    matters: Vec<TodoMatter>,
}

impl JournalSchema {
    fn insert(&mut self, matter_contents: TodoMatterContents) -> io::Result<()> {
        let number = self.seq.next().ok_or(Error::new(
            ErrorKind::Other,
            "reached maximum value of sequence",
        ))?;
        self.matters
            .push(TodoMatter::new(number + 1, matter_contents));
        Ok(())
    }

    fn update(&mut self, number: usize, matter_contents: TodoMatterContents) -> io::Result<()> {
        let matter = self.find(number)?;
        self.matters[number - 1] = matter.clone().update(matter_contents);
        Ok(())
    }

    fn find(&self, number: usize) -> io::Result<&TodoMatter> {
        self.matters.get(number - 1).ok_or(Error::new(
            ErrorKind::NotFound,
            format!("Todo {} is not found", number),
        ))
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq)]
struct JournalSequence {
    next: Option<usize>,
}

impl JournalSequence {
    fn increment(&mut self) {
        self.next = self.next.and_then(|n| n.checked_add(1));
    }
}

impl Default for JournalSequence {
    fn default() -> Self {
        Self {
            next: Some(Default::default()),
        }
    }
}

impl Iterator for JournalSequence {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next;
        self.increment();
        next
    }
}
