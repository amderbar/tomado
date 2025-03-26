pub use clap::Parser;
use clap::Subcommand;

/// [x] Manage your To-Do and Work time with @TomaDo
#[derive(Debug, Parser)]
#[command(name = "TomaDo", version, about, long_about)]
pub struct CommandLineArgs {
    #[command(subcommand)]
    pub action: Action,
}

#[derive(Debug, Subcommand)]
pub enum Action {
    /// make a new todo.
    Add {
        /// The task description text.
        title: String,
    },
    /// make a todo done. Alias of 'edit ${ID} --done'.
    Done {
        /// The task ID.
        number: usize,
    },
    /// edit a todo.
    Edit {
        /// The task ID.
        number: usize,
    },
    /// display a todo list.
    List,
    /// display today's todo list.
    Today,
    /// view a todo detail.
    View {
        /// The task ID.
        number: usize,
    },
    /// throw a todo to trash.
    Trash {
        /// The task ID.
        number: usize,
    },
    /// display or edit TomaDo configuration.
    Config,
}
