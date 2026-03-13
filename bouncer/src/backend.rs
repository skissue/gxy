pub mod tui;

use crate::app::App;

pub struct RunResult {
    pub exec: String,
    pub url: String,
}

pub trait Backend {
    fn run(&mut self, app: &mut App) -> Result<Option<RunResult>, Box<dyn std::error::Error>>;
}
