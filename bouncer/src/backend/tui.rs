use std::io::{self, stdout};

use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{Terminal, backend::CrosstermBackend};

use crate::app::App;
use crate::message::{Action, Message};
use crate::ui;

use super::{Backend, RunResult};

pub struct TuiBackend;

impl Backend for TuiBackend {
    fn run(&mut self, app: &mut App) -> Result<Option<RunResult>, Box<dyn std::error::Error>> {
        enable_raw_mode()?;
        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;

        let result = event_loop(&mut terminal, app);

        disable_raw_mode()?;
        execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
        terminal.show_cursor()?;

        result
    }
}

fn event_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
) -> Result<Option<RunResult>, Box<dyn std::error::Error>> {
    loop {
        terminal.draw(|frame| ui::draw(frame, app))?;

        if let Event::Key(key) = event::read()? {
            if key.kind != KeyEventKind::Press {
                continue;
            }

            let msg = if app.show_browser_picker {
                match key.code {
                    KeyCode::Esc => Some(Message::CloseBrowserPicker),
                    KeyCode::Up | KeyCode::Char('k') => Some(Message::SelectPrevious),
                    KeyCode::Down | KeyCode::Char('j') => Some(Message::SelectNext),
                    KeyCode::Enter => Some(Message::ConfirmSelection),
                    _ => None,
                }
            } else {
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => Some(Message::Quit),
                    KeyCode::Char('c') => Some(Message::ToggleCleaning),
                    KeyCode::Enter => Some(Message::OpenBrowserPicker),
                    _ => None,
                }
            };

            if let Some(msg) = msg {
                match app.update(msg) {
                    Action::None => {}
                    Action::Quit => return Ok(None),
                    Action::OpenUrl { exec, url } => {
                        return Ok(Some(RunResult { exec, url }));
                    }
                }
            }
        }
    }
}
