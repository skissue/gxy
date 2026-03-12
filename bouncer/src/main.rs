mod app;
mod rules;
mod ui;

use std::io::{self, stdout};

use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{Terminal, backend::CrosstermBackend};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: bouncer <url>");
        std::process::exit(1);
    }

    let data = include_str!("../data.minify.json");
    let cleaner = rules::Cleaner::from_json(data);
    let original_url = args[1].clone();
    let cleaned_url = cleaner.clean(&original_url);

    let mut app = app::App::new(original_url, cleaned_url);

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Event loop
    let result = run(&mut terminal, &mut app);

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    if let Err(err) = result {
        eprintln!("Error: {err}");
        std::process::exit(1);
    }

    Ok(())
}

fn run(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut app::App,
) -> io::Result<()> {
    loop {
        terminal.draw(|frame| ui::draw(frame, app))?;

        if let Event::Key(key) = event::read()? {
            if key.kind != KeyEventKind::Press {
                continue;
            }
            match key.code {
                KeyCode::Char('q') | KeyCode::Esc => {
                    app.should_quit = true;
                    return Ok(());
                }
                KeyCode::Char('c') => {
                    app.cleaning_enabled = !app.cleaning_enabled;
                }
                KeyCode::Enter => {
                    let url = app.active_url().to_string();
                    // Restore terminal before spawning
                    disable_raw_mode()?;
                    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
                    terminal.show_cursor()?;

                    std::process::Command::new("xdg-open")
                        .arg(&url)
                        .spawn()
                        .ok();

                    std::process::exit(0);
                }
                _ => {}
            }
        }
    }
}
