mod app;
mod browser;
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

    let browsers = browser::discover_browsers("bouncer");
    let mut app = app::App::new(original_url, cleaned_url, browsers);

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

            if app.show_browser_picker {
                match key.code {
                    KeyCode::Esc => {
                        app.show_browser_picker = false;
                    }
                    KeyCode::Up | KeyCode::Char('k') => {
                        if app.selected_browser > 0 {
                            app.selected_browser -= 1;
                        }
                    }
                    KeyCode::Down | KeyCode::Char('j') => {
                        if app.selected_browser + 1 < app.browsers.len() {
                            app.selected_browser += 1;
                        }
                    }
                    KeyCode::Enter => {
                        if let Some(entry) = app.browsers.get(app.selected_browser) {
                            let url = app.active_url().to_string();
                            let exec = entry.exec.clone();

                            disable_raw_mode()?;
                            execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
                            terminal.show_cursor()?;

                            browser::open_url_with(&exec, &url);
                            std::process::exit(0);
                        }
                    }
                    _ => {}
                }
            } else {
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => {
                        app.should_quit = true;
                        return Ok(());
                    }
                    KeyCode::Char('c') => {
                        app.cleaning_enabled = !app.cleaning_enabled;
                    }
                    KeyCode::Enter => {
                        app.show_browser_picker = true;
                    }
                    _ => {}
                }
            }
        }
    }
}
