mod app;
mod backend;
mod browser;
mod message;
mod rules;
mod ui;

use backend::{Backend, tui::TuiBackend};

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

    let mut backend = TuiBackend;
    let result = backend.run(&mut app)?;

    if let Some(run_result) = result {
        browser::open_url_with(&run_result.exec, &run_result.url);
    }

    Ok(())
}
