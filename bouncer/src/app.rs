use crate::browser::BrowserEntry;

pub struct App {
    pub original_url: String,
    pub cleaned_url: String,
    pub cleaning_enabled: bool,
    pub should_quit: bool,
    pub browsers: Vec<BrowserEntry>,
    pub selected_browser: usize,
    pub show_browser_picker: bool,
}

impl App {
    pub fn new(original_url: String, cleaned_url: String, browsers: Vec<BrowserEntry>) -> Self {
        let default_idx = browsers
            .iter()
            .position(|b| b.is_default)
            .unwrap_or(0);

        Self {
            original_url,
            cleaned_url,
            cleaning_enabled: true,
            should_quit: false,
            browsers,
            selected_browser: default_idx,
            show_browser_picker: false,
        }
    }

    pub fn active_url(&self) -> &str {
        if self.cleaning_enabled {
            &self.cleaned_url
        } else {
            &self.original_url
        }
    }
}
