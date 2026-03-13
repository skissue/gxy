use crate::browser::BrowserEntry;
use crate::message::{Action, Message};

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

    pub fn update(&mut self, msg: Message) -> Action {
        match msg {
            Message::ToggleCleaning => {
                self.cleaning_enabled = !self.cleaning_enabled;
                Action::None
            }
            Message::OpenBrowserPicker => {
                self.show_browser_picker = true;
                Action::None
            }
            Message::CloseBrowserPicker => {
                self.show_browser_picker = false;
                Action::None
            }
            Message::SelectNext => {
                if self.selected_browser + 1 < self.browsers.len() {
                    self.selected_browser += 1;
                }
                Action::None
            }
            Message::SelectPrevious => {
                if self.selected_browser > 0 {
                    self.selected_browser -= 1;
                }
                Action::None
            }
            Message::ConfirmSelection => {
                if let Some(entry) = self.browsers.get(self.selected_browser) {
                    Action::OpenUrl {
                        exec: entry.exec.clone(),
                        url: self.active_url().to_string(),
                    }
                } else {
                    Action::None
                }
            }
            Message::Quit => {
                self.should_quit = true;
                Action::Quit
            }
        }
    }
}
