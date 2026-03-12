pub struct App {
    pub original_url: String,
    pub cleaned_url: String,
    pub cleaning_enabled: bool,
    pub should_quit: bool,
}

impl App {
    pub fn new(original_url: String, cleaned_url: String) -> Self {
        Self {
            original_url,
            cleaned_url,
            cleaning_enabled: true,
            should_quit: false,
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
