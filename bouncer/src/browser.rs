use freedesktop_desktop_entry::{DesktopEntry, Iter, default_paths, get_languages_from_env};

pub struct BrowserEntry {
    pub desktop_id: String,
    pub name: String,
    pub exec: String,
    pub is_default: bool,
}

pub fn discover_browsers(own_desktop_id: &str) -> Vec<BrowserEntry> {
    let locales = get_languages_from_env();
    let default_browser = get_default_browser();

    let mut browsers: Vec<BrowserEntry> = Iter::new(default_paths())
        .filter_map(|path| {
            let entry = DesktopEntry::from_path(&path, Some(&locales)).ok()?;

            if entry.id() == own_desktop_id {
                return None;
            }

            if entry.type_() != Some("Application") {
                return None;
            }

            if entry.hidden() || entry.no_display() {
                return None;
            }

            let mime_types = entry.mime_type()?;
            if !mime_types.contains(&"x-scheme-handler/https") {
                return None;
            }

            let name = entry.name(&locales)?.to_string();
            let exec = entry.exec()?.to_string();
            let desktop_id = entry.id().to_string();

            let is_default = default_browser
                .as_ref()
                .map(|d| d.trim_end_matches(".desktop") == desktop_id)
                .unwrap_or(false);

            Some(BrowserEntry {
                desktop_id,
                name,
                exec,
                is_default,
            })
        })
        .collect();

    browsers.sort_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()));

    browsers
}

fn get_default_browser() -> Option<String> {
    let output = std::process::Command::new("xdg-mime")
        .args(["query", "default", "x-scheme-handler/https"])
        .output()
        .ok()?;
    let s = String::from_utf8(output.stdout).ok()?;
    let trimmed = s.trim().to_string();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed)
    }
}

pub fn open_url_with(exec: &str, url: &str) {
    let replaced = exec
        .replace("%u", url)
        .replace("%U", url)
        .replace("%f", url)
        .replace("%F", url);

    // Strip remaining field codes
    let stripped = stripped_field_codes(&replaced);

    let parts: Vec<&str> = stripped.split_whitespace().collect();
    if let Some((cmd, args)) = parts.split_first() {
        std::process::Command::new(cmd)
            .args(args)
            .spawn()
            .ok();
    }
}

fn stripped_field_codes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            if let Some(&next) = chars.peek() {
                match next {
                    'i' | 'c' | 'k' | 'd' | 'D' | 'n' | 'N' | 'v' | 'm' => {
                        chars.next();
                        continue;
                    }
                    '%' => {
                        chars.next();
                        result.push('%');
                        continue;
                    }
                    _ => {}
                }
            }
        }
        result.push(c);
    }
    result
}
