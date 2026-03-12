use regex::Regex;
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Deserialize)]
struct RulesFile {
    providers: HashMap<String, Provider>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Provider {
    url_pattern: String,
    complete_provider: Option<bool>,
    #[serde(default)]
    rules: Vec<String>,
    #[serde(default)]
    raw_rules: Vec<String>,
    #[serde(default)]
    referral_marketing: Vec<String>,
    #[serde(default)]
    exceptions: Vec<String>,
    #[serde(default)]
    redirections: Vec<String>,
}

struct CompiledProvider {
    url_pattern: Regex,
    complete_provider: bool,
    rules: Vec<Regex>,
    raw_rules: Vec<Regex>,
    referral_marketing: Vec<Regex>,
    exceptions: Vec<Regex>,
    redirections: Vec<Regex>,
}

fn compile_provider(p: &Provider) -> Option<CompiledProvider> {
    let url_pattern = Regex::new(&p.url_pattern).ok()?;

    let compile_list = |patterns: &[String]| -> Vec<Regex> {
        patterns
            .iter()
            .filter_map(|s| Regex::new(s).ok())
            .collect()
    };

    let rules: Vec<Regex> = p
        .rules
        .iter()
        .filter_map(|r| Regex::new(&format!("(?:&|[/?#&])({r}=[^&]*)")).ok())
        .collect();

    let referral_marketing: Vec<Regex> = p
        .referral_marketing
        .iter()
        .filter_map(|r| Regex::new(&format!("(?:&|[/?#&])({r}=[^&]*)")).ok())
        .collect();

    Some(CompiledProvider {
        url_pattern,
        complete_provider: p.complete_provider.unwrap_or(false),
        rules,
        raw_rules: compile_list(&p.raw_rules),
        referral_marketing,
        exceptions: compile_list(&p.exceptions),
        redirections: compile_list(&p.redirections),
    })
}

pub struct Cleaner {
    providers: Vec<CompiledProvider>,
}

impl Cleaner {
    pub fn from_json(data: &str) -> Self {
        let rules_file: RulesFile = serde_json::from_str(data).expect("failed to parse rules");
        let providers = rules_file
            .providers
            .values()
            .filter_map(|p| compile_provider(p))
            .collect();
        Cleaner { providers }
    }

    pub fn clean(&self, url: &str) -> String {
        clean_url(url, &self.providers)
    }
}

fn clean_url(url: &str, providers: &[CompiledProvider]) -> String {
    let mut result = url.to_string();

    for provider in providers {
        if !provider.url_pattern.is_match(&result) {
            continue;
        }

        if provider
            .exceptions
            .iter()
            .any(|ex| ex.is_match(&result))
        {
            continue;
        }

        if provider.complete_provider {
            continue;
        }

        for redir in &provider.redirections {
            if let Some(caps) = redir.captures(&result) {
                if let Some(target) = caps.get(1) {
                    let decoded = url_decode(target.as_str());
                    result = decoded;
                    return clean_url(&result, providers);
                }
            }
        }

        for rule in &provider.rules {
            while let Some(m) = rule.find(&result) {
                result = remove_param(&result, m);
            }
        }

        for rule in &provider.referral_marketing {
            while let Some(m) = rule.find(&result) {
                result = remove_param(&result, m);
            }
        }

        for raw in &provider.raw_rules {
            result = raw.replace_all(&result, "").to_string();
        }
    }

    result
}

fn remove_param(url: &str, m: regex::Match) -> String {
    let start = m.start();
    let end = m.end();
    let leading_char = url.as_bytes()[start] as char;

    let mut new_url = url[..start].to_string();
    let rest = &url[end..];

    if !rest.is_empty() && leading_char == '?' {
        new_url.push('?');
        if rest.starts_with('&') {
            new_url.push_str(&rest[1..]);
        } else {
            new_url.push_str(rest);
        }
    } else {
        new_url.push_str(rest);
    }

    new_url
}

fn url_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(hex) = u8::from_str_radix(
                std::str::from_utf8(&bytes[i + 1..i + 3]).unwrap_or(""),
                16,
            ) {
                result.push(hex as char);
                i += 3;
                continue;
            }
        }
        result.push(bytes[i] as char);
        i += 1;
    }
    result
}
