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

    // Rules get wrapped: each rule name becomes a pattern matching a query/path parameter
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

fn clean_url(url: &str, providers: &[CompiledProvider]) -> String {
    let mut result = url.to_string();

    for provider in providers {
        if !provider.url_pattern.is_match(&result) {
            continue;
        }

        // Check exceptions
        if provider
            .exceptions
            .iter()
            .any(|ex| ex.is_match(&result))
        {
            continue;
        }

        // Complete provider means block entirely — for CLI we just return as-is
        if provider.complete_provider {
            continue;
        }

        // Check redirections first
        for redir in &provider.redirections {
            if let Some(caps) = redir.captures(&result) {
                if let Some(target) = caps.get(1) {
                    let decoded = url_decode(target.as_str());
                    result = decoded;
                    // Recurse with the new URL
                    return clean_url(&result, providers);
                }
            }
        }

        // Apply rules (remove matching query parameters)
        for rule in &provider.rules {
            while let Some(m) = rule.find(&result) {
                // Keep the leading separator if it's '?' — we need to replace it with '?'
                // for the next param, or remove entirely
                let start = m.start();
                let end = m.end();
                let leading_char = result.as_bytes()[start] as char;

                // Remove the matched parameter
                let mut new_url = result[..start].to_string();
                let rest = &result[end..];

                if !rest.is_empty() && leading_char == '?' {
                    // There are more params after; keep the '?'
                    new_url.push('?');
                    // rest starts with '&' if there are more params; skip it
                    if rest.starts_with('&') {
                        new_url.push_str(&rest[1..]);
                    } else {
                        new_url.push_str(rest);
                    }
                } else {
                    new_url.push_str(rest);
                }

                result = new_url;
            }
        }

        // Apply referral marketing rules (same logic as rules)
        for rule in &provider.referral_marketing {
            while let Some(m) = rule.find(&result) {
                let start = m.start();
                let end = m.end();
                let leading_char = result.as_bytes()[start] as char;

                let mut new_url = result[..start].to_string();
                let rest = &result[end..];

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

                result = new_url;
            }
        }

        // Apply raw rules
        for raw in &provider.raw_rules {
            result = raw.replace_all(&result, "").to_string();
        }
    }

    result
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

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: bouncer <url>");
        std::process::exit(1);
    }

    let data = include_str!("../data.minify.json");
    let rules_file: RulesFile = serde_json::from_str(data).expect("failed to parse rules");

    let providers: Vec<CompiledProvider> = rules_file
        .providers
        .values()
        .filter_map(|p| compile_provider(p))
        .collect();

    let cleaned = clean_url(&args[1], &providers);
    println!("{cleaned}");
}
