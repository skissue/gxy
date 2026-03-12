use ratatui::{
    Frame,
    layout::{Constraint, Flex, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, HighlightSpacing, List, ListItem, ListState, Paragraph, Wrap},
};

use crate::app::App;

pub fn draw(frame: &mut Frame, app: &App) {
    let chunks = Layout::vertical([
        Constraint::Min(5),
        Constraint::Length(3),
    ])
    .split(frame.area());

    // Main block
    let main_block = Block::default()
        .borders(Borders::ALL)
        .title(" bouncer ");

    let active_label = if app.cleaning_enabled {
        "Cleaned URL"
    } else {
        "Original URL (cleaning disabled)"
    };

    let status_line = if app.original_url != app.cleaned_url {
        Line::from(vec![
            Span::raw("  Status: "),
            Span::styled("✔ Cleaned", Style::default().fg(Color::Green)),
        ])
    } else {
        Line::from(vec![
            Span::raw("  Status: "),
            Span::styled("— No changes", Style::default().fg(Color::Yellow)),
        ])
    };

    let text = vec![
        Line::from(""),
        Line::from(Span::styled(
            "  Original URL:",
            Style::default().add_modifier(Modifier::BOLD),
        )),
        Line::from(format!("  {}", app.original_url)),
        Line::from(""),
        Line::from(Span::styled(
            format!("  {active_label}:"),
            Style::default().add_modifier(Modifier::BOLD),
        )),
        Line::from(Span::styled(
            format!("  {}", app.active_url()),
            if app.cleaning_enabled {
                Style::default().fg(Color::Green)
            } else {
                Style::default().fg(Color::White)
            },
        )),
        Line::from(""),
        status_line,
    ];

    let paragraph = Paragraph::new(text)
        .block(main_block)
        .wrap(Wrap { trim: false });
    frame.render_widget(paragraph, chunks[0]);

    if app.show_browser_picker {
        draw_browser_picker(frame, app, chunks[1]);
    } else {
        draw_footer(frame, chunks[1]);
    }
}

fn centered_rect(width: u16, height: u16, area: Rect) -> Rect {
    let vertical = Layout::vertical([Constraint::Length(height)])
        .flex(Flex::Center)
        .split(area);
    Layout::horizontal([Constraint::Length(width)])
        .flex(Flex::Center)
        .split(vertical[0])[0]
}

fn draw_browser_picker(frame: &mut Frame, app: &App, _area: ratatui::layout::Rect) {
    let list_height = app.browsers.len() as u16 + 2; // +2 for borders
    let footer_height: u16 = 3;
    let total_height = list_height + footer_height;
    let width = 40;

    let popup = centered_rect(width, total_height, frame.area());

    frame.render_widget(Clear, popup);

    let picker_chunks = Layout::vertical([
        Constraint::Length(list_height),
        Constraint::Length(footer_height),
    ])
    .split(popup);

    let items: Vec<ListItem> = app
        .browsers
        .iter()
        .map(|b| {
            let label = if b.is_default {
                format!("  {} (default)", b.name)
            } else {
                format!("  {}", b.name)
            };
            ListItem::new(label)
        })
        .collect();

    let list = List::new(items)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Select Browser "),
        )
        .highlight_style(
            Style::default()
                .fg(Color::Black)
                .bg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        )
        .highlight_symbol("▶ ")
        .highlight_spacing(HighlightSpacing::Always);

    let mut state = ListState::default();
    state.select(Some(app.selected_browser));
    frame.render_stateful_widget(list, picker_chunks[0], &mut state);

    // Picker footer
    let footer_block = Block::default().borders(Borders::ALL);
    let footer = Paragraph::new(Line::from(vec![
        Span::styled("  [↑↓]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Select   ", Style::default().add_modifier(Modifier::DIM)),
        Span::styled("[Enter]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Open   ", Style::default().add_modifier(Modifier::DIM)),
        Span::styled("[Esc]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Back", Style::default().add_modifier(Modifier::DIM)),
    ]))
    .block(footer_block);
    frame.render_widget(footer, picker_chunks[1]);
}

fn draw_footer(frame: &mut Frame, area: ratatui::layout::Rect) {
    let footer_block = Block::default().borders(Borders::ALL);
    let footer = Paragraph::new(Line::from(vec![
        Span::styled("  [c]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Toggle cleaning   ", Style::default().add_modifier(Modifier::DIM)),
        Span::styled("[Enter]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Open URL   ", Style::default().add_modifier(Modifier::DIM)),
        Span::styled("[q]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Quit", Style::default().add_modifier(Modifier::DIM)),
    ]))
    .block(footer_block);
    frame.render_widget(footer, area);
}
