use ratatui::{
    Frame,
    layout::{Constraint, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph, Wrap},
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

    // Footer
    let footer_block = Block::default()
        .borders(Borders::ALL);

    let footer = Paragraph::new(Line::from(vec![
        Span::styled("  [c]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Toggle cleaning   ", Style::default().add_modifier(Modifier::DIM)),
        Span::styled("[Enter]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Open URL   ", Style::default().add_modifier(Modifier::DIM)),
        Span::styled("[q]", Style::default().add_modifier(Modifier::DIM)),
        Span::styled(" Quit", Style::default().add_modifier(Modifier::DIM)),
    ]))
    .block(footer_block);
    frame.render_widget(footer, chunks[1]);
}
