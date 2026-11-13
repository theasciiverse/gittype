//! A title's `page` (its `<id>.md` in hub) as Ratatui text. hub's build only
//! lets through the subset in SCHEMA.md: ## and ### headings, paragraphs,
//! lists, bold, italic, inline code and links. Anything else is shown plain.

use pulldown_cmark::{Event, HeadingLevel, Parser, Tag, TagEnd};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span, Text};

use crate::theme;

pub fn render(md: &str) -> Text<'static> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    let mut cur: Vec<Span<'static>> = Vec::new();
    let mut style = Style::default().fg(theme::TEXT);
    let mut stack: Vec<Style> = Vec::new();
    let mut list_depth = 0usize;
    let mut ordered: Vec<Option<u64>> = Vec::new();
    let mut heading = false;

    let flush = |lines: &mut Vec<Line<'static>>, cur: &mut Vec<Span<'static>>| {
        if !cur.is_empty() {
            lines.push(Line::from(std::mem::take(cur)));
        }
    };

    for ev in Parser::new(md) {
        match ev {
            Event::Start(Tag::Heading { level, .. }) => {
                flush(&mut lines, &mut cur);
                if !lines.is_empty() {
                    lines.push(Line::default());
                }
                let mark = if level == HeadingLevel::H2 { "## " } else { "### " };
                cur.push(Span::styled(mark, Style::default().fg(theme::GREEN)));
                stack.push(style);
                style = Style::default().fg(theme::TEXT_HI).add_modifier(Modifier::BOLD);
                heading = true;
            }
            Event::End(TagEnd::Heading(_)) => {
                flush(&mut lines, &mut cur);
                style = stack.pop().unwrap_or(style);
                heading = false;
            }
            Event::Start(Tag::Paragraph) => {
                if list_depth == 0 {
                    flush(&mut lines, &mut cur);
                    if !lines.is_empty() {
                        lines.push(Line::default());
                    }
                }
            }
            Event::End(TagEnd::Paragraph) => {
                if list_depth == 0 {
                    flush(&mut lines, &mut cur);
                }
            }
            Event::Start(Tag::List(first)) => {
                flush(&mut lines, &mut cur);
                if list_depth == 0 && !lines.is_empty() {
                    lines.push(Line::default());
                }
                list_depth += 1;
                ordered.push(first);
            }
            Event::End(TagEnd::List(_)) => {
                flush(&mut lines, &mut cur);
                list_depth = list_depth.saturating_sub(1);
                ordered.pop();
            }
            Event::Start(Tag::Item) => {
                flush(&mut lines, &mut cur);
                let indent = "  ".repeat(list_depth.saturating_sub(1));
                let bullet = match ordered.last_mut() {
                    Some(Some(n)) => {
                        let b = format!("{indent}{n}. ");
                        *n += 1;
                        b
                    }
                    _ => format!("{indent}• "),
                };
                cur.push(Span::styled(bullet, Style::default().fg(theme::GREEN)));
            }
            Event::End(TagEnd::Item) => flush(&mut lines, &mut cur),
            Event::Start(Tag::Strong) => {
                stack.push(style);
                style = style.fg(theme::TEXT_HI).add_modifier(Modifier::BOLD);
            }
            Event::Start(Tag::Emphasis) => {
                stack.push(style);
                style = style.add_modifier(Modifier::ITALIC);
            }
            Event::Start(Tag::Link { .. }) => {
                stack.push(style);
                style = style.fg(theme::AMBER).add_modifier(Modifier::UNDERLINED);
            }
            Event::End(TagEnd::Strong | TagEnd::Emphasis | TagEnd::Link) => {
                style = stack.pop().unwrap_or(style);
            }
            Event::Code(c) => cur.push(Span::styled(c.to_string(), Style::default().fg(theme::AMBER_SOFT))),
            Event::Text(t) => {
                let t = if heading { t.to_uppercase() } else { t.to_string() };
                cur.push(Span::styled(t, style));
            }
            Event::SoftBreak => cur.push(Span::styled(" ", style)),
            Event::HardBreak => flush(&mut lines, &mut cur),
            _ => {}
        }
    }
    flush(&mut lines, &mut cur);
    Text::from(lines)
}

#[cfg(test)]
mod tests {
    #[test]
    fn renders_subset() {
        let t = super::render("Intro *here*.\n\n## Controls\n\n- **q**: quit\n- `--x`: flag\n");
        let plain: Vec<String> = t.lines.iter().map(|l| l.spans.iter().map(|s| s.content.as_ref()).collect()).collect();
        assert_eq!(plain, vec!["Intro here.", "", "## CONTROLS", "", "• q: quit", "• --x: flag"]);
    }
}
