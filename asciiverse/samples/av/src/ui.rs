//! Layout from the design: menu row on top, one title list on the left, a
//! store-page style detail pane on the right (preview, facts, actions, page).

use ratatui::Frame;
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Constraint, Layout, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, BorderType, Borders, Clear, List, ListItem, ListState, Paragraph, Widget, Wrap};

use crate::app::{App, Focus, Hit, Mine, Modal, ShowRow, Status};
use crate::catalog::Title;
use crate::cli::human;
use crate::downloads::{Download, Phase};
use crate::{md, theme};

// 26 + the 102-column preview keeps av inside 128 columns
const LIST_W: u16 = 26;

pub fn draw(f: &mut Frame, app: &mut App) {
    app.hits.clear();
    let area = f.area();
    f.render_widget(Block::default().style(Style::default().bg(theme::BG).fg(theme::TEXT)), area);
    if let Some(start) = app.intro {
        draw_intro(f.buffer_mut(), area, start.elapsed().as_secs_f64());
        return;
    }
    let [head, body, foot] = Layout::vertical([Constraint::Length(2), Constraint::Min(5), Constraint::Length(1)]).areas(area);
    draw_header(f, app, head);
    let [list, detail] = Layout::horizontal([Constraint::Length(LIST_W), Constraint::Min(20)]).areas(body);
    let card_h = if app.focus == Focus::Show { app.show_rows().len() as u16 + 4 } else { 3 };
    let [search, card, list] = Layout::vertical([Constraint::Length(3), Constraint::Length(card_h), Constraint::Min(3)]).areas(list);
    draw_search(f, app, search);
    draw_show(f, app, card);
    draw_list(f, app, list);
    app.hits.push((detail, Hit::Detail));
    app.page_view = None;
    // one copy a frame: drawing it also changes app (hits, scroll)
    match app.current().cloned() {
        Some(t) => draw_detail(f, app, &t, detail),
        None => {
            let msg = if app.titles.is_empty() && app.is_loading() {
                "Loading the catalog..."
            } else if app.titles.is_empty() {
                "No catalog to show yet. F5 tries again."
            } else if !app.filter.is_empty() {
                "Nothing matches the search. Esc clears it."
            } else if app.show.installed || app.show.updates || app.show.wishlist {
                "None of your titles are in the groups shown. Change what to show in the card above the list (↑ from its top)."
            } else {
                "No groups are switched on. Pick some in the card above the list (↑ from its top)."
            };
            f.render_widget(Paragraph::new(msg).wrap(Wrap { trim: true }).style(Style::default().fg(theme::DIM)).block(pane(false, "")), detail);
        }
    }
    draw_foot(f, app, foot);
    // the dialog's buttons go last, on top of everything under it
    let buttons = app.modal.as_ref().map(|m| draw_modal(f, app, m, area)).unwrap_or_default();
    app.hits.extend(buttons);
}

fn pane(focused: bool, title: &str) -> Block<'static> {
    Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Plain)
        .border_style(Style::default().fg(if focused { theme::GREEN } else { theme::LINE }))
        .title(Span::styled(title.to_string(), Style::default().fg(theme::DIM)))
        .style(Style::default().bg(theme::PANEL))
}

/// The website's wordmark, hand-lettered in half blocks.
const LOGO: [&str; 2] = ["▄▀█ █▀ █▀▀ █ █ █ █ █▀▀ █▀█ █▀ █▀▀", "█▀█ ▄█ █▄▄ █ █ ▀▄▀ ██▄ █▀▄ ▄█ ██▄"];
const SLOGAN: [&str; 2] = ["W H E R E   T E X T", "C O M E S   A L I V E"];

/// Green to amber across the wordmark, as on the website (green-hi, green at 45%, amber).
fn logo_color(x: usize, width: usize) -> ratatui::style::Color {
    let stops = [(0.0, (0x7f, 0xdc, 0x5e)), (0.45, (0x5f, 0xbf, 0x45)), (1.0, (0xff, 0xb0, 0x00))];
    let t = x as f32 / width.saturating_sub(1).max(1) as f32;
    let k = if t <= stops[1].0 { 0 } else { 1 };
    let ((t0, a), (t1, b)) = (stops[k], stops[k + 1]);
    let u = ((t - t0) / (t1 - t0)).clamp(0.0, 1.0);
    let mix = |p: u8, q: u8| (p as f32 + (q as f32 - p as f32) * u).round() as u8;
    ratatui::style::Color::Rgb(mix(a.0, b.0), mix(a.1, b.1), mix(a.2, b.2))
}

fn draw_header(f: &mut Frame, app: &mut App, area: Rect) {
    f.render_widget(Block::default().style(Style::default().bg(theme::PANEL)), area);

    // top left: the wordmark, with the slogan beside it on its two rows
    let lw = LOGO[0].chars().count();
    let sw = SLOGAN.iter().map(|s| s.chars().count()).max().unwrap_or(0);
    let total = (lw + 2 + sw) as u16;
    let x0 = area.x + 1;
    for row in 0..2u16 {
        let mut spans: Vec<Span> = LOGO[row as usize]
            .chars()
            .enumerate()
            .map(|(x, c)| Span::styled(c.to_string(), Style::default().fg(logo_color(x, lw)).add_modifier(Modifier::BOLD)))
            .collect();
        spans.push(Span::raw("  "));
        spans.push(Span::styled(SLOGAN[row as usize], Style::default().fg(theme::DIM)));
        f.render_widget(Paragraph::new(Line::from(spans)), Rect { x: x0, y: area.y + row, width: total.min(area.width), height: 1 });
    }

    // right of the slogan, the menu row: the player's library and the
    // catalog count on top, downloads, help and support below
    let rx = x0 + total + 3;
    let rw = (area.x + area.width).saturating_sub(rx + 1);
    let top = Rect { x: rx, y: area.y, width: rw, height: 1 };
    let bottom = Rect { x: rx, y: area.y + 1, width: rw, height: 1 };

    let updates = app.updates();
    let plural = |n: usize, one: &str, many: &str| format!("{n} {}", if n == 1 { one } else { many });
    let chips = [
        ("● ", plural(app.state.installed.len(), "installed", "installed"), theme::INSTALLED, app.state.installed.len(), Mine::Installed),
        ("▲ ", plural(updates, "update", "updates"), theme::UPDATE, updates, Mine::Updates),
        ("♥ ", plural(app.state.wishlist.len(), "wishlisted", "wishlisted"), theme::AMBER_SOFT, app.state.wishlist.len(), Mine::Wishlist),
    ];
    let mut spans = Vec::new();
    let mut x = rx;
    for (mark, label, color, n, m) in chips {
        let st = Style::default().fg(if n == 0 { theme::FAINT } else { color });
        let w = (mark.chars().count() + label.chars().count()) as u16;
        app.hits.push((Rect { x, y: area.y, width: w, height: 1 }, Hit::Mine(m)));
        spans.push(Span::styled(mark, st));
        spans.push(Span::styled(label, st));
        spans.push(Span::raw("  "));
        x += w + 2;
    }
    f.render_widget(Paragraph::new(Line::from(spans)), top);
    let chips_end = x;
    let shown = app.visible().len();
    let count = if app.is_loading() && app.titles.is_empty() {
        Span::styled("loading the catalog", Style::default().fg(theme::DIM))
    } else if app.offline {
        Span::styled("offline: cached catalog", Style::default().fg(theme::AMBER))
    } else if shown != app.titles.len() {
        Span::styled(format!("showing {shown} of {}", app.titles.len()), Style::default().fg(theme::DIM))
    } else {
        Span::styled(format!("{} titles", app.titles.len()), Style::default().fg(theme::FAINT))
    };
    // only beside the library, never over it, when the row is narrow
    if chips_end + count.width() as u16 <= top.right() {
        f.render_widget(Paragraph::new(Line::from(count)).alignment(Alignment::Right), top);
    }

    // help, downloads and support on the right; downloads in red after a
    // failure, amber while something runs
    let failed = app.downloads.failed();
    let dl_label = if failed > 0 { format!("F2 downloads ✗{failed}") } else { "F2 downloads".to_string() };
    let dl_color = if failed > 0 {
        theme::UPDATE
    } else if app.downloads.running().is_some() {
        theme::AMBER
    } else {
        theme::DIM
    };
    let mut links = vec![
        ("F1 help".to_string(), Hit::Help, theme::DIM),
        (dl_label, Hit::Downloads, dl_color),
        // amber while the catalog is being fetched
        ("F5 refresh".to_string(), Hit::Refresh, if app.is_loading() { theme::AMBER } else { theme::DIM }),
        ("♥ support".to_string(), Hit::Support, theme::AMBER),
    ];
    let width_of = |ls: &[(String, Hit, ratatui::style::Color)]| -> u16 {
        ls.iter().map(|(l, _, _)| l.chars().count() as u16).sum::<u16>() + 3 * (ls.len() as u16).saturating_sub(1)
    };
    // F5 shows only where the row has room for it; F1 help and the key
    // line name it everywhere
    if width_of(&links) > bottom.width {
        links.retain(|(_, h, _)| !matches!(h, Hit::Refresh));
    }
    let lw = width_of(&links);
    let mut x = (bottom.x + bottom.width).saturating_sub(lw);
    let mut spans = Vec::new();
    for (i, (label, hit, color)) in links.into_iter().enumerate() {
        if i > 0 {
            spans.push(Span::raw("   "));
            x += 3;
        }
        let w = label.chars().count() as u16;
        app.hits.push((Rect { x, y: bottom.y, width: w, height: 1 }, hit));
        spans.push(Span::styled(label, Style::default().fg(color)));
        x += w;
    }
    f.render_widget(Paragraph::new(Line::from(spans)).alignment(Alignment::Right), bottom);

    // the running download on the left; a click opens the pane
    let dw = bottom.width.saturating_sub(lw + 2);
    if let Some(line) = download_line(app, dw as usize) {
        app.hits.push((Rect { x: bottom.x, y: bottom.y, width: (line.width() as u16).min(dw), height: 1 }, Hit::Downloads));
        f.render_widget(Paragraph::new(line), Rect { width: dw, ..bottom });
    } else if let Some(v) = app.av_newer.as_ref().map(|r| r.version.clone()) {
        // otherwise a newer av; a click says how to update
        let label = format!("▲ av {v} is out");
        let w = (label.chars().count() as u16).min(dw);
        app.hits.push((Rect { x: bottom.x, y: bottom.y, width: w, height: 1 }, Hit::AvUpdate));
        f.render_widget(Paragraph::new(Span::styled(label, Style::default().fg(theme::AMBER))), Rect { width: dw, ..bottom });
    }
}

/// "↓ tetrix  45% ████▌──────" for the running download, with "+2 queued"
/// when others wait; the step's words while there is no byte count yet
/// (starting, checksum ok, extracting).
fn download_line(app: &App, w: usize) -> Option<Line<'static>> {
    let d = app.downloads.running()?;
    let queued = app.downloads.queued();
    let more = if queued > 0 { format!("  +{queued} queued") } else { String::new() };
    let name: String = d.id.chars().take(16).collect();
    let mut spans = vec![Span::styled(format!("↓ {name} "), Style::default().fg(theme::AMBER))];
    let bar_w = w.saturating_sub(name.chars().count() + 3 + 5 + more.len()).clamp(4, 16);
    spans.extend(bar_spans(d, bar_w));
    spans.push(Span::styled(more, Style::default().fg(theme::FAINT)));
    Some(Line::from(spans))
}

/// "45% ████▌─────" while bytes arrive, the step's words otherwise.
fn bar_spans(d: &Download, w: usize) -> Vec<Span<'static>> {
    let downloading = d.total > 0 && d.done < d.total && (d.step.is_empty() || d.step == "downloading");
    if !downloading {
        return vec![Span::styled(d.step.clone(), Style::default().fg(theme::DIM))];
    }
    let eighths = (u128::from(d.done) * w as u128 * 8 / u128::from(d.total)) as usize;
    let (full, part) = (eighths / 8, eighths % 8);
    let mut fill = "█".repeat(full);
    if part > 0 && full < w {
        fill.push([' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉'][part]);
    }
    let track = "─".repeat(w.saturating_sub(fill.chars().count()));
    vec![
        Span::styled(format!("{:>3}% ", d.done * 100 / d.total), Style::default().fg(theme::TEXT)),
        Span::styled(fill, Style::default().fg(theme::GREEN)),
        Span::styled(track, Style::default().fg(theme::LINE)),
    ]
}

fn draw_search(f: &mut Frame, app: &mut App, area: Rect) {
    let active = app.focus == Focus::List;
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(if !app.filter.is_empty() { theme::GREEN } else { theme::LINE }))
        .title(Span::styled(" search ", Style::default().fg(theme::DIM)))
        .style(Style::default().bg(theme::PANEL));
    let mut spans = vec![Span::styled("/ ", Style::default().fg(theme::GREEN))];
    if app.filter.is_empty() {
        spans.push(Span::styled("type to search", Style::default().fg(theme::FAINT)));
    } else {
        spans.push(Span::styled(app.filter.clone(), Style::default().fg(theme::TEXT_HI)));
        if active {
            spans.push(Span::styled("▏", Style::default().fg(theme::GREEN)));
        }
        let n = app.visible().len();
        let count = format!(" {n}");
        let used: usize = spans.iter().map(|s| s.width()).sum();
        let room = (area.width as usize).saturating_sub(2 + used + count.len());
        spans.push(Span::raw(" ".repeat(room)));
        spans.push(Span::styled(count, Style::default().fg(theme::FAINT)));
    }
    f.render_widget(Paragraph::new(Line::from(spans)).block(block), area);
    app.hits.push((area, Hit::Search));
}

/// The card under the search box: folded, one line saying what the list
/// shows; open, a box per kind and two that narrow to the player's titles.
fn draw_show(f: &mut Frame, app: &mut App, area: Rect) {
    let open = app.focus == Focus::Show;
    let rows = app.show_rows();
    let mut lines = Vec::new();
    let w = area.width.saturating_sub(2) as usize;
    let caret = if open { "▾ " } else { "▸ " };
    lines.push(Line::from(vec![
        Span::styled(caret, Style::default().fg(theme::GREEN)),
        Span::styled(show_summary(app, &rows, w - 2), Style::default().fg(if open { theme::TEXT_HI } else { theme::TEXT })),
    ]));
    if open {
        for (i, r) in rows.iter().enumerate() {
            if *r == ShowRow::Installed {
                let label = "─ only mine ";
                lines.push(Line::from(vec![
                    Span::styled(label, Style::default().fg(theme::FAINT)),
                    Span::styled("─".repeat(w.saturating_sub(label.chars().count())), Style::default().fg(theme::LINE)),
                ]));
            }
            let (name, count) = match r {
                ShowRow::All => ("All".to_string(), app.titles.len()),
                ShowRow::Kind(k) => (section_name(k).to_string(), app.titles.iter().filter(|t| &t.kind == k).count()),
                ShowRow::Installed => ("Installed".to_string(), app.state.installed.len()),
                ShowRow::Updates => ("Updates".to_string(), app.updates()),
                ShowRow::Wishlist => ("Wishlisted".to_string(), app.state.wishlist.len()),
            };
            let on = app.row_on(r);
            let boxed = if on { "[x] " } else { "[ ] " };
            let num = count.to_string();
            let room = w.saturating_sub(4 + num.len() + 1);
            let name: String = name.chars().take(room).collect();
            let mut st = Style::default().fg(if on { theme::TEXT_HI } else { theme::DIM });
            if i == app.show_row {
                st = st.bg(theme::PANEL_HI).add_modifier(Modifier::BOLD);
            }
            lines.push(Line::from(vec![
                Span::styled(boxed, st.fg(if on { theme::GREEN } else { theme::DIM })),
                Span::styled(format!("{name:<room$} "), st),
                Span::styled(num, st.fg(theme::FAINT)),
            ]));
            app.hits.push((Rect { x: area.x + 1, y: area.y + lines.len() as u16, width: area.width.saturating_sub(2), height: 1 }, Hit::ShowRow(i)));
        }
    }
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(if open { theme::GREEN } else { theme::LINE }))
        .title(Span::styled(" show ", Style::default().fg(theme::DIM)))
        .style(Style::default().bg(theme::PANEL));
    f.render_widget(Paragraph::new(lines).block(block), area);
    // the folded line goes under the rows' hits, so a row click wins
    app.hits.insert(0, (Rect { x: area.x, y: area.y, width: area.width, height: 2 }, Hit::ShowHead));
}

/// What the list shows, in a few words: "everything", or the groups that are
/// on, then "· mine" parts.
fn show_summary(app: &App, rows: &[ShowRow], w: usize) -> String {
    let kinds: Vec<&ShowRow> = rows.iter().filter(|r| matches!(r, ShowRow::Kind(_))).collect();
    let on: Vec<&str> = kinds
        .iter()
        .filter(|r| app.row_on(r))
        .map(|r| if let ShowRow::Kind(k) = r { short_name(k) } else { "" })
        .collect();
    let mut s = match on.len() {
        0 => "nothing".to_string(),
        n if n == kinds.len() => "everything".to_string(),
        _ => on.join(", "),
    };
    let mine: Vec<&str> = [(app.show.installed, "installed"), (app.show.updates, "updates"), (app.show.wishlist, "wishlisted")]
        .into_iter()
        .filter(|(on, _)| *on)
        .map(|(_, w)| w)
        .collect();
    if !mine.is_empty() {
        s += &format!(" · {}", mine.join(" or "));
    }
    if s.chars().count() > w {
        s = s.chars().take(w.saturating_sub(1)).collect::<String>() + "…";
    }
    s
}

fn short_name(kind: &str) -> &'static str {
    match kind {
        "adventure" => "Adventures",
        "art" => "Art",
        k => section_name(k),
    }
}

fn section_name(kind: &str) -> &'static str {
    match kind {
        "gone" => "No longer in the catalog",
        "game" => "Games",
        "adventure" => "Text adventures",
        "program" => "Programs",
        "art" => "Art & Demos",
        "movie" => "Movies",
        "mud" => "MUDs",
        _ => "Other",
    }
}

fn draw_list(f: &mut Frame, app: &mut App, area: Rect) {
    // the list is grouped by kind (see App::refresh); a header row starts each
    // group. Headers are drawn only: selection still counts titles, and the
    // highlighted row is mapped past the headers above it.
    let inner_w = LIST_W.saturating_sub(2) as usize;
    let mut items: Vec<ListItem> = Vec::new();
    let mut selected_row = 0;
    let mut row_title: Vec<Option<usize>> = Vec::new(); // list row to title index, None for headers
    let mut last_kind: Option<String> = None;
    for (i, t) in app.visible().iter().enumerate() {
        // titles no longer in the catalog make a group of their own, last
        let group = if t.gone { "gone" } else { t.kind.as_str() };
        if last_kind.as_deref() != Some(group) {
            let label = format!("── {} ", section_name(group));
            let fill = inner_w.saturating_sub(label.chars().count());
            items.push(ListItem::new(Line::from(vec![
                Span::styled(label, Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::BOLD)),
                Span::styled("─".repeat(fill), Style::default().fg(theme::LINE)),
            ])));
            last_kind = Some(group.to_string());
            row_title.push(None);
        }
        row_title.push(Some(i));
        if i == app.selected {
            selected_row = items.len();
        }
        let st = app.status_of(t);
        let color = match st {
            Status::Installed => theme::INSTALLED,
            Status::Update => theme::UPDATE,
            Status::Busy => theme::AMBER,
            Status::NotInstalled => theme::NOT_INSTALLED,
        };
        let mark = match st {
            Status::Installed => "● ",
            Status::Update => "▲ ",
            Status::Busy => "↓ ",
            Status::NotInstalled => "  ",
        };
        let (tag, tag_style) = if t.new {
            ("NEW!", Style::default().fg(theme::GREEN_HI).add_modifier(Modifier::BOLD))
        } else if t.old {
            ("OLD!", Style::default().fg(theme::AMBER).add_modifier(Modifier::BOLD))
        } else {
            ("", Style::default())
        };
        // a tag needs a space before it, so long names give way to it
        let w = LIST_W.saturating_sub(if tag.is_empty() { 5 } else { 10 }) as usize;
        let name: String = t.title.chars().take(w).collect();
        items.push(ListItem::new(Line::from(vec![
            Span::styled(mark, Style::default().fg(color)),
            Span::styled(format!("{name:<w$}"), Style::default().fg(color)),
            Span::styled(if tag.is_empty() { String::new() } else { format!(" {tag}") }, tag_style),
        ])));
    }
    let list = List::new(items)
        .block(pane(app.focus == Focus::List, " titles "))
        .highlight_style(Style::default().bg(theme::PANEL_HI).add_modifier(Modifier::BOLD))
        // the list scrolls before the selection reaches an edge, so the next
        // titles are always in view (ratatui shrinks it on a short list)
        .scroll_padding(if app.list_clicked { 0 } else { LIST_SCROLL_MARGIN });
    // a fresh offset each frame would scroll the selection to the bottom
    // edge, so clicking a row made the list jump under the pointer
    let mut st = ListState::default().with_offset(app.list_offset).with_selected(Some(selected_row));
    f.render_stateful_widget(list, area, &mut st);
    app.list_offset = st.offset();
    app.hits.push((area, Hit::List));
    let inner = Rect { x: area.x + 1, y: area.y + 1, width: area.width.saturating_sub(2), height: area.height.saturating_sub(2) };
    for (k, row) in row_title.iter().enumerate().skip(st.offset()).take(inner.height as usize) {
        if let Some(i) = row {
            let y = inner.y + (k - st.offset()) as u16;
            app.hits.push((Rect { x: inner.x, y, width: inner.width, height: 1 }, Hit::Row(*i)));
        }
    }
}

/// Rows kept in view above and below the selected title while moving through the list.
const LIST_SCROLL_MARGIN: usize = 6;

/// Rows the scrolling column may hold below the preview; pages are short.
const COLUMN_SPARE: u16 = 400;

fn draw_detail(f: &mut Frame, app: &mut App, t: &Title, area: Rect) {
    // The whole right side is one column that scrolls: the preview, the title
    // row, then facts and page flowing on beneath it. It is drawn into a tall
    // off-screen buffer and the visible window is copied into the frame.
    let (cw, ch) = app.player.as_ref().map(|(_, p)| (p.cast.cols as u16, p.cast.rows as u16)).unwrap_or((100, 30));
    let pw = (cw + 2).min(area.width);
    let ph = ch + 2;
    let side_w = area.width.saturating_sub(pw + 1);
    let side_by_side = side_w >= 34;

    let virt = Rect { x: 0, y: 0, width: area.width, height: ph + COLUMN_SPARE };
    let mut buf = Buffer::empty(virt);
    Block::default().style(Style::default().bg(theme::PANEL)).render(virt, &mut buf);
    draw_preview(&mut buf, app, t, Rect { x: 0, y: 0, width: pw, height: ph });

    let mut text = Text::default();
    let page_y;
    // button click areas in the off-screen column
    let mut button_at: Vec<Rect> = Vec::new();
    let mut place = |x0: u16, y: u16, b: &Buttons, buf: &mut Buffer| {
        for (k, line) in b.lines.iter().enumerate() {
            Paragraph::new(line.clone()).render(Rect { x: x0, y: y + k as u16, width: b.width, height: 1 }, buf);
        }
        button_at.extend(b.at.iter().map(|&(x, w)| Rect { x: x0 + x, y, width: w, height: b.height() }));
    };
    if side_by_side {
        // wide terminals: everything about the title beside the preview,
        // with tall buttons, as there is room for them here
        let side_text_w = side_w.saturating_sub(1).max(1);
        let lines = vec![
            Line::from(Span::styled(t.title.clone(), title_style())),
            title_rule(t, side_text_w),
            Line::from(Span::styled(t.description.clone(), Style::default().fg(theme::DIM))),
            Line::default(),
        ];
        // the buttons sit below the title and description as they wrap in
        // this narrow column, not on a fixed row: a two-line description
        // pushes them down, and their click areas must go down with them
        let para = Paragraph::new(lines).wrap(Wrap { trim: true });
        let above = para.line_count(side_text_w) as u16;
        para.render(Rect { x: pw + 1, y: 0, width: side_text_w, height: ph }, &mut buf);
        let b = buttons(app, t, true, side_text_w);
        place(pw + 1, above, &b, &mut buf);
        let mut rest: Vec<Line> = progress(app, t).into_iter().collect();
        rest.push(Line::default());
        rest.extend(facts(app, t));
        let y = above + b.height();
        Paragraph::new(rest).wrap(Wrap { trim: true }).render(Rect { x: pw + 1, y, width: side_text_w, height: ph.saturating_sub(y) }, &mut buf);
        page_y = ph;
    } else {
        // under the preview: tall buttons on the right when the pane has the
        // rows to spare, with the title and its rule beside them; on a
        // minimum-size terminal one row of title and buttons
        let tall = area.height >= ph + 7;
        let b = buttons(app, t, tall, pw / 2);
        let bx = pw.saturating_sub(b.width);
        place(bx, ph, &b, &mut buf);
        let name_w = pw.saturating_sub(b.width + 1);
        let name_y = if tall { ph + 1 } else { ph };
        Paragraph::new(Span::styled(t.title.clone(), title_style())).render(Rect { x: 0, y: name_y, width: name_w, height: 1 }, &mut buf);
        if tall {
            Paragraph::new(title_rule(t, name_w)).render(Rect { x: 0, y: ph + 2, width: name_w, height: 1 }, &mut buf);
        }
        let desc_y = ph + b.height();
        let line = progress(app, t)
            .unwrap_or_else(|| Line::from(Span::styled(t.description.clone(), Style::default().fg(theme::DIM))));
        Paragraph::new(line).wrap(Wrap { trim: true }).render(Rect { x: 0, y: desc_y, width: pw, height: 2 }, &mut buf);
        page_y = desc_y + 1 + u16::from(t.description.chars().count() > pw as usize);
        text.lines.extend(facts(app, t));
        text.lines.push(Line::default());
    }
    text.lines.extend(page_text(t, app.state.installed.get(&t.id).map(|i| i.version.as_str())).lines);
    let page_w = if side_by_side { area.width.saturating_sub(1) } else { pw };
    Paragraph::new(text)
        .wrap(Wrap { trim: false })
        .block(
            Block::default()
                .borders(Borders::TOP)
                .border_style(Style::default().fg(if app.focus == Focus::Page { theme::GREEN } else { theme::LINE })),
        )
        .render(Rect { x: 0, y: page_y, width: page_w, height: virt.height - page_y }, &mut buf);

    // the content ends at the last row with anything drawn on it
    let blank = |y: u16| (0..virt.width).all(|x| buf[(x, y)].symbol() == " ");
    let mut end = virt.height;
    while end > page_y + 1 && blank(end - 1) {
        end -= 1;
    }
    let max_scroll = end.saturating_sub(area.height);
    app.page_scroll = app.page_scroll.min(max_scroll);
    let top = app.page_scroll;
    app.page_view = Some((area, top));

    // text selected with the mouse, in reading order
    let selected = app.selection.as_ref().filter(|s| s.id == t.id).map(|s| {
        let (a, b) = if (s.from.1, s.from.0) <= (s.to.1, s.to.0) { (s.from, s.to) } else { (s.to, s.from) };
        move |x: u16, y: u16| (a.1, a.0) <= (y, x) && (y, x) <= (b.1, b.0)
    });
    if app.copy_wanted {
        app.copy_wanted = false;
        if let Some(s) = app.selection.as_ref().filter(|s| s.id == t.id) {
            app.to_copy = Some(selected_text(&buf, s.from, s.to, page_w));
        }
    }

    let out = f.buffer_mut();
    for y in 0..area.height.min(virt.height - top) {
        for x in 0..area.width {
            let mut cell = buf[(x, top + y)].clone();
            if selected.as_ref().is_some_and(|on| on(x, top + y)) {
                cell.modifier.insert(Modifier::REVERSED);
            }
            out[(area.x + x, area.y + y)] = cell;
        }
    }
    // each button row that is on screen answers the mouse
    for (i, r) in button_at.into_iter().enumerate() {
        for y in r.y..r.y + r.height {
            if y >= top && y - top < area.height {
                app.hits.push((Rect { x: area.x + r.x, y: area.y + y - top, width: r.width, height: 1 }, Hit::Button(i)));
            }
        }
    }
    // say when there is more above or below
    let hint = |s: &str| Span::styled(s.to_string(), Style::default().fg(theme::AMBER).bg(theme::PANEL_HI));
    if top < max_scroll {
        let r = Rect { x: area.x + area.width.saturating_sub(10), y: area.y + area.height - 1, width: 10, height: 1 };
        Paragraph::new(hint(" ↓ more ")).alignment(Alignment::Right).render(r, out);
    }
    if top > 0 {
        let r = Rect { x: area.x + area.width.saturating_sub(10), y: area.y, width: 10, height: 1 };
        Paragraph::new(hint(" ↑ more ")).alignment(Alignment::Right).render(r, out);
    }
}

/// The text between two cells of the page column, as it reads: rows that a
/// paragraph wrapped are joined with a space again, so copied sentences
/// come out whole; a blank row stays a paragraph break.
fn selected_text(buf: &Buffer, from: (u16, u16), to: (u16, u16), wrap_w: u16) -> String {
    let (a, b) = if (from.1, from.0) <= (to.1, to.0) { (from, to) } else { (to, from) };
    let w = buf.area.width;
    let cells = |y: u16, x0: u16, x1: u16| (x0..x1.min(w)).map(|x| buf[(x, y)].symbol()).collect::<String>().trim_end().to_string();
    let mut out = String::new();
    for y in a.1..=b.1 {
        let part = cells(y, if y == a.1 { a.0 } else { 0 }, if y == b.1 { b.0 + 1 } else { w });
        if y > a.1 {
            // whole rows decide: the paragraph moved this row's first word
            // down because it didn't fit on the row above
            let (prev, row) = (cells(y - 1, 0, w), cells(y, 0, w));
            let first_word = row.trim_start().split(' ').next().unwrap_or("");
            let wrapped = !prev.is_empty()
                && !first_word.is_empty()
                && prev.chars().count() + 1 + first_word.chars().count() > (wrap_w as usize).saturating_sub(2);
            out.push_str(if wrapped { " " } else { "\n" });
            out.push_str(if wrapped { part.trim_start() } else { &part });
        } else {
            out.push_str(&part);
        }
    }
    out
}

fn draw_preview(buf: &mut Buffer, app: &App, t: &Title, area: Rect) {
    let b = Block::default().borders(Borders::ALL).border_style(Style::default().fg(theme::LINE));
    let inner = b.inner(area);
    b.render(area, buf);
    match &app.player {
        Some((id, p)) if *id == t.id => p.render(inner, buf),
        _ => {
            Block::default().style(Style::default().bg(crate::cast::BG)).render(inner, buf);
            if t.is_mud() {
                Paragraph::new(mud_card(t, app.lera().is_some())).alignment(Alignment::Center).render(inner, buf);
            } else {
                let msg = app.cast_note(t).unwrap_or_default();
                let y = inner.y + inner.height / 2;
                Paragraph::new(Span::styled(msg, Style::default().fg(theme::FAINT)))
                    .alignment(Alignment::Center)
                    .render(Rect { y, height: 1, ..inner }, buf);
            }
        }
    }
}

fn mud_card(t: &Title, lera: bool) -> Text<'static> {
    let mut l = vec![Line::default(); 3];
    l.push(Line::from(Span::styled(t.title.to_uppercase(), Style::default().fg(theme::AMBER).add_modifier(Modifier::BOLD))));
    if let Some(s) = t.since {
        l.push(Line::from(Span::styled(format!("online since {s}"), Style::default().fg(theme::DIM))));
    }
    l.push(Line::default());
    if let Some(c) = &t.connect {
        l.push(Line::from(Span::styled(format!("{} {}", c.host, c.port), Style::default().fg(theme::GREEN_HI))));
    }
    l.push(Line::default());
    if lera {
        l.push(Line::from(Span::styled("Connect opens the world in Lera.", Style::default().fg(theme::FAINT))));
    } else {
        l.push(Line::from(Span::styled("Install Lera, the Asciiverse MUD client, to play here.", Style::default().fg(theme::FAINT))));
        l.push(Line::from(Span::styled("Until then Connect opens the world in telnet.", Style::default().fg(theme::FAINT))));
    }
    Text::from(l)
}

/// The title's buttons as drawn: one row, or three when tall, where a `▄`
/// row over the label and a `▀` row under it, in the button's own color,
/// make one solid block two cells high.
struct Buttons {
    lines: Vec<Line<'static>>,
    /// each button's column from the left edge of the row, and its width
    at: Vec<(u16, u16)>,
    width: u16,
}

impl Buttons {
    fn height(&self) -> u16 {
        self.lines.len() as u16
    }
}

/// The widest layout that fits in `max_w`: every button one width with
/// roomy padding, then less padding, then each as wide as its own label.
fn buttons(app: &App, t: &Title, tall: bool, max_w: u16) -> Buttons {
    let acts = app.actions(t);
    let label_w: Vec<u16> = acts.iter().map(|(_, l, _)| Span::raw(l.clone()).width() as u16).collect();
    let widest = label_w.iter().copied().max().unwrap_or(0);
    let n = acts.len() as u16;
    // (same width for all, padding each side, gap between)
    let tries = [(true, 3, 2), (true, 2, 2), (false, 2, 2), (false, 1, 1)];
    let total = |(same, pad, gap): (bool, u16, u16)| {
        label_w.iter().map(|w| if same { widest } else { *w } + 2 * pad).sum::<u16>() + gap * n.saturating_sub(1)
    };
    let (same, pad, gap) = tries.into_iter().find(|t| total(*t) <= max_w).unwrap_or(tries[3]);

    let (mut top, mut mid, mut bot, mut at) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    let mut x = 0;
    for (i, (_, label, enabled)) in acts.iter().enumerate() {
        let sel = app.focus == Focus::Actions && app.action == i;
        let (fg, bg) = match (enabled, sel) {
            (false, _) => (theme::FAINT, theme::PANEL_HI),
            (true, true) => (theme::BG, theme::GREEN_HI),
            (true, false) => (theme::GREEN_HI, theme::PANEL_HI),
        };
        let mut st = Style::default().fg(fg).bg(bg);
        if sel {
            st = st.add_modifier(Modifier::BOLD);
        }
        if i > 0 {
            for row in [&mut top, &mut mid, &mut bot] {
                row.push(Span::raw(" ".repeat(gap as usize)));
            }
            x += gap;
        }
        let w = if same { widest } else { label_w[i] } + 2 * pad;
        let left = (w - label_w[i]) / 2;
        let right = w - label_w[i] - left;
        mid.push(Span::styled(format!("{}{label}{}", " ".repeat(left as usize), " ".repeat(right as usize)), st));
        top.push(Span::styled("▄".repeat(w as usize), Style::default().fg(bg)));
        bot.push(Span::styled("▀".repeat(w as usize), Style::default().fg(bg)));
        at.push((x, w));
        x += w;
    }
    let lines = if tall { vec![Line::from(top), Line::from(mid), Line::from(bot)] } else { vec![Line::from(mid)] };
    Buttons { lines, at, width: x }
}

/// The title's name, set apart from everything under it.
fn title_style() -> Style {
    Style::default().fg(theme::AMBER).add_modifier(Modifier::BOLD)
}

fn title_rule(t: &Title, max_w: u16) -> Line<'static> {
    let w = (Span::raw(t.title.clone()).width() as u16).min(max_w);
    Line::from(Span::styled("─".repeat(w as usize), Style::default().fg(theme::LINE)))
}

fn progress(app: &App, t: &Title) -> Option<Line<'static>> {
    let d = app.downloads.list.iter().find(|d| d.id == t.id && !d.finished())?;
    let mut spans = vec![Span::styled("↓ ", Style::default().fg(theme::AMBER))];
    if d.phase == Phase::Queued {
        spans.push(Span::styled("queued; F2 shows the downloads", Style::default().fg(theme::AMBER)));
    } else {
        spans.extend(bar_spans(d, 20));
    }
    Some(Line::from(spans))
}

fn facts(app: &App, t: &Title) -> Vec<Line<'static>> {
    let mut lines = Vec::new();
    let fact = |k: &str, v: String| {
        Line::from(vec![Span::styled(format!("{k:<11}"), Style::default().fg(theme::FAINT)), Span::styled(v, Style::default().fg(theme::TEXT))])
    };
    if t.is_mud() {
        if let Some(c) = &t.connect {
            lines.push(fact("Address", format!("{}:{}", c.host, c.port)));
        }
        if let Some(w) = &t.website {
            lines.push(fact("Website", w.clone()));
        }
        return lines;
    }
    if let Some(d) = &t.developer {
        lines.push(fact("Developer", d.name.clone()));
    }
    if let Some(u) = &t.upstream {
        lines.push(fact("Original", u.name.clone()));
    }
    if let Some(name) = t.collection_name(&app.collections) {
        let n = app.titles.iter().filter(|o| o.collection == t.collection).count();
        lines.push(fact("Part of", if n > 1 { format!("{name} ({n} games)") } else { name.to_string() }));
    }
    if !t.version.is_empty() {
        let inst = app.state.installed.get(&t.id).map(|i| format!("  (installed {})", i.version)).unwrap_or_default();
        lines.push(fact("Version", format!("{}{inst}", t.version)));
    }
    if !t.license.is_empty() {
        lines.push(fact("License", t.license.clone()));
    }
    if let Some(p) = app.state.played.get(&t.id) {
        let last = p.last_played.get(..10).unwrap_or("");
        lines.push(fact("Played", format!("{}, last on {last}", crate::launch::duration(p.seconds))));
    }
    if let Some(m) = t.min_terminal {
        lines.push(fact("Needs", format!("{}x{} terminal{}", m.cols, m.rows, if t.requires.mouse { ", mouse" } else { "" })));
    }
    if t.requires.gamepad {
        lines.push(fact("Gamepad", "supported".to_string()));
    }
    // only when this machine lacks them, with where to read what to do
    if t.requires.glyphs.iter().any(|g| crate::glyphs::missing(g)) {
        lines.push(Line::from(vec![
            Span::styled(format!("{:<11}", "Needs"), Style::default().fg(theme::FAINT)),
            Span::styled("a font with Braille: ", Style::default().fg(theme::AMBER_SOFT)),
            Span::styled(crate::glyphs::HELP_URL, Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::UNDERLINED)),
        ]));
    }
    // only the libraries this machine lacks
    let libs = crate::libs::missing(&t.system_deps);
    if !libs.is_empty() {
        lines.push(Line::from(vec![
            Span::styled(format!("{:<11}", "Needs"), Style::default().fg(theme::FAINT)),
            Span::styled(
                format!("{} {}", if libs.len() == 1 { "system package" } else { "system packages" }, libs.join(", ")),
                Style::default().fg(theme::AMBER_SOFT),
            ),
        ]));
    }
    if crate::catalog::pick_platform(t).is_none() && !t.platforms.is_empty() {
        lines.push(Line::from(Span::styled("No build for this platform", Style::default().fg(theme::UPDATE))));
    }
    lines
}

/// How many releases' notes the page shows.
const NEWS_SHOWN: usize = 3;

fn page_text(t: &Title, installed: Option<&str>) -> Text<'static> {
    let mut text = Text::from(provenance(t));
    if !t.page.trim().is_empty() {
        text.lines.extend(md::render(&t.page).lines);
        text.lines.extend(news(t, installed));
        return text;
    }
    let mut lines = vec![Line::from(Span::styled(t.description.clone(), Style::default().fg(theme::TEXT)))];
    if !t.topics.is_empty() {
        lines.push(Line::default());
        lines.push(Line::from(Span::styled(t.topics.join("  ·  "), Style::default().fg(theme::FAINT))));
    }
    text.lines.extend(lines);
    text.lines.extend(news(t, installed));
    text
}

/// NEWS under the page: the latest releases' notes, newest first; the ones
/// newer than the installed version marked as what an update brings.
fn news(t: &Title, installed: Option<&str>) -> Vec<Line<'static>> {
    let shown: Vec<_> = t.news.iter().filter(|n| !n.notes.trim().is_empty()).take(NEWS_SHOWN).collect();
    if shown.is_empty() {
        return Vec::new();
    }
    let head = Style::default().fg(theme::DIM).add_modifier(Modifier::BOLD);
    let mut lines = vec![Line::default(), Line::from(Span::styled("NEWS", head))];
    // everything above the installed version is what an update brings
    let mut newer = installed.is_some_and(|v| v != t.version);
    for n in shown {
        if installed == Some(n.version.as_str()) {
            newer = false;
        }
        let mut spans = vec![Span::styled(n.version.clone(), Style::default().fg(theme::GREEN).add_modifier(Modifier::BOLD))];
        if !n.date.is_empty() {
            spans.push(Span::styled(format!("  {}", n.date), Style::default().fg(theme::FAINT)));
        }
        if newer {
            spans.push(Span::styled("  new in the update", Style::default().fg(theme::UPDATE)));
        } else if installed == Some(n.version.as_str()) {
            spans.push(Span::styled("  installed", Style::default().fg(theme::INSTALLED)));
        }
        lines.push(Line::from(spans));
        lines.extend(md::render(&n.notes).lines);
        lines.push(Line::default());
    }
    lines
}

// Where it comes from, above the page: who made it, what we changed, and the links
fn provenance(t: &Title) -> Vec<Line<'static>> {
    if t.provenance.is_none() && t.upstream.is_none() {
        return Vec::new();
    }
    let head = Style::default().fg(theme::DIM).add_modifier(Modifier::BOLD);
    let mut lines = vec![Line::from(Span::styled("WHERE IT COMES FROM", head))];
    if let Some(p) = &t.provenance {
        lines.push(Line::from(Span::styled(p.clone(), Style::default().fg(theme::TEXT))));
    }
    let link = |k: &str, v: String| {
        Line::from(vec![Span::styled(format!("{k:<13}"), Style::default().fg(theme::FAINT)), Span::styled(v, Style::default().fg(theme::GREEN))])
    };
    if let Some(u) = t.upstream.as_ref().filter(|u| !u.url.is_empty()) {
        lines.push(link("Original", u.url.clone()));
    }
    if let Some(r) = &t.repo {
        lines.push(link("Our source", format!("https://github.com/{r}")));
        if let Some(c) = &t.changes {
            let k = if c.contains("PATCHES") { "Our changes" } else { "Provenance" };
            lines.push(link(k, format!("https://github.com/{r}/blob/HEAD/{c}")));
        }
    }
    lines.push(Line::default());
    lines
}

fn draw_foot(f: &mut Frame, app: &App, area: Rect) {
    let hints = match app.focus {
        Focus::Show => "↑↓ move  Space switch on or off  ↓ past the end or Esc back to the list  type to search",
        Focus::List => "↑↓ select  ↑ at the top: what to show  / or type to search  Enter actions  Tab panels  F1 help  Esc quit",
        Focus::Actions => "←→ choose  Enter do it  ↑↓ back to list  / search  Tab page  Esc back",
        Focus::Page => "↑↓ scroll  Space pause preview  / search  Tab list  Esc back",
    };
    let line = Line::from(vec![
        Span::styled(format!(" {} ", app.status), Style::default().fg(theme::AMBER_SOFT)),
        Span::styled(hints, Style::default().fg(theme::FAINT)),
    ]);
    f.render_widget(Paragraph::new(line).style(Style::default().bg(theme::PANEL)), area);
}

/// F2: every download this session, running first, then the queue, the
/// failed and cancelled ones (Enter retries) and the finished ones; and
/// underneath them the updates on offer, each started only by a yes.
fn downloads_text(app: &App, w: usize) -> Text<'static> {
    let q = &app.downloads;
    let order = q.ordered();
    let mut lines = Vec::new();
    if order.is_empty() {
        lines.push(Line::from(Span::styled("Nothing downloading.", Style::default().fg(theme::DIM))));
    }
    let updates = app.offers().len();
    let mut section = "";
    for (row, &i) in order.iter().enumerate() {
        let d = &q.list[i];
        let name = match d.phase {
            Phase::Running => "Downloading",
            Phase::Queued => "Queued",
            Phase::Failed(_) => "Failed",
            Phase::Cancelled => "Cancelled",
            Phase::Done => "Done",
        };
        if name != section {
            if !section.is_empty() {
                lines.push(Line::default());
            }
            lines.push(Line::from(Span::styled(name, Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::BOLD))));
            section = name;
        }
        let sel = row == app.dl_sel;
        let base = if sel { Style::default().bg(theme::PANEL_HI).add_modifier(Modifier::BOLD) } else { Style::default() };
        let (mark, color) = match d.phase {
            Phase::Running => ("↓", theme::AMBER),
            Phase::Queued => ("·", theme::DIM),
            Phase::Failed(_) => ("✗", theme::UPDATE),
            Phase::Cancelled => ("-", theme::FAINT),
            Phase::Done => ("✓", theme::INSTALLED),
        };
        let what = match (&d.from, &d.phase) {
            (Some(f), Phase::Done) => format!("updated {f} → {}", d.version),
            (None, Phase::Done) => format!("installed {}", d.version),
            (Some(f), _) => format!("update {f} → {}", d.version),
            (None, _) => format!("install {}", d.version),
        };
        let name: String = d.name.chars().take(19).collect();
        let what: String = what.chars().take(24).collect();
        let mut spans = vec![
            Span::styled(format!(" {mark} "), base.fg(color)),
            Span::styled(format!("{name:<20}"), base.fg(theme::TEXT_HI)),
            Span::styled(format!("{what:<25}"), base.fg(theme::DIM)),
        ];
        match &d.phase {
            Phase::Running => {
                spans.extend(bar_spans(d, 14));
                if d.total > 0 && d.step == "downloading" {
                    let speed = d.speed();
                    let left = if speed > 0.0 {
                        let secs = ((d.total - d.done.min(d.total)) as f64 / speed) as u64;
                        format!("  {}:{:02}", secs / 60, secs % 60)
                    } else {
                        String::new()
                    };
                    spans.push(Span::styled(
                        format!("  {} / {}  {}/s{left}", human(d.done), human(d.total), human(speed as u64)),
                        Style::default().fg(theme::FAINT),
                    ));
                }
            }
            Phase::Failed(e) => {
                let room = w.saturating_sub(3 + 20 + 25);
                let first: String = e.lines().next().unwrap_or("").chars().take(room).collect();
                spans.push(Span::styled(first, Style::default().fg(theme::UPDATE)));
            }
            _ => {}
        }
        lines.push(Line::from(spans));
        // the selected failure gets its whole message
        if sel && let Phase::Failed(e) = &d.phase {
            for l in e.lines() {
                lines.push(Line::from(Span::styled(format!("     {l}"), Style::default().fg(theme::DIM))));
            }
        }
    }
    // underneath the downloads: updates waiting, offered one by one
    let offers = app.offers();
    if !offers.is_empty() {
        if !order.is_empty() {
            lines.push(Line::default());
        }
        lines.push(Line::from(Span::styled("Updates available", Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::BOLD))));
        for (k, t) in offers.iter().enumerate() {
            let sel = order.len() + k == app.dl_sel;
            let base = if sel { Style::default().bg(theme::PANEL_HI).add_modifier(Modifier::BOLD) } else { Style::default() };
            let have = app.state.installed.get(&t.id).map(|i| i.version.clone()).unwrap_or_default();
            let name: String = t.title.chars().take(19).collect();
            let what: String = format!("{have} → {}", t.version).chars().take(24).collect();
            let mut spans = vec![
                Span::styled(" ▲ ", base.fg(theme::UPDATE)),
                Span::styled(format!("{name:<20}"), base.fg(theme::TEXT_HI)),
                Span::styled(format!("{what:<25}"), base.fg(theme::DIM)),
            ];
            if sel {
                spans.push(Span::styled("Enter: update   Del: not now", Style::default().fg(theme::GREEN_HI)));
            }
            lines.push(Line::from(spans));
        }
    }
    lines.push(Line::default());
    let hint = format!(
        "↑↓ move  Enter yes/retry  r retry all  Del cancel/not now  u update all ({updates})  c clear done  Esc close"
    );
    lines.push(Line::from(Span::styled(hint, Style::default().fg(theme::FAINT))));
    Text::from(lines)
}

/// F1: the keys, the menu row, and the font note from the design.
/// F1 help; `log` is av.log, named for problem reports, and `newer` the
/// latest av when this one is older.
fn help_text(log: &std::path::Path, newer: Option<&crate::catalog::AvRelease>) -> Text<'static> {
    let head = |s: &'static str| Line::from(Span::styled(s, Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::BOLD)));
    let key = |k: &'static str, what: &'static str| {
        Line::from(vec![Span::styled(format!("  {k:<12}"), Style::default().fg(theme::GREEN_HI)), Span::raw(what)])
    };
    let text = |s: &'static str| Line::from(Span::styled(s, Style::default().fg(theme::TEXT)));
    let mut lines = Vec::new();
    if let Some(r) = newer {
        lines.push(Line::from(Span::styled(format!("{}. To update, run:", crate::selfupdate::headline(r)), Style::default().fg(theme::AMBER).add_modifier(Modifier::BOLD))));
        lines.push(Line::from(Span::styled(format!("  {}", crate::selfupdate::how()), Style::default().fg(theme::GREEN_HI))));
        lines.push(Line::default());
    }
    lines.extend([
        head("Keys"),
        key("↑ ↓", "move in the list; ↑ at its top opens the show card"),
        key("/ or type", "search names, topics, genres and developers"),
        key("Enter", "the title's actions: install, play, wishlist"),
        key("Tab", "move between the list, the actions and the page"),
        key("Space", "show card: switch a box; page: pause the preview"),
        key("F2", "downloads: progress, the queue, retry, update all"),
        key("F5", "fetch the catalog again"),
        key("F1", "this help"),
        key("W", "in this help: the welcome page again"),
        key("Esc", "back one step; at the top, quit"),
        Line::default(),
        head("Mouse"),
        text("  Drag over a title's page to copy the text. Shift and drag selects"),
        text("  anything on the screen, the way your terminal does it."),
        Line::default(),
        head("Menu row"),
        text("  ● installed, ▲ updates and ♥ wishlisted: click one to show only"),
        text("  those, click it again to show everything. ♥ support: how to help"),
        text("  Asciiverse keep going."),
        Line::default(),
        head("Fonts"),
        text("  Previews and some titles draw with Braille and Unicode block"),
        text("  characters. If they look broken, switch your terminal to a font"),
        text("  that has them."),
        Line::default(),
        head("From a shell"),
        text("  av help lists the commands: install, play, update, search and more."),
        Line::default(),
        head("Something wrong?"),
        text("  Tell us at github.com/theasciiverse/av/issues, and add the log:"),
        Line::from(Span::styled(format!("  {}", crate::paths::tilde(log)), Style::default().fg(theme::GREEN_HI))),
        Line::default(),
        Line::from(Span::styled("any key to close", Style::default().fg(theme::FAINT))),
    ]);
    Text::from(lines)
}

/// The first run's page: what av is, how to start, where titles start from,
/// and the font note. Shown once; W in help brings it back.
fn welcome_text(bin: Option<&std::path::Path>, on_path: bool) -> Text<'static> {
    let head = |s: &'static str| Line::from(Span::styled(s, Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::BOLD)));
    let key = |k: &'static str, what: &'static str| {
        Line::from(vec![Span::styled(format!("  {k:<12}"), Style::default().fg(theme::GREEN_HI)), Span::raw(what)])
    };
    let text = |s: String| Line::from(Span::styled(s, Style::default().fg(theme::TEXT)));
    let code = |s: &'static str| Line::from(Span::styled(s, Style::default().fg(theme::GREEN_HI)));
    let mut lines = vec![
        Line::from(Span::styled("Welcome to Asciiverse!", Style::default().fg(theme::AMBER).add_modifier(Modifier::BOLD))),
        Line::default(),
        text("  Games, text adventures, programs, art and MUDs for the terminal, in".into()),
        text("  one list: browse them, watch them play, install and start them, all".into()),
        text("  without leaving this window.".into()),
        Line::default(),
        head("Getting started"),
        key("↑ ↓", "pick a title; its preview plays on the right"),
        key("Enter", "Install, then Play"),
        key("/ or type", "search"),
        key("↑ at top", "choose what the list shows"),
        key("F1", "help, any time"),
        Line::default(),
        head("Starting titles"),
    ];
    match bin {
        Some(_) if on_path => {
            lines.push(text("  Installed titles start from av, or from your shell by name.".into()));
        }
        Some(_) => {
            lines.push(text("  Installed titles start from av, or with av play <title>. To start".into()));
            lines.push(text("  them by name too, add this to your shell's startup file:".into()));
            lines.push(code("    export PATH=\"$HOME/.local/bin:$PATH\""));
        }
        None => lines.push(text("  Installed titles start from av, or with av play <title>.".into())),
    }
    lines.push(text("  av never edits your shell's files, and titles add nothing to your menus.".into()));
    lines.extend([
        Line::default(),
        head("Fonts"),
        text("  Previews and some titles draw with Braille and Unicode block".into()),
        text("  characters. If they look broken, switch your terminal to a font".into()),
        text("  that has them.".into()),
        Line::default(),
        Line::from(Span::styled("any key to begin", Style::default().fg(theme::FAINT))),
    ]);
    Text::from(lines)
}

/// The wordmark as pixels: each half block is two pixels, top and bottom.
fn logo_pixels() -> Vec<Vec<bool>> {
    let mut rows = vec![Vec::new(); 4];
    for (r, line) in LOGO.iter().enumerate() {
        for c in line.chars() {
            let (top, bottom) = match c {
                '█' => (true, true),
                '▀' => (true, false),
                '▄' => (false, true),
                _ => (false, false),
            };
            rows[r * 2].push(top);
            rows[r * 2 + 1].push(bottom);
        }
    }
    rows
}

/// A colour of the intro's line, cycling through the palette with `phase`.
fn cycle(phase: f64) -> ratatui::style::Color {
    let stops = [(0x7f, 0xdc, 0x5e), (0x5f, 0xbf, 0x45), (0xff, 0xb0, 0x00), (0xe9, 0xc2, 0x7a)];
    let p = phase.rem_euclid(stops.len() as f64);
    let (i, u) = (p as usize, p.fract());
    let (a, b) = (stops[i], stops[(i + 1) % stops.len()]);
    let mix = |p: u8, q: u8| (p as f64 + (q as f64 - p as f64) * u).round() as u8;
    ratatui::style::Color::Rgb(mix(a.0, b.0), mix(a.1, b.1), mix(a.2, b.2))
}

/// The intro, `t` seconds in: a line grows in from both edges in cycling
/// colours, then the wordmark appears large with a bright band rolling over
/// its gradient, then the slogan. app::INTRO_SECS long; any key skips it.
fn draw_intro(buf: &mut Buffer, area: Rect, t: f64) {
    const GROW: f64 = 0.7;
    let mid = area.y + area.height / 2;
    let w = area.width as f64;
    // the line: in from both edges until they meet, then it parts round the wordmark
    if t < GROW + 0.4 {
        let reach = ((t / GROW).min(1.0) * w / 2.0).ceil() as u16;
        for i in 0..reach.min(area.width) {
            let color = cycle(t * 3.0 + i as f64 / 8.0);
            for x in [area.x + i, area.right().saturating_sub(1 + i)] {
                buf[(x, mid)].set_symbol("━").set_fg(color);
            }
        }
        return;
    }
    let pix = logo_pixels();
    let (pw, ph) = (pix[0].len() as u16, pix.len() as u16);
    // as large as fits: each pixel `s` cells wide and `s` half cells tall
    let s = ((area.width.saturating_sub(4)) / pw).clamp(1, 3);
    let (cw, ch) = (pw * s, (ph * s).div_ceil(2));
    let x0 = area.x + area.width.saturating_sub(cw) / 2;
    let y0 = mid.saturating_sub(ch / 2 + 1);
    // the bright band sweeps across once, starting as the wordmark appears
    let sweep = ((t - GROW - 0.4) / 1.0) * (cw as f64 + 20.0) - 10.0;
    let lit = |px: u16, py: u16| -> bool {
        let (x, y) = ((px / s) as usize, (py / s) as usize);
        pix.get(y).and_then(|r| r.get(x)).copied().unwrap_or(false)
    };
    for cy in 0..ch {
        for cx in 0..cw {
            let (top, bottom) = (lit(cx, cy * 2), lit(cx, cy * 2 + 1));
            let symbol = match (top, bottom) {
                (true, true) => "█",
                (true, false) => "▀",
                (false, true) => "▄",
                (false, false) => continue,
            };
            let base = logo_color(cx as usize, cw as usize);
            let glow = (1.0 - ((cx as f64 - sweep).abs() / 6.0)).max(0.0);
            let color = if let ratatui::style::Color::Rgb(r, g, b) = base {
                let up = |c: u8| (c as f64 + (255.0 - c as f64) * glow * 0.85).round() as u8;
                ratatui::style::Color::Rgb(up(r), up(g), up(b))
            } else {
                base
            };
            let (x, y) = (x0 + cx, y0 + cy);
            if x < area.right() && y < area.bottom() {
                buf[(x, y)].set_symbol(symbol).set_fg(color);
            }
        }
    }
    // the slogan, once the band has passed
    if t > GROW + 1.2 {
        let slogan = format!("{}   {}", SLOGAN[0], SLOGAN[1]);
        let sw = slogan.chars().count() as u16;
        let y = y0 + ch + 1;
        if y < area.bottom() {
            buf.set_string(area.x + area.width.saturating_sub(sw) / 2, y, slogan, Style::default().fg(theme::DIM));
        }
    }
}

fn centered(area: Rect, w: u16, h: u16) -> Rect {
    let w = w.min(area.width);
    let h = h.min(area.height);
    Rect { x: area.x + (area.width - w) / 2, y: area.y + (area.height - h) / 2, width: w, height: h }
}

/// Draws a dialog and returns where its buttons are, for clicks.
fn draw_modal(f: &mut Frame, app: &App, m: &Modal, area: Rect) -> Vec<(Rect, Hit)> {
    let (title, body, w, h): (&str, Text, u16, u16) = match m {
        Modal::Quit => (" quit ", Text::from("Leave Asciiverse?\n\nEnter to quit, any other key to stay."), 44, 7),
        Modal::Downloads => {
            let w = area.width.saturating_sub(4).min(104);
            let text = downloads_text(app, w.saturating_sub(2) as usize);
            let h = (text.lines.len() as u16 + 2).min(area.height.saturating_sub(2));
            (" downloads ", text, w, h)
        }
        Modal::Help => {
            let text = help_text(&app.paths.log(), app.av_newer.as_ref());
            // as tall as it wraps to: a long log path takes two rows
            let h = wrapped_height(&text, 76 - 2) + 2;
            (" help ", text, 76, h)
        }
        Modal::Welcome => {
            let text = welcome_text(app.paths.bin.as_deref(), app.paths.bin_on_path());
            let h = wrapped_height(&text, 76 - 2) + 2;
            (" welcome ", text, 76, h)
        }
        Modal::Message(s) => {
            let text = Text::from(format!("{s}\n\nany key to close"));
            let h = wrapped_height(&text, 70) + 2;
            (" note ", text, 72, h)
        }
        Modal::AvUpdate => {
            let Some(r) = &app.av_newer else { return Vec::new() };
            let mut lines = vec![
                Line::from(Span::styled(format!("{}.", crate::selfupdate::headline(r)), Style::default().fg(theme::AMBER).add_modifier(Modifier::BOLD))),
                Line::default(),
                Line::from("To update, run this in a terminal:"),
                Line::from(Span::styled(format!("  {}", crate::selfupdate::how()), Style::default().fg(theme::GREEN_HI))),
            ];
            if !r.notes.trim().is_empty() {
                lines.extend([Line::default(), Line::from(Span::styled("What's new", Style::default().fg(theme::AMBER_SOFT).add_modifier(Modifier::BOLD)))]);
                lines.extend(crate::md::render(&r.notes).lines);
            }
            lines.extend([Line::default(), Line::from(Span::styled("any key to close", Style::default().fg(theme::FAINT)))]);
            let text = Text::from(lines);
            let h = wrapped_height(&text, 72) + 2;
            (" av update ", text, 76, h)
        }
        Modal::Size => {
            let (w, h) = (area.width, area.height);
            let (mw, mh) = (crate::app::MIN_COLS, crate::app::MIN_ROWS);
            let fits = w >= mw && h >= mh;
            let size = Span::styled(format!("{w}x{h}"), Style::default().fg(if fits { theme::GREEN_HI } else { theme::AMBER }).add_modifier(Modifier::BOLD));
            // one paragraph each, so they wrap on any width; this terminal's
            // size first, so a very short one still shows what matters
            let mut lines = if fits {
                vec![Line::from(vec![Span::raw("This terminal is "), size, Span::raw(format!(" now. That fits: Asciiverse looks best at {mw}x{mh} or bigger."))])]
            } else {
                vec![
                    Line::from(vec![Span::raw("This terminal is "), size, Span::raw(format!(". Asciiverse looks best at {mw}x{mh} or bigger."))]),
                    Line::from("Make the window bigger, or the font smaller (Ctrl and -), and this follows along."),
                ]
            };
            lines.extend([Line::default(), Line::from("At that size every title's preview shows in full, with its buttons under it.")]);
            // wrapped, so it reads on the narrow terminals it is written for
            let text = Text::from(lines);
            let inner_w = 62.min(area.width.saturating_sub(2));
            let body_h = wrapped_height(&text, inner_w);
            let roomy = area.height >= body_h + 6;
            let r = centered(area, 64, body_h + if roomy { 6 } else { 3 });
            let inner = modal_box(f, " terminal size ", r);
            // on a very short one, the text and the button come first
            let (body, row, hint) = if roomy {
                let [body, _, row, _, hint] = Layout::vertical([Constraint::Min(0), Constraint::Length(1), Constraint::Length(1), Constraint::Length(1), Constraint::Length(1)]).areas(inner);
                (body, row, Some(hint))
            } else {
                let [body, row] = Layout::vertical([Constraint::Min(0), Constraint::Length(1)]).areas(inner);
                (body, row, None)
            };
            f.render_widget(Paragraph::new(text).wrap(Wrap { trim: false }), body);
            let button = Span::styled(" Continue ", Style::default().fg(theme::BG).bg(theme::GREEN_HI).add_modifier(Modifier::BOLD));
            let bw = button.width() as u16;
            f.render_widget(Paragraph::new(button), row);
            if let Some(hint) = hint {
                f.render_widget(Paragraph::new(Span::styled("Enter or a click to go on", Style::default().fg(theme::FAINT))), hint);
            }
            return vec![(Rect { width: bw.min(row.width), ..row }, Hit::Choice(0))];
        }
        Modal::Choice { title, text, options, sel } => {
            let h = 6 + text.lines().count() as u16 + text.len() as u16 / 60;
            let r = centered(area, 64, h);
            let inner = modal_box(f, title, r);
            // the text on top, then the buttons and the hint on fixed rows at
            // the bottom, so each button's place is known for a click
            let [body, _, row, _, hint] = Layout::vertical([
                Constraint::Min(0),
                Constraint::Length(1),
                Constraint::Length(1),
                Constraint::Length(1),
                Constraint::Length(1),
            ])
            .areas(inner);
            f.render_widget(Paragraph::new(text.as_str()).wrap(Wrap { trim: false }), body);
            let mut spans = Vec::new();
            let mut hits = Vec::new();
            let mut x = row.x;
            for (i, (label, _)) in options.iter().enumerate() {
                let st = if i == *sel {
                    Style::default().fg(theme::BG).bg(theme::GREEN_HI).add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(theme::GREEN_HI).bg(theme::PANEL)
                };
                let button = Span::styled(format!(" {label} "), st);
                let w = button.width() as u16;
                hits.push((Rect { x, y: row.y, width: w.min(row.right().saturating_sub(x)), height: 1 }.intersection(row), Hit::Choice(i)));
                x = x.saturating_add(w + 2);
                spans.push(button);
                spans.push(Span::raw("  "));
            }
            f.render_widget(Paragraph::new(Line::from(spans)), row);
            let tip = "click, or arrows and Enter; Esc cancels";
            f.render_widget(Paragraph::new(Span::styled(tip, Style::default().fg(theme::FAINT))), hint);
            return hits;
        }
        Modal::Uninstall { id } => {
            let name = app.titles.iter().find(|t| t.id == *id).map(|t| t.title.clone()).unwrap_or(id.clone());
            let opt = |on: bool, s: &str| {
                Span::styled(
                    format!(" {} {s} ", if on { "◉" } else { "○" }),
                    if on { Style::default().fg(theme::BG).bg(theme::GREEN_HI) } else { Style::default().fg(theme::DIM) },
                )
            };
            let body = Text::from(vec![
                Line::from(format!("Uninstall {name}?")),
                Line::default(),
                Line::from(opt(app.uninstall_keep_data, "keep its saves and settings")),
                Line::from(opt(!app.uninstall_keep_data, "remove its saves and settings too")),
                Line::default(),
                Line::from(Span::styled("arrows choose, Enter uninstalls, Esc cancels", Style::default().fg(theme::FAINT))),
            ]);
            (" uninstall ", body, 56, 10)
        }
    };
    let r = centered(area, w, h);
    let inner = modal_box(f, title, r);
    // taller than the screen: it scrolls, and the last row says so
    let full = wrapped_height(&body, inner.width);
    let max = full.saturating_sub(inner.height.saturating_sub(1));
    let tall = full > inner.height;
    app.modal_scroll_max.set(if tall { max } else { 0 });
    if tall {
        let [text, hint] = Layout::vertical([Constraint::Min(0), Constraint::Length(1)]).areas(inner);
        let top = app.modal_scroll.min(max);
        f.render_widget(Paragraph::new(body).wrap(Wrap { trim: false }).scroll((top, 0)), text);
        let more = format!("↑↓ PgUp PgDn to scroll ({} of {} lines)", (top + text.height).min(full), full);
        f.render_widget(Paragraph::new(Span::styled(more, Style::default().fg(theme::AMBER))), hint);
    } else {
        f.render_widget(Paragraph::new(body).wrap(Wrap { trim: false }), inner);
    }
    Vec::new()
}

/// How many rows `text` takes wrapped at `width`, counted the way the
/// paragraph wraps it (ratatui's rendered-line-info feature).
fn wrapped_height(text: &Text, width: u16) -> u16 {
    Paragraph::new(text.clone()).wrap(Wrap { trim: false }).line_count(width.max(1)) as u16
}

/// A dialog's frame, drawn over whatever is under it; returns its inside.
fn modal_box(f: &mut Frame, title: &str, r: Rect) -> Rect {
    f.render_widget(Clear, r);
    let b = Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Double)
        .border_style(Style::default().fg(theme::AMBER))
        .title(Span::styled(title.to_string(), Style::default().fg(theme::AMBER)))
        .style(Style::default().bg(theme::PANEL_HI).fg(theme::TEXT_HI));
    let inner = b.inner(r);
    f.render_widget(b, r);
    inner
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app::App;
    use crate::paths::Paths;
    use ratatui::backend::TestBackend;

    /// An App over empty folders, for drawing dialogs.
    fn app(name: &str) -> App {
        let dir = std::env::temp_dir().join(format!("av-ui-{name}-{}", std::process::id()));
        let paths = Paths { install: dir.join("data"), state: dir.join("data"), cache: dir.join("cache"), config: dir.join("config"), bin: None };
        std::fs::create_dir_all(&paths.cache).unwrap();
        App::new(paths, "http://127.0.0.1:9/catalog.json".into())
    }

    /// The screen's text after drawing `m` at `w`x`h`.
    fn draw_note(app: &App, m: &Modal, w: u16, h: u16) -> Vec<String> {
        let mut t = ratatui::Terminal::new(TestBackend::new(w, h)).unwrap();
        t.draw(|f| {
            draw_modal(f, app, m, f.area());
        })
        .unwrap();
        let buf = t.backend().buffer().clone();
        (0..h).map(|y| (0..w).map(|x| buf[(x, y)].symbol()).collect::<String>()).collect()
    }

    #[test]
    fn help_scrolls_on_a_small_terminal() {
        let mut a = app("help");
        let rows = draw_note(&a, &Modal::Help, 80, 24);
        assert!(rows.iter().any(|r| r.contains("to scroll")), "says it scrolls");
        assert!(!rows.iter().any(|r| r.contains("any key to close")), "the end is below");
        a.modal_scroll = a.modal_scroll_max.get();
        let rows = draw_note(&a, &Modal::Help, 80, 24);
        assert!(rows.iter().any(|r| r.contains("Something wrong?")) || rows.iter().any(|r| r.contains("any key to close")), "scrolled to the end: {rows:#?}");
        assert!(rows.iter().any(|r| r.contains("any key to close")));
        // tall enough: no scrolling
        let rows = draw_note(&a, &Modal::Help, 80, 60);
        assert!(!rows.iter().any(|r| r.contains("to scroll")));
        assert!(rows.iter().any(|r| r.contains("any key to close")));
    }

    #[test]
    fn the_size_note_reads_on_small_terminals() {
        let a = app("size");
        for (w, h) in [(60, 12), (50, 8), (128, 40)] {
            let rows = draw_note(&a, &Modal::Size, w, h).join("\n");
            assert!(rows.contains("Continue"), "{w}x{h}: the button shows\n{rows}");
            assert!(rows.contains("128x40"), "{w}x{h}: the size it wants shows\n{rows}");
            assert!(rows.contains(&format!("{w}x{h}")), "{w}x{h}: this terminal's size shows\n{rows}");
        }
    }

    #[test]
    fn release_notes_are_rendered_not_raw() {
        let mut a = app("notes");
        a.av_newer = Some(crate::catalog::AvRelease { version: "9.9.9".into(), date: String::new(), notes: "- **Bold** and `code`".into() });
        let rows = draw_note(&a, &Modal::AvUpdate, 100, 30).join("\n");
        assert!(rows.contains("av 9.9.9 is out"));
        assert!(rows.contains("Bold and code"), "{rows}");
        assert!(!rows.contains("**"));
    }

    #[test]
    fn selected_text_joins_wrapped_rows() {
        // a 20-wide paragraph, wrapped as ratatui wraps it, then a new one
        let lines = ["Points joined by", "lines along every", "row.", "", "Next paragraph."];
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, lines.len() as u16));
        for (y, l) in lines.iter().enumerate() {
            buf.set_string(0, y as u16, l, Style::default());
        }
        let all = selected_text(&buf, (0, 0), (19, 4), 20);
        assert_eq!(all, "Points joined by lines along every row.\n\nNext paragraph.");
        // from the middle of a row to the middle of a later one, dragged backwards
        assert_eq!(selected_text(&buf, (4, 1), (7, 0), 20), "joined by lines");
    }

    #[test]
    fn buttons_beside_the_preview_are_clickable_where_they_are_drawn() {
        // wide enough for the side-by-side layout, with a description that
        // wraps over several rows in the narrow column beside the preview
        let mut a = app("buttons-beside");
        let t = Title {
            id: "wide".into(),
            title: "Wide".into(),
            description: "A description long enough to wrap over several rows in the narrow column that sits beside the preview, pushing the buttons down".into(),
            ..Title::default()
        };
        // beside the preview, tall under it, and one row under it at the minimum size
        for (w, h, rows) in [(200, 60, 3), (102, 60, 3), (102, 37, 1)] {
            a.hits.clear();
            let mut term = ratatui::Terminal::new(TestBackend::new(w, h)).unwrap();
            term.draw(|f| draw_detail(f, &mut a, &t, f.area())).unwrap();
            let buf = term.backend().buffer().clone();
            let labels: Vec<String> = a.actions(&t).iter().map(|(_, l, _)| l.to_string()).collect();
            let mut found = vec![(0, false); labels.len()];
            for (r, hit) in &a.hits {
                if let Hit::Button(i) = hit {
                    let under: String = (r.x..r.x + r.width).map(|x| buf[(x, r.y)].symbol().to_string()).collect();
                    // the label's row, or the half-block rows above and under it
                    let edge = under.chars().all(|c| c == '▄') || under.chars().all(|c| c == '▀');
                    assert!(edge || under.contains(labels[*i].as_str()), "{w}x{h}: button {i} ({}) is clickable over {under:?}", labels[*i]);
                    found[*i].0 += 1;
                    found[*i].1 |= !edge;
                }
            }
            for (i, (n, label)) in found.iter().enumerate() {
                assert_eq!((*n, *label), (rows, true), "{w}x{h}: button {i} is clickable on all its {rows} rows");
            }
        }
    }

    #[test]
    fn the_list_scrolls_before_the_selection_reaches_the_edge() {
        let mut a = app("scroll-margin");
        a.titles = (0..60)
            .map(|i| Title { id: format!("t{i:02}"), title: format!("Title {i:02}"), kind: "game".into(), ..Title::default() })
            .collect();
        let (h, inner) = (22u16, 20usize);
        let mut term = ratatui::Terminal::new(TestBackend::new(40, h)).unwrap();
        // title i is list row i + 1, under the "games" section line
        let rows_below = |a: &App| (a.list_offset + inner).saturating_sub(a.selected + 1 + 1);
        for i in 0..60 {
            a.selected = i;
            term.draw(|f| draw_list(f, &mut a, f.area())).unwrap();
            let below = rows_below(&a);
            let left = 59 - i;
            assert!(below >= LIST_SCROLL_MARGIN.min(left), "at title {i}, only {below} rows show below it");
        }
        // at the end of the list the selection sits on the last line
        assert_eq!(rows_below(&a), 0);
        // and back up: the margin holds above it too
        for i in (0..60).rev() {
            a.selected = i;
            term.draw(|f| draw_list(f, &mut a, f.area())).unwrap();
            let above = (i + 1) - a.list_offset;
            assert!(above >= LIST_SCROLL_MARGIN.min(i + 1), "at title {i}, only {above} rows show above it");
        }
        assert_eq!(a.list_offset, 0);
    }

    #[test]
    fn a_click_near_the_edge_does_not_move_the_list() {
        let mut a = app("scroll-click");
        a.titles = (0..60)
            .map(|i| Title { id: format!("t{i:02}"), title: format!("Title {i:02}"), kind: "game".into(), ..Title::default() })
            .collect();
        let mut term = ratatui::Terminal::new(TestBackend::new(40, 22)).unwrap();
        term.draw(|f| draw_list(f, &mut a, f.area())).unwrap();
        // the second-to-last row on screen, inside the margin
        a.selected = 17;
        a.list_clicked = true;
        term.draw(|f| draw_list(f, &mut a, f.area())).unwrap();
        assert_eq!(a.list_offset, 0, "the clicked title stays under the pointer");
    }

    fn titled(version: &str, news: &[(&str, &str)]) -> Title {
        let mut t = Title { version: version.into(), ..Title::default() };
        t.news = news.iter().map(|(v, notes)| crate::catalog::News { version: (*v).into(), date: "2026-10-28".into(), notes: (*notes).into() }).collect();
        t
    }

    fn plain(lines: &[Line]) -> Vec<String> {
        lines.iter().map(|l| l.spans.iter().map(|s| s.content.as_ref()).collect()).collect()
    }

    #[test]
    fn news_marks_what_an_update_brings() {
        let t = titled("1.2.0", &[("1.2.0", "Faster."), ("1.1.0", "Sound."), ("1.0.0", "First."), ("0.9.0", "Old.")]);
        let text = plain(&news(&t, Some("1.1.0")));
        assert!(text.contains(&"NEWS".to_string()));
        assert!(text.contains(&"1.2.0  2026-10-28  new in the update".to_string()), "{text:?}");
        assert!(text.contains(&"1.1.0  2026-10-28  installed".to_string()), "{text:?}");
        assert!(text.contains(&"1.0.0  2026-10-28".to_string()), "{text:?}");
        assert!(!text.iter().any(|l| l.starts_with("0.9.0")), "only the latest {NEWS_SHOWN}");
        // not installed: nothing is marked; no notes: no NEWS at all
        assert!(!plain(&news(&t, None)).iter().any(|l| l.contains("new in the update") || l.contains("installed")));
        assert!(news(&titled("1.0.0", &[("1.0.0", " ")]), None).is_empty());
    }

    #[test]
    fn help_and_welcome_fit_their_dialogs_without_wrapping() {
        // 76 wide less the frame; a wrapped line would push the last one out of the box
        let inner = 74;
        let bin = std::path::Path::new("/home/someone/.local/bin");
        for (bin, on_path) in [(Some(bin), false), (Some(bin), true), (None, false)] {
            let logs = [
                std::path::Path::new("/home/someone/.cache/asciiverse/av.log"),
                std::path::Path::new(r"C:\Users\someone.longer\AppData\Local\Asciiverse\cache\av.log"),
            ];
            for text in logs.map(|l| help_text(l, None)).into_iter().chain([welcome_text(bin, on_path)]) {
                for l in &text.lines {
                    assert!(l.width() <= inner, "too wide ({}): {:?}", l.width(), plain(std::slice::from_ref(l)));
                }
            }
        }
    }

    #[test]
    fn the_intro_draws_at_every_moment_and_size() {
        for (w, h) in [(140, 42), (80, 24), (20, 5), (1, 1)] {
            let area = Rect::new(0, 0, w, h);
            for i in 0..30 {
                let mut buf = Buffer::empty(area);
                draw_intro(&mut buf, area, i as f64 * crate::app::INTRO_SECS / 29.0);
            }
        }
    }
}
