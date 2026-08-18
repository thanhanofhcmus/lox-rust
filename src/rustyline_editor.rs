use rustyline::{Config, EditMode, Editor, highlight::MatchingBracketHighlighter, history::DefaultHistory};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter, Validator};

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct ReplHelper {
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

pub type RLEditor = Editor<ReplHelper, DefaultHistory>;

pub fn new() -> rustyline::Result<RLEditor> {
    let c = Config::builder()
        .edit_mode(EditMode::Emacs)
        .auto_add_history(false)
        .history_ignore_space(true)
        .color_mode(rustyline::ColorMode::Forced)
        .build();

    let mut rl = RLEditor::with_config(c)?;
    rl.set_helper(Some(ReplHelper {
        highlighter: MatchingBracketHighlighter::new(),
    }));

    rl.add_history_entry("_dbg_heap_stats();")?;
    rl.add_history_entry("_dbg_state();")?;

    Ok(rl)
}
