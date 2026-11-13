//! Tab completion. `av completions <shell>` prints a script for the shell;
//! the script calls the hidden `av __complete <words so far>` on every Tab,
//! which answers from the cached catalog and installed.json only, never the
//! network, so it is instant and works offline. The shells only filter by
//! what has been typed of the current word.

use std::collections::BTreeSet;

use anyhow::{Result, bail};

use crate::catalog;
use crate::paths::Paths;
use crate::state::State;

pub const SHELLS: [&str; 4] = ["bash", "zsh", "fish", "powershell"];

const COMMANDS: [&str; 9] = ["install", "update", "uninstall", "play", "list", "search", "info", "completions", "help"];

const BASH: &str = r#"# av tab completion for bash; in ~/.bashrc:  eval "$(av completions bash)"
_av() {
    local IFS=$'\n'
    COMPREPLY=($(compgen -W "$(av __complete "${COMP_WORDS[@]:1:COMP_CWORD-1}" 2>/dev/null)" -- "${COMP_WORDS[COMP_CWORD]}"))
}
complete -o default -F _av av
"#;

// The #compdef line and the funcstack test let the same file work sourced
// and autoloaded from a completion folder on $fpath (the .deb puts it there).
const ZSH: &str = r#"#compdef av
# av tab completion for zsh; in ~/.zshrc, after compinit:  source <(av completions zsh)
_av() {
    local -a c
    c=(${(f)"$(av __complete "${(@)words[2,CURRENT-1]}" 2>/dev/null)"})
    if (( ${#c} )); then compadd -a c; else _files; fi
}
if [[ $funcstack[1] == _av ]]; then _av "$@"; else compdef _av av; fi
"#;

const FISH: &str = r#"# av tab completion for fish:  av completions fish > ~/.config/fish/completions/av.fish
complete -c av -f -a '(av __complete (commandline -opc)[2..-1] 2>/dev/null)'
"#;

const POWERSHELL: &str = r#"# av tab completion for PowerShell; in $PROFILE:  av completions powershell | Out-String | Invoke-Expression
Register-ArgumentCompleter -Native -CommandName av, av.exe -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)
    $words = @($commandAst.CommandElements | Select-Object -Skip 1 | ForEach-Object { $_.ToString() })
    if ($wordToComplete -ne '') { $words = @($words | Select-Object -SkipLast 1) }
    av __complete @words 2>$null | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
    }
}
"#;

pub fn script(shell: &str) -> Result<&'static str> {
    Ok(match shell {
        "bash" => BASH,
        "zsh" => ZSH,
        "fish" => FISH,
        "powershell" | "pwsh" => POWERSHELL,
        _ => bail!("no completion for {shell:?}; av completes in {}", SHELLS.join(", ")),
    })
}

/// What the cached catalog and installed.json know, for `candidates`.
pub struct Known {
    /// every catalog id
    pub titles: Vec<String>,
    /// MUD ids, playable without installing
    pub muds: BTreeSet<String>,
    pub installed: BTreeSet<String>,
}

impl Known {
    pub fn load(paths: &Paths) -> Known {
        let state = State::load(&paths.state);
        let (titles, muds) = match catalog::cached(&paths.cache, &catalog::url()) {
            Some(c) => (
                c.titles.keys().cloned().collect(),
                c.titles.values().filter(|t| t.is_mud()).map(|t| t.id.clone()).collect(),
            ),
            None => Default::default(),
        };
        Known { titles, muds, installed: state.installed.into_keys().collect() }
    }
}

/// Candidates for the word after `words` (the words already typed after `av`).
pub fn candidates(words: &[String], k: &Known) -> Vec<String> {
    let Some((cmd, args)) = words.split_first() else {
        return COMMANDS.iter().map(|s| s.to_string()).collect();
    };
    let installed = || k.installed.iter().cloned().collect::<Vec<_>>();
    // a title named once needs no second offer
    let fresh = |v: Vec<String>| v.into_iter().filter(|t| !args.contains(t)).collect();
    match cmd.as_str() {
        "install" => fresh(k.titles.clone()),
        "update" | "upgrade" | "uninstall" | "remove" => fresh(installed()),
        // only the title: what follows goes to the title itself
        "play" | "run" if args.is_empty() => k.installed.union(&k.muds).cloned().collect(),
        "info" | "show" if args.is_empty() => k.titles.clone(),
        "list" | "ls" if args.is_empty() => ["installed", "updates", "wishlist"].map(String::from).to_vec(),
        "completions" if args.is_empty() => SHELLS.map(String::from).to_vec(),
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn w(s: &str) -> Vec<String> {
        s.split_whitespace().map(String::from).collect()
    }

    #[test]
    fn completes_by_position() {
        let k = Known {
            titles: w("nethack solitty zork"),
            muds: ["discworld".to_string()].into(),
            installed: ["solitty".to_string(), "zork".to_string()].into(),
        };
        assert!(candidates(&[], &k).contains(&"install".to_string()));
        assert_eq!(candidates(&w("install"), &k), w("nethack solitty zork"));
        assert_eq!(candidates(&w("install nethack"), &k), w("solitty zork"));
        assert_eq!(candidates(&w("uninstall"), &k), w("solitty zork"));
        assert_eq!(candidates(&w("play"), &k), w("discworld solitty zork"));
        assert!(candidates(&w("play zork"), &k).is_empty());
        assert_eq!(candidates(&w("list"), &k), w("installed updates wishlist"));
        assert!(candidates(&w("search"), &k).is_empty());
        for s in SHELLS {
            assert!(script(s).unwrap().contains("__complete"));
        }
    }
}
