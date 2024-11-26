use core::str;

use git2::{Delta, Repository, StatusOptions};

pub struct GitCompletions;

pub trait AutocompStrat {
    fn suggest(args: &[&str]) -> Option<Vec<String>>;
}

pub fn suggest_autocomp(cmd: &str) -> Option<Vec<String>> {
    let parts = cmd.trim().split(" ").collect::<Vec<_>>();

    let (cmd, parts) = match &parts[..] {
        [] => return None,
        [head, parts @ ..] => (*head, parts),
    };

    match cmd {
        "git" => GitCompletions::suggest(parts),
        _ => None,
    }
}

impl GitCompletions {
    fn get_branches(prefix: Option<&str>) -> Vec<String> {
        let Ok(repo) = Repository::discover(".") else {
            return Default::default();
        };
        let Ok(branches) = repo.branches(None) else {
            return Default::default();
        };
        branches
            .into_iter()
            .filter_map(Result::ok)
            .filter_map(|(b, _)| b.name().ok().flatten().map(str::to_string))
            .filter(|name| prefix.map(|p| name.starts_with(p)).unwrap_or(true))
            .collect::<Vec<_>>()
    }

    fn get_changed_files(prefix: Option<&str>, exclude: Option<&[&str]>) -> Vec<String> {
        let Ok(repo) = Repository::discover(".") else {
            return Default::default();
        };

        let mut stopts = StatusOptions::new();
        stopts.include_untracked(true).include_ignored(false);
        let statuses = repo.statuses(Some(&mut stopts));

        let mut files = statuses
            .into_iter()
            .map(|e| {
                e.iter()
                    .map(|se| se.path().map(|p| p.to_string()))
                    .collect::<Vec<_>>()
            })
            .flat_map(|s| s.into_iter())
            .flatten()
            .collect::<Vec<_>>();

        if let Some(prefix) = prefix {
            files.retain(|f| f.starts_with(prefix))
        }
        if let Some(exclude) = exclude {
            files.retain(|f| !exclude.contains(&f.as_str()))
        }
        files
    }

    fn get_commands() -> Vec<String> {
        [
            "clone", "init", "add", "mv", "restore", "rm", "bisect", "diff", "grep", "log", "show",
            "status", "branch", "checkout", "commit", "merge", "rebase", "reset", "switch", "tag",
            "fetch", "pull", "push",
        ]
        .iter()
        .copied()
        .map(str::to_string)
        .collect()
    }
}

impl AutocompStrat for GitCompletions {
    fn suggest(args: &[&str]) -> Option<Vec<String>> {
        match args {
            [] => Some(Self::get_commands()),
            ["checkout"] => Some(Self::get_branches(None)),
            ["checkout", prefix] => Some(Self::get_branches(Some(prefix))),
            ["add"] => Some(Self::get_changed_files(None, None)),
            ["add", prefix] => Some(Self::get_changed_files(Some(prefix), None)),
            ["add", files @ .., prefix] => Some(Self::get_changed_files(Some(prefix), Some(files))),
            [partial] => {
                let cmds: Vec<_> = Self::get_commands()
                    .into_iter()
                    .filter(|c| c.starts_with(partial) && c.as_str() != *partial)
                    .collect();
                if cmds.is_empty() {
                    None
                } else {
                    Some(cmds)
                }
            }

            _ => None,
        }
    }
}
