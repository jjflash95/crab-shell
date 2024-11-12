use git2::Repository;

pub fn get_current_branch() -> Option<String> {
    let current_dir = std::env::current_dir().ok()?;
    let repo = Repository::discover(current_dir).ok()?;
    let branch = repo.head().ok()?.shorthand().map(ToString::to_string);
    branch
}

