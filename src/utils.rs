use std::cmp;

use fuzzy_matcher::{skim::SkimMatcherV2, FuzzyMatcher as _};
use itertools::Itertools as _;

pub struct MatchedString<'a>(pub &'a str, pub bool, pub (i64, Vec<usize>));

pub fn fuzzy_sort_strings<'a>(needle: &str, haystack: &'a [String]) -> Vec<MatchedString<'a>> {
    let matcher = SkimMatcherV2::default();
    let mut matched = haystack
        .iter()
        .unique()
        .map(|c| (c, matcher.fuzzy_indices(c, &needle)))
        .filter(|(_, m)| m.is_some())
        .map(|(c, m)| MatchedString::new(c, m.unwrap()))
        .collect::<Vec<_>>();

    matched.sort();
    matched
}

pub fn split_last_unescaped(haystack: &str, filter: fn(char) -> bool) -> (&str, &str) {
    let mut chars = haystack.char_indices().rev();

    while let Some((i, c)) = chars.next() {
        if filter(c) && chars.next().is_some_and(|(_, c)| c != '\\') {
            return (&haystack[..i], &haystack[i..]);
        }
    }

    ("", haystack)
}

pub fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t'
}

pub fn is_control_flow(c: char) -> bool {
    matches!(c, '|' | '&' | '>' | '<' | '(' | ')' | '{' | '}')
}

pub fn remove_escape_codes(s: &str) -> String {
    let mut out = String::new();
    let mut peek = s.chars().peekable();

    while let Some(c) = peek.next() {
        if c == '\\' && peek.peek().is_some() {
            out.push(peek.next().unwrap());
        } else {
            out.push(c)
        }
    }

    out
}

pub fn add_escape_codes(s: &str) -> String {
    let mut out = String::new();

    for c in s.chars() {
        if c == ' ' || c == '(' || c == ')' {
            out.push('\\');
        }
        out.push(c)
    }

    out
}

pub fn cmd_from_parts<'a>(start: &'a str, mid: &'a str, tip: &'a str) -> String {
    let mut out = "".to_string();
    if !start.is_empty() {
        out.push_str(start);
    };
    if !mid.is_empty() {
        out.push_str(mid);
    };

    if !out.is_empty() {
        out.push(' ');
    };
    out.push_str(tip);
    out
}

impl<'a> MatchedString<'a> {
    pub fn new(cmd: &'a str, (score, indices): (i64, Vec<usize>)) -> Self {
        Self(cmd, false, (score, indices))
    }

    pub fn set_selected(&mut self) {
        self.1 = true
    }
}

impl<'a> Ord for MatchedString<'a> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let this = &self.2;
        let that = &other.2;
        this.cmp(that)
    }
}

impl<'a> PartialEq for MatchedString<'a> {
    fn eq(&self, other: &Self) -> bool {
        let this = &self.2;
        let that = &other.2;
        this.eq(that)
    }
}

impl<'a> Eq for MatchedString<'a> {}
impl<'a> PartialOrd for MatchedString<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
