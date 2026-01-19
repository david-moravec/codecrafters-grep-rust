use anyhow::Result;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::regex_parser::{Anchor, Parser, RegexMatchable, State, StateKind, StateMut};

struct StateCollection {
    states: Vec<StateMut>,
    id: i64,
}

impl StateCollection {
    pub fn new(start_id: i64) -> Self {
        Self {
            states: vec![],
            id: start_id,
        }
    }

    pub fn add_state(&mut self, state: StateMut) {
        if state.list_id.get() == self.id {
            return;
        }
        state.list_id.set(self.id);

        match state.kind {
            StateKind::Split(ref n1, ref n2, _) => {
                self.add_state(n1.get().unwrap().clone());
                self.add_state(n2.get().unwrap().clone());
            }
            StateKind::Repeat(ref repeating_state) => {
                if repeating_state.can_repeat_more() {
                    self.add_state(repeating_state.to_repeat.clone());
                }

                if repeating_state.number_of_repeats_met() {
                    self.add_state(repeating_state.next.get().unwrap().clone());

                    if !repeating_state.can_repeat_more() {
                        return repeating_state.hit_count.set(0);
                    }
                }

                repeating_state
                    .hit_count
                    .set(repeating_state.hit_count.get() + 1);
            }
            StateKind::StartRecording(_, ref next) | StateKind::StopRecording(_, ref next) => {
                self.add_state(next.get().unwrap().clone());
            }
            StateKind::Replay(_, _) => panic!("Cannot use thompson Algorithm with backreference"),
            _ => self.states.push(state.clone()),
        }
    }

    pub fn clear(&mut self) {
        self.id += 1;
        self.reset_list_ids();
        self.states.clear();
    }

    pub fn has_match(&self) -> bool {
        self.states.iter().any(|s| s.kind.is_match())
    }

    pub fn check_match_after_reaching_end(&self) -> bool {
        if self.states.len() > 0 {
            self.states
                .iter()
                .any(|s| s.kind.check_is_match_after_reaching_end_of_input())
        } else {
            false
        }
    }

    fn reset_list_ids(&self) {
        for state in self.states.iter() {
            state.list_id.set(-1);
        }
    }
}

impl Drop for StateCollection {
    fn drop(&mut self) {
        self.reset_list_ids();
    }
}

pub struct RegexMatch {
    pub match_str: String,
    pub start: usize,
    pub end: usize,
}

impl RegexMatch {
    pub fn new(input: &str, start: usize, end: usize) -> Self {
        RegexMatch {
            match_str: String::from_iter(input.chars().collect::<Vec<char>>()[start..end].iter()),
            start,
            end,
        }
    }
}

pub struct RegexPattern<'a> {
    pattern: &'a str,
    start_state: State,
    last_list_id: Cell<i64>,
    to_match: Cell<Vec<char>>,
    is_backref_present: bool,
    backrefs: Cell<HashMap<u32, Vec<char>>>,
    recording_backrefs_ids: Cell<HashSet<u32>>,
}

impl<'a> RegexPattern<'a> {
    pub fn new(pattern: &'a str) -> Result<Self> {
        let mut parser = Parser::new(&pattern);
        let start_state = parser.parse()?;

        return Ok(RegexPattern {
            pattern,
            start_state,
            last_list_id: Cell::new(0),
            to_match: Cell::new(vec![]),
            is_backref_present: parser.is_backreference_present,
            backrefs: Cell::new(HashMap::new()),
            recording_backrefs_ids: Cell::new(HashSet::new()),
        });
    }

    pub fn matches(&self, to_match: &str) -> Result<bool> {
        self.backrefs.set(HashMap::new());
        self.recording_backrefs_ids.set(HashSet::new());

        let matches_vec = if self.is_backref_present {
            if let Some(matched) = self.match_backtracking(to_match)? {
                Ok(vec![matched])
            } else {
                Ok(vec![])
            }
        } else {
            self.match_no_backtracking(to_match)
        };

        match matches_vec {
            Ok(v) => Ok(v.len() > 0),
            Err(e) => Err(e),
        }
    }

    pub fn all_matches(&self, to_match: &str) -> Result<Vec<RegexMatch>> {
        self.backrefs.set(HashMap::new());
        self.recording_backrefs_ids.set(HashSet::new());

        if self.is_backref_present {
            if let Some(matched) = self.match_backtracking(to_match)? {
                Ok(vec![matched])
            } else {
                Ok(vec![])
            }
        } else {
            self.match_no_backtracking(to_match)
        }
    }

    fn match_no_backtracking(&self, to_match: &str) -> Result<Vec<RegexMatch>> {
        let chars: Vec<char> = to_match.chars().collect();

        let mut matches_vec = vec![];

        let mut i = 0;

        while i < chars.len() {
            match self.thompson_algorithm(&chars[i..].iter().collect::<String>(), i == 0) {
                Ok(matches) => {
                    if matches.0 {
                        matches_vec.push(RegexMatch::new(to_match, i, i + matches.1));
                    }

                    i += {
                        if matches.1 > 0 {
                            matches.1
                        } else {
                            1
                        }
                    };
                }
                Err(e) => return Err(e),
            }
        }

        return Ok(matches_vec);
    }

    fn thompson_algorithm(
        &self,
        to_match: &str,
        starts_from_beginning: bool,
    ) -> Result<(bool, usize)> {
        let chars: Vec<char> = to_match.chars().collect();
        let mut next_states: StateCollection = StateCollection::new(self.last_list_id.get());
        let mut current_states: StateCollection = StateCollection::new(self.last_list_id.get());

        if let Some(start) = self.init_state(starts_from_beginning) {
            current_states.add_state(start);
        } else {
            return Ok((false, 0));
        }

        for (i, c) in chars.iter().enumerate() {
            next_states.clear();
            current_states.reset_list_ids();

            self.last_list_id.set(next_states.id);

            if current_states.states.len() == 0 {
                return Ok((false, 0));
            } else if current_states.has_match() {
                for state in current_states.states.iter() {
                    if let State {
                        kind: StateKind::Simple(ref matchable, _),
                        id: _,
                        list_id: _,
                    } = **state
                    {
                        if matchable.matches(*c) {
                            return Ok((true, i + 1));
                        }
                    }
                }

                return Ok((true, i));
            }

            self.thompson_step(*c, &current_states, &mut next_states);

            current_states.states = next_states.states.clone();
        }

        // For pattern to match it needs to reach match state or end of string state
        //  before exhausting input string

        Ok((current_states.check_match_after_reaching_end(), chars.len()))
    }

    fn thompson_step(
        &self,
        c: char,
        current_states: &StateCollection,
        next_states: &mut StateCollection,
    ) {
        for state in current_states.states.iter() {
            if let State {
                kind: StateKind::Simple(ref matchable, ref n),
                id: _,
                list_id: _,
            } = **state
            {
                if matchable.matches(c) {
                    next_states.add_state(n.get().unwrap().clone());
                } else {
                    if let State {
                        kind: StateKind::Repeat(ref repeating_state),
                        id: _,
                        list_id: _,
                    } = *n.get().unwrap().clone()
                    {
                        repeating_state.hit_count.set(0);
                    }
                }
            } else {
                panic!("Only statates with matchable can be matched");
            }
        }
    }

    fn match_backtracking(&self, to_match: &str) -> Result<Option<RegexMatch>> {
        let mut init_state: Rc<State>;

        for (i, _) in to_match.chars().enumerate() {
            if let Some(start) = self.init_state(i == 0) {
                init_state = start;
            } else {
                return Ok(None);
            }

            self.to_match.set(
                to_match.chars().collect::<Vec<char>>()[i..]
                    .into_iter()
                    .map(|c| *c)
                    .collect(),
            );

            let matched = self.backtracking(init_state, 0)?;

            if matched.0 {
                return Ok(Some(RegexMatch::new(to_match, i, matched.1)));
            }
        }

        Ok(None)
    }

    fn backtracking(&self, state: Rc<State>, to_match_index: usize) -> Result<(bool, usize)> {
        if state.kind.leads_directly_to_match() {
            return Ok((true, to_match_index));
        }

        match state.kind {
            StateKind::Simple(ref matchable, ref next) => {
                let to_match = self.to_match.take();

                if to_match.len() <= to_match_index {
                    let len_to_match = to_match.len();
                    self.to_match.set(to_match);

                    return Ok((
                        state.kind.check_is_match_after_reaching_end_of_input(),
                        len_to_match,
                    ));
                }

                let current_char = to_match[to_match_index];

                self.to_match.set(to_match);

                if matchable.matches(current_char) {
                    self.record_backref(current_char)?;

                    return self.backtracking(next.get().unwrap().clone(), to_match_index + 1);
                } else {
                    Ok((false, 0))
                }
            }
            StateKind::StartRecording(i, ref next) => {
                let mut recording_backref = self.recording_backrefs_ids.take();
                recording_backref.insert(i);
                self.recording_backrefs_ids.set(recording_backref);

                self.backtracking(next.get().unwrap().clone(), to_match_index)
            }
            StateKind::StopRecording(i, ref next) => {
                let mut recording_backref = self.recording_backrefs_ids.take();
                recording_backref.remove(&i);
                self.recording_backrefs_ids.set(recording_backref);

                self.backtracking(next.get().unwrap().clone(), to_match_index)
            }
            StateKind::Replay(i, ref next) => {
                let backrefs = self.backrefs.take();
                let to_replay = backrefs.get(&i).unwrap().clone();
                let to_skip = to_replay.len();
                let to_match = self.to_match.take();
                self.backrefs.set(backrefs);

                for (i, c) in to_replay.iter().enumerate() {
                    if to_match[to_match_index + i] != *c {
                        self.to_match.set(to_match);

                        return Ok((false, 0));
                    }

                    self.record_backref(*c)?;
                }

                self.to_match.set(to_match);

                return self.backtracking(next.get().unwrap().clone(), to_match_index + to_skip);
            }
            StateKind::Split(ref next1, ref next2, _) => {
                let matched = self.backtracking(next1.get().unwrap().clone(), to_match_index)?;

                if matched.0 {
                    return Ok((true, matched.1));
                } else {
                    return self.backtracking(next2.get().unwrap().clone(), to_match_index);
                }
            }
            _ => panic!("Only statates with matchable can be matched"),
        }
    }

    fn record_backref(&self, c: char) -> Result<()> {
        let recording_backrefs = self.recording_backrefs_ids.take();
        let mut backrefs = self.backrefs.take();

        for i in recording_backrefs.iter() {
            let mut backref = match backrefs.remove(i) {
                Some(s) => s,
                None => vec![],
            };

            backref.push(c);
            backrefs.insert(*i, backref);
        }

        self.backrefs.set(backrefs);
        self.recording_backrefs_ids.set(recording_backrefs);

        Ok(())
    }

    fn init_state(&self, starts_from_beginning: bool) -> Option<Rc<State>> {
        if let State {
            kind: StateKind::Start(ref s),
            id: _,
            list_id: _,
        } = self.start_state
        {
            match s.get() {
                Some(s) => {
                    if let State {
                        kind: StateKind::Simple(RegexMatchable::Anchor(Anchor::Start), ref n1),
                        id: _,
                        list_id: _,
                    } = **s
                    {
                        if starts_from_beginning {
                            return Some(n1.get().unwrap().clone());
                        } else {
                            return None;
                        }
                    } else {
                        return Some(s.clone());
                    }
                }
                None => return None,
            }
        } else {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fragment_patch() {}

    #[test]
    fn test_regex_pattern_match_easy() {
        let regex = match RegexPattern::new("\\dahoj7") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("7ahoj7").unwrap());
        assert!(!regex.matches("aahoj7").unwrap());
    }

    #[test]
    fn test_regex_pattern_match_very_easy() {
        let regex = match RegexPattern::new("\\d") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("0").unwrap());
        assert!(regex.matches("1").unwrap());
        assert!(regex.matches("2").unwrap());
        assert!(regex.matches("3").unwrap());
        assert!(regex.matches("4").unwrap());
        assert!(regex.matches("5").unwrap());
        assert!(regex.matches("6").unwrap());
        assert!(regex.matches("7").unwrap());
        assert!(regex.matches("8").unwrap());
        assert!(regex.matches("9").unwrap());
        assert!(regex.matches("abc_0_xyz").unwrap());
        assert!(!regex.matches("aahoj").unwrap());
    }

    #[test]
    fn test_regex_pattern_match_positive_char_group() {
        let regex = match RegexPattern::new("[abc]") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("apple").unwrap());
        assert!(regex.matches("cab").unwrap());
        assert!(!regex.matches("dog").unwrap());
        assert!(regex.matches("a1b2c3").unwrap());
        assert!(regex.matches("aahoj").unwrap());
        assert!(!regex.matches("dhoj").unwrap());
    }

    #[test]
    fn test_regex_pattern_match_negative_char_group() {
        let regex = match RegexPattern::new("[^abc]") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("cat").unwrap());
        assert!(!regex.matches("cab").unwrap());
        assert!(regex.matches("dog").unwrap());
    }

    #[test]
    fn test_regex_combine_easy() {
        let regex = match RegexPattern::new("\\d apple") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        match regex.matches("1 apple") {
            Ok(b) => assert!(b),
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
        assert!(regex.matches("1 apple").unwrap());
        assert!(!regex.matches("1 appx").unwrap());
        assert!(!regex.matches("apple").unwrap());
        assert!(!regex.matches(" apple").unwrap());
        assert!(!regex.matches("x apple").unwrap());
    }

    #[test]
    fn test_regex_combine_special() {
        let regex = match RegexPattern::new("\\d\\\\d\\\\d apples") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        match regex.matches("sally has 12 apples") {
            Ok(b) => assert!(!b),
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
    }

    #[test]
    fn test_regex_start_of_string_anchor() {
        let regex = match RegexPattern::new("^log") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("log").unwrap());
        assert!(regex.matches("logs").unwrap());
        assert!(!regex.matches("dlog").unwrap());

        let regex = match RegexPattern::new("^\\d\\d\\d") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        print!("\n{:?}\n", regex.start_state);

        assert!(regex.matches("125ahoj").unwrap());
        assert!(!regex.matches("12ahoj").unwrap());
    }

    #[test]
    fn test_regex_end_of_string_anchor() {
        let regex = match RegexPattern::new("dog$") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        print!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("hotdog").unwrap());
        assert!(regex.matches("dog").unwrap());
        assert!(!regex.matches("dogs").unwrap());

        let regex = match RegexPattern::new("\\d\\d\\d$") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        print!("\n{:?}\n", regex.start_state);

        assert!(regex.matches("ahoj125").unwrap());
        assert!(regex.matches("125").unwrap());
        assert!(!regex.matches("125j").unwrap());
    }

    #[test]
    fn test_regex_plus_sign() {
        let regex = match RegexPattern::new("a+") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        // print!("\n{:?}\n", regex.start_state);
        // assert!(false);
        assert!(regex.matches("apple").unwrap());
        assert!(regex.matches("Saas").unwrap());
        assert!(regex.matches("cats").unwrap());
        assert!(regex.matches("caats").unwrap());

        assert!(!regex.matches("dog").unwrap());
        assert!(!regex.matches("cts").unwrap());

        let regex = match RegexPattern::new("ca+ts") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        assert!(regex.matches("caats").unwrap());
        assert!(regex.matches("cats").unwrap());
        assert!(regex.matches("caaatsx").unwrap());
        assert!(regex.matches("caaaaaaaaaaaaatsx").unwrap());

        assert!(!regex.matches("ctsx").unwrap());

        let regex = match RegexPattern::new("ca+t") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        assert!(regex.matches("cat").unwrap());
        assert!(regex.matches("caaaaaaaaaaaaatsx").unwrap());

        assert!(!regex.matches("ctsx").unwrap());
    }

    #[test]
    fn test_regex_plus_sign_complex() {
        let regex = match RegexPattern::new("^abc_\\d+_xyz$") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        // assert!(false);
        assert!(regex.matches("abc_123_xyz").unwrap());
    }

    #[test]
    fn test_regex_zero_or_one_times() {
        let regex = match RegexPattern::new("dogs?") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        // assert!(false);
        assert!(regex.matches("dogs").unwrap());
        assert!(regex.matches("dog").unwrap());
        assert!(!regex.matches("cat").unwrap());

        let regex = match RegexPattern::new("colou?r") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);

        assert!(regex.matches("color").unwrap());
        assert!(regex.matches("colour").unwrap());
        assert!(!regex.matches("collor").unwrap());

        let regex = match RegexPattern::new("ca?a?t") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        assert!(regex.matches("cat").unwrap());

        let regex = match RegexPattern::new("\\d?") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);

        assert!(regex.matches("1").unwrap());
        assert!(regex.matches("").unwrap());
        assert!(!regex.matches("c").unwrap());
    }

    #[test]
    fn test_wildcard() {
        let regex = match RegexPattern::new("d.g") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("dog").unwrap());
        assert!(regex.matches("dag").unwrap());
        assert!(regex.matches("d9g").unwrap());
        assert!(!regex.matches("cog").unwrap());
        assert!(!regex.matches("dg").unwrap());

        let regex = match RegexPattern::new("...") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("cat").unwrap());

        let regex = match RegexPattern::new(".\\d.") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("a1b").unwrap());

        let regex = match RegexPattern::new("g.+gol") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("goøö0Ogol").unwrap());
    }

    #[test]
    fn test_pipe() {
        let regex = match RegexPattern::new("(cat|dog)") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("dog").unwrap());
        assert!(regex.matches("cat").unwrap());
        assert!(regex.matches("doghouse").unwrap());
        assert!(!regex.matches("apple").unwrap());

        let regex = match RegexPattern::new("I like (cats|dogs)") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("I like dogs").unwrap());
        assert!(regex.matches("I like cats").unwrap());

        let regex = match RegexPattern::new("(red|blue|green)") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("red").unwrap());
        assert!(regex.matches("blue").unwrap());
        assert!(regex.matches("green").unwrap());
        assert!(!regex.matches("rex").unwrap());

        let regex = match RegexPattern::new("^I see \\d+ (cat|dog)s?$") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("I see 1 cat").unwrap());
    }

    #[test]
    fn test_backrefs() {
        let regex = match RegexPattern::new("(cat) and \\1") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("cat and cat").unwrap());
        assert!(!regex.matches("dog and dog").unwrap());
        assert!(!regex.matches("cat and dog").unwrap());

        let regex = match RegexPattern::new("(\\w+) and \\1") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("cat and cat").unwrap());
        assert!(regex.matches("dog and dog").unwrap());
        assert!(!regex.matches("cat and dog").unwrap());
        assert!(!regex.matches("dog and cat").unwrap());

        let regex = match RegexPattern::new("(\\d+)-\\1") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("123-123").unwrap());
        assert!(!regex.matches("123-124").unwrap());

        let regex = match RegexPattern::new("^([act]+) is \\1, not [^xyz]+$") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("cat is cat, not dog").unwrap());
    }

    #[test]
    fn test_nested_backrefs() {
        let regex = match RegexPattern::new("(\"(cat) and \\2\") is the same as \\1") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex
            .matches("\"cat and cat\" is the same as \"cat and cat\"")
            .unwrap());
    }

    #[test]
    fn test_match_exact_number_of_times() {
        let regex = match RegexPattern::new("ca{3}t") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("caaat").unwrap());
        assert!(!regex.matches("caat").unwrap());
        assert!(!regex.matches("caaaat").unwrap());

        let regex = match RegexPattern::new("d\\d{2}g") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("d42g").unwrap());
        assert!(!regex.matches("d1g").unwrap());
        assert!(!regex.matches("d123g").unwrap());

        let regex = match RegexPattern::new("c[xyz]{4}w") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("czyxzw").unwrap());
        assert!(!regex.matches("czyxw").unwrap());
    }

    #[test]
    fn test_match_at_least_number_of_times() {
        let regex = match RegexPattern::new("ca{2,}t") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("caat").unwrap());
        assert!(regex.matches("caaaaaaaaaat").unwrap());

        let regex = match RegexPattern::new("x\\d{3,}y") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("x8888y").unwrap());
        assert!(!regex.matches("x43y").unwrap());

        let regex = match RegexPattern::new("b[aeiou]{2,}r") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("baeiuor").unwrap());
        assert!(!regex.matches("bar").unwrap());
    }

    #[test]
    fn test_match_range_number_of_times() {
        let regex = match RegexPattern::new("ca{2,4}t") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("caat").unwrap());
        assert!(regex.matches("caaat").unwrap());
        assert!(regex.matches("caaaat").unwrap());
        assert!(!regex.matches("caaaaaaaaaat").unwrap());
        assert!(!regex.matches("cat").unwrap());

        let regex = match RegexPattern::new("n\\d{1,3}m") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("n123m").unwrap());
        assert!(!regex.matches("n1234").unwrap());

        let regex = match RegexPattern::new("p[xyz]{2,3}q") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("pzzzq").unwrap());
        assert!(!regex.matches("pzyzyq").unwrap());
    }

    #[test]
    fn test_debug() {
        let regex = match RegexPattern::new("ca{2,}t") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };
        println!("\n{:?}\n", regex.start_state);
        assert!(regex.matches("caaaaaaaaaat").unwrap());
        assert!(regex.matches("caat").unwrap());
        assert!(!regex.matches("cat").unwrap());
    }
}
