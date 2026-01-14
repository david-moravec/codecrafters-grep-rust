use anyhow::{anyhow, Result};
use std::cell::{Cell, OnceCell};
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::rc::Rc;

use crate::regex_scanner::{Scanner, Token};

#[derive(Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Copy)]
pub enum RegexAtom {
    Escaped(char),
    Char(char),
}

impl RegexAtom {
    pub fn matches(&self, c: char) -> bool {
        match self {
            &Self::Char(ch) => {
                if ch == '.' {
                    c != '\n'
                } else {
                    c == ch
                }
            }
            &Self::Escaped(ch) => match ch {
                'd' => c >= '0' && c <= '9',
                'w' => c.is_alphanumeric() || c == '_',
                _ => c == ch,
            },
        }
    }
}

impl Debug for RegexAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Escaped(c) => write!(f, "\\{}", c),
            Self::Char(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Anchor {
    Start,
    End,
}

#[derive(Clone)]
pub enum RegexMatchable {
    PositiveCharGroup(HashSet<RegexAtom>),
    NegativeCharGroup(HashSet<RegexAtom>),
    Atom(RegexAtom),
    Anchor(Anchor),
}

impl RegexMatchable {
    pub fn matches(&self, c: char) -> bool {
        match self {
            Self::PositiveCharGroup(atoms) => atoms.iter().any(|a| a.matches(c)),
            Self::NegativeCharGroup(atoms) => !atoms.iter().any(|a| a.matches(c)),
            Self::Atom(atom) => atom.matches(c),
            Self::Anchor(_) => return false,
        }
    }
}

impl Debug for RegexMatchable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(atom) => write!(f, "{:?}", atom),
            Self::PositiveCharGroup(atoms) => {
                write!(f, "[")?;
                for atom in atoms.iter() {
                    if let Err(e) = write!(f, "{:?}", atom) {
                        return Err(e);
                    }
                }
                write!(f, "]")?;
                return Ok(());
            }
            Self::NegativeCharGroup(atoms) => {
                write!(f, "[^")?;
                for atom in atoms.iter() {
                    if let Err(e) = write!(f, "{:?}", atom) {
                        return Err(e);
                    }
                }
                write!(f, "]")?;
                return Ok(());
            }
            Self::Anchor(anchor) => match anchor {
                Anchor::Start => write!(f, "^"),
                Anchor::End => write!(f, "$"),
            },
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum SplitType {
    Pipe,
    Plus,
    QuestionMark,
    Wildcard,
}

impl Debug for SplitType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pipe => write!(f, "|"),
            Self::Plus => write!(f, "+"),
            Self::QuestionMark => write!(f, "?"),
            Self::Wildcard => write!(f, "*"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum RepeatTimes {
    Exactly(u32),
    AtLeast(u32),
    RangeInclusive(u32, u32),
}

impl Debug for RepeatTimes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exactly(n) => write!(f, "{{{:}}}", n),
            Self::AtLeast(n) => write!(f, "{{{:},}}", n),
            Self::RangeInclusive(min, max) => write!(f, "{{{:},{:}}}", min, max),
        }
    }
}

#[derive(Clone)]
pub struct RepeatingState {
    pub to_repeat: StateMut,
    pub next: OnceCell<StateMut>,
    pub how_many_times: RepeatTimes,
    pub hit_count: Cell<u32>,
}

impl RepeatingState {
    pub fn number_of_repeats_met(&self) -> bool {
        let hit_count = self.hit_count.get();

        match self.how_many_times {
            RepeatTimes::Exactly(n) => hit_count == n,
            RepeatTimes::AtLeast(n) => hit_count >= n,
            RepeatTimes::RangeInclusive(min, max) => hit_count >= min && hit_count <= max,
        }
    }

    pub fn can_repeat_more(&self) -> bool {
        let hit_count = self.hit_count.get();

        match self.how_many_times {
            RepeatTimes::Exactly(n) => hit_count < n,
            RepeatTimes::AtLeast(_) => true,
            RepeatTimes::RangeInclusive(_, max) => hit_count < max,
        }
    }
}

impl Debug for RepeatingState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(...){:?}{:?}",
            // self.to_repeat,
            self.how_many_times,
            self.next.get().unwrap()
        )
    }
}

#[derive(Clone, Debug)]
pub enum StateKind {
    Simple(RegexMatchable, OnceCell<StateMut>),
    Split(OnceCell<StateMut>, OnceCell<StateMut>, SplitType),
    Match,
    Start(OnceCell<StateMut>),
    StartRecording(u32, OnceCell<StateMut>),
    StopRecording(u32, OnceCell<StateMut>),
    Replay(u32, OnceCell<StateMut>),
    Repeat(RepeatingState),
}

static mut STATE_ID: usize = 0;

#[derive(Clone)]
pub struct State {
    pub kind: StateKind,
    pub id: usize,
    pub list_id: Cell<i64>,
}

impl State {
    pub fn new(state_kind: StateKind) -> Self {
        unsafe {
            STATE_ID += 1;
            return Self {
                kind: state_kind,
                id: STATE_ID,
                list_id: Cell::new(-1),
            };
        }
    }

    pub fn patch(&self, next_state: StateMut) -> Result<()> {
        match self.kind {
            StateKind::Simple(_, ref next)
            | StateKind::Start(ref next)
            | StateKind::StartRecording(_, ref next)
            | StateKind::Replay(_, ref next)
            | StateKind::StopRecording(_, ref next) => {
                if next.get().is_none() {
                    let _ = next.set(next_state);
                    // *next = Some(next_state);
                    return Ok(());
                }
                Err(anyhow!("State is already connected"))
            }
            StateKind::Split(ref next1, ref next2, _) => {
                if next1.get().is_some() && next2.get().is_some() {
                    return Err(anyhow!("State1 or State2 is already connected"));
                }
                if next1.get().is_none() {
                    let _ = next1.set(next_state.clone());
                }
                if next2.get().is_none() {
                    let _ = next2.set(next_state);
                }
                Ok(())
            }
            StateKind::Match => Err(anyhow!("Cannot patch mathed State")),
            StateKind::Repeat(ref state) => {
                if state.next.get().is_none() {
                    let _ = state.next.set(next_state);
                    // *next = Some(next_state);
                    return Ok(());
                }
                Err(anyhow!("State is already connected"))
            }
        }
    }

    pub fn patch_multiple_states_by_self(self, list_states: Vec<Rc<Self>>) -> Result<()> {
        let rc_self = Rc::new(self);

        for state in list_states {
            state.patch(rc_self.clone())?
        }

        Ok(())
    }

    pub fn matchable(&self) -> Option<&RegexMatchable> {
        if let Self {
            kind: StateKind::Simple(matchable, _),
            id: _,
            list_id: _,
        } = self
        {
            return Some(matchable);
        } else {
            None
        }
    }

    pub fn matches(&self, c: char) -> bool {
        self.kind.matches(c)
    }
}

impl From<RegexMatchable> for State {
    fn from(value: RegexMatchable) -> Self {
        unsafe {
            STATE_ID += 1;
            Self {
                kind: StateKind::Simple(value, OnceCell::new()),
                id: STATE_ID,
                list_id: Cell::new(-1),
            }
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.kind {
            StateKind::Split(ref s1, ref s2, split_t) => match split_t {
                SplitType::Pipe => {
                    write!(f, "{:?}|{:?}", s1.get().unwrap(), s2.get().unwrap())
                }
                _ => {
                    // prevent cycles (s1 is always state that might refer to itself)
                    if let State {
                        kind: StateKind::Simple(ref matchable, _),
                        id: _,
                        list_id: _,
                    } = **s1.get().unwrap()
                    {
                        match split_t {
                            SplitType::QuestionMark => write!(f, "{:?}", matchable)?,
                            _ => {}
                        }
                    } else {
                        if s1.get().unwrap().id != self.id {
                            write!(f, "{:?}", s1.get().unwrap())?;
                        }
                    }
                    write!(f, "{:?}{:?}", split_t, s2.get().unwrap())
                }
            },
            StateKind::Start(state) => match state.get() {
                Some(ref s) => write!(f, "{:?}", s),
                None => write!(f, "No regex\n"),
            },
            StateKind::Simple(matchable, state) => {
                write!(f, "{:?}{:?}", matchable, state.get().unwrap())
            }
            StateKind::StartRecording(_, state) => {
                write!(f, "({:?}", state.get().unwrap())
            }
            StateKind::StopRecording(_, state) => {
                write!(f, "){:?}", state.get().unwrap())
            }
            StateKind::Replay(i, state) => {
                write!(f, "\\{:}{:?}", i, state.get().unwrap())
            }
            StateKind::Repeat(ref repeat) => {
                write!(f, "{:?}", repeat)
            }
            StateKind::Match => write!(f, ""),
        }
    }
}

impl StateKind {
    pub fn check_is_match_after_reaching_end_of_input(&self) -> bool {
        match self {
            Self::Simple(atom, n) => {
                if let RegexMatchable::Anchor(pos) = atom {
                    if let Anchor::End = pos {
                        n.get().unwrap().kind.is_match()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => self.is_match(),
        }
    }

    pub fn leads_directly_to_match(&self) -> bool {
        match self {
            Self::Match => true,
            Self::Start(s_opt) => match s_opt.get() {
                Some(s) => s.kind.leads_directly_to_match(),
                None => false,
            },
            Self::Simple(_, _)
            | StateKind::StartRecording(_, _)
            | StateKind::Replay(_, _)
            | StateKind::StopRecording(_, _) => false,
            Self::Split(s1, s2, split_t) => match split_t {
                SplitType::Pipe => {
                    s1.get().unwrap().kind.leads_directly_to_match()
                        || s2.get().unwrap().kind.leads_directly_to_match()
                }
                SplitType::Plus | SplitType::QuestionMark | SplitType::Wildcard => {
                    s2.get().unwrap().kind.leads_directly_to_match()
                }
            },
            Self::Repeat(ref repeating) => {
                repeating.hit_count.set(0);

                match repeating.how_many_times {
                    RepeatTimes::Exactly(0)
                    | RepeatTimes::AtLeast(0)
                    | RepeatTimes::RangeInclusive(0, _) => {
                        return repeating.next.get().unwrap().kind.leads_directly_to_match()
                    }
                    _ => return false,
                }
            }
        }
    }

    pub fn matches(&self, c: char) -> bool {
        match self {
            Self::Simple(matchable, _) => matchable.matches(c),
            _ => unreachable!("Only simple state with matchable can be matched"),
        }
    }

    pub fn is_match(&self) -> bool {
        match self {
            Self::Match => true,
            Self::Repeat(ref repeating) => {
                repeating.hit_count.set(0);
                false
            }
            _ => false,
        }
    }
}

impl Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for State {}

pub type StateMut = Rc<State>;

#[derive(Clone)]
struct Fragment {
    state: StateMut,
    out: Vec<StateMut>,
}

impl Fragment {
    pub fn new(state: StateMut, out: Vec<StateMut>) -> Self {
        return Fragment {
            state: state.clone(),
            out,
        };
    }

    pub fn patch(&mut self, next_fragment: Self) -> Result<()> {
        for state in self.out.iter() {
            state.patch(next_fragment.state.clone())?;
        }

        self.out = next_fragment.out;

        Ok(())
    }

    pub fn patch_by(&mut self, patch: StateMut) -> Result<()> {
        for state in self.out.iter() {
            state.patch(patch.clone())?;
        }

        self.out = vec![patch];

        Ok(())
    }

    pub fn patch_match(&mut self) -> Result<()> {
        for state in self.out.iter() {
            state.patch(Rc::new(State::new(StateKind::Match)))?;
        }

        Ok(())
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub is_backreference_present: bool,
    current_backref_id: u32,
}

impl Parser {
    pub fn new(pattern: &str) -> Self {
        return Parser {
            tokens: Scanner::new(pattern).tokenize(),
            current: 0,
            is_backreference_present: false,
            current_backref_id: 0,
        };
    }

    pub fn parse(&mut self) -> Result<State> {
        unsafe { STATE_ID = 0 }

        if self.is_at_end() {
            return Ok(State::new(StateKind::Start(OnceCell::new())));
        }

        let mut fragments: Vec<Fragment> = vec![];

        while !self.is_at_end() {
            fragments.push(self.fragment()?);
        }

        // we checked above that we have something to parse
        // and self.state returns state so we have at least one
        let mut current = fragments.pop().unwrap();
        // println!("FinalState before match -> {:?}", current.state);
        current.patch_match()?;
        // println!("FinalState after  match -> {:?}", current.state);

        while let Some(mut prev) = fragments.pop() {
            prev.patch(current)?;
            current = prev;
        }

        let start = State::new(StateKind::Start(OnceCell::from(current.state.clone())));

        return Ok(start);
    }

    fn consume(&mut self, expected_token: Token) -> Result<()> {
        if self.peek()? == expected_token {
            self.advance();
            Ok(())
        } else {
            Err(anyhow!(
                "Expected {:?}, got {:?}",
                expected_token,
                self.peek().unwrap()
            ))
        }
    }

    fn advance(&mut self) -> Token {
        self.current += 1;
        return self.tokens[self.current - 1];
    }

    fn is_at_end(&self) -> bool {
        return self.peek().is_err();
    }

    fn peek(&self) -> Result<Token> {
        match self.tokens.get(self.current).map(|x| *x) {
            Some(t) => Ok(t),
            None => Err(anyhow!("End of file reached")),
        }
    }

    fn peek_next(&self) -> Result<Token> {
        match self.tokens.get(self.current + 1).map(|x| *x) {
            Some(t) => Ok(t),
            None => Err(anyhow!("End of file reached")),
        }
    }

    fn char_group(&mut self) -> Result<RegexMatchable> {
        self.consume(Token::LeftSquareBracket)?;

        let result = match self.peek()? {
            Token::Caret => {
                self.consume(Token::Caret)?;
                Ok(RegexMatchable::NegativeCharGroup(self.regex_atom_set()?))
            }
            _ => Ok(RegexMatchable::PositiveCharGroup(self.regex_atom_set()?)),
        };

        self.consume(Token::RightSquareBracket)?;
        result
    }

    fn regex_atom_set(&mut self) -> Result<HashSet<RegexAtom>> {
        let mut atoms: HashSet<RegexAtom> = HashSet::new();

        while !self.is_at_end() {
            match self.peek()? {
                Token::RightSquareBracket => break,
                _ => atoms.insert(self.atom()?),
            };
        }

        return Ok(atoms);
    }

    fn atom(&mut self) -> Result<RegexAtom> {
        match self.peek()? {
            Token::Character(c) => {
                self.advance();
                Ok(RegexAtom::Char(c))
            }
            Token::Escape => {
                self.consume(Token::Escape)?;
                Ok(RegexAtom::Escaped(self.advance().to_char()))
            }
            token => Err(anyhow!("Expected character or '\\', got {:?}", token)),
        }
    }

    fn regex_matchable(&mut self) -> Result<RegexMatchable> {
        match self.peek()? {
            Token::LeftSquareBracket => self.char_group(),
            Token::Escape | Token::Character(_) => Ok(RegexMatchable::Atom(self.atom()?)),
            token => Err(anyhow!("Expected bracket/escape/character got {:?}", token)),
        }
    }

    fn wildcard(&mut self, fragment: &mut Fragment) -> Result<()> {
        self.consume(Token::Asterisk)?;

        let split_rc = Rc::new(State::new(StateKind::Split(
            OnceCell::from(fragment.state.clone()),
            OnceCell::new(),
            SplitType::Plus,
        )));

        fragment.patch_by(split_rc.clone())?;

        *fragment = Fragment::new(split_rc.clone(), vec![split_rc]);

        Ok(())
    }

    fn question_mark(&mut self, fragment: &mut Fragment) -> Result<()> {
        self.consume(Token::QuestionMark)?;

        let state = fragment.state.clone();

        let zero_or_one = Rc::new(State::new(StateKind::Split(
            OnceCell::from(state.clone()),
            OnceCell::new(),
            SplitType::QuestionMark,
        )));

        *fragment = Fragment::new(zero_or_one.clone(), vec![zero_or_one, state]);

        Ok(())
    }

    fn plus(&mut self, fragment: &mut Fragment) -> Result<()> {
        self.consume(Token::Plus)?;

        let split_rc = Rc::new(State::new(StateKind::Split(
            OnceCell::from(fragment.state.clone()),
            OnceCell::new(),
            SplitType::Plus,
        )));

        fragment.patch_by(split_rc)
    }

    fn state_regex_matchable(&mut self) -> Result<Fragment> {
        let state = Rc::new(State::from(self.regex_matchable()?));
        let fragment = Fragment::new(state.clone(), vec![state.clone()]);

        Ok(fragment)
    }

    fn state(&mut self) -> Result<Fragment> {
        match self.peek()? {
            Token::Caret => {
                self.advance();
                let anchor = Rc::new(State::from(RegexMatchable::Anchor(Anchor::Start)));
                Ok(Fragment::new(anchor.clone(), vec![anchor]))
            }
            Token::Dollar => {
                self.advance();
                let anchor = Rc::new(State::from(RegexMatchable::Anchor(Anchor::End)));
                Ok(Fragment::new(anchor.clone(), vec![anchor]))
            }
            Token::LeftBracket => self.bracketed_fragment(),
            Token::Escape => {
                if let Token::Character(c) = self.peek_next()? {
                    if c.is_numeric() {
                        let backref = Rc::new(State::new(StateKind::Replay(
                            c.to_digit(10).expect("not a number"),
                            OnceCell::new(),
                        )));
                        self.is_backreference_present = true;
                        self.advance();
                        self.advance();
                        Ok(Fragment::new(backref.clone(), vec![backref]))
                    } else {
                        self.state_regex_matchable()
                    }
                } else {
                    self.state_regex_matchable()
                }
            }
            _ => self.state_regex_matchable(),
        }
    }

    fn fragment(&mut self) -> Result<Fragment> {
        let mut fragments: Vec<Fragment> = vec![];

        while !self.is_at_end() {
            fragments.push(self.pipe()?);
        }

        let mut current = fragments.pop().unwrap();

        while let Some(mut prev) = fragments.pop() {
            prev.patch(current)?;
            current = prev;
        }

        return Ok(current);
    }

    fn match_number_of_times(&mut self, fragment: &mut Fragment) -> Result<()> {
        self.consume(Token::LeftBrace)?;

        let repeat: RepeatTimes;

        if let Token::Character(c) = self.peek()? {
            self.advance();
            let min = c.to_digit(10).expect("Not a number");

            if let Token::Character(',') = self.peek()? {
                self.advance();

                if let Token::Character(c) = self.peek()? {
                    self.advance();
                    repeat = RepeatTimes::RangeInclusive(min, c.to_digit(10).expect("Not a number"))
                } else {
                    repeat = RepeatTimes::AtLeast(min)
                }
            } else {
                repeat = RepeatTimes::Exactly(min)
            }
        } else {
            return Err(anyhow!(
                "Expected specifying how many times preceeding element should be matched"
            ));
        };

        self.consume(Token::RightBrace)?;

        let repeat_state = Rc::new(State::new(StateKind::Repeat(RepeatingState {
            to_repeat: fragment.state.clone(),
            next: OnceCell::new(),
            how_many_times: repeat,
            hit_count: Cell::new(0),
        })));

        fragment.patch_by(repeat_state.clone())?;

        *fragment = Fragment::new(repeat_state.clone(), vec![repeat_state]);

        Ok(())
    }

    fn quantify(&mut self, fragment: &mut Fragment) -> Result<()> {
        if self.is_at_end() {
            return Ok(());
        }

        match self.peek().unwrap() {
            Token::QuestionMark => self.question_mark(fragment)?,
            Token::Asterisk => self.wildcard(fragment)?,
            Token::Plus => self.plus(fragment)?,
            Token::LeftBrace => self.match_number_of_times(fragment)?,
            _ => {}
        };

        Ok(())
    }

    fn state_fragment(&mut self) -> Result<Fragment> {
        let mut fragments: Vec<Fragment> = vec![];

        while !self.is_at_end() {
            let mut last_fragment = match self.peek()? {
                Token::Pipe | Token::RightBracket => break,
                _ => self.state()?,
            };

            self.quantify(&mut last_fragment)?;
            fragments.push(last_fragment);
        }

        let mut current = fragments.pop().unwrap();

        while let Some(mut prev) = fragments.pop() {
            prev.patch(current)?;
            current = prev;
        }

        return Ok(current);
    }

    fn pipe(&mut self) -> Result<Fragment> {
        let mut fragment = self.state_fragment()?;
        let mut fragment2: Fragment;

        while !self.is_at_end() {
            if self.peek().unwrap() != Token::Pipe {
                return Ok(fragment);
            }

            self.consume(Token::Pipe)?;
            fragment2 = self.state_fragment()?;
            let pipe = Rc::new(State::new(StateKind::Split(
                OnceCell::from(fragment.state),
                OnceCell::from(fragment2.state),
                SplitType::Pipe,
            )));

            fragment = Fragment::new(
                pipe,
                Vec::from_iter(
                    fragment
                        .out
                        .iter()
                        .cloned()
                        .chain(fragment2.out.iter().cloned()),
                ),
            );
        }

        return Ok(fragment);
    }

    fn bracketed_fragment(&mut self) -> Result<Fragment> {
        let mut fragments: Vec<Fragment> = vec![];

        self.consume(Token::LeftBracket)?;
        self.current_backref_id += 1;
        let current_ref_id = self.current_backref_id;

        let state = Rc::new(State::new(StateKind::StartRecording(
            current_ref_id,
            OnceCell::new(),
        )));
        fragments.push(Fragment::new(state.clone(), vec![state.clone()]));

        while self.peek()? != Token::RightBracket {
            fragments.push(self.pipe()?);
        }
        self.consume(Token::RightBracket)?;
        let state = Rc::new(State::new(StateKind::StopRecording(
            current_ref_id,
            OnceCell::new(),
        )));
        fragments.push(Fragment::new(state.clone(), vec![state.clone()]));

        let mut current = fragments.pop().unwrap();

        while let Some(mut prev) = fragments.pop() {
            prev.patch(current)?;
            current = prev;
        }

        return Ok(current);
    }
}
