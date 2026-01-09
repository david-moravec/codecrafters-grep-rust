use anyhow::{anyhow, Result};
use std::cell::{Cell, OnceCell};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
enum Token {
    Escape,
    Character(char),
    LeftBracket,
    RightBracket,
    LeftSquareBracket,
    RightSquareBracket,
    Pipe,
    Asterisk,
    QuestionMark,
    Plus,
    Caret,
    Dollar,
}

impl Token {
    pub fn to_char(&self) -> char {
        match self {
            Self::Character(c) => *c,
            Self::Escape => '\\',
            Self::LeftBracket => '(',
            Self::RightBracket => ')',
            Self::LeftSquareBracket => '[',
            Self::RightSquareBracket => ']',
            Self::Pipe => '|',
            Self::Asterisk => '*',
            Self::QuestionMark => '?',
            Self::Plus => '+',
            Self::Caret => '^',
            Self::Dollar => '$',
        }
    }
}

struct Scanner<'a> {
    input: &'a str,
    chars: Vec<char>,
    current: usize,
}

//TODO: try to parametrize Scanner by macro containing the token table
impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        return Scanner {
            input,
            chars: input.chars().collect(),
            current: 0,
        };
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.current = 0;
        let mut tokens = Vec::new();

        while let Some(c) = self.advance() {
            if c == '\\' {
                tokens.push(Token::Escape)
            } else if c == '|' {
                tokens.push(Token::Pipe);
            } else if c == '(' {
                tokens.push(Token::LeftBracket)
            } else if c == ')' {
                tokens.push(Token::RightBracket)
            } else if c == '[' {
                tokens.push(Token::LeftSquareBracket)
            } else if c == ']' {
                tokens.push(Token::RightSquareBracket)
            } else if c == '?' {
                tokens.push(Token::QuestionMark)
            } else if c == '+' {
                tokens.push(Token::Plus)
            } else if c == '*' {
                tokens.push(Token::Asterisk)
            } else if c == '^' {
                tokens.push(Token::Caret)
            } else if c == '$' {
                tokens.push(Token::Dollar)
            } else {
                tokens.push(Token::Character(c))
            }
        }
        return tokens;
    }

    fn advance(&mut self) -> Option<char> {
        if self.current == self.chars.len() {
            return None;
        }
        self.current += 1;

        return Some(self.chars[self.current - 1]);
    }
}

#[derive(Clone, Hash, Eq, PartialEq, PartialOrd, Ord, Copy)]
enum RegexAtom {
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
enum Anchor {
    Start,
    End,
}

#[derive(Clone)]
enum RegexMatchable {
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
enum SplitType {
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

#[derive(Clone)]
enum StateKind {
    Simple(RegexMatchable, OnceCell<StateMut>),
    Split(OnceCell<StateMut>, OnceCell<StateMut>, SplitType),
    Match,
    Start(OnceCell<StateMut>),
    StartRecording(u32, OnceCell<StateMut>),
    StopRecording(u32, OnceCell<StateMut>),
    Replay(u32, OnceCell<StateMut>),
}

static mut STATE_ID: usize = 0;

#[derive(Clone)]
struct State {
    kind: StateKind,
    id: usize,
    list_id: Cell<i64>,
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

type StateMut = Rc<State>;

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

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
    is_backreference_present: bool,
    current_backref_id: u32,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        return Parser {
            tokens,
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
        // self.consume(Token::Wildcard)?;
        !unimplemented!()
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
        let mut fragment = Fragment::new(state.clone(), vec![state.clone()]);

        if let Ok(current) = self.peek() {
            match current {
                Token::QuestionMark => self.question_mark(&mut fragment)?,
                Token::Asterisk => self.wildcard(&mut fragment)?,
                Token::Plus => self.plus(&mut fragment)?,
                _ => {}
            };
        };

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

    fn state_fragment(&mut self) -> Result<Fragment> {
        let mut fragments: Vec<Fragment> = vec![];

        while !self.is_at_end() {
            match self.peek()? {
                Token::Pipe | Token::RightBracket => break,
                Token::LeftBracket => fragments.push(self.bracketed_fragment()?),
                _ => fragments.push(self.state()?),
            }
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

        if let State {
            kind: StateKind::Split(ref n1, ref n2, _),
            id: _,
            list_id: _,
        } = *state
        {
            self.add_state(n1.get().unwrap().clone());
            self.add_state(n2.get().unwrap().clone());
            return;
        }

        match state.kind {
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
        self.states
            .iter()
            .any(|s| s.kind.check_is_match_after_reaching_end_of_input())
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
        let mut scanner = Scanner::new(pattern);
        let tokens = scanner.tokenize();
        let mut parser = Parser::new(&tokens);
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

        if self.is_backref_present {
            self.match_backtracking(to_match)
        } else {
            self.match_no_backtracking(to_match)
        }
    }

    fn match_no_backtracking(&self, to_match: &str) -> Result<bool> {
        let chars: Vec<char> = to_match.chars().collect();
        for i in 0..chars.len() {
            match self.thompson_algorithm(&chars[i..].iter().collect::<String>(), i == 0) {
                Ok(matches) => {
                    if matches {
                        return Ok(true);
                    }
                }
                Err(e) => return Err(e),
            }
        }

        return Ok(self.start_state.kind.leads_directly_to_match());
    }

    fn thompson_step(
        &self,
        c: char,
        // index: usize,
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
                }
            } else {
                panic!("Only statates with matchable can be matched");
            }
        }
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

    fn thompson_algorithm(&self, to_match: &str, starts_from_beginning: bool) -> Result<bool> {
        let chars: Vec<char> = to_match.chars().collect();
        let mut next_states: StateCollection = StateCollection::new(self.last_list_id.get());
        let mut current_states: StateCollection = StateCollection::new(self.last_list_id.get());

        if let Some(start) = self.init_state(starts_from_beginning) {
            current_states.add_state(start);
        } else {
            return Ok(false);
        }

        for c in chars.iter() {
            next_states.clear();
            current_states.reset_list_ids();

            self.last_list_id.set(next_states.id);

            if current_states.states.len() == 0 {
                return Ok(false);
            } else if current_states.has_match() {
                return Ok(true);
            }

            self.thompson_step(*c, &current_states, &mut next_states);

            current_states.states = next_states.states.clone();
        }

        // For pattern to match it needs to reach match state or end of string state
        //  before exhausting input string
        Ok(current_states.check_match_after_reaching_end())
    }

    fn match_backtracking(&self, to_match: &str) -> Result<bool> {
        let mut init_state: Rc<State>;

        for (i, _) in to_match.chars().enumerate() {
            if let Some(start) = self.init_state(i == 0) {
                init_state = start;
            } else {
                return Ok(false);
            }

            self.to_match.set(
                to_match.chars().collect::<Vec<char>>()[i..]
                    .into_iter()
                    .map(|c| *c)
                    .collect(),
            );

            if self.backtracking(init_state, 0)? {
                return Ok(true);
            }
        }

        Ok(false)
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

    fn backtracking(&self, state: Rc<State>, to_match_index: usize) -> Result<bool> {
        if state.kind.leads_directly_to_match() {
            return Ok(true);
        }

        match state.kind {
            StateKind::Simple(ref matchable, ref next) => {
                let to_match = self.to_match.take();

                if to_match.len() <= to_match_index {
                    self.to_match.set(to_match);

                    return Ok(state.kind.check_is_match_after_reaching_end_of_input());
                }

                let current_char = to_match[to_match_index];

                self.to_match.set(to_match);

                if matchable.matches(current_char) {
                    self.record_backref(current_char)?;
                    return self.backtracking(next.get().unwrap().clone(), to_match_index + 1);
                } else {
                    Ok(false)
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
                let to_replay = backrefs.get(&i).unwrap();
                let to_match = self.to_match.take();

                for (i, c) in to_replay.iter().enumerate() {
                    if to_match[to_match_index + i] != *c {
                        self.backrefs.set(backrefs);
                        self.to_match.set(to_match);

                        return Ok(false);
                    }
                }

                let to_skip = to_replay.len();

                self.backrefs.set(backrefs);
                self.to_match.set(to_match);

                return self.backtracking(next.get().unwrap().clone(), to_match_index + to_skip);
            }
            StateKind::Split(ref next1, ref next2, _) => {
                if self.backtracking(next1.get().unwrap().clone(), to_match_index)? {
                    return Ok(true);
                } else {
                    return self.backtracking(next2.get().unwrap().clone(), to_match_index);
                }
            }
            _ => panic!("Only statates with matchable can be matched"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_pattern_tokenize_easy() {
        let mut scanner = Scanner::new("\\dahoj7");
        let tokens = scanner.tokenize();

        assert!(tokens[0] == Token::Escape);
        assert!(tokens[1] == Token::Character('d'));
        assert!(tokens[2] == Token::Character('a'));
    }

    #[test]
    fn test_regex_pattern_tokenize_harder() {
        let mut scanner = Scanner::new("(aa)|(bb)d?(a+c*)*");
        let tokens = scanner.tokenize();

        assert!(tokens[0] == Token::LeftBracket);
        assert!(tokens[1] == Token::Character('a'));
        assert!(tokens[2] == Token::Character('a'));
        assert!(tokens[3] == Token::RightBracket);
        assert!(tokens[4] == Token::Pipe);
        assert!(tokens[5] == Token::LeftBracket);
        assert!(tokens[6] == Token::Character('b'));
        assert!(tokens[7] == Token::Character('b'));
        assert!(tokens[8] == Token::RightBracket);
        assert!(tokens[9] == Token::Character('d'));
        assert!(tokens[10] == Token::QuestionMark);
        assert!(tokens[11] == Token::LeftBracket);
        assert!(tokens[12] == Token::Character('a'));
        assert!(tokens[13] == Token::Plus);
        assert!(tokens[14] == Token::Character('c'));
        assert!(tokens[15] == Token::Asterisk);
        assert!(tokens[16] == Token::RightBracket);
        assert!(tokens[17] == Token::Asterisk);
    }

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
    fn test_regex_pattern_tokenize_complex() {
        let mut scanner = Scanner::new("^abc_\\d+_xyz$");
        let tokens = scanner.tokenize();

        assert!(tokens[0] == Token::Caret);
        assert!(tokens[1] == Token::Character('a'));
        assert!(tokens[2] == Token::Character('b'));
        assert!(tokens[3] == Token::Character('c'));
        assert!(tokens[4] == Token::Character('_'));
        assert!(tokens[5] == Token::Escape);
        assert!(tokens[6] == Token::Character('d'));
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
    fn test_debug() {
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
}
