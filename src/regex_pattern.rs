use anyhow::{anyhow, Result};
use std::cell::RefCell;
use std::collections::HashSet;
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
            } else if c.is_alphabetic() || c == '.' || c == '_' || c == ' ' || c == '@' {
                tokens.push(Token::Character(c))
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
    pub fn matches(&self, c: char, index: usize) -> bool {
        match self {
            Self::PositiveCharGroup(atoms) => atoms.iter().any(|a| a.matches(c)),
            Self::NegativeCharGroup(atoms) => !atoms.iter().any(|a| a.matches(c)),
            Self::Atom(atom) => atom.matches(c),
            Self::Anchor(anchor) => match anchor {
                Anchor::Start => index == 0,
                Anchor::End => false,
            },
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
    Simple(RegexMatchable, Option<Rc<RefCell<State>>>),
    Split(
        Option<Rc<RefCell<State>>>,
        Option<Rc<RefCell<State>>>,
        SplitType,
    ),
    Match,
    Start(Option<Rc<RefCell<State>>>),
}

static mut STATE_ID: usize = 0;

#[derive(Clone)]
struct State {
    kind: StateKind,
    id: usize,
}

impl State {
    pub fn new(state_kind: StateKind) -> Self {
        unsafe {
            STATE_ID += 1;
            return Self {
                kind: state_kind,
                id: STATE_ID,
            };
        }
    }

    pub fn patch(&mut self, next_state: Rc<RefCell<State>>) -> Result<()> {
        match self.kind {
            StateKind::Simple(_, ref mut next) | StateKind::Start(ref mut next) => {
                if let None = next {
                    *next = Some(next_state);
                    return Ok(());
                }
                Err(anyhow!("State is already connected"))
            }
            StateKind::Split(ref mut next1, ref mut next2, _) => {
                if next1.is_some() && next2.is_some() {
                    return Err(anyhow!("State1 or State2 is already connected"));
                }
                if let None = next1 {
                    *next1 = Some(next_state.clone());
                }
                if let None = next2 {
                    *next2 = Some(next_state);
                }
                Ok(())
            }
            StateKind::Match => Err(anyhow!("Cannot patch mathed State")),
        }
    }

    pub fn patch_multiple_states_by_self(self, list_states: Vec<Rc<RefCell<Self>>>) -> Result<()> {
        let rc_self = Rc::new(RefCell::new(self));

        for state in list_states {
            state.borrow_mut().patch(rc_self.clone())?
        }

        Ok(())
    }

    pub fn matchable(&self) -> Option<&RegexMatchable> {
        if let Self {
            kind: StateKind::Simple(matchable, _),
            id: _,
        } = self
        {
            return Some(matchable);
        } else {
            None
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.kind {
            StateKind::Split(ref s1, ref s2, split_t) => match split_t {
                SplitType::Pipe => {
                    write!(
                        f,
                        "{:?}|{:?}",
                        s1.as_ref().unwrap().borrow(),
                        s2.as_ref().unwrap().borrow()
                    )
                }
                _ => {
                    // prevent cycles (s1 is always state that might refer to itself)
                    if let State {
                        kind: StateKind::Simple(ref matchable, _),
                        id: _,
                    } = *s1.as_ref().unwrap().borrow()
                    {
                        write!(f, "{:?}", matchable)?;
                    } else {
                        if s1.as_ref().unwrap().borrow().id != self.id {
                            write!(f, "{:?}", s1.as_ref().unwrap().borrow())?;
                        }
                    }
                    write!(f, "{:?}{:?}", split_t, s2.as_ref().unwrap().borrow())
                }
            },
            StateKind::Start(state) => match state {
                Some(ref s) => write!(f, "{:?}", s.borrow()),
                None => write!(f, "No regex\n"),
            },
            StateKind::Simple(matchable, state) => {
                write!(f, "{:?}{:?}", matchable, state.as_ref().unwrap().borrow())
            }
            StateKind::Match => write!(f, " -> MATCH"),
        }
    }
}

impl StateKind {
    pub fn check_is_match_after_reaching_end_of_input(&self) -> bool {
        match self {
            Self::Simple(atom, _) => {
                if let RegexMatchable::Anchor(pos) = atom {
                    if let Anchor::End = pos {
                        true
                    } else {
                        self.leads_directly_to_match()
                    }
                } else {
                    false
                }
            }
            _ => self.leads_directly_to_match(),
        }
    }

    pub fn leads_directly_to_match(&self) -> bool {
        match self {
            Self::Match => true,
            Self::Start(s_opt) => match s_opt {
                Some(s) => s.borrow().kind.leads_directly_to_match(),
                None => false,
            },
            Self::Simple(_, _) => false,
            Self::Split(s1, s2, split_t) => match split_t {
                SplitType::Pipe => {
                    s1.as_ref().unwrap().borrow().kind.leads_directly_to_match()
                        || s2.as_ref().unwrap().borrow().kind.leads_directly_to_match()
                }
                SplitType::Plus | SplitType::QuestionMark | SplitType::Wildcard => {
                    s2.as_ref().unwrap().borrow().kind.leads_directly_to_match()
                }
            },
        }
    }

    pub fn matches(&self, c: char, index: usize) -> Option<Vec<Rc<State>>> {
        match self {
            Self::Simple(ref atom, ref next) => {
                if atom.matches(c, index) {
                    if let RegexMatchable::Anchor(_) = atom {
                        return next.as_ref().unwrap().borrow().kind.matches(c, index);
                    }

                    if let Self::Split(ref next1, ref next2, _) =
                        next.as_ref().unwrap().borrow().kind
                    {
                        return Some(vec![
                            Rc::new(next1.as_ref().unwrap().borrow().clone()),
                            Rc::new(next2.as_ref().unwrap().borrow().clone()),
                        ]);
                    }
                    Some(vec![Rc::new(next.as_ref().unwrap().borrow().clone())])
                } else {
                    None
                }
            }
            Self::Split(ref next1, ref next2, _) => {
                return Some(vec![
                    Rc::new(next1.as_ref().unwrap().borrow().clone()),
                    Rc::new(next2.as_ref().unwrap().borrow().clone()),
                ])
            }
            Self::Match => return None,
            Self::Start(_) => {
                panic!("Start should be never reached during matching")
            }
        }
    }
}

impl State {
    pub fn matches(&self, c: char, index: usize) -> Option<Vec<Rc<Self>>> {
        self.kind.matches(c, index)
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

type StateMut = Rc<RefCell<State>>;

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
            state.borrow_mut().patch(next_fragment.state.clone())?;
        }

        self.out = next_fragment.out;

        Ok(())
    }

    pub fn patch_by(&mut self, patch: StateMut) -> Result<()> {
        for state in self.out.iter() {
            state.borrow_mut().patch(patch.clone())?;
        }

        self.out = vec![patch];

        Ok(())
    }

    pub fn patch_match(&mut self) -> Result<()> {
        for state in self.out.iter() {
            state
                .borrow_mut()
                .patch(Rc::new(RefCell::new(State::new(StateKind::Match))))?;
        }

        Ok(())
    }
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        return Parser { tokens, current: 0 };
    }

    pub fn parse(&mut self) -> Result<State> {
        unsafe { STATE_ID = 0 }

        if self.is_at_end() {
            return Ok(State::new(StateKind::Start(None)));
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

        let start = State::new(StateKind::Start(Some(Rc::new(RefCell::new(
            current.state.borrow().clone(),
        )))));

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

    fn pipe(&mut self, fragment: &mut Fragment) -> Result<()> {
        // self.consume(Token::Pipe)?;
        // Err(anyhow!("ahoj"))
        !unimplemented!()
    }

    fn wildcard(&mut self, fragment: &mut Fragment) -> Result<()> {
        // self.consume(Token::Wildcard)?;
        !unimplemented!()
    }

    fn question_mark(&mut self, fragment: &mut Fragment) -> Result<()> {
        self.consume(Token::QuestionMark)?;

        let state = Rc::new(RefCell::new(fragment.state.borrow().clone()));

        let zero_or_one = Rc::new(RefCell::new(State::new(StateKind::Split(
            Some(state.clone()),
            None,
            SplitType::QuestionMark,
        ))));

        *fragment = Fragment::new(zero_or_one.clone(), vec![zero_or_one, state]);

        Ok(())
    }

    fn plus(&mut self, fragment: &mut Fragment) -> Result<()> {
        self.consume(Token::Plus)?;

        let split_rc = Rc::new(RefCell::new(State::new(StateKind::Split(
            Some(fragment.state.clone()),
            None,
            SplitType::Plus,
        ))));

        fragment.patch_by(split_rc)
    }

    fn state(&mut self) -> Result<Fragment> {
        match self.peek()? {
            Token::Caret => {
                self.advance();
                let anchor = Rc::new(RefCell::new(State::new(StateKind::Simple(
                    RegexMatchable::Anchor(Anchor::Start),
                    None,
                ))));
                Ok(Fragment::new(anchor.clone(), vec![anchor]))
            }
            Token::Dollar => {
                self.advance();
                let anchor = Rc::new(RefCell::new(State::new(StateKind::Simple(
                    RegexMatchable::Anchor(Anchor::End),
                    None,
                ))));
                Ok(Fragment::new(anchor.clone(), vec![anchor]))
            }
            _ => {
                let state = Rc::new(RefCell::new(State::new(StateKind::Simple(
                    self.regex_matchable()?,
                    None,
                ))));
                let mut fragment = Fragment::new(state.clone(), vec![state.clone()]);

                if let Ok(current) = self.peek() {
                    match current {
                        Token::QuestionMark => self.question_mark(&mut fragment)?,
                        Token::Asterisk => self.wildcard(&mut fragment)?,
                        Token::Plus => self.plus(&mut fragment)?,
                        Token::Pipe => self.pipe(&mut fragment)?,
                        _ => {}
                    };
                };

                Ok(fragment)
            }
        }
    }

    fn fragment(&mut self) -> Result<Fragment> {
        let mut fragments: Vec<Fragment> = vec![];

        while !self.is_at_end() {
            fragments.push(self.state()?);
        }

        let mut current = fragments.pop().unwrap();

        while let Some(mut prev) = fragments.pop() {
            prev.patch(current)?;
            current = prev;
        }

        return Ok(current);
    }

    fn sub_fragment(&mut self) -> Result<Fragment> {
        self.consume(Token::LeftBracket)?;
        let result = self.fragment();
        self.consume(Token::RightBracket)?;

        return result;
    }
}

pub struct RegexPattern<'a> {
    tokens: Vec<Token>,
    pattern: &'a str,
    start_state: State,
}

impl<'a> RegexPattern<'a> {
    pub fn new(pattern: &'a str) -> Result<Self> {
        let mut scanner = Scanner::new(pattern);
        let tokens = scanner.tokenize();
        let start_state = Parser::new(&tokens).parse()?;

        return Ok(RegexPattern {
            tokens,
            pattern,
            start_state,
        });
    }

    pub fn matches(&self, to_match: &str) -> Result<bool> {
        return self.try_match_starting_from_each_char(to_match);
    }

    fn try_match_starting_from_each_char(&self, to_match: &str) -> Result<bool> {
        let chars: Vec<char> = to_match.chars().collect();
        for i in 0..chars.len() {
            match self.match_starting_from(&chars[i..].iter().collect::<String>(), i) {
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

    fn match_starting_from(&self, to_match: &str, starting_from: usize) -> Result<bool> {
        let chars: Vec<char> = to_match.chars().collect();
        let mut next_states: HashSet<Rc<State>> = HashSet::new();
        let mut current_states: HashSet<Rc<State>> = HashSet::new();

        if let StateKind::Start(ref s) = self.start_state.kind {
            match s {
                Some(s) => {
                    if let State {
                        kind: StateKind::Simple(RegexMatchable::Anchor(Anchor::Start), _),
                        id: _,
                    } = *s.borrow()
                    {
                        if starting_from != 0 {
                            return Ok(false);
                        }
                    }

                    current_states.insert(Rc::new(s.borrow().clone()));
                }
                None => return Ok(false),
            }
        }

        for c in chars.iter() {
            next_states.clear();

            if current_states.len() == 0 {
                return Ok(false);
            }

            for state in current_states.iter() {
                if let StateKind::Match = state.kind {
                    return Ok(true);
                }

                if let Some(next) = state.matches(*c, starting_from) {
                    for n in next.iter() {
                        next_states.insert(n.clone());
                    }
                }
            }

            current_states = next_states.clone();
        }

        // For pattern to match it needs to reach match state or end of string state
        //  before exhausting input string
        Ok(current_states
            .iter()
            .any(|s| s.kind.check_is_match_after_reaching_end_of_input()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_matches(match_res: Result<bool>) {
        match match_res {
            Ok(b) => assert!(b),
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
    }

    fn test_not_matches(match_res: Result<bool>) {
        match match_res {
            Ok(b) => assert!(!b),
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
    }

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
        test_matches(regex.matches("7ahoj7"));
        test_not_matches(regex.matches("aahoj7"));
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
        test_matches(regex.matches("0"));
        test_matches(regex.matches("1"));
        test_matches(regex.matches("2"));
        test_matches(regex.matches("3"));
        test_matches(regex.matches("4"));
        test_matches(regex.matches("5"));
        test_matches(regex.matches("6"));
        test_matches(regex.matches("7"));
        test_matches(regex.matches("8"));
        test_matches(regex.matches("9"));
        test_matches(regex.matches("abc_0_xyz"));
        test_not_matches(regex.matches("aahoj"));
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
    }
}
