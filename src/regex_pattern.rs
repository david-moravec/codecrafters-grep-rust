use anyhow::{anyhow, Result};
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
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
    WildCard,
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
            Self::WildCard => '*',
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
            } else if c.is_alphabetic() || c == ' ' || c == '@' {
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
                tokens.push(Token::WildCard)
            } else if c == '^' {
                tokens.push(Token::Caret)
            } else if c == '$' {
                tokens.push(Token::Dollar)
            }
        }
        return tokens;
    }

    fn previous(&self) -> char {
        return self.chars[self.current - 1];
    }

    fn advance(&mut self) -> Option<char> {
        if self.current == self.chars.len() {
            return None;
        }
        self.current += 1;

        return Some(self.chars[self.current - 1]);
    }

    fn peek(&self) -> Option<char> {
        return self.chars.get(self.current + 1).map(|x| *x);
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
            &Self::Char(ch) => c == ch,
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

#[derive(Clone)]
enum StateMut {
    Simple(RegexMatchable, Option<Rc<RefCell<StateMut>>>),
    Split(Option<Rc<RefCell<StateMut>>>, Option<Rc<RefCell<StateMut>>>),
    Match,
    Start(Option<Rc<RefCell<StateMut>>>),
}

impl StateMut {
    pub fn out(&self) -> Vec<Option<Rc<RefCell<StateMut>>>> {
        match self.clone() {
            next => match next {
                StateMut::Simple(_, ref_state) => vec![ref_state],
                StateMut::Split(ref1, ref2) => vec![ref1, ref2],
                StateMut::Match => vec![],
                StateMut::Start(state) => vec![state],
            },
        }
    }

    pub fn patch(&mut self, next_state: StateMut) -> Result<()> {
        match self {
            Self::Simple(_, ref mut next) | Self::Start(ref mut next) => {
                *next = Some(Rc::new(RefCell::new(next_state)));
                Ok(())
            }
            Self::Split(ref mut next1, ref mut next2) => {
                *next1 = Some(Rc::new(RefCell::new(next_state.clone())));
                *next2 = Some(Rc::new(RefCell::new(next_state)));
                Ok(())
            }
            Self::Match => Err(anyhow!("Cannot patch mathed State")),
        }
    }

    fn _into_state(self) -> Result<State> {
        let result = match self {
            Self::Simple(atom, state) => {
                let state = state.unwrap().borrow().clone()._into_state()?;

                State::Simple(atom, Rc::new(state))
            }
            Self::Split(state1, state2) => State::Split(
                Rc::new(state2.unwrap().borrow().clone()._into_state()?),
                Rc::new(state1.unwrap().borrow().clone()._into_state()?),
            ),
            Self::Match => State::Match,
            Self::Start(state) => {
                let state = state.unwrap().borrow().clone()._into_state()?;
                State::Start(Some(Rc::new(state)))
            }
        };

        Ok(result)
    }

    pub fn into_state(self) -> Result<Option<State>> {
        match self {
            Self::Start(s) => match s {
                Some(t) => Ok(Some(t.borrow().clone()._into_state()?)),
                None => Ok(None),
            },
            _ => Err(anyhow!("Can transform only from StateBuilder::Start")),
        }
    }
}

impl Debug for StateMut {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            StateMut::Match => write!(f, " -> MATCH"),
            StateMut::Start(next) => write!(f, "{:?}", next),
            StateMut::Simple(atom, next) => write!(f, "{:?}{:?}", atom, next),
            StateMut::Split(n1, n2) => write!(f, "{:?}|{:?}", n1, n2),
        }
    }
}

#[derive(Clone)]
enum State {
    Simple(RegexMatchable, Rc<Self>),
    Split(Rc<Self>, Rc<Self>),
    Match,
    Start(Option<Rc<Self>>),
}

impl State {
    pub fn check_is_match_after_reaching_end_of_input(&self) -> bool {
        match self {
            State::Match => true,
            State::Simple(atom, next) => {
                if let RegexMatchable::Anchor(pos) = atom {
                    if let Anchor::End = pos {
                        next.check_is_match_after_reaching_end_of_input()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn matches(&self, c: char, index: usize) -> Option<Vec<Rc<Self>>> {
        match self {
            State::Simple(ref atom, ref next) => {
                if atom.matches(c, index) {
                    if let RegexMatchable::Anchor(_) = atom {
                        return next.matches(c, index);
                    }
                    Some(vec![next.clone()])
                } else {
                    None
                }
            }
            State::Split(ref next1, ref next2) => return Some(vec![next1.clone(), next2.clone()]),
            State::Match => return None,
            State::Start(_) => {
                panic!("Start should be never reached during matching")
            }
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            State::Match => write!(f, " -> MATCH"),
            State::Start(next) => write!(f, "{:?}", next),
            State::Simple(atom, next) => write!(f, "{:?}{:?}", atom, next),
            State::Split(n1, n2) => write!(f, "{:?}|{:?}", n1, n2),
        }
    }
}

type FragmentState = Option<Rc<StateMut>>;

#[derive(Clone)]
struct Fragment {
    state: Rc<RefCell<StateMut>>,
    out: Vec<Rc<RefCell<StateMut>>>,
}

impl Fragment {
    pub fn new(state: Rc<RefCell<StateMut>>, out: Vec<Rc<RefCell<StateMut>>>) -> Self {
        return Fragment {
            state: state.clone(),
            out,
        };
    }

    pub fn patch(&mut self, next_fragment: Self) {
        for state in self.out.iter() {
            if let StateMut::Simple(_, ref mut next_opt) = *state.borrow_mut() {
                *next_opt = Some(next_fragment.state.clone());
            }
        }

        self.out = next_fragment.out;
    }

    pub fn patch_match(&mut self) {
        for state in self.out.iter() {
            if let StateMut::Simple(_, ref mut next_opt) = *state.borrow_mut() {
                *next_opt = Some(Rc::new(RefCell::new(StateMut::Match)));
            }
        }
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
        if self.is_at_end() {
            return Ok(State::Start(None));
        }

        let mut fragments: Vec<Fragment> = vec![];

        while !self.is_at_end() {
            fragments.push(self.fragment()?);
        }

        // we checked above that we have something to parse
        // and self.state returns state so we have at least one
        let mut current = fragments.pop().unwrap();
        // println!("FinalState before match -> {:?}", current.state);
        current.patch_match();
        // println!("FinalState after  match -> {:?}", current.state);

        while let Some(mut prev) = fragments.pop() {
            prev.patch(current);
            current = prev;
        }

        let start = StateMut::Start(Some(Rc::new(RefCell::new(current.state.borrow().clone()))));

        return start._into_state();
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

    fn try_advance(&mut self) -> Result<Token> {
        match self.tokens.get(self.current + 1).map(|x| *x) {
            Some(t) => {
                self.current += 1;
                Ok(t)
            }
            None => Err(anyhow!("End of file reached")),
        }
    }

    fn advance_if_matches(&mut self, token: Token) -> Option<Token> {
        if self.is_at_end() {
            return None;
        } else if std::mem::discriminant(&token) == std::mem::discriminant(&self.peek().unwrap()) {
            return Some(self.advance());
        } else {
            return None;
        }
    }

    fn advance_if_matches_many(&mut self, tokens: &[Token]) -> Option<Token> {
        for token in tokens {
            let matches = self.advance_if_matches(*token);

            if matches.is_some() {
                return matches;
            }
        }

        return None;
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

    fn peek_next(&self) -> Option<Token> {
        return self.tokens.get(self.current + 1).map(|x| *x);
    }

    fn positive_char_group(&mut self) -> Result<Fragment> {
        self.consume(Token::LeftSquareBracket)?;
        let state = match self.peek()? {
            Token::Caret => {
                self.consume(Token::Caret)?;

                Rc::new(RefCell::new(StateMut::Simple(
                    RegexMatchable::NegativeCharGroup(self.char_group()?),
                    None,
                )))
            }
            _ => Rc::new(RefCell::new(StateMut::Simple(
                RegexMatchable::PositiveCharGroup(self.char_group()?),
                None,
            ))),
        };
        self.consume(Token::RightSquareBracket)?;

        return Ok(Fragment::new(state.clone(), vec![state.clone()]));
    }

    fn char_group(&mut self) -> Result<HashSet<RegexAtom>> {
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

    fn pipe(&mut self, prev_state: &mut Fragment) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn wildcard(&mut self, prev_state: &mut Fragment) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn question_mark(&mut self, prev_state: &mut Fragment) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn plus(&mut self, prev_state: &mut Fragment) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn state_simple(&mut self) -> Result<Rc<RefCell<StateMut>>> {
        match self.peek()? {
            Token::Caret => {
                self.advance();
                Ok(Rc::new(RefCell::new(StateMut::Simple(
                    RegexMatchable::Anchor(Anchor::Start),
                    None,
                ))))
            }
            Token::Dollar => {
                self.advance();
                Ok(Rc::new(RefCell::new(StateMut::Simple(
                    RegexMatchable::Anchor(Anchor::End),
                    None,
                ))))
            }
            _ => Ok(Rc::new(RefCell::new(StateMut::Simple(
                RegexMatchable::Atom(self.atom()?),
                None,
            )))),
        }
    }

    fn fragment_simple(&mut self) -> Result<Fragment> {
        let mut states: Vec<Rc<RefCell<StateMut>>> = vec![];

        while !self.is_at_end() {
            match self.peek()? {
                Token::Character(_) | Token::Escape | Token::Dollar | Token::Caret => {
                    states.push(self.state_simple()?);
                }
                _ => break,
            }
        }

        let mut current = states.pop().unwrap();
        let out = current.clone();

        while let Some(prev) = states.pop() {
            if let StateMut::Simple(_, ref mut next_opt) = *prev.borrow_mut() {
                *next_opt = Some(current.clone());
            }
            current = prev;
        }
        let fragment = Fragment::new(current, vec![out]);

        return Ok(fragment);
    }

    fn sub_fragment(&mut self) -> Result<Fragment> {
        self.consume(Token::LeftBracket)?;
        let result = self.fragment_simple();
        self.consume(Token::RightBracket)?;

        return result;
    }

    fn bracket_fragment(&mut self) -> Result<Fragment> {
        match self.peek()? {
            Token::LeftBracket => return self.sub_fragment(),
            Token::LeftSquareBracket => return self.positive_char_group(),
            Token::Escape | Token::Character(_) | Token::Caret | Token::Dollar => {
                return self.fragment_simple()
            }
            token => Err(anyhow!("Expected bracket/escape/character got {:?}", token)),
        }
    }

    fn fragment(&mut self) -> Result<Fragment> {
        let mut state1 = self.bracket_fragment()?;

        if self.is_at_end() {
            return Ok(state1);
        }

        match self.peek()? {
            Token::Pipe => return self.pipe(&mut state1),
            Token::WildCard => return self.wildcard(&mut state1),
            Token::QuestionMark => return self.question_mark(&mut state1),
            Token::Plus => return self.plus(&mut state1),
            _ => Ok(state1),
        }
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

        return Ok(false);
    }

    fn match_starting_from(&self, to_match: &str, starting_from: usize) -> Result<bool> {
        // println!("Matching: {}", to_match);
        // println!("Pattern : {:?}", self.start_state);

        let chars: Vec<char> = to_match.chars().collect();
        let mut next_states: Vec<Rc<State>> = vec![];
        let mut current_states: Vec<Rc<State>> = vec![];

        if let State::Start(ref s) = self.start_state {
            match s {
                Some(s) => current_states.push(s.clone()),
                None => return Ok(false),
            }
        }

        for c in chars.iter() {
            next_states.clear();

            for state in current_states.iter() {
                if let State::Match = **state {
                    return Ok(true);
                }

                if let Some(next) = state.matches(*c, starting_from) {
                    for n in next.iter() {
                        next_states.push(n.clone());
                    }
                }
            }

            current_states = next_states.clone();
        }

        // For pattern to match it needs to reach match state or end of string state
        //  before exhausting input string
        Ok(current_states
            .iter()
            .any(|s| s.check_is_match_after_reaching_end_of_input()))
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
        assert!(tokens[15] == Token::WildCard);
        assert!(tokens[16] == Token::RightBracket);
        assert!(tokens[17] == Token::WildCard);
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
}
