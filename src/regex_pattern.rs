use anyhow::{anyhow, Result};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
enum Token {
    Escape,
    Character(char),
    LeftBracket,
    RightBracket,
    Pipe,
    WildCard,
    QuestionMark,
    Plus,
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
            } else if c.is_alphabetic() {
                tokens.push(Token::Character(c))
            } else if c == '|' {
                tokens.push(Token::Pipe);
            } else if c == '(' {
                tokens.push(Token::LeftBracket)
            } else if c == ')' {
                tokens.push(Token::RightBracket)
            } else if c == '?' {
                tokens.push(Token::QuestionMark)
            } else if c == '+' {
                tokens.push(Token::Plus)
            } else if c == '*' {
                tokens.push(Token::WildCard)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RegexAtom {
    CharacterClass(char),
    Char(char),
}

#[derive(Clone)]
enum StateMut {
    Simple(RegexAtom, Option<Rc<RefCell<StateMut>>>),
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
                State::Start(Rc::new(state))
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
            StateMut::Match => write!(f, "MATCH"),
            StateMut::Start(next) => write!(f, "{:?}", next),
            StateMut::Simple(atom, next) => write!(f, "{:?}{:?}", atom, next),
            StateMut::Split(n1, n2) => write!(f, "{:?}|{:?}", n1, n2),
        }
    }
}

#[derive(Clone)]
enum State {
    Simple(RegexAtom, Rc<Self>),
    Split(Rc<Self>, Rc<Self>),
    Match,
    Start(Rc<Self>),
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            State::Match => write!(f, "MATCH"),
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
            println!("before match {:?}", state);
            if let StateMut::Simple(_, ref mut next_opt) = *state.borrow_mut() {
                *next_opt = Some(Rc::new(RefCell::new(StateMut::Match)));
            }
            println!("after match  {:?}", state);
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

    pub fn parse(&mut self) -> Result<StateMut> {
        if self.is_at_end() {
            return Err(anyhow!("Nothing to parse"));
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

        return Ok(current.state.borrow().clone());
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

    fn escaped(&mut self) -> Result<StateMut> {
        self.consume(Token::Escape)?;

        match self.advance() {
            Token::Character(c) => Ok(StateMut::Simple(RegexAtom::CharacterClass(c), None)),
            _ => Err(anyhow!("Expected escaped symbol got {:?}", self.peek()?)),
        }
    }

    fn character(&mut self) -> Result<StateMut> {
        match self.advance() {
            Token::Character(c) => return Ok(StateMut::Simple(RegexAtom::Char(c), None)),
            _ => Err(anyhow!("Expected character symbol got {:?}", self.peek()?)),
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
        Ok(Rc::new(RefCell::new(match self.peek()? {
            Token::Escape => self.escaped(),
            Token::Character(_) => self.character(),
            token => Err(anyhow!("Expected escaped char or char got {:?}", token)),
        }?)))
    }

    fn fragment_simple(&mut self) -> Result<Fragment> {
        let mut states: Vec<Rc<RefCell<StateMut>>> = vec![];

        while !self.is_at_end() {
            match self.peek()? {
                Token::Character(_) | Token::Escape => {
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
            Token::Escape | Token::Character(_) => return self.fragment_simple(),
            token => Err(anyhow!("Expected bracket/escape/character got {:?}", token)),
        }
    }

    fn fragment(&mut self) -> Result<Fragment> {
        let mut state1 = self.fragment_simple()?;

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
        let start_state =
            StateMut::Start(Some(Rc::new(RefCell::new(Parser::new(&tokens).parse()?))))
                ._into_state()?;

        return Ok(RegexPattern {
            tokens,
            pattern,
            start_state,
        });
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
        assert!(tokens[15] == Token::WildCard);
        assert!(tokens[16] == Token::RightBracket);
        assert!(tokens[17] == Token::WildCard);
    }

    #[test]
    fn test_fragment_patch() {}

    #[test]
    fn test_regex_pattern_parse_easy() {
        let regex = match RegexPattern::new("\\dahoj7") {
            Ok(regex) => regex,
            Err(err) => {
                println!("{}", err);
                assert!(false);
                return;
            }
        };

        let state: State = regex.start_state;

        print!("\n{:?}\n", state);
        assert!(false);
    }
}
