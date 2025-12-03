use anyhow::{anyhow, Result};
use std::fmt::{Debug, Display, Formatter};

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

#[derive(Debug, Clone)]
enum StateType {
    Simple(Option<Box<State>>),
    Split(Option<Box<State>>, Option<Box<State>>),
    Match,
    Start(Option<Box<State>>),
}

impl Display for StateType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            StateType::Match => write!(f, "MATCH"),
            StateType::Start(next) => write!(f, "{:?}", next),
            StateType::Simple(next) => write!(f, "{:?}", next),
            StateType::Split(n1, n2) => write!(f, "{:?} | {:?}", n1, n2),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RegexAtom {
    CharacterClass(char),
    Char(char),
}

#[derive(Clone)]
struct State {
    atom: RegexAtom,
    next: Option<StateType>,
}

impl State {
    pub fn new(atom: RegexAtom) -> State {
        State { atom, next: None }
    }

    pub fn start() -> Self {
        return State {
            atom: RegexAtom::Char('0'),
            next: Some(StateType::Start(None)),
        };
    }

    pub fn out(&self) -> Vec<Option<Box<State>>> {
        match self.next.clone() {
            Some(next) => match next {
                StateType::Simple(ref_state) => vec![ref_state],
                StateType::Split(ref1, ref2) => vec![ref1, ref2],
                StateType::Match => vec![],
                StateType::Start(state) => vec![state],
            },
            None => vec![],
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "State ({:?})\n", self.atom)?;
        write!(f, "   |\n")?;
        write!(f, "   -> ")?;

        match &self.next {
            Some(next) => write!(f, "{}", next),
            None => write!(f, "()"),
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "State ({:?})\n", self.atom)?;
        write!(f, "   |\n")?;
        write!(f, "   -> ")?;

        match &self.next {
            Some(next) => write!(f, "{:?}", next),
            None => write!(f, "()"),
        }
    }
}

type FragmentState = Option<Box<State>>;

#[derive(Clone)]
struct Fragment {
    state: Box<State>,
    out: Vec<FragmentState>,
}

impl Fragment {
    pub fn new(state: State) -> Self {
        let outs = state.out();

        return Fragment {
            state: Box::new(state),
            out: outs,
        };
    }

    pub fn patch(&mut self, next_fragment: &Self) {
        // let mut new_out: Vec<FragmentState> = vec![];

        // for state in self.out.iter_mut() {
        //     new_out.extend(state.out().into_iter());
        //     state.next = Some(StateType::Simple(next_fragment.state.clone()))
        // }
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
            return Err(anyhow!("Nothing to parse"));
        }

        let fragment = self.fragment()?;

        return Ok(*fragment.state);
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

    fn escaped(&mut self) -> Result<RegexAtom> {
        self.consume(Token::Escape)?;

        match self.advance() {
            Token::Character(c) => Ok(RegexAtom::CharacterClass(c)),
            _ => Err(anyhow!("Expected escaped symbol got {:?}", self.peek()?)),
        }
    }

    fn character(&mut self) -> Result<RegexAtom> {
        match self.advance() {
            Token::Character(c) => return Ok(RegexAtom::Char(c)),
            _ => Err(anyhow!("Expected character symbol got {:?}", self.peek()?)),
        }
    }

    fn atom(&mut self) -> Result<RegexAtom> {
        match self.peek()? {
            Token::Escape => return self.escaped(),
            Token::Character(_) => return self.character(),
            _ => Err(anyhow!("Unrecognized token")),
        }
    }

    fn sub_fragment(&mut self) -> Result<Fragment> {
        self.consume(Token::LeftBracket)?;
        let fragment = self.fragment();
        self.consume(Token::RightBracket)?;

        return fragment;
    }

    fn pipe(&mut self) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn wildcard(&mut self) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn question_mark(&mut self) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn plus(&mut self) -> Result<Fragment> {
        Err(anyhow!("ahoj"))
    }

    fn fragment(&mut self) -> Result<Fragment> {
        match self.peek()? {
            Token::LeftBracket => return self.sub_fragment(),
            Token::Escape | Token::Character(_) => return self.state(),
            Token::Pipe => return self.pipe(),
            Token::WildCard => return self.wildcard(),
            Token::QuestionMark => return self.question_mark(),
            Token::Plus => return self.plus(),
            _ => Err(anyhow!("Syntax error in expression")),
        }
    }

    fn state(&mut self) -> Result<Fragment> {
        let mut fragment = Fragment::new(State::new(self.atom()?));

        while !self.is_at_end() {
            match self.atom() {
                Ok(atom) => fragment.patch(&Fragment::new(State::new(atom))),
                Err(_) => return Ok(fragment),
            }
        }

        return Ok(fragment);
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
        let mut start_state = State::start();
        start_state.next = Some(StateType::Start(Some(Box::new(
            Parser::new(&tokens).parse()?,
        ))));

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
        let regex = RegexPattern::new("\\dahoj7").unwrap();
        let tokens = regex.tokens;

        assert!(tokens[0] == Token::Escape);
        assert!(tokens[1] == Token::Character('d'));
        assert!(tokens[2] == Token::Character('a'));
    }

    #[test]
    fn test_regex_pattern_tokenize_harder() {
        let regex = RegexPattern::new("(aa)|(bb)d?(a+c*)*").unwrap();
        let tokens = regex.tokens;

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

        print!("\n{}\n", state);

        assert!(state.atom == RegexAtom::Char('0'));
        assert!(state.atom != RegexAtom::Char('0'));
    }
}
