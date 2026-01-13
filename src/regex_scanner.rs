#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Token {
    Escape,
    Character(char),
    LeftBracket,
    RightBracket,
    LeftSquareBracket,
    RightSquareBracket,
    LeftBrace,
    RightBrace,
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
            Self::LeftBrace => '{',
            Self::RightBrace => '}',
            Self::Pipe => '|',
            Self::Asterisk => '*',
            Self::QuestionMark => '?',
            Self::Plus => '+',
            Self::Caret => '^',
            Self::Dollar => '$',
        }
    }
}

pub struct Scanner<'a> {
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
            } else if c == '{' {
                tokens.push(Token::LeftBrace)
            } else if c == '}' {
                tokens.push(Token::RightBrace)
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
}
