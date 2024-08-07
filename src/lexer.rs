use std::{
    fs::File,
    io::{BufReader, Bytes},
    iter::Peekable,
};

#[derive(Debug, Clone, PartialEq)]
pub enum LexerToken {
    Identifier(String),
    Symbol(Symbol),
    Literal(LexerLiteral),
    Keyword(Keyword),
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Bang,
    NotEqual,
    And,
    Or,

    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,

    Semicolon,
    Comma,
    Dot,
    Hash,

    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LSquareBracket,
    RSquareBracket,
    LAngleBracket,
    RAngleBracket,

    Increment,
    Decrement,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerLiteral {
    Int(i32),
    Float(f32),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    If,
    Else,
    While,
    For,
    Return,
    Break,
    Continue,
    Type(LexerType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerType {
    Null,
    Void,
    Int,
    Bool,
    Char,
    Float,
    Double,
    Long,
    Short,
    Unsigned,
}

struct LexerIterator {
    iterator: Peekable<Bytes<BufReader<File>>>,
}

impl LexerIterator {
    fn new(iterator: Bytes<BufReader<File>>) -> Self {
        LexerIterator {
            iterator: iterator.peekable(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        match self.iterator.peek() {
            Some(byte) => match byte {
                Ok(byte) => Some(char::from(*byte)),
                Err(_) => None,
            },
            None => None,
        }
    }

    fn next(&mut self) -> Option<char> {
        match self.iterator.next() {
            Some(byte) => match byte {
                Ok(byte) => Some(char::from(byte)),
                Err(_) => None,
            },
            None => None,
        }
    }
}

pub fn lex(iterator: Bytes<BufReader<File>>) -> Result<Vec<LexerToken>, String> {
    let mut tokens: Vec<LexerToken> = Vec::new();
    let mut iterator = LexerIterator::new(iterator);
    loop {
        let token = scan(&mut iterator)?;
        if let LexerToken::EOF = token {
            tokens.push(token);
            return Ok(tokens);
        }
        tokens.push(token);
    }
}

fn get_next_character(iterator: &mut LexerIterator) -> Option<char> {
    loop {
        let character = iterator.next()?;
        if !character.is_ascii_whitespace() {
            return Some(character);
        }
    }
}

fn scan(iterator: &mut LexerIterator) -> Result<LexerToken, String> {
    let character = match get_next_character(iterator) {
        Some(character) => character,
        None => return Ok(LexerToken::EOF),
    };

    return match character {
        '+' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '+' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::Increment))
                }
                _ => Ok(LexerToken::Symbol(Symbol::Plus)),
            }
        }
        '-' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '-' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::Decrement))
                }
                _ => Ok(LexerToken::Symbol(Symbol::Minus)),
            }
        }
        '*' => Ok(LexerToken::Symbol(Symbol::Star)),
        '(' => Ok(LexerToken::Symbol(Symbol::LBracket)),
        ')' => Ok(LexerToken::Symbol(Symbol::RBracket)),
        '{' => Ok(LexerToken::Symbol(Symbol::LBrace)),
        '}' => Ok(LexerToken::Symbol(Symbol::RBrace)),
        '[' => Ok(LexerToken::Symbol(Symbol::LSquareBracket)),
        ']' => Ok(LexerToken::Symbol(Symbol::RSquareBracket)),
        ';' => Ok(LexerToken::Symbol(Symbol::Semicolon)),
        ',' => Ok(LexerToken::Symbol(Symbol::Comma)),
        '.' => Ok(LexerToken::Symbol(Symbol::Dot)),
        '~' => Ok(LexerToken::Symbol(Symbol::BitwiseNot)),
        '#' => Ok(LexerToken::Symbol(Symbol::Hash)),
        '/' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '/' => {
                    iterator.next();
                    loop {
                        match iterator.next() {
                            Some('\n') => return scan(iterator),
                            Some(_) => continue,
                            None => {
                                return Err("Missing newline character after comment".to_string())
                            }
                        }
                    }
                }
                '*' => loop {
                    iterator.next();
                    match iterator.next() {
                        Some('*') => match iterator.next() {
                            Some('/') => return scan(iterator),
                            Some(_) => continue,
                            None => {
                                return Err("Missing backslash to end block comment".to_string())
                            }
                        },
                        Some(_) => continue,
                        None => return Err("Missing */ to end block comment".to_string()),
                    }
                },
                _ => Ok(LexerToken::Symbol(Symbol::Slash)),
            }
        }
        '=' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '=' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::DoubleEqual))
                }
                _ => Ok(LexerToken::Symbol(Symbol::Equal)),
            }
        }
        '!' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '=' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::NotEqual))
                }
                _ => Ok(LexerToken::Symbol(Symbol::Bang)),
            }
        }
        '<' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '=' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::LessThanOrEqual))
                }
                _ => Ok(LexerToken::Symbol(Symbol::LAngleBracket)),
            }
        }
        '>' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '=' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::GreaterThanOrEqual))
                }
                _ => Ok(LexerToken::Symbol(Symbol::RAngleBracket)),
            }
        }
        '&' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '&' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::And))
                }
                _ => Ok(LexerToken::Symbol(Symbol::BitwiseAnd)),
            }
        }
        '|' => {
            let character = match iterator.peek() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match character {
                '|' => {
                    iterator.next();
                    Ok(LexerToken::Symbol(Symbol::Or))
                }
                _ => Ok(LexerToken::Symbol(Symbol::BitwiseOr)),
            }
        }
        '"' => Ok(LexerToken::Literal(LexerLiteral::String(
            scan_string_literal(iterator)?,
        ))),
        '\'' => {
            let character = match iterator.next() {
                Some(character) => character,
                None => return Ok(LexerToken::EOF),
            };

            match iterator.next() {
                Some('\'') => Ok(LexerToken::Literal(LexerLiteral::Char(character))),
                _ => return Err("Missing ' for char literal".to_string()),
            }
        }
        _ => {
            if character.is_digit(10) {
                return Ok(LexerToken::Literal(scan_int_literal(iterator, character)?));
            }

            if character.is_alphabetic() || character == '_' {
                let phrase = scan_identifier(iterator, character);
                return Ok(match phrase.as_str() {
                    "if" => LexerToken::Keyword(Keyword::If),
                    "else" => LexerToken::Keyword(Keyword::Else),
                    "return" => LexerToken::Keyword(Keyword::Return),
                    "break" => LexerToken::Keyword(Keyword::Break),
                    "continue" => LexerToken::Keyword(Keyword::Continue),
                    "while" => LexerToken::Keyword(Keyword::While),
                    "for" => LexerToken::Keyword(Keyword::For),
                    "NULL" => LexerToken::Keyword(Keyword::Type(LexerType::Null)),
                    "void" => LexerToken::Keyword(Keyword::Type(LexerType::Void)),
                    "int" => LexerToken::Keyword(Keyword::Type(LexerType::Int)),
                    "bool" => LexerToken::Keyword(Keyword::Type(LexerType::Bool)),
                    "char" => LexerToken::Keyword(Keyword::Type(LexerType::Char)),
                    "float" => LexerToken::Keyword(Keyword::Type(LexerType::Float)),
                    "double" => LexerToken::Keyword(Keyword::Type(LexerType::Double)),
                    "long" => LexerToken::Keyword(Keyword::Type(LexerType::Long)),
                    "short" => LexerToken::Keyword(Keyword::Type(LexerType::Short)),
                    "unsigned" => LexerToken::Keyword(Keyword::Type(LexerType::Unsigned)),
                    "true" => LexerToken::Literal(LexerLiteral::Bool(true)),
                    "false" => LexerToken::Literal(LexerLiteral::Bool(false)),
                    _ => LexerToken::Identifier(phrase.to_string()),
                });
            }

            Err(format!("Unrecognised input '{}'", character))
        }
    };
}

fn scan_int_literal(iterator: &mut LexerIterator, character: char) -> Result<LexerLiteral, String> {
    let mut value = String::from(character);
    let mut whole_value = true;
    loop {
        let character = match iterator.peek() {
            Some(character) => character,
            None => return Err("".to_string()),
        };

        if character.is_digit(10) {
            iterator.next();
            value.push(character)
        } else if character == '.' {
            iterator.next();
            whole_value = false;
            value.push(character)
        } else {
            if whole_value {
                match value.parse::<i32>() {
                    Ok(value) => return Ok(LexerLiteral::Int(value)),
                    Err(_) => {
                        return Err(format!("Invalid integer literal '{}'", value).to_string())
                    }
                }
            }
            match value.parse::<f32>() {
                Ok(value) => return Ok(LexerLiteral::Float(value)),
                Err(_) => return Err(format!("Invalid float literal '{}'", value).to_string()),
            }
        }
    }
}

fn scan_identifier(iterator: &mut LexerIterator, character: char) -> String {
    let mut phrase = String::new();
    phrase.push(character);

    loop {
        if let Some(character) = iterator.peek() {
            if character.is_alphabetic() || character.is_digit(10) || character == '_' {
                iterator.next();
                phrase.push(character);
            } else {
                return phrase;
            }
        }
    }
}

fn scan_string_literal(iterator: &mut LexerIterator) -> Result<String, String> {
    let mut literal = String::new();
    loop {
        match iterator.next() {
            Some('\n') | None => return Err("Missing string literal end quotes".to_string()),
            Some(character) => {
                if character == '"' {
                    return Ok(literal);
                }
                literal.push(character)
            }
        }
    }
}
