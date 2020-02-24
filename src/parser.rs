use std::char;
use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum LexError {
    UnexpectedChar(char),
    MalformedEscapeSequence,
    MalformedNumber,
    MalformedChar,
    Nothing,
}

type LERR = LexError;

impl Error for LexError {
    fn description(&self) -> &str {
        match *self {
            LERR::UnexpectedChar(_) => "Unexpected character",
            LERR::MalformedEscapeSequence => "Unexpected values in escape sequence",
            LERR::MalformedNumber => "Unexpected characters in number",
            LERR::MalformedChar => "Char constant not a single character",
            LERR::Nothing => "This error is for internal use only",
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LERR::UnexpectedChar(c) => write!(f, "Unexpected '{}'", c),
            _ => write!(f, "{}", self.description()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorType {
    BadInput(String),
    InputPastEndOfFile,
    UnknownOperator,
    MissingRightParen,
    MissingLeftBrace,
    MissingRightBrace,
    MissingRightBracket,
    MalformedCallExpr,
    MalformedIndexExpr,
    VarExpectsIdentifier(Token),
    FnMissingName(Token),
    FnMissingParams,
}

type PERR = ParseErrorType;

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError(ParseErrorType, usize, usize);

impl ParseError {
    pub fn error_type(&self) -> &ParseErrorType {
        &self.0
    }
    pub fn line(&self) -> usize {
        self.1
    }
    pub fn position(&self) -> usize {
        self.2
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self.0 {
            PERR::BadInput(ref p) => p,
            PERR::InputPastEndOfFile => "Script is incomplete",
            PERR::UnknownOperator => "Unknown operator",
            PERR::MissingRightParen => "Expecting ')'",
            PERR::MissingLeftBrace => "Expecting '{'",
            PERR::MissingRightBrace => "Expecting '}'",
            PERR::MissingRightBracket => "Expecting ']'",
            PERR::MalformedCallExpr => "Invalid expression in function call arguments",
            PERR::MalformedIndexExpr => "Invalid index in indexing expression",
            PERR::VarExpectsIdentifier(_) => "Expecting name of a variable",
            PERR::FnMissingName(_) => "Expecting name in function declaration",
            PERR::FnMissingParams => "Expecting parameters in function declaration",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            PERR::BadInput(ref s) => {
                write!(f, "{}", s)?;
            }
            PERR::VarExpectsIdentifier(ref token) | PERR::FnMissingName(ref token) => match token {
                Token::None => write!(f, "{}", self.description())?,
                _ => write!(
                    f,
                    "{} (but gets {:?} token instead)",
                    self.description(),
                    token
                )?,
            },
            _ => write!(f, "{}", self.description())?,
        }

        if self.line() > 0 {
            write!(f, " at line {}, position {}", self.line(), self.position())
        } else {
            write!(f, " but script is incomplete")
        }
    }
}

pub struct AST(pub(crate) Vec<Stmt>, pub(crate) Vec<FnDef>);

#[derive(Debug, PartialEq, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    If(Box<Expr>, Box<Stmt>),
    IfElse(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    Loop(Box<Stmt>),
    For(String, Box<Expr>, Box<Stmt>),
    Let(String, Option<Box<Expr>>),
    Block(Vec<Stmt>),
    Expr(Box<Expr>),
    Break,
    Return,
    ReturnWithVal(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    IntegerConstant(i64),
    FloatConstant(f64),
    Identifier(String),
    CharConstant(char),
    StringConstant(String),
    FunctionCall(String, Vec<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Box<Expr>),
    Index(String, Box<Expr>),
    Array(Vec<Expr>),
    True,
    False,
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    None,
    IntegerConstant(i64),
    FloatConstant(f64),
    Identifier(String),
    CharConstant(char),
    StringConst(String),
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Plus,
    UnaryPlus,
    Minus,
    UnaryMinus,
    Multiply,
    Divide,
    SemiColon,
    Colon,
    Comma,
    Period,
    Equals,
    True,
    False,
    Let,
    If,
    Else,
    While,
    Loop,
    LessThan,
    GreaterThan,
    Bang,
    LessThanEqualsTo,
    GreaterThanEqualsTo,
    EqualsTo,
    NotEqualsTo,
    Pipe,
    Or,
    Ampersand,
    And,
    Fn,
    Break,
    Return,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    OrAssign,
    XOrAssign,
    LeftShift,
    RightShift,
    XOr,
    Modulo,
    ModuloAssign,
    PowerOf,
    PowerOfAssign,
    For,
    In,
    LexErr(LexError),
}

impl Token {
    // if another operator is after these, it's probably an unary operator
    // not sure about fn's name
    pub fn is_next_unary(&self) -> bool {
        use self::Token::*;

        match *self {
            LeftBrace        | // (+expr) - is unary
            // RightBrace    | {expr} - expr not unary & is closing
            LeftParen        | // {-expr} - is unary
            // RightParen    | (expr) - expr not unary & is closing
            LeftBracket      | // [-expr] - is unary
            // RightBracket  | [expr] - expr not unary & is closing
            Plus             |
            UnaryPlus        |
            Minus            |
            UnaryMinus       |
            Multiply         |
            Divide           |
            Colon            |
            Comma            |
            Period           |
            Equals           |
            LessThan         |
            GreaterThan      |
            Bang             |
            LessThanEqualsTo |
            GreaterThanEqualsTo |
            EqualsTo         |
            NotEqualsTo      |
            Pipe             |
            Or               |
            Ampersand        |
            And              |
            If               |
            While            |
            PlusAssign       |
            MinusAssign      |
            MultiplyAssign   |
            DivideAssign     |
            LeftShiftAssign  |
            RightShiftAssign |
            AndAssign        |
            OrAssign         |
            XOrAssign        |
            LeftShift        |
            RightShift       |
            XOr              |
            Modulo           |
            ModuloAssign     |
            Return           |
            PowerOf          |
            In               |
            PowerOfAssign => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_bin_op(&self) -> bool {
        use self::Token::*;

        match *self {
            RightBrace       |
            RightParen       |
            RightBracket     |
            Plus             |
            Minus            |
            Multiply         |
            Divide           |
            Comma            |
            // Period           | <- does period count?
            Equals           |
            LessThan         |
            GreaterThan      |
            LessThanEqualsTo |
            GreaterThanEqualsTo |
            EqualsTo         |
            NotEqualsTo      |
            Pipe             |
            Or               |
            Ampersand        |
            And              |
            PowerOf => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_un_op(&self) -> bool {
        use self::Token::*;

        match *self {
            UnaryPlus | UnaryMinus | Equals | Bang | Return => true,
            _ => false,
        }
    }
}

pub struct TokenIterator<'a> {
    last: Token,
    line: usize,
    pos: usize,
    char_stream: Peekable<Chars<'a>>,
}

impl<'a> TokenIterator<'a> {
    fn advance(&mut self) {
        self.pos += 1;
    }
    fn advance_line(&mut self) {
        self.line += 1;
        self.pos = 0;
    }

    pub fn parse_string_const(
        &mut self,
        enclosing_char: char,
    ) -> Result<String, (LexError, usize, usize)> {
        let mut result = Vec::new();
        let mut escape = false;

        while let Some(nxt) = self.char_stream.next() {
            self.advance();
            if nxt == '\n' {
                self.advance_line();
            }

            match nxt {
                '\\' if !escape => escape = true,
                '\\' if escape => {
                    escape = false;
                    result.push('\\');
                }
                't' if escape => {
                    escape = false;
                    result.push('\t');
                }
                'n' if escape => {
                    escape = false;
                    result.push('\n');
                }
                'r' if escape => {
                    escape = false;
                    result.push('\r');
                }
                'x' if escape => {
                    escape = false;
                    let mut out_val: u32 = 0;
                    for _ in 0..2 {
                        if let Some(c) = self.char_stream.next() {
                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                            }
                        } else {
                            return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                        }
                        self.advance();
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                    }
                }
                'u' if escape => {
                    escape = false;
                    let mut out_val: u32 = 0;
                    for _ in 0..4 {
                        if let Some(c) = self.char_stream.next() {
                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                            }
                        } else {
                            return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                        }
                        self.advance();
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                    }
                }
                'U' if escape => {
                    escape = false;
                    let mut out_val: u32 = 0;
                    for _ in 0..8 {
                        if let Some(c) = self.char_stream.next() {
                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                            }
                        } else {
                            return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                        }
                        self.advance();
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err((LERR::MalformedEscapeSequence, self.line, self.pos));
                    }
                }
                x if enclosing_char == x && escape => result.push(x),
                x if enclosing_char == x && !escape => break,
                _ if escape => return Err((LERR::MalformedEscapeSequence, self.line, self.pos)),
                x => {
                    escape = false;
                    result.push(x);
                }
            }
        }

        let out: String = result.iter().cloned().collect();
        Ok(out)
    }

    fn inner_next(&mut self) -> Option<(Token, usize, usize)> {
        while let Some(c) = self.char_stream.next() {
            self.advance();

            let line = self.line;
            let pos = self.pos;

            match c {
                '\n' => self.advance_line(),
                '0'..='9' => {
                    let mut result = Vec::new();
                    let mut radix_base: Option<u32> = None;
                    result.push(c);

                    while let Some(&nxt) = self.char_stream.peek() {
                        match nxt {
                            '0'..='9' => {
                                result.push(nxt);
                                self.char_stream.next();
                                self.advance();
                            }
                            '.' => {
                                result.push(nxt);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&nxt_float) = self.char_stream.peek() {
                                    match nxt_float {
                                        '0'..='9' => {
                                            result.push(nxt_float);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                            }
                            'x' | 'X' => {
                                result.push(nxt);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&nxt_hex) = self.char_stream.peek() {
                                    match nxt_hex {
                                        '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                            result.push(nxt_hex);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                                radix_base = Some(16);
                            }
                            'o' | 'O' => {
                                result.push(nxt);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&nxt_oct) = self.char_stream.peek() {
                                    match nxt_oct {
                                        '0'..='8' => {
                                            result.push(nxt_oct);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                                radix_base = Some(8);
                            }
                            'b' | 'B' => {
                                result.push(nxt);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&nxt_bin) = self.char_stream.peek() {
                                    match nxt_bin {
                                        '0' | '1' | '_' => {
                                            result.push(nxt_bin);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                                radix_base = Some(2);
                            }
                            _ => break,
                        }
                    }

                    if let Some(radix) = radix_base {
                        let out: String = result
                            .iter()
                            .cloned()
                            .skip(2)
                            .filter(|c| c != &'_')
                            .collect();
                        if let Ok(val) = i64::from_str_radix(&out, radix) {
                            return Some((Token::IntegerConstant(val), line, pos));
                        }
                    }

                    let out: String = result.iter().cloned().collect();

                    return Some((
                        if let Ok(val) = out.parse::<i64>() {
                            Token::IntegerConstant(val)
                        } else if let Ok(val) = out.parse::<f64>() {
                            Token::FloatConstant(val)
                        } else {
                            Token::LexErr(LERR::MalformedNumber)
                        },
                        line,
                        pos,
                    ));
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut result = Vec::new();
                    result.push(c);

                    while let Some(&nxt) = self.char_stream.peek() {
                        match nxt {
                            x if x.is_alphanumeric() || x == '_' => {
                                result.push(x);
                                self.char_stream.next();
                                self.advance();
                            }
                            _ => break,
                        }
                    }

                    let out: String = result.iter().cloned().collect();

                    return Some((
                        match out.as_ref() {
                            "true" => Token::True,
                            "false" => Token::False,
                            "let" => Token::Let,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "while" => Token::While,
                            "loop" => Token::Loop,
                            "break" => Token::Break,
                            "return" => Token::Return,
                            "fn" => Token::Fn,
                            "for" => Token::For,
                            "in" => Token::In,
                            x => Token::Identifier(x.to_string()),
                        },
                        line,
                        pos,
                    ));
                }
                '"' => {
                    return match self.parse_string_const('"') {
                        Ok(out) => Some((Token::StringConst(out), line, pos)),
                        Err(e) => Some((Token::LexErr(e.0), e.1, e.2)),
                    }
                }
                '\'' => match self.parse_string_const('\'') {
                    Ok(result) => {
                        let mut chars = result.chars();

                        return Some((
                            if let Some(out) = chars.next() {
                                if chars.count() != 0 {
                                    Token::LexErr(LERR::MalformedChar)
                                } else {
                                    Token::CharConstant(out)
                                }
                            } else {
                                Token::LexErr(LERR::MalformedChar)
                            },
                            line,
                            pos,
                        ));
                    }
                    Err(e) => return Some((Token::LexErr(e.0), e.1, e.2)),
                },
                '{' => return Some((Token::LeftBrace, line, pos)),
                '}' => return Some((Token::RightBrace, line, pos)),
                '(' => return Some((Token::LeftParen, line, pos)),
                ')' => return Some((Token::RightParen, line, pos)),
                '[' => return Some((Token::LeftBracket, line, pos)),
                ']' => return Some((Token::RightBracket, line, pos)),
                '+' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::PlusAssign
                            }
                            _ if self.last.is_next_unary() => Token::UnaryPlus,
                            _ => Token::Plus,
                        },
                        line,
                        pos,
                    ))
                }
                '-' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::MinusAssign
                            }
                            _ if self.last.is_next_unary() => Token::UnaryMinus,
                            _ => Token::Minus,
                        },
                        line,
                        pos,
                    ))
                }
                '*' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::MultiplyAssign
                            }
                            _ => Token::Multiply,
                        },
                        line,
                        pos,
                    ))
                }
                '/' => match self.char_stream.peek() {
                    Some(&'/') => {
                        self.char_stream.next();
                        self.advance();
                        while let Some(c) = self.char_stream.next() {
                            if c == '\n' {
                                self.advance_line();
                                break;
                            } else {
                                self.advance();
                            }
                        }
                    }
                    Some(&'*') => {
                        let mut level = 1;
                        self.char_stream.next();
                        self.advance();
                        while let Some(c) = self.char_stream.next() {
                            self.advance();

                            match c {
                                '/' => {
                                    if let Some('*') = self.char_stream.next() {
                                        level += 1;
                                    }
                                    self.advance();
                                }
                                '*' => {
                                    if let Some('/') = self.char_stream.next() {
                                        level -= 1;
                                    }
                                    self.advance();
                                }
                                '\n' => self.advance_line(),
                                _ => (),
                            }

                            if level == 0 {
                                break;
                            }
                        }
                    }
                    Some(&'=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::DivideAssign, line, pos));
                    }
                    _ => return Some((Token::Divide, line, pos)),
                },
                ';' => return Some((Token::SemiColon, line, pos)),
                ':' => return Some((Token::Colon, line, pos)),
                ',' => return Some((Token::Comma, line, pos)),
                '.' => return Some((Token::Period, line, pos)),
                '=' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::EqualsTo, line, pos));
                    }
                    _ => return Some((Token::Equals, line, pos)),
                },
                '<' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::LessThanEqualsTo, line, pos));
                    }
                    Some(&'<') => {
                        self.char_stream.next();
                        self.advance();
                        return match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Some((Token::LeftShiftAssign, line, pos))
                            }
                            _ => {
                                self.char_stream.next();
                                self.advance();
                                Some((Token::LeftShift, line, pos))
                            }
                        };
                    }
                    _ => return Some((Token::LessThan, line, pos)),
                },
                '>' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::GreaterThanEqualsTo
                            }
                            Some(&'>') => {
                                self.char_stream.next();
                                self.advance();
                                match self.char_stream.peek() {
                                    Some(&'=') => {
                                        self.char_stream.next();
                                        self.advance();
                                        Token::RightShiftAssign
                                    }
                                    _ => {
                                        self.char_stream.next();
                                        self.advance();
                                        Token::RightShift
                                    }
                                }
                            }
                            _ => Token::GreaterThan,
                        },
                        line,
                        pos,
                    ))
                }
                '!' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::NotEqualsTo
                            }
                            _ => Token::Bang,
                        },
                        line,
                        pos,
                    ))
                }
                '|' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'|') => {
                                self.char_stream.next();
                                self.advance();
                                Token::Or
                            }
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::OrAssign
                            }
                            _ => Token::Pipe,
                        },
                        line,
                        pos,
                    ))
                }
                '&' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'&') => {
                                self.char_stream.next();
                                self.advance();
                                Token::And
                            }
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::AndAssign
                            }
                            _ => Token::Ampersand,
                        },
                        line,
                        pos,
                    ))
                }
                '^' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::XOrAssign
                            }
                            _ => Token::XOr,
                        },
                        line,
                        pos,
                    ))
                }
                '%' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::ModuloAssign
                            }
                            _ => Token::Modulo,
                        },
                        line,
                        pos,
                    ))
                }
                '~' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::PowerOfAssign
                            }
                            _ => Token::PowerOf,
                        },
                        line,
                        pos,
                    ))
                }
                x if x.is_whitespace() => (),
                x => return Some((Token::LexErr(LERR::UnexpectedChar(x)), line, pos)),
            }
        }

        None
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = (Token, usize, usize);

    // TODO - perhaps this could be optimized?
    fn next(&mut self) -> Option<Self::Item> {
        self.inner_next().map(|x| {
            self.last = x.0.clone();
            x
        })
    }
}

pub fn lex(input: &str) -> TokenIterator<'_> {
    TokenIterator {
        last: Token::LexErr(LERR::Nothing),
        line: 1,
        pos: 0,
        char_stream: input.chars().peekable(),
    }
}

fn get_precedence(token: &Token) -> i32 {
    match *token {
        Token::Equals
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::AndAssign
        | Token::OrAssign
        | Token::XOrAssign
        | Token::ModuloAssign
        | Token::PowerOfAssign => 10,
        Token::Or | Token::XOr | Token::Pipe => 11,
        Token::And | Token::Ampersand => 12,
        Token::LessThan
        | Token::LessThanEqualsTo
        | Token::GreaterThan
        | Token::GreaterThanEqualsTo
        | Token::EqualsTo
        | Token::NotEqualsTo => 15,
        Token::Plus | Token::Minus => 20,
        Token::Divide | Token::Multiply | Token::PowerOf => 40,
        Token::LeftShift | Token::RightShift => 50,
        Token::Modulo => 60,
        Token::Period => 100,
        _ => -1,
    }
}

fn parse_paren_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    match input.peek() {
        Some((Token::RightParen, _, _)) => {
            input.next();
            return Ok(Expr::Unit);
        }
        _ => (),
    }

    let expr = parse_expr(input)?;

    match input.next() {
        Some((Token::RightParen, _, _)) => Ok(expr),
        _ => Err(ParseError(PERR::MissingRightParen, 0, 0)),
    }
}

fn parse_call_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
) -> Result<Expr, ParseError> {
    let mut args = Vec::new();

    if let Some(&(Token::RightParen, _, _)) = input.peek() {
        input.next();
        return Ok(Expr::FunctionCall(id, args));
    }

    loop {
        match parse_expr(input) {
            Ok(arg) => args.push(arg),
            Err(mut err) => {
                err.0 = PERR::MalformedCallExpr;
                return Err(err);
            }
        }

        match input.peek() {
            Some(&(Token::RightParen, _, _)) => {
                input.next();
                return Ok(Expr::FunctionCall(id, args));
            }
            Some(&(Token::Comma, _, _)) => (),
            Some(&(_, line, pos)) => return Err(ParseError(PERR::MalformedCallExpr, line, pos)),
            None => return Err(ParseError(PERR::MalformedCallExpr, 0, 0)),
        }

        input.next();
    }
}

fn parse_index_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
) -> Result<Expr, ParseError> {
    match parse_expr(input) {
        Ok(idx) => match input.peek() {
            Some(&(Token::RightBracket, _, _)) => {
                input.next();
                return Ok(Expr::Index(id, Box::new(idx)));
            }
            Some(&(_, line, pos)) => return Err(ParseError(PERR::MalformedIndexExpr, line, pos)),
            None => return Err(ParseError(PERR::MalformedIndexExpr, 0, 0)),
        },
        Err(mut err) => {
            err.0 = PERR::MalformedIndexExpr;
            return Err(err);
        }
    }
}

fn parse_ident_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
) -> Result<Expr, ParseError> {
    match input.peek() {
        Some(&(Token::LeftParen, _, _)) => {
            input.next();
            parse_call_expr(id, input)
        }
        Some(&(Token::LeftBracket, _, _)) => {
            input.next();
            parse_index_expr(id, input)
        }
        _ => Ok(Expr::Identifier(id)),
    }
}

fn parse_array_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let mut arr = Vec::new();

    let skip_contents = match input.peek() {
        Some(&(Token::RightBracket, _, _)) => true,
        _ => false,
    };

    if !skip_contents {
        while let Some(_) = input.peek() {
            arr.push(parse_expr(input)?);
            if let Some(&(Token::Comma, _, _)) = input.peek() {
                input.next();
            }

            if let Some(&(Token::RightBracket, _, _)) = input.peek() {
                break;
            }
        }
    }

    match input.peek() {
        Some(&(Token::RightBracket, _, _)) => {
            input.next();
            Ok(Expr::Array(arr))
        }
        Some(&(_, line, pos)) => Err(ParseError(PERR::MissingRightBracket, line, pos)),
        None => Err(ParseError(PERR::MissingRightBracket, 0, 0)),
    }
}

fn parse_primary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    match input.next() {
        Some((token, line, pos)) => match token {
            Token::IntegerConstant(ref x) => Ok(Expr::IntegerConstant(*x)),
            Token::FloatConstant(ref x) => Ok(Expr::FloatConstant(*x)),
            Token::StringConst(ref s) => Ok(Expr::StringConstant(s.clone())),
            Token::CharConstant(ref c) => Ok(Expr::CharConstant(*c)),
            Token::Identifier(ref s) => parse_ident_expr(s.clone(), input),
            Token::LeftParen => parse_paren_expr(input),
            Token::LeftBracket => parse_array_expr(input),
            Token::True => Ok(Expr::True),
            Token::False => Ok(Expr::False),
            Token::LexErr(le) => Err(ParseError(PERR::BadInput(le.to_string()), line, pos)),
            _ => Err(ParseError(
                PERR::BadInput(format!("Unexpected {:?} token", token)),
                line,
                pos,
            )),
        },
        None => Err(ParseError(PERR::InputPastEndOfFile, 0, 0)),
    }
}

fn parse_unary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let token = match input.peek() {
        Some((tok, _, _)) => tok.clone(),
        None => return Err(ParseError(PERR::InputPastEndOfFile, 0, 0)),
    };

    match token {
        Token::UnaryMinus => {
            input.next();
            Ok(Expr::FunctionCall(
                "-".to_string(),
                vec![parse_primary(input)?],
            ))
        }
        Token::UnaryPlus => {
            input.next();
            parse_primary(input)
        }
        Token::Bang => {
            input.next();
            Ok(Expr::FunctionCall(
                "!".to_string(),
                vec![parse_primary(input)?],
            ))
        }
        _ => parse_primary(input),
    }
}

fn parse_binop<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    prec: i32,
    lhs: Expr,
) -> Result<Expr, ParseError> {
    let mut lhs_curr = lhs;

    loop {
        let mut curr_prec = -1;

        if let Some(&(ref curr_op, _, _)) = input.peek() {
            curr_prec = get_precedence(curr_op);
        }

        if curr_prec < prec {
            return Ok(lhs_curr);
        }

        if let Some((op_token, line, pos)) = input.next() {
            let mut rhs = parse_unary(input)?;

            let mut next_prec = -1;

            if let Some(&(ref next_op, _, _)) = input.peek() {
                next_prec = get_precedence(next_op);
            }

            if curr_prec < next_prec {
                rhs = parse_binop(input, curr_prec + 1, rhs)?;
            } else if curr_prec >= 100 {
                // Always bind right to left for precedence over 100
                rhs = parse_binop(input, curr_prec, rhs)?;
            }

            lhs_curr = match op_token {
                Token::Plus => Expr::FunctionCall("+".to_string(), vec![lhs_curr, rhs]),
                Token::Minus => Expr::FunctionCall("-".to_string(), vec![lhs_curr, rhs]),
                Token::Multiply => Expr::FunctionCall("*".to_string(), vec![lhs_curr, rhs]),
                Token::Divide => Expr::FunctionCall("/".to_string(), vec![lhs_curr, rhs]),
                Token::Equals => Expr::Assignment(Box::new(lhs_curr), Box::new(rhs)),
                Token::PlusAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("+".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::MinusAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("-".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::Period => Expr::Dot(Box::new(lhs_curr), Box::new(rhs)),
                Token::EqualsTo => Expr::FunctionCall("==".to_string(), vec![lhs_curr, rhs]),
                Token::NotEqualsTo => Expr::FunctionCall("!=".to_string(), vec![lhs_curr, rhs]),
                Token::LessThan => Expr::FunctionCall("<".to_string(), vec![lhs_curr, rhs]),
                Token::LessThanEqualsTo => {
                    Expr::FunctionCall("<=".to_string(), vec![lhs_curr, rhs])
                }
                Token::GreaterThan => Expr::FunctionCall(">".to_string(), vec![lhs_curr, rhs]),
                Token::GreaterThanEqualsTo => {
                    Expr::FunctionCall(">=".to_string(), vec![lhs_curr, rhs])
                }
                Token::Or => Expr::FunctionCall("||".to_string(), vec![lhs_curr, rhs]),
                Token::And => Expr::FunctionCall("&&".to_string(), vec![lhs_curr, rhs]),
                Token::XOr => Expr::FunctionCall("^".to_string(), vec![lhs_curr, rhs]),
                Token::OrAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("|".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::AndAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("&".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::XOrAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("^".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::MultiplyAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("*".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::DivideAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("/".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::Pipe => Expr::FunctionCall("|".to_string(), vec![lhs_curr, rhs]),
                Token::LeftShift => Expr::FunctionCall("<<".to_string(), vec![lhs_curr, rhs]),
                Token::RightShift => Expr::FunctionCall(">>".to_string(), vec![lhs_curr, rhs]),
                Token::LeftShiftAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("<<".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::RightShiftAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall(">>".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::Ampersand => Expr::FunctionCall("&".to_string(), vec![lhs_curr, rhs]),
                Token::Modulo => Expr::FunctionCall("%".to_string(), vec![lhs_curr, rhs]),
                Token::ModuloAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("%".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::PowerOf => Expr::FunctionCall("~".to_string(), vec![lhs_curr, rhs]),
                Token::PowerOfAssign => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FunctionCall("~".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                _ => return Err(ParseError(PERR::UnknownOperator, line, pos)),
            };
        }
    }
}

fn parse_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let lhs = parse_unary(input)?;
    parse_binop(input, 0, lhs)
}

fn parse_if<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = parse_expr(input)?;
    let body = parse_block(input)?;

    match input.peek() {
        Some(&(Token::Else, _, _)) => {
            input.next();
            let else_body = parse_block(input)?;
            Ok(Stmt::IfElse(
                Box::new(guard),
                Box::new(body),
                Box::new(else_body),
            ))
        }
        _ => Ok(Stmt::If(Box::new(guard), Box::new(body))),
    }
}

fn parse_while<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = parse_expr(input)?;
    let body = parse_block(input)?;

    Ok(Stmt::While(Box::new(guard), Box::new(body)))
}

fn parse_loop<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let body = parse_block(input)?;

    Ok(Stmt::Loop(Box::new(body)))
}

fn parse_for<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let name = match input.next() {
        Some((Token::Identifier(ref s), _, _)) => s.clone(),
        Some((token, line, pos)) => {
            return Err(ParseError(PERR::VarExpectsIdentifier(token), line, pos))
        }
        None => return Err(ParseError(PERR::VarExpectsIdentifier(Token::None), 0, 0)),
    };

    match input.next() {
        Some((Token::In, _, _)) => {}
        Some((token, line, pos)) => {
            return Err(ParseError(PERR::VarExpectsIdentifier(token), line, pos))
        }
        None => return Err(ParseError(PERR::VarExpectsIdentifier(Token::None), 0, 0)),
    }

    let expr = parse_expr(input)?;

    let body = parse_block(input)?;

    Ok(Stmt::For(name, Box::new(expr), Box::new(body)))
}

fn parse_var<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let name = match input.next() {
        Some((Token::Identifier(ref s), _, _)) => s.clone(),
        Some((token, line, pos)) => {
            return Err(ParseError(PERR::VarExpectsIdentifier(token), line, pos))
        }
        None => return Err(ParseError(PERR::VarExpectsIdentifier(Token::None), 0, 0)),
    };

    match input.peek() {
        Some(&(Token::Equals, _, _)) => {
            input.next();
            let initializer = parse_expr(input)?;
            Ok(Stmt::Let(name, Some(Box::new(initializer))))
        }
        _ => Ok(Stmt::Let(name, None)),
    }
}

fn parse_block<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(&(Token::LeftBrace, _, _)) => (),
        Some(&(_, line, pos)) => return Err(ParseError(PERR::MissingLeftBrace, line, pos)),
        None => return Err(ParseError(PERR::MissingLeftBrace, 0, 0)),
    }

    input.next();

    let mut stmts = Vec::new();

    let skip_body = match input.peek() {
        Some(&(Token::RightBrace, _, _)) => true,
        _ => false,
    };

    if !skip_body {
        while let Some(_) = input.peek() {
            stmts.push(parse_stmt(input)?);

            if let Some(&(Token::SemiColon, _, _)) = input.peek() {
                input.next();
            }

            if let Some(&(Token::RightBrace, _, _)) = input.peek() {
                break;
            }
        }
    }

    match input.peek() {
        Some(&(Token::RightBrace, _, _)) => {
            input.next();
            Ok(Stmt::Block(stmts))
        }
        Some(&(_, line, pos)) => Err(ParseError(PERR::MissingRightBrace, line, pos)),
        None => Err(ParseError(PERR::MissingRightBrace, 0, 0)),
    }
}

fn parse_expr_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    Ok(Stmt::Expr(Box::new(parse_expr(input)?)))
}

fn parse_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(&(Token::If, _, _)) => parse_if(input),
        Some(&(Token::While, _, _)) => parse_while(input),
        Some(&(Token::Loop, _, _)) => parse_loop(input),
        Some(&(Token::For, _, _)) => parse_for(input),
        Some(&(Token::Break, _, _)) => {
            input.next();
            Ok(Stmt::Break)
        }
        Some(&(Token::Return, _, _)) => {
            input.next();
            match input.peek() {
                Some(&(Token::SemiColon, _, _)) => Ok(Stmt::Return),
                _ => {
                    let ret = parse_expr(input)?;
                    Ok(Stmt::ReturnWithVal(Box::new(ret)))
                }
            }
        }
        Some(&(Token::LeftBrace, _, _)) => parse_block(input),
        Some(&(Token::Let, _, _)) => parse_var(input),
        _ => parse_expr_stmt(input),
    }
}

fn parse_fn<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<FnDef, ParseError> {
    input.next();

    let name = match input.next() {
        Some((Token::Identifier(ref s), _, _)) => s.clone(),
        Some((token, line, pos)) => return Err(ParseError(PERR::FnMissingName(token), line, pos)),
        None => return Err(ParseError(PERR::FnMissingName(Token::None), 0, 0)),
    };

    match input.peek() {
        Some(&(Token::LeftParen, _, _)) => {
            input.next();
        }
        Some(&(_, line, pos)) => return Err(ParseError(PERR::FnMissingParams, line, pos)),
        None => return Err(ParseError(PERR::FnMissingParams, 0, 0)),
    }

    let mut params = Vec::new();

    let skip_params = match input.peek() {
        Some(&(Token::RightParen, _, _)) => {
            input.next();
            true
        }
        _ => false,
    };

    if !skip_params {
        loop {
            match input.next() {
                Some((Token::RightParen, _, _)) => break,
                Some((Token::Comma, _, _)) => (),
                Some((Token::Identifier(ref s), _, _)) => {
                    params.push(s.clone());
                }
                Some((_, line, pos)) => return Err(ParseError(PERR::MalformedCallExpr, line, pos)),
                None => return Err(ParseError(PERR::MalformedCallExpr, 0, 0)),
            }
        }
    }

    let body = parse_block(input)?;

    Ok(FnDef {
        name: name,
        params: params,
        body: Box::new(body),
    })
}

fn parse_top_level<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<AST, ParseError> {
    let mut stmts = Vec::new();
    let mut fndefs = Vec::new();

    while let Some(_) = input.peek() {
        match input.peek() {
            Some(&(Token::Fn, _, _)) => fndefs.push(parse_fn(input)?),
            _ => stmts.push(parse_stmt(input)?),
        }

        if let Some(&(Token::SemiColon, _, _)) = input.peek() {
            input.next();
        }
    }

    Ok(AST(stmts, fndefs))
}

pub fn parse<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<AST, ParseError> {
    parse_top_level(input)
}
