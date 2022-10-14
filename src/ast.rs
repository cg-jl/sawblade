// Note: I'm building a fast parser that
// doesn't give a fuck about spans.

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Block<'a> {
    pub name: LinkageLabel<'a>,
    pub spec: Option<Spec<'a>>,
    pub arguments: Option<Vec<&'a str>>, // no ignored arguments!
    pub stmts: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Spec<'a> {
    pub arguments: Option<Vec<&'a str>>,
    pub returns: Option<Vec<&'a str>>,
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum LinkageLabel<'a> {
    Export(&'a str),
    Internal(&'a str),
}

impl<'a> LinkageLabel<'a> {
    pub const fn name(self) -> &'a str {
        match self {
            LinkageLabel::Export(name) | LinkageLabel::Internal(name) => name,
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Statement<'a> {
    Assign {
        bindings: Vec<Lvalue<'a>>,
        value: Expr<'a>,
    },
    Return(Expr<'a>),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Expr<'a> {
    Insn {
        name: &'a str,
        args: Vec<Rvalue<'a>>,
    },
    Copied(Vec<Rvalue<'a>>),
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Lvalue<'a> {
    Ignore,
    Named(&'a str),
}

impl<'a> Lvalue<'a> {
    #[inline]
    pub const fn name_if_not_ignored(&self) -> Option<&'a str> {
        match self {
            Self::Ignore => None,
            Self::Named(name) => Some(name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Rvalue<'a> {
    Label(&'a str),
    Constant(u64),
    Binding(&'a str),
}

#[derive(Debug)]
enum LRvalue<'a> {
    Ignore,
    Constant(u64),
    Binding(&'a str),
    Label(&'a str),
}

impl<'a> LRvalue<'a> {
    const fn try_as_rvalue(self) -> Option<Rvalue<'a>> {
        match self {
            Self::Constant(v) => Some(Rvalue::Constant(v)),
            Self::Binding(v) => Some(Rvalue::Binding(v)),
            Self::Label(l) => Some(Rvalue::Label(l)),
            Self::Ignore => None,
        }
    }

    const fn try_as_lvalue(self) -> Option<Lvalue<'a>> {
        match self {
            Self::Ignore => Some(Lvalue::Ignore),
            Self::Binding(v) => Some(Lvalue::Named(v)),
            _ => None,
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
}

fn is_delim(ch: char) -> bool {
    ch.is_whitespace() || "()[]{}\";=".contains(ch)
}

impl<'a> Parser<'a> {
    const fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    fn current(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn accept(&mut self) {
        if let Some(c) = self.current() {
            self.offset += c.len_utf8();
        }
    }

    fn lex_name_end(&mut self) -> &'a str {
        let start = self.offset;
        while self.current().is_some_and(|c| !is_delim(c)) {
            self.accept();
        }
        &self.input[start..self.offset]
    }

    fn lex_insn_name(&mut self) -> Option<&'a str> {
        if self.current().is_some_and(|c| c.is_ascii_alphabetic()) {
            Some(self.lex_name_end())
        } else {
            None
        }
    }

    fn lex_name_end_nonempty(&mut self) -> Option<&'a str> {
        let name = self.lex_name_end();
        if name.is_empty() {
            None
        } else {
            Some(name)
        }
    }

    fn parse_constant(&mut self) -> Option<u64> {
        let mut value = 0u64;

        while let Some(c) = self.current().filter(|c| c.is_ascii_digit()) {
            let digit = (c as u8) - b'0';
            value = value.checked_mul(10)?.checked_add(digit as u64)?;
            self.accept();
        }

        Some(value)
    }

    fn parse_lrvalue(&mut self) -> Option<LRvalue<'a>> {
        match self.current()? {
            '_' => {
                self.accept();
                if self.current().is_some_and(|ch| is_delim(ch)) {
                    Some(LRvalue::Ignore)
                } else {
                    None
                }
            }
            '%' => {
                self.accept();
                self.lex_name_end_nonempty().map(LRvalue::Binding)
            }
            '@' => {
                self.accept();
                self.lex_name_end_nonempty().map(LRvalue::Label)
            }
            ch if ch.is_ascii_digit() => self.parse_constant().map(LRvalue::Constant),
            _ => None,
        }
    }

    fn parse_rvalue(&mut self) -> Option<Rvalue<'a>> {
        self.parse_lrvalue().and_then(LRvalue::try_as_rvalue)
    }

    fn current_is_whitespace(&self) -> bool {
        self.current().is_some_and(|ch| ch.is_whitespace())
    }

    fn whitespace(&mut self) {
        while self.current_is_whitespace() {
            self.accept();
        }
    }

    fn parse_linkage(&mut self) -> Option<LinkageLabel<'a>> {
        match self.current()? {
            '%' => {
                self.accept();
                self.lex_name_end_nonempty().map(LinkageLabel::Internal)
            }
            '"' => {
                self.accept();
                let name = self.lex_name_end_nonempty()?;
                if !self.current().is_some_and(|ch| ch == '"') {
                    None
                } else {
                    self.accept();
                    Some(LinkageLabel::Export(name))
                }
            }
            _ => None,
        }
    }

    fn current_input(&self) -> &'a str {
        &self.input[self.offset..]
    }

    fn parse_name_list(&mut self) -> Option<Vec<&'a str>> {
        if self.current()? != '[' {
            return None;
        }
        self.accept();

        let mut values = Vec::new();

        loop {
            self.whitespace();
            if let Some(next) = self.lex_name_end_nonempty() {
                values.push(next);
                if self.current_is_whitespace() {
                    continue;
                }
            }
            break;
        }

        if self.current()? != ']' {
            None
        } else {
            self.accept();
            Some(values)
        }
    }

    // assumes first '{' was consumed
    fn parse_spec(&mut self) -> Option<Spec<'a>> {
        let mut returns = None;
        let mut arguments = None;
        loop {
            self.whitespace();
            if self.current_input().starts_with("return") {
                self.offset += "return".len();
                self.whitespace();
                returns = Some(self.parse_name_list()?);
                continue;
            }
            if self.current_input().starts_with("arguments") {
                self.offset += "arguments".len();
                self.whitespace();
                arguments = Some(self.parse_name_list()?);
                continue;
            }
            break;
        }

        if self.current()? != '}' {
            None
        } else {
            self.accept();
            Some(Spec { returns, arguments })
        }
    }

    fn parse_argument_list(&mut self) -> Option<Vec<&'a str>> {
        let mut args = Vec::new();
        loop {
            self.whitespace();
            if self.current() == Some('%') {
                self.accept();
                args.push(self.lex_name_end_nonempty()?);
                if self.current_is_whitespace() {
                    continue;
                }
            }
            break;
        }

        if self.current()? != ')' {
            None
        } else {
            self.accept();
            Some(args)
        }
    }

    fn collect_rvalues(&mut self) -> Option<Vec<Rvalue<'a>>> {
        let mut args = Vec::new();
        loop {
            self.whitespace();
            if let Some(arg) = self.parse_rvalue() {
                args.push(arg);
                if self.current_is_whitespace() {
                    continue;
                }
            }
            break;
        }
        if args.is_empty() {
            None
        } else {
            Some(args)
        }
    }

    fn parse_expr(&mut self) -> Option<Expr<'a>> {
        let opt_insn = self.lex_insn_name();
        self.whitespace();
        self.collect_rvalues().map(move |args| {
            if let Some(name) = opt_insn {
                Expr::Insn { name, args }
            } else {
                Expr::Copied(args)
            }
        })
    }

    fn parse_statement(&mut self) -> Option<Statement<'a>> {
        // try to recognize a return by instruction
        if let Some(name) = self.lex_insn_name() {
            let args = self.collect_rvalues()?;
            Some(Statement::Return(Expr::Insn { name, args }))
        } else {
            let mut lrvalues = Vec::new();
            loop {
                self.whitespace();
                if let Some(next) = self.parse_lrvalue() {
                    lrvalues.push(next);
                    if self.current_is_whitespace() {
                        continue;
                    }
                }
                break;
            }

            // if we didn't catch any lrvalues then we have an empty statement.
            if lrvalues.is_empty() {
                return None;
            }

            // check if we're at an assignment
            if self.current() == Some('=') {
                self.accept();
                self.whitespace();
                // all of the lrvalues were lvalues!
                let mut bindings = Vec::with_capacity(lrvalues.len());
                for lrvalue in lrvalues {
                    bindings.push(lrvalue.try_as_lvalue()?);
                }

                self.parse_expr()
                    .map(move |value| Statement::Assign { bindings, value })
            } else {
                // all of the lrvalues were rvalues!
                let mut args = Vec::with_capacity(lrvalues.len());
                for lrvalue in lrvalues {
                    args.push(lrvalue.try_as_rvalue()?);
                }

                Some(Statement::Return(Expr::Copied(args)))
            }
        }
    }

    fn whitespace_track_newline(&mut self) -> bool {
        let mut had_newline = false;
        while self.current_is_whitespace() {
            // SAFE: current is whitespace returns true only when
            // there's a Some.
            had_newline |= unsafe { self.current().unwrap_unchecked() } == '\n';
            self.accept();
        }

        had_newline
    }

    fn parse_block(&mut self) -> Option<Block<'a>> {
        self.whitespace();
        if !self.current_input().starts_with("block") {
            return None;
        }

        self.offset += "block".len();
        self.whitespace();
        let linkage = self.parse_linkage()?;
        self.whitespace();
        let (spec, arguments) = if self.current_input().starts_with("::") {
            self.offset += "::".len();
            self.whitespace();
            let spec = if self.current() == Some('{') {
                self.accept();
                let spec = self.parse_spec()?;
                self.whitespace();
                Some(spec)
            } else {
                None
            };

            let arguments = if self.current() == Some('(') {
                self.accept();
                let args = self.parse_argument_list()?;
                self.whitespace();
                Some(args)
            } else {
                None
            };

            (spec, arguments)
        } else {
            (None, None)
        };

        // parse statement list
        if self.current()? != '{' {
            return None;
        }

        self.accept();

        let mut stmts = Vec::new();

        loop {
            self.whitespace();
            if let Some(next) = self.parse_statement() {
                stmts.push(next);

                let had_newline = self.whitespace_track_newline();
                if self.current() == Some(';') {
                    self.accept();
                    continue;
                } else if had_newline {
                    continue;
                }
            }
            break;
        }

        if self.current() != Some('}') {
            None
        } else {
            self.accept();
            Some(Block {
                name: linkage,
                spec,
                arguments,
                stmts,
            })
        }
    }
}

pub fn parse_source(input: &str) -> Vec<Block> {
    let mut parser = Parser::new(input);
    let mut blocks = Vec::new();
    while let Some(block) = parser.parse_block() {
        blocks.push(block);
    }
    blocks
}
