use super::{channel::TokenChannel, error::ErrorType, token_type::TokenType};
use strum::EnumIs;

/// Macro arithmetic/logical expression has integer and float modes.
/// Float is only enabled in `%sysevalf`
#[derive(Debug, Clone, Copy)]
pub(super) enum MacroEvalNumericMode {
    Integer,
    Float,
}

/// Macro arithmetic/logical expressions may be in maro call
/// argument position (e.g. in `%scan`) or not (e.g. in `%if`).
/// And in argument position, they may be followed by a regular
/// macro argument (e.g. in `%scan`) or another expression (e.g. in `%SUBSTR`).
/// `None` implies that `,` is lexed as a macro string while
/// for other cases it is lexed as a terminator.
///
/// Luckily, if expression is followed by another expression, the
/// later one is always the last argument, so we do not need to define
/// an arbitrary depth of this.
#[derive(Debug, Clone, Copy)]
pub(super) enum MacroEvalNextArgumentMode {
    None,
    EvalExpr,
    MacroArg,
}

/// Packed flags for macro eval expressions (arithmetic/logical)
///
/// The following flags are packed into a single byte:
/// - Float mode (enabled in `%sysevalf`)
/// - Terminate on comma (enabled where eval context is for a macro call
///     argument)
/// - Terminate on statement (enabled for expr after `%if`, `%to`, etc.)
/// - Terminate on semicolon (enabled for expr after `%if`, `%by`, etc.)
///     `%if` is there despite semi being a session error, because
///     SAS will behave this way when trying to recover from error.
/// - Followed by another expression (enabled for first expr in `%SUBSTR`)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct MacroEvalExprFlags(u8);

impl MacroEvalExprFlags {
    const FLOAT_MODE_MASK: u8 = 0b0000_0001;
    const TERMINATE_ON_COMMA_MASK: u8 = 0b0000_0010;
    const TERMINATE_ON_STAT_MASK: u8 = 0b0000_0100;
    const TERMINATE_ON_SEMI_MASK: u8 = 0b0000_1000;
    const FOLLOWED_BY_EXPR_MASK: u8 = 0b0001_0000;

    pub(super) const fn new(
        numeric_mode: MacroEvalNumericMode,
        next_argument_mode: MacroEvalNextArgumentMode,
        terminate_on_stat: bool,
        terminate_on_semi: bool,
    ) -> Self {
        let mut bits = 0;
        if matches!(numeric_mode, MacroEvalNumericMode::Float) {
            bits |= Self::FLOAT_MODE_MASK;
        }
        match next_argument_mode {
            MacroEvalNextArgumentMode::None => {}
            MacroEvalNextArgumentMode::EvalExpr => {
                bits |= Self::TERMINATE_ON_COMMA_MASK;
                bits |= Self::FOLLOWED_BY_EXPR_MASK;
            }
            MacroEvalNextArgumentMode::MacroArg => {
                bits |= Self::TERMINATE_ON_COMMA_MASK;
            }
        }
        if terminate_on_stat {
            bits |= Self::TERMINATE_ON_STAT_MASK;
        }
        if terminate_on_semi {
            bits |= Self::TERMINATE_ON_SEMI_MASK;
        }
        Self(bits)
    }

    pub(super) const fn float_mode(self) -> bool {
        self.0 & Self::FLOAT_MODE_MASK != 0
    }

    pub(super) const fn terminate_on_comma(self) -> bool {
        self.0 & Self::TERMINATE_ON_COMMA_MASK != 0
    }

    pub(super) const fn terminate_on_stat(self) -> bool {
        self.0 & Self::TERMINATE_ON_STAT_MASK != 0
    }

    pub(super) const fn terminate_on_semi(self) -> bool {
        self.0 & Self::TERMINATE_ON_SEMI_MASK != 0
    }

    pub(super) const fn followed_by_expr(self) -> bool {
        self.0 & Self::FOLLOWED_BY_EXPR_MASK != 0
    }
}

/// The lexer mode
#[derive(Debug, Clone, PartialEq, Eq, Default, EnumIs)]
pub(crate) enum LexerMode {
    /// Default mode aka open code (non macro)
    #[default]
    Default,
    /// String expression, aka double quoted string mode.
    /// `bool` flag indicates if statement is allowed and should be lexed,
    /// which is a thing in open code, but not in macro expressions
    StringExpr { allow_stat: bool },
    /// Insignificant WS/comment space. E.g. between macro name and parens in a call
    /// this is the mode where we want to lex all consecutive whitespace and comments
    /// and then return to the previous mode
    WsOrCStyleCommentOnly,
    /// A special mode where only a specific non-letter char is expected.
    /// In this mode we also auto-recover if the expected character is not found
    /// emitting an error but also creating the expected token    
    ExpectSymbol(char, TokenType, TokenChannel),
    /// A common case where we expect a semicolon or EOF. Works like
    /// `ExpectSymbol` but with a special case for EOF
    ExpectSemiOrEOF,
    /// A special mode that goes after non-statement macro identifiers
    /// that checks if the first NON-ws or cstyle follower is (.
    /// If found, adds necessary mode stack to parse the macro call args.
    /// If not, perorms roll back, so that ws/cstyle comments can be
    /// relexed in different mode.
    ///
    /// Note - it should alwys be preceded by the `WsOrCStyleCommentOnly` mode
    /// and a checkpoint created!
    MaybeMacroCallArgs,
    /// Macro call argument or value mode. I.e. inside the parens of a macro call,
    /// before `=`.
    MacroCallArgOrValue {
        /// The current parenthesis nesting level.
        /// Macro arguments allow balanced parenthesis nesting and
        /// inside these parenthesis, `,` and `=` are not treated as
        /// terminators.
        pnl: u32,
    },
    /// Macro call value mode. I.e. inside the parens of a macro call,
    /// after `=` for user defined macros or the only valid mode for
    /// built-in macro calls, since they do not support named arguments.
    MacroCallValue {
        /// The bool value indicates whether next argument mode should be
        /// pushed on to the stack upon `,`. `false` is used in some
        /// built-in macro call lexing, where we pre-populate the stack
        /// due to special handling of the arguments as eval expressions.
        populate_next_arg_stack: bool,
        /// The current parenthesis nesting level.
        /// Macro arguments allow balanced parenthesis nesting and
        /// inside these parenthesis, `,` is not treated as
        /// terminators.
        pnl: u32,
    },
    /// The state for lexing inside an %str/%nrstr call. I.e. in `%str(-->1+1<--)`.
    MacroStrQuotedExpr {
        /// Boolean flag indicates if % and & are masked, i.e. this is %nrstr.
        mask_macro: bool,
        /// The current parenthesis nesting level.
        /// Macro arguments allow balanced parenthesis nesting and
        /// inside these parenthesis, `,` is not treated as
        /// terminators.
        pnl: u32,
    },
    /// Macro arithmetic/logical expression, as in `%eval(-->1+1<--)`, or `%if 1+1`    
    MacroEval {
        /// See `MacroEvalExprFlags` for the packed flags explanation
        macro_eval_flags: MacroEvalExprFlags,
        /// The current parenthesis nesting level.
        /// Macro arguments allow balanced parenthesis nesting and
        /// inside these parenthesis, `,` is not treated as
        /// terminators.
        pnl: u32,
    },
    /// Mode for dispatching various types of macro DO statements.
    /// Nothing is lexed in this mode, rather the stack is populated
    /// based on the lookahead.
    MacroDo,
    /// Mode for lexing right after %let/%local/%global/%do, where
    /// we expect a variable name expression. Boolean flag indicates if we
    /// have found at least one token of the variable name.
    /// `ErrorType` is used to supply relevant error message, if any is
    /// emitted by SAS if no name is found.
    MacroVarNameExpr(bool, Option<ErrorType>),
    /// Mode for lexing unrestricted macro text expressions terminated by semi.
    /// These are used for %let initializations, %put, etc.
    MacroSemiTerminatedTextExpr,
}
