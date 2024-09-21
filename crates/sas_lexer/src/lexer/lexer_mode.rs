use super::{channel::TokenChannel, error::ErrorType, token_type::TokenType};
use strum::EnumIs;
#[cfg(test)]
use strum::EnumIter;

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

/// The context of the macro argmunet/value context.
///
/// - `BuiltIn` for built-in macros like `%scan`
/// - `MacroCall` for user defined macros and built-ins that allow named arguments
/// - `MacroDef` for macro definitions
#[derive(Debug, Clone, Copy)]
#[cfg_attr(test, derive(EnumIter, PartialEq, Eq))]
#[repr(u8)]
pub(super) enum MacroArgContext {
    /// Built-in macro call
    BuiltInMacro,
    /// User defined macro call or built-in macro that allows named arguments
    MacroCall,
    /// Macro definition
    MacroDef,
}

const MACRO_CALL_CONTEXT: u8 = MacroArgContext::MacroCall as u8;
const MACRO_DEF_CONTEXT: u8 = MacroArgContext::MacroDef as u8;

/// Packed flags for macro call argument name or value.
///
/// The following flags are packed into a single byte:
/// - The type of the macro call. Built-ins do not support named arguments.
/// - Whether to auto populate next argument stack or leave
///    it to the caller to do so.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct MacroArgNameValueFlags(u8);

impl MacroArgNameValueFlags {
    const CONTEXT_MASK: u8 = 0b0000_0011;
    const POPULATE_NEXT_ARG_STACK_MASK: u8 = 0b0000_0100;

    pub(super) const fn new(context: MacroArgContext, populate_next_arg_stack: bool) -> Self {
        let mut bits = context as u8;

        if populate_next_arg_stack {
            bits |= Self::POPULATE_NEXT_ARG_STACK_MASK;
        }
        Self(bits)
    }

    pub(super) const fn context(self) -> MacroArgContext {
        match self.0 & Self::CONTEXT_MASK {
            MACRO_CALL_CONTEXT => MacroArgContext::MacroCall,
            MACRO_DEF_CONTEXT => MacroArgContext::MacroDef,
            _ => MacroArgContext::BuiltInMacro,
        }
    }

    pub(super) const fn populate_next_arg_stack(self) -> bool {
        self.0 & Self::POPULATE_NEXT_ARG_STACK_MASK != 0
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
    ExpectSymbol(TokenType, TokenChannel),
    /// A common case where we expect a semicolon or EOF. Works like
    /// `ExpectSymbol` but with a special case for EOF
    ExpectSemiOrEOF,
    /// A special mode that goes after non-statement macro identifiers
    /// and any trailing whitespace or cstyle comments. It is a mode
    /// that checks if the first NON-ws or cstyle follower is (.
    /// If found, adds necessary mode stack to parse the macro call args.
    /// If not, performs roll back, so that ws/cstyle comments can be
    /// relexed in different mode.
    ///
    /// Note - it should alwys be preceded by the `WsOrCStyleCommentOnly` mode
    /// and a checkpoint created!
    MaybeMacroCallArgs,
    /// Macro call argument or value mode. I.e. inside the parens of a macro call,
    /// before `=`.
    MacroCallArgOrValue {
        /// The packed flags for macro argument name or value. See `MacroArgNameValueFlags`
        flags: MacroArgNameValueFlags,
        /// The current parenthesis nesting level.
        /// Macro arguments allow balanced parenthesis nesting and
        /// inside these parenthesis, `,` and `=` are not treated as
        /// terminators.
        pnl: u32,
    },
    /// A special mode that goes after `%macro name` and any trailing
    /// whitespace or cstyle comments.
    /// It checks if the first NON-ws or cstyle follower is `(`.
    /// If found, adds necessary mode stack to parse the macro def args.
    ///
    /// Note - it should always be preceded by the `WsOrCStyleCommentOnly` mode
    /// as it literally checks the `next_char` only.    
    MaybeMacroDefArgs,
    /// Macro def argument. I.e. inside the parens of a macro definition,
    /// before an optional `=`. It reads the argument name, optional `=`
    /// and populates the following mode stack as necessary until all
    /// arguments are read.
    MacroDefArg,
    /// A special mode that goes after argument name in macro def argument
    /// list. It checks if the next non-ws or cstyle follower is `=`, ','
    /// or something else.
    /// If `=` found, adds necessary mode stack to parse the default value.
    /// If `,` found, adds necessary mode stack to parse the next argument.
    ///
    /// Note - it should always be preceded by the `WsOrCStyleCommentOnly` mode
    /// as it literally checks the `next_char` only.
    MacroDefNextArgOrDefaultValue,
    /// A mode for lexing identifiers in macro definitions. Both the name of the
    /// macro and argument names. In this mode ascii only identifiers are allowed
    /// and a SAS error is emitted if no identifier is found.
    MacroDefName,
    /// Macro call value mode. I.e. inside the parens of a macro call,
    /// after `=` for user defined macros or the only valid mode for
    /// built-in macro calls, since they do not support named arguments.
    MacroCallValue {
        /// The packed flags for macro argument name or value. See `MacroArgNameValueFlags`
        flags: MacroArgNameValueFlags,
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
    /// Mode for dispatching two types of macro `%local`/`%global` statements.
    /// The common case is to move into `MacroStatOptionsTextExpr` mode,
    /// but these statements also have a `/ readonly` variations which
    /// with the tale performing exactly as `%let`.
    MacroLocalGlobal {
        /// Boolean flag indicates if this is a local or global statement.
        is_local: bool,
    },
    /// Mode for lexing right after `%let`/`%local`/`%global`/`%do`, where
    /// we expect a variable name expression. Boolean flag indicates if we
    /// have found at least one token of the variable name.
    /// `ErrorType` is used to supply relevant error message, if any is
    /// emitted by SAS if no name is found.
    MacroVarNameExpr(bool, Option<ErrorType>),
    /// Mode for lexing unrestricted macro text expressions terminated by semi.
    /// These are used for `%let` initializations, `%put`, etc.
    MacroSemiTerminatedTextExpr,
    /// Mode for lexing text expressions where WS is a delimiter and `=` is
    /// lexed as an `assign` token. Also semi terminated.
    /// It sued both with macro statement options following the `/` in `%copy`, `%macro`,
    /// and for valists in `%global`, `%local`, `%input` and others.
    MacroStatOptionsTextExpr,
}

#[cfg(test)]
mod tests {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn test_macro_eval_expr_flags() {
        let flags = MacroEvalExprFlags::new(
            MacroEvalNumericMode::Float,
            MacroEvalNextArgumentMode::EvalExpr,
            true,
            true,
        );
        assert!(flags.float_mode());
        assert!(flags.terminate_on_comma());
        assert!(flags.terminate_on_stat());
        assert!(flags.terminate_on_semi());
        assert!(flags.followed_by_expr());

        let flags = MacroEvalExprFlags::new(
            MacroEvalNumericMode::Integer,
            MacroEvalNextArgumentMode::None,
            false,
            false,
        );
        assert!(!flags.float_mode());
        assert!(!flags.terminate_on_comma());
        assert!(!flags.terminate_on_stat());
        assert!(!flags.terminate_on_semi());
        assert!(!flags.followed_by_expr());

        let flags = MacroEvalExprFlags::new(
            MacroEvalNumericMode::Integer,
            MacroEvalNextArgumentMode::MacroArg,
            false,
            true,
        );
        assert!(!flags.float_mode());
        assert!(flags.terminate_on_comma());
        assert!(!flags.terminate_on_stat());
        assert!(flags.terminate_on_semi());
        assert!(!flags.followed_by_expr());
    }

    #[test]
    fn test_macro_arg_name_value_flags() {
        for context in MacroArgContext::iter() {
            for populate_next_arg_stack in [true, false].iter() {
                let flags = MacroArgNameValueFlags::new(context, *populate_next_arg_stack);
                assert_eq!(flags.context(), context);
                assert_eq!(flags.populate_next_arg_stack(), *populate_next_arg_stack);
            }
        }
    }

    #[test]
    fn test_lexer_mode_default() {
        assert_eq!(LexerMode::default(), LexerMode::Default);
    }
}
