from enum import IntEnum


class ErrorKind(IntEnum):
    UNTERMINATED_STRING_LITERAL = 1001
    UNTERMINATED_COMMENT = 1002
    UNTERMINATED_DATALINES = 1003
    INVALID_NUMERIC_LITERAL = 1004
    UNTERMINATED_HEX_NUMERIC_LITERAL = 1005
    MISSING_EXPECTED_R_PAREN = 1007
    MISSING_EXPECTED_ASSIGN = 1008
    MISSING_EXPECTED_L_PAREN = 1009
    MISSING_EXPECTED_COMMA = 1010
    MISSING_EXPECTED_F_SLASH = 1011
    MISSING_EXPECTED_SEMI_OR_EOF = 1012
    INVALID_OR_OUT_OF_ORDER_STATEMENT = 1013
    INVALID_MACRO_LET_VAR_NAME = 1014
    INVALID_MACRO_LOCAL_GLOBAL_READONLY_VAR_NAME = 1015
    MISSING_MACRO_LOCAL_READONLY_KW = 1016
    MISSING_MACRO_GLOBAL_READONLY_KW = 1017
    INVALID_MACRO_DEF_NAME = 1018
    INVALID_MACRO_DEF_ARG_NAME = 1019
    UNEXPECTED_SEMI_IN_DO_LOOP = 1020
    OPEN_CODE_RECURSION_ERROR = 1021
    INVALID_HEX_STRING_CONSTANT = 1022
    MISSING_SYSFUNC_FUNC_NAME = 1023
    MISSING_SYSCALL_ROUTINE_NAME = 1024
    TOKEN_IDX_OUT_OF_BOUNDS = 2001
    STRING_LITERAL_OUT_OF_BOUNDS = 2002
    FILE_TOO_LARGE = 3001
    INTERNAL_ERROR_MISSING_CHECKPOINT = 9001
    INTERNAL_ERROR_NO_TOKEN_TEXT = 9002
    INTERNAL_ERROR_OUT_OF_BOUNDS = 9003
    INTERNAL_ERROR_EMPTY_MODE_STACK = 9004
    INTERNAL_ERROR_NO_TOKEN_TO_REPLACE = 9005
    INTERNAL_ERROR_UNEXPECTED_TOKEN_TYPE = 9006
    INTERNAL_ERROR_UNEXPECTED_MODE_STACK = 9007
    INTERNAL_ERROR_INFINITE_LOOP = 9008
    INTERNAL_ERROR_EMPTY_PENDING_STAT_STACK = 9009


ERROR_MESSAGE: dict[ErrorKind, str] = {
    ErrorKind.UNTERMINATED_STRING_LITERAL: "Unterminated string literal",
    ErrorKind.UNTERMINATED_COMMENT: "Unterminated comment",
    ErrorKind.UNTERMINATED_DATALINES: "Unterminated datalines",
    ErrorKind.INVALID_NUMERIC_LITERAL: "Invalid numeric literal",
    ErrorKind.UNTERMINATED_HEX_NUMERIC_LITERAL: "Missing `x` at the end of a hex numeric literal",
    ErrorKind.MISSING_EXPECTED_R_PAREN: "Missing expected character: ')'",
    ErrorKind.MISSING_EXPECTED_ASSIGN: "Missing expected character: '='",
    ErrorKind.MISSING_EXPECTED_L_PAREN: "Missing expected character: '('",
    ErrorKind.MISSING_EXPECTED_COMMA: "Missing expected character: ','",
    ErrorKind.MISSING_EXPECTED_F_SLASH: "Missing expected character: '/'",
    ErrorKind.MISSING_EXPECTED_SEMI_OR_EOF: "Missing expected character: ';' or end of file",
    ErrorKind.INVALID_OR_OUT_OF_ORDER_STATEMENT: "ERROR 180-322: Statement is not valid or it is used out of proper order.",
    ErrorKind.INVALID_MACRO_LET_VAR_NAME: "ERROR: Expecting a variable name after %LET.",
    ErrorKind.INVALID_MACRO_LOCAL_GLOBAL_READONLY_VAR_NAME: "ERROR: The macro variable name is either all blank or missing.",
    ErrorKind.MISSING_MACRO_LOCAL_READONLY_KW: "ERROR: Unrecognized keyword on %LOCAL statement.",
    ErrorKind.MISSING_MACRO_GLOBAL_READONLY_KW: "ERROR: Unrecognized keyword on %GLOBAL statement.",
    ErrorKind.INVALID_MACRO_DEF_NAME: "ERROR: Invalid macro name.  It should be a valid SAS identifier no longer than 32 characters.\nERROR: A dummy macro will be compiled.",
    ErrorKind.INVALID_MACRO_DEF_ARG_NAME: "ERROR: Invalid macro parameter name. It should be a valid SAS identifier no longer than 32 characters.\nERROR: A dummy macro will be compiled.",
    ErrorKind.UNEXPECTED_SEMI_IN_DO_LOOP: "ERROR: An unexpected semicolon occurred in the %DO statement.\nERROR: A dummy macro will be compiled.",
    ErrorKind.OPEN_CODE_RECURSION_ERROR: "ERROR: Open code statement recursion detected.",
    ErrorKind.INVALID_HEX_STRING_CONSTANT: "Invalid hex string constant.",
    ErrorKind.MISSING_SYSFUNC_FUNC_NAME: "ERROR: Function name missing in %SYSFUNC or %QSYSFUNC macro function reference.",
    ErrorKind.MISSING_SYSCALL_ROUTINE_NAME: "ERROR: CALL routine name missing in %SYSCALL macro statement.",
    ErrorKind.TOKEN_IDX_OUT_OF_BOUNDS: "Requested token index out of bounds",
    ErrorKind.STRING_LITERAL_OUT_OF_BOUNDS: "String literal range out of bounds",
    ErrorKind.FILE_TOO_LARGE: "Lexing of files larger than 4GB is not supported",
    ErrorKind.INTERNAL_ERROR_MISSING_CHECKPOINT: "No checkpoint to rollback",
    ErrorKind.INTERNAL_ERROR_NO_TOKEN_TEXT: "No token text",
    ErrorKind.INTERNAL_ERROR_OUT_OF_BOUNDS: "Internal out of bounds request",
    ErrorKind.INTERNAL_ERROR_EMPTY_MODE_STACK: "Empty mode stack",
    ErrorKind.INTERNAL_ERROR_NO_TOKEN_TO_REPLACE: "No token to replace",
    ErrorKind.INTERNAL_ERROR_UNEXPECTED_TOKEN_TYPE: "Unexpected token type",
    ErrorKind.INTERNAL_ERROR_UNEXPECTED_MODE_STACK: "Unexpected mode stack",
    ErrorKind.INTERNAL_ERROR_INFINITE_LOOP: "Infinite loop detected",
    ErrorKind.INTERNAL_ERROR_EMPTY_PENDING_STAT_STACK: "Empty pending stat stack",
}


def is_internal_error(error_kind: ErrorKind) -> bool:
    return error_kind in range(9000, 10000)


def is_code_error(error_kind: ErrorKind) -> bool:
    return error_kind in range(1000, 2000)


def is_warning(error_kind: ErrorKind) -> bool:
    return error_kind in range(4000, 5000)
