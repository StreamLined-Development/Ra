const Allocator = std.mem.Allocator;
const std = @import("std");

pub const Token = struct {
    type: TokenType,
    text: []const u8,
    line: usize,
    column: usize,

    fn deinit(self: Token, allocator: Allocator) void {
        // Only free if we allocated (in this case, we don't)
        _ = self;
        _ = allocator;
    }
};

pub const TokenType = enum {
    identifier,

    // Literals
    lit_float,
    lit_int,
    lit_uint,
    lit_str,
    lit_char,
    lit_bool,

    // Keywords - Declarations
    kw_generic,
    kw_typealias,
    kw_union,
    kw_enum,
    kw_struct,
    kw_fun,
    kw_trait,
    kw_const,
    kw_var,
    kw_let,
    kw_macro,

    // keywords - comptime
    kw_comptime,
    kw_inline,
    kw_comp_error,
    kw_extern,

    // Keywords - memory manegement
    kw_delete,
    kw_new,
    kw_defer,
    kw_unsafe,

    // keywords - controll flow
    kw_match,
    kw_loop,
    kw_while,
    kw_for,
    kw_break,
    kw_continue,
    kw_return,
    kw_if,
    kw_else,
    kw_and,
    kw_or,
    kw_try,
    kw_ifNull,
    kw_ifErr,

    // keywords - random
    impl,
    kw_implement,
    kw_must,
    kw_where,
    kw_pub,
    kw_priv,
    kw_mut,

    // keywords Concurrency
    kw_process,
    kw_message,
    kw_await,
    kw_spawn,
    kw_mailbox,

    // keywords - import
    kw_import,
    kw_as,
    kw_in,

    // Keywords - Assembly
    kw_asm,
    kw_assembly,
    kw_clobbers,
    kw_inputs,
    kw_outputs,

    // Keywords - Types
    kw_type,
    kw_strA, // ASCII
    kw_str, //UTF-8
    kw_str16, //UTF-16
    kw_str32, //UTF-32
    kw_isize,
    kw_usize,
    kw_i8,
    kw_i16,
    kw_i32,
    kw_i64,
    kw_i128,
    kw_u8,
    kw_u16,
    kw_u32,
    kw_u64,
    kw_u128,
    kw_f16,
    kw_f32,
    kw_f64,
    kw_f128,
    kw_bool,
    kw_char,
    kw_stringA,
    kw_string,
    kw_string16,
    kw_string32,
    kw_array,
    kw_map,
    kw_vec,

    // Operators - Arithmetic
    plus, // +
    minus, // -
    multiply, // *
    divide, // /
    modulo, // %
    power, // **
    plus_plus, // ++
    minus_minus, // --

    // Operators - Bitwise
    bit_and, // &
    bit_or, // |
    bit_xor, // ^
    bit_not, // ~, !
    left_shift, // <<
    right_shift, // >>

    // Operators - Comparison
    equal,
    not_equal,
    less,
    less_equal,
    greater,
    greater_equal,

    // Operators - Other
    assign,
    arrow, // ->
    fat_arrow, // =>
    incl_range, // ...
    excl_range, // ..
    pipeline, // |
    piping, // |>

    // Punctuation
    semicolon,
    double_colon,
    colon,
    comma,
    dot,
    question_mark,
    exclamation,
    at_sign, // @
    hash,
    dollar,
    ampersand,
    pipe,
    caret, // ^
    backslash,
    forward_slash,
    triple_dot,
    double_dot,
    double_quotes,
    quotes,
    underscore, // _

    // Delimiters - Brackets
    left_bracket,
    right_bracket,
    left_paren,
    right_paren,
    left_brace,
    right_brace,

    // Special
    eof,
    Error,
    unknown,
};
