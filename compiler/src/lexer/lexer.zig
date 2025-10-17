const std = @import("std");
const TokenType = @import("tokens.zig").TokenType;
const Token = @import("tokens.zig").Token;
const Allocator = std.mem.Allocator;

const Lexer = struct {
    input: []const u8,
    position: usize, // Index of input str
    line: usize, // line of code
    column: usize, // char in line

    fn init(input: []const u8) Lexer {
        return Lexer{
            .input = input,
            .position = 0,
            .line = 1,
            .column = 1,
        };
    }

    fn current(self: Lexer) ?u8 {
        if (self.position < self.input.len) {
            return self.input[self.position];
        }
        return null;
    }

    fn advance(self: *Lexer) ?u8 {
        if (self.position >= self.input.len) return null;

        const ch = self.input[self.position];
        self.position += 1;

        if (ch == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        return ch;
    }

    fn skip_whitespace(self: *Lexer) void {
        while (self.current()) |ch| {
            if (std.ascii.isWhitespace(ch)) {
                _ = self.advance();
            } else {
                break;
            }
        }
    }

    fn make_token(self: Lexer, token_type: TokenType, start_pos: usize) Token {
        return Token{
            .type = token_type,
            .text = self.input[start_pos..self.position], // Zero-copy slice!
            .line = self.line,
            .column = self.column,
        };
    }

    fn read_number(self: *Lexer) Token {
        const start_pos = self.position;
        var has_dot = false;

        while (self.current()) |ch| {
            if (std.ascii.isDigit(ch)) {
                _ = self.advance();
            } else if (ch == '.' and !has_dot) {
                // Check if this is a range operator (.. or ...) rather than a decimal point
                if (self.position + 1 < self.input.len and self.input[self.position + 1] == '.') {
                    // This is a range operator, don't consume the dot
                    break;
                }
                // Check if next character after dot is a digit (valid float)
                if (self.position + 1 < self.input.len and std.ascii.isDigit(self.input[self.position + 1])) {
                    has_dot = true;
                    _ = self.advance();
                } else {
                    // Not a valid float, stop parsing
                    break;
                }
            } else {
                break;
            }
        }

        return self.make_token(if (has_dot) .lit_float else .lit_int, start_pos);
    }

    fn read_identifier(self: *Lexer) Token {
        const start_pos = self.position;
        while (self.current()) |ch| {
            if (std.ascii.isAlphanumeric(ch) or ch == '_') {
                _ = self.advance();
            } else {
                break;
            }
        }

        // Check for keywords
        const text = self.input[start_pos..self.position];
        if (std.mem.eql(u8, text, "const")) {
            return self.make_token(.kw_const, start_pos);
        } else if (std.mem.eql(u8, text, "let")) {
            return self.make_token(.kw_let, start_pos);
        } else if (std.mem.eql(u8, text, "var")) {
            return self.make_token(.kw_var, start_pos);
        } else if (std.mem.eql(u8, text, "if")) {
            return self.make_token(.kw_if, start_pos);
        } else if (std.mem.eql(u8, text, "else")) {
            return self.make_token(.kw_else, start_pos);
        } else if (std.mem.eql(u8, text, "while")) {
            return self.make_token(.kw_while, start_pos);
        } else if (std.mem.eql(u8, text, "for")) {
            return self.make_token(.kw_for, start_pos);
        } else if (std.mem.eql(u8, text, "loop")) {
            return self.make_token(.kw_loop, start_pos);
        } else if (std.mem.eql(u8, text, "inline")) {
            return self.make_token(.kw_inline, start_pos);
        } else if (std.mem.eql(u8, text, "return")) {
            return self.make_token(.kw_return, start_pos);
        } else if (std.mem.eql(u8, text, "func")) {
            return self.make_token(.kw_func, start_pos);
        } else if (std.mem.eql(u8, text, "fum")) {
            return self.make_token(.kw_fum, start_pos);
        } else if (std.mem.eql(u8, text, "process")) {
            return self.make_token(.kw_process, start_pos);
        } else if (std.mem.eql(u8, text, "message")) {
            return self.make_token(.kw_message, start_pos);
        } else if (std.mem.eql(u8, text, "impl")) {
            return self.make_token(.impl, start_pos);
        } else if (std.mem.eql(u8, text, "implement")) {
            return self.make_token(.kw_implement, start_pos);

        } else if (std.mem.eql(u8, text, "try")) {
            return self.make_token(.kw_try, start_pos);
        } else if (std.mem.eql(u8, text, "ifNull")) {
            return self.make_token(.kw_ifNull, start_pos);
        } else if (std.mem.eql(u8, text, "ifErr")) {
            return self.make_token(.kw_ifErr, start_pos);
        } else if (std.mem.eql(u8, text, "spawn")) {
            return self.make_token(.kw_spawn, start_pos);
        } else if (std.mem.eql(u8, text, "bool")) {
            return self.make_token(.kw_bool, start_pos);
        } else if (std.mem.eql(u8, text, "i8")) {
            return self.make_token(.kw_i8, start_pos);
        } else if (std.mem.eql(u8, text, "i16")) {
            return self.make_token(.kw_i16, start_pos);
        } else if (std.mem.eql(u8, text, "i32")) {
            return self.make_token(.kw_i32, start_pos);
        } else if (std.mem.eql(u8, text, "i64")) {
            return self.make_token(.kw_i64, start_pos);
        } else if (std.mem.eql(u8, text, "i128")) {
            return self.make_token(.kw_i128, start_pos);
        } else if (std.mem.eql(u8, text, "u8")) {
            return self.make_token(.kw_u8, start_pos);
        } else if (std.mem.eql(u8, text, "u16")) {
            return self.make_token(.kw_u16, start_pos);
        } else if (std.mem.eql(u8, text, "u32")) {
            return self.make_token(.kw_u32, start_pos);
        } else if (std.mem.eql(u8, text, "u64")) {
            return self.make_token(.kw_u64, start_pos);
        } else if (std.mem.eql(u8, text, "u128")) {
            return self.make_token(.kw_u128, start_pos);
        } else if (std.mem.eql(u8, text, "f16")) {
            return self.make_token(.kw_f16, start_pos);
        } else if (std.mem.eql(u8, text, "f32")) {
            return self.make_token(.kw_f32, start_pos);
        } else if (std.mem.eql(u8, text, "f64")) {
            return self.make_token(.kw_f64, start_pos);
        } else if (std.mem.eql(u8, text, "f128")) {
            return self.make_token(.kw_f128, start_pos);
        } else if (std.mem.eql(u8, text, "str")) {
            return self.make_token(.kw_str, start_pos);
        } else if (std.mem.eql(u8, text, "self")) {
            return self.make_token(.identifier, start_pos);
        } else if (std.mem.eql(u8, text, "import")) {
            return self.make_token(.kw_import, start_pos);
        } else if (std.mem.eql(u8, text, "as")) {
            return self.make_token(.kw_as, start_pos);
        } else if (std.mem.eql(u8, text, "in")) {
            return self.make_token(.kw_in, start_pos);
        } else if (std.mem.eql(u8, text, "struct")) {
            return self.make_token(.kw_struct, start_pos);
        } else if (std.mem.eql(u8, text, "enum")) {
            return self.make_token(.kw_enum, start_pos);
        } else if (std.mem.eql(u8, text, "union")) {
            return self.make_token(.kw_union, start_pos);
        } else if (std.mem.eql(u8, text, "trait")) {
            return self.make_token(.kw_trait, start_pos);
        } else if (std.mem.eql(u8, text, "match")) {
            return self.make_token(.kw_match, start_pos);
        } else if (std.mem.eql(u8, text, "mut")) {
            return self.make_token(.kw_mut, start_pos);
        } else if (std.mem.eql(u8, text, "pub")) {
            return self.make_token(.kw_pub, start_pos);
        } else if (std.mem.eql(u8, text, "priv")) {
            return self.make_token(.kw_priv, start_pos);
        } else if (std.mem.eql(u8, text, "str8")) {
            return self.make_token(.kw_str, start_pos);
        } else if (std.mem.eql(u8, text, "strA")) {
            return self.make_token(.kw_strA, start_pos);
        } else if (std.mem.eql(u8, text, "str16")) {
            return self.make_token(.kw_str16, start_pos);
        } else if (std.mem.eql(u8, text, "str32")) {
            return self.make_token(.kw_str32, start_pos);
        } else if (std.mem.eql(u8, text, "stringA")) {
            return self.make_token(.kw_stringA, start_pos);
        } else if (std.mem.eql(u8, text, "string")) {
            return self.make_token(.kw_string, start_pos);
        } else if (std.mem.eql(u8, text, "string16")) {
            return self.make_token(.kw_string16, start_pos);
        } else if (std.mem.eql(u8, text, "string32")) {
            return self.make_token(.kw_string32, start_pos);
        } else if (std.mem.eql(u8, text, "generic")) {
            return self.make_token(.kw_generic, start_pos);
        } else if (std.mem.eql(u8, text, "typealias")) {
            return self.make_token(.kw_typealias, start_pos);
        } else if (std.mem.eql(u8, text, "true") or std.mem.eql(u8, text, "false")) {
            return self.make_token(.lit_bool, start_pos);
        } else {
            return self.make_token(.identifier, start_pos);
        }
    }

    fn next_token(self: *Lexer) Token {
        self.skip_whitespace();

        const start_pos = self.position;
        const start_line = self.line;
        const start_column = self.column;

        const ch = self.current() orelse {
            return Token{
                .type = .eof,
                .text = "",
                .line = start_line,
                .column = start_column,
            };
        };

        switch (ch) {
            // Single character tokens
            '(' => {
                _ = self.advance();
                return self.make_token(.left_paren, start_pos);
            },
            ')' => {
                _ = self.advance();
                return self.make_token(.right_paren, start_pos);
            },
            '{' => {
                _ = self.advance();
                return self.make_token(.left_brace, start_pos);
            },
            '}' => {
                _ = self.advance();
                return self.make_token(.right_brace, start_pos);
            },
            '[' => {
                _ = self.advance();
                return self.make_token(.left_bracket, start_pos);
            },
            ']' => {
                _ = self.advance();
                return self.make_token(.right_bracket, start_pos);
            },
            ';' => {
                _ = self.advance();
                return self.make_token(.semicolon, start_pos);
            },
            ',' => {
                _ = self.advance();
                return self.make_token(.comma, start_pos);
            },
            '+' => {
                _ = self.advance();
                return self.make_token(.plus, start_pos);
            },
            '-' => {
                _ = self.advance();
                if (self.current() == '>') {
                    _ = self.advance();
                    return self.make_token(.arrow, start_pos);
                }
                return self.make_token(.minus, start_pos);
            },
            '*' => {
                _ = self.advance();
                if (self.current() == '*') {
                    _ = self.advance();
                    return self.make_token(.power, start_pos);
                }
                return self.make_token(.multiply, start_pos);
            },
            '%' => {
                _ = self.advance();
                return self.make_token(.modulo, start_pos);
            },
            '/' => {
                _ = self.advance();
                if (self.current() == '/') {
                    // Single line comment
                    while (self.current()) |comment_ch| {
                        if (comment_ch == '\n') break;
                        _ = self.advance();
                    }
                    return self.next_token(); // Skip comment and get next token
                }
                return self.make_token(.divide, start_pos);
            },
            '=' => {
                _ = self.advance();
                if (self.current() == '=') {
                    _ = self.advance();
                    return self.make_token(.equal, start_pos);
                } else if (self.current() == '>') {
                    _ = self.advance();
                    return self.make_token(.arrow, start_pos);
                }
                return self.make_token(.assign, start_pos);
            },
            '>' => {
                _ = self.advance();
                if (self.current() == '=') {
                    _ = self.advance();
                    return self.make_token(.greater_equal, start_pos);
                }
                return self.make_token(.greater, start_pos);
            },
            '<' => {
                _ = self.advance();
                if (self.current() == '=') {
                    _ = self.advance();
                    return self.make_token(.less_equal, start_pos);
                }
                return self.make_token(.less, start_pos);
            },
            ':' => {
                _ = self.advance();
                if (self.current() == ':') {
                    _ = self.advance();
                    return self.make_token(.double_colon, start_pos);
                }
                return self.make_token(.colon, start_pos);
            },
            '$' => {
                _ = self.advance();
                return self.make_token(.dollar, start_pos);
            },
            '#' => {
                _ = self.advance();
                return self.make_token(.hash, start_pos);
            },
            '@' => {
                _ = self.advance();
                return self.make_token(.at_sign, start_pos);
            },
            '^' => {
                _ = self.advance();
                return self.make_token(.caret, start_pos);
            },
            '.' => {
                _ = self.advance();
                if (self.current() == '.') {
                    _ = self.advance();
                    if (self.current() == '.') {
                        _ = self.advance();
                        return self.make_token(.incl_range, start_pos); // ...
                    } else {
                        return self.make_token(.excl_range, start_pos); // ..
                    }
                }
                return self.make_token(.dot, start_pos);
            },
            '&' => {
                _ = self.advance();
                return self.make_token(.ampersand, start_pos);
            },
            '"' => {
                _ = self.advance();
                // Read until closing quote
                while (self.current()) |quote_ch| {
                    if (quote_ch == '"') {
                        _ = self.advance();
                        break;
                    } else if (quote_ch == '\\') {
                        _ = self.advance(); // Skip escape
                        _ = self.advance(); // Skip escaped char
                    } else {
                        _ = self.advance();
                    }
                }
                return self.make_token(.lit_str, start_pos);
            },
            '!' => {
                _ = self.advance();
                if (self.current() == '=') {
                    _ = self.advance();
                    return self.make_token(.not_equal, start_pos);
                }
                return self.make_token(.exclamation, start_pos);
            },
            '|' => {
                _ = self.advance();
                if (self.current() == '>') {
                    _ = self.advance();
                    return self.make_token(.piping, start_pos);
                }
                return self.make_token(.pipe, start_pos);
            },
            '_' => {
                _ = self.advance();
                // Check if it's followed by alphanumeric (making it an identifier)
                if (self.current()) |next_ch| {
                    if (std.ascii.isAlphanumeric(next_ch)) {
                        // Backtrack and read as identifier
                        self.position -= 1;
                        self.column -= 1;
                        return self.read_identifier();
                    }
                }
                // It's a standalone underscore
                return self.make_token(.underscore, start_pos);
            },
            '?' => {
                _ = self.advance();
                return self.make_token(.question_mark, start_pos);
            },

            // Multi-character tokens
            '0'...'9' => return self.read_number(),
            'a'...'z', 'A'...'Z' => return self.read_identifier(),

            // Unknown character (including UTF-8)
            else => {
                // Skip UTF-8 continuation bytes
                if (ch >= 0x80) {
                    // This is a UTF-8 multi-byte character, skip all bytes
                    _ = self.advance();
                    while (self.current()) |utf8_ch| {
                        if (utf8_ch < 0x80 or utf8_ch >= 0xC0) break;
                        _ = self.advance();
                    }
                    return Token{
                        .type = .unknown,
                        .text = self.input[start_pos..self.position],
                        .line = start_line,
                        .column = start_column,
                    };
                } else {
                    _ = self.advance();
                    return Token{
                        .type = .Error,
                        .text = self.input[start_pos..self.position],
                        .line = start_line,
                        .column = start_column,
                    };
                }
            },
        }
    }
};

pub fn tokenize(allocator: Allocator, input: []const u8) ![]Token {
    var lexer = Lexer.init(input);
    var tokens = std.ArrayList(Token).init(allocator);

    try tokens.ensureTotalCapacity(32);

    while (true) {
        const token = lexer.next_token();

        // Always add the token (even EOF)
        try tokens.append(token);

        // Stop when we reach EOF
        if (token.type == .eof) {
            break;
        }
    }

    return tokens.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read from example.txt
    const source = try std.fs.cwd().readFileAlloc(allocator, "src/lexer/example.txt", 1024 * 1024);
    defer allocator.free(source);

    // Tokenize
    const tokens = try tokenize(allocator, source);
    defer allocator.free(tokens);

    // Write to tokens.txt
    const file = try std.fs.cwd().createFile("src/lexer/tokens.txt", .{});
    defer file.close();

    const writer = file.writer();
    for (tokens) |token| {
        try writer.print("Token: {s} ({s}) at line {d}, col {d}\n", .{ @tagName(token.type), token.text, token.line, token.column });
    }
}
