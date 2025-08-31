const std = @import("std");
const tokenize = @import("lexer/lexer.zig").tokenize;
const parse = @import("parser/parser.zig").parse;
const print_ast = @import("parser/parser.zig").print_ast;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Read source
    const source = try std.fs.cwd().readFileAlloc(allocator, "src/lexer/example.txt", 1024 * 1024);
    defer allocator.free(source);
    
    // Tokenize
    const tokens = try tokenize(allocator, source);
    defer allocator.free(tokens);
    
    // Parse into AST
    const program = parse(allocator, tokens) catch |err| {
        std.debug.print("Parse error: {s}\n", .{@errorName(err)});
        
        // Print tokens for debugging
        const debug_file = try std.fs.cwd().createFile("src/parser/debug_tokens.txt", .{});
        defer debug_file.close();
        const debug_writer = debug_file.writer();
        
        for (tokens, 0..) |token, i| {
            try debug_writer.print("{d}: {s} ({s})\n", .{ i, @tagName(token.type), token.text });
        }
        
        return;
    };
    
    // Print AST
    try print_ast(allocator, program);
    
    std.debug.print("Parsing complete. AST written to src/parser/ast_output.txt\n", .{});
}