const std = @import("std");
const tokenize = @import("lexer/lexer.zig").tokenize;
const parse = @import("parser/parser.zig").parse;
const print_ast = @import("parser/parser.zig").print_ast;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Get command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    
    const input_file = if (args.len > 1) args[1] else "src/lexer/example.txt";
    
    // Read source
    const source = try std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024);
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
    
    // Run syntax analysis
    const syntactic_analyzer = @import("ast_analysis/syntactic_analyzer.zig");
    const errors = syntactic_analyzer.analyze_syntax(allocator, program) catch |err| {
        std.debug.print("Syntax analysis error: {}\n", .{err});
        return;
    };
    defer allocator.free(errors);
    
    if (errors.len == 0) {
        std.debug.print("✓ No syntax errors found\n", .{});
    } else {
        std.debug.print("✗ Found {} syntax error(s):\n", .{errors.len});
        for (errors, 0..) |error_item, i| {
            std.debug.print("  [{d}] {s}: {s}\n", .{ i + 1, @tagName(error_item.severity), error_item.message });
        }
    }
    
    std.debug.print("Parsing complete. AST written to src/parser/ast_output.txt\n", .{});
}