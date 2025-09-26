const std = @import("std");
const AST = @import("parser/ast.zig");
const syntactic_analyzer = @import("ast_analysis/syntactic_analyzer.zig");

pub fn main() !void {
    // The analyzer should not create its own test program
    // It should be called from main.zig with the actual parsed program
    std.debug.print("Analyzer should be called from main.zig, not run standalone\n", .{});
}