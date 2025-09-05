const std = @import("std");
const AST = @import("parser/ast.zig");
const syntactic_analyzer = @import("ast_analysis/syntactic_analyzer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Read AST output file
    const ast_content = std.fs.cwd().readFileAlloc(allocator, "src/parser/ast_output.txt", 1024 * 1024) catch |err| {
        std.debug.print("Error reading AST file: {}\n", .{err});
        return;
    };
    defer allocator.free(ast_content);
    
    std.debug.print("AST file content:\n{s}\n", .{ast_content});
    
    // Create test case matching current AST with enum usage
    var test_items = std.ArrayList(AST.ASTNode).init(allocator);
    defer test_items.deinit();
    
    // Create declaration using undeclared enum variant (should error)
    try test_items.append(AST.ASTNode{ .declaration = AST.Declaration{
        .kind = .let_decl,
        .is_mut = false,
        .name = "dir",
        .type_annotation = null,
        .initializer = AST.Expression{ .identifier = "Up" },
        .fields = null,
        .attributes = null,
    }});
    
    // Create Direction enum
    const direction_variants = try allocator.alloc(AST.EnumVariant, 4);
    direction_variants[0] = AST.EnumVariant{ .name = "Up", .fields = null };
    direction_variants[1] = AST.EnumVariant{ .name = "Down", .fields = null };
    direction_variants[2] = AST.EnumVariant{ .name = "Left", .fields = null };
    direction_variants[3] = AST.EnumVariant{ .name = "Right", .fields = null };
    
    try test_items.append(AST.ASTNode{ .enum_def = AST.EnumDef{
        .name = "Direction",
        .generic_params = null,
        .variants = direction_variants,
    }});
    
    // Create valid enum usage (should be ok)
    try test_items.append(AST.ASTNode{ .declaration = AST.Declaration{
        .kind = .let_decl,
        .is_mut = false,
        .name = "valid_dir",
        .type_annotation = null,
        .initializer = AST.Expression{ .identifier = "Down" },
        .fields = null,
        .attributes = null,
    }});
    
    const test_program = AST.Program{
        .items = try test_items.toOwnedSlice(),
    };
    
    // Run syntax analysis
    const errors = syntactic_analyzer.analyze_syntax(allocator, test_program) catch |err| {
        std.debug.print("Analysis error: {}\n", .{err});
        return;
    };
    defer allocator.free(errors);
    
    // Print results
    if (errors.len == 0) {
        std.debug.print("✓ No syntax errors found\n", .{});
    } else {
        std.debug.print("✗ Found {} syntax error(s):\n", .{errors.len});
        for (errors, 0..) |error_item, i| {
            std.debug.print("  [{d}] {s}: {s}\n", .{ i + 1, @tagName(error_item.severity), error_item.message });
        }
    }
}