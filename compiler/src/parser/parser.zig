const std = @import("std");
const Token = @import("../lexer/tokens.zig").Token;
const TokenType = @import("../lexer/tokens.zig").TokenType;
const Allocator = std.mem.Allocator;
const AST = @import("ast.zig");
const ParseError = error{
    UnexpectedToken,
    UnexpectedEOF,
    OutOfMemory,
};
const Parser = struct {
    tokens: []const Token,
    position: usize,
    allocator: Allocator,
    trait_definitions: std.ArrayList(AST.TraitDef),
    fn init(tokens: []const Token, allocator: Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .position = 0,
            .allocator = allocator,
            .trait_definitions = std.ArrayList(AST.TraitDef).init(allocator),
        };
    }
    fn current(self: Parser) ?Token {
        if (self.position < self.tokens.len) {
            return self.tokens[self.position];
        }
        return null;
    }
    fn advance(self: *Parser) ?Token {
        if (self.position < self.tokens.len) {
            const token = self.tokens[self.position];
            self.position += 1;
            return token;
        }
        return null;
    }
    fn expect(self: *Parser, token_type: TokenType) ParseError!Token {
        const token = self.current() orelse return ParseError.UnexpectedEOF;
        if (token.type != token_type) return ParseError.UnexpectedToken;
        return self.advance().?;
    }
    fn match(self: *Parser, token_type: TokenType) bool {
        if (self.current()) |token| {
            if (token.type == token_type) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }
    fn parse_program(self: *Parser) ParseError!AST.Program {
        var items = std.ArrayList(AST.ASTNode).init(self.allocator);
        while (self.current()) |token| {
            if (token.type == .eof) break;
            switch (token.type) {
                .kw_while => {
                    const while_expr = self.parse_expression() catch |err| {
                        std.debug.print("While loop parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .expression = while_expr });
                },
                .kw_for => {
                    const for_expr = self.parse_expression() catch |err| {
                        std.debug.print("For loop parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .expression = for_expr });
                },
                .kw_loop => {
                    const loop_expr = self.parse_expression() catch |err| {
                        std.debug.print("Loop parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .expression = loop_expr });
                },
                .kw_var, .kw_let, .kw_const, .kw_process, .kw_func, .kw_fm, .kw_fum, .at_sign => {
                    const decl = self.parse_declaration() catch |err| {
                        std.debug.print("Declaration parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        if (self.position < self.tokens.len) {
                            std.debug.print("Failed token: {s} ({s})\n", .{ @tagName(self.tokens[self.position].type), self.tokens[self.position].text });
                        }
                        self.skip_to_semicolon();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .declaration = decl });
                },
                .kw_struct => {
                    const struct_def = self.parse_struct() catch |err| {
                        std.debug.print("Struct parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .struct_def = struct_def });
                },
                .kw_enum => {
                    const enum_def = self.parse_enum() catch |err| {
                        std.debug.print("Enum parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .enum_def = enum_def });
                },
                .kw_union => {
                    const union_def = self.parse_union() catch |err| {
                        std.debug.print("Union parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .enum_def = union_def });
                },
                .kw_trait => {
                    const trait_def = self.parse_trait() catch |err| {
                        std.debug.print("Trait parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try self.trait_definitions.append(trait_def);
                    try items.append(AST.ASTNode{ .trait_def = trait_def });
                },
                .impl => {
                    const impl_block = self.parse_impl_block() catch |err| {
                        std.debug.print("Impl parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .impl_block = impl_block });
                },
                .kw_implement => {
                    const impl_block = self.parse_implement_block() catch |err| {
                        std.debug.print("Implement parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .impl_block = impl_block });
                },
                .kw_generic => {
                    const generic_def = self.parse_generic() catch |err| {
                        std.debug.print("Generic parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .generic_def = generic_def });
                },
                .kw_typealias => {
                    const typealias_decl = self.parse_typealias() catch |err| {
                        std.debug.print("Typealias parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_semicolon();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .declaration = typealias_decl });
                },
                .kw_inline => {
                    const inline_expr = self.parse_expression() catch |err| {
                        std.debug.print("Inline expression parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_semicolon();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .expression = inline_expr });
                },
                .identifier => {
                    const expr = self.parse_expression() catch {
                        _ = self.advance();
                        continue;
                    };
                    _ = self.match(.semicolon);
                    try items.append(AST.ASTNode{ .expression = expr });
                },
                .kw_match => {
                    const match_expr = self.parse_expression() catch |err| {
                        std.debug.print("Match expression parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
                    try items.append(AST.ASTNode{ .expression = match_expr });
                },
                else => {
                    _ = self.advance();
                },
            }
        }
        const program = AST.Program{ .items = try items.toOwnedSlice() };
        self.validate_all_implementations(program) catch |err| {
            std.debug.print("Validation failed: {s}\n", .{@errorName(err)});
        };
        self.validate_fum_grouping(program) catch |err| {
            std.debug.print("Fum grouping validation failed: {s}\n", .{@errorName(err)});
            return err;
        };
        return program;
    }
    fn skip_to_semicolon(self: *Parser) void {
        while (self.current()) |token| {
            if (token.type == .semicolon or token.type == .eof) {
                _ = self.advance();
                break;
            }
            _ = self.advance();
        }
    }
    fn skip_to_brace(self: *Parser) void {
        while (self.current()) |token| {
            if (token.type == .right_brace or token.type == .eof) {
                _ = self.advance();
                break;
            }
            _ = self.advance();
        }
    }
    fn infer_type_from_expression_with_params(self: *Parser, expr: AST.Expression, params: []AST.Parameter) ?AST.Type {
        switch (expr) {
            .binary_op => |binop| {
                const left_type = self.infer_type_from_expression_with_params(binop.left.*, params);
                const right_type = self.infer_type_from_expression_with_params(binop.right.*, params);
                
                // For arithmetic operations, return the operand type
                if (std.mem.eql(u8, binop.operator, "+") or 
                    std.mem.eql(u8, binop.operator, "-") or 
                    std.mem.eql(u8, binop.operator, "*") or 
                    std.mem.eql(u8, binop.operator, "/")) {
                    
                    // Return the first non-null type, preferring left
                    if (left_type) |ltype| {
                        return ltype;
                    }
                    if (right_type) |rtype| {
                        return rtype;
                    }
                    return AST.Type{ .basic = "i32" }; // Default fallback
                }
                
                // For comparison operations, result is bool
                if (std.mem.eql(u8, binop.operator, "==") or 
                    std.mem.eql(u8, binop.operator, "!=") or 
                    std.mem.eql(u8, binop.operator, "<") or 
                    std.mem.eql(u8, binop.operator, ">") or 
                    std.mem.eql(u8, binop.operator, "<=") or 
                    std.mem.eql(u8, binop.operator, ">=")) {
                    return AST.Type{ .basic = "bool" };
                }
                
                return left_type orelse right_type;
            },
            .literal => |lit| {
                switch (lit) {
                    .number => |num| {
                        // Check if it's a whole number
                        const floor_val = @floor(num);
                        if (@abs(num - floor_val) < 0.0001) {
                            return AST.Type{ .basic = "i32" };
                        } else {
                            return AST.Type{ .basic = "f32" };
                        }
                    },
                    .string => return AST.Type{ .basic = "str" },
                    .boolean => return AST.Type{ .basic = "bool" },
                    else => return null,
                }
            },
            .identifier => |id| {
                // Look up parameter type
                for (params) |param| {
                    if (std.mem.eql(u8, param.name, id)) {
                        return param.param_type;
                    }
                }
                // Default to i32 for unknown identifiers (could be improved with symbol table)
                return AST.Type{ .basic = "i32" };
            },
            else => return null,
        }
    }
    
    fn infer_type_from_expression(self: *Parser, expr: AST.Expression) ?AST.Type {
        return self.infer_type_from_expression_with_params(expr, &[_]AST.Parameter{});
    }
    
    fn types_equal(self: *Parser, type1: AST.Type, type2: AST.Type) bool {
        _ = self;
        switch (type1) {
            .basic => |name1| {
                switch (type2) {
                    .basic => |name2| return std.mem.eql(u8, name1, name2),
                    else => return false,
                }
            },
            .named => |name1| {
                switch (type2) {
                    .named => |name2| return std.mem.eql(u8, name1, name2),
                    else => return false,
                }
            },
            else => return false, // For now, only handle basic types
        }
    }
    

    
    fn parse_declaration(self: *Parser) ParseError!AST.Declaration {
        var attributes = std.ArrayList(AST.MacroCall).init(self.allocator);
        while (self.current() != null and self.current().?.type == .at_sign) {
            _ = self.advance(); 
            const macro_name = try self.expect(.identifier);
            _ = try self.expect(.left_paren);
            var args = std.ArrayList(AST.Expression).init(self.allocator);
            if (self.current() != null and self.current().?.type != .right_paren) {
                while (true) {
                    try args.append(try self.parse_expression());
                    if (!self.match(.comma)) break;
                }
            }
            _ = try self.expect(.right_paren);
            try attributes.append(AST.MacroCall{
                .name = macro_name.text,
                .args = try args.toOwnedSlice(),
            });
        }
        const kind_token = self.advance().?;
        const is_mut = self.match(.kw_mut);
        const has_pointer_prefix = self.match(.caret);
        

        
        const name_token = try self.expect(.identifier);
        const pointer_in_name = has_pointer_prefix;
        var array_size: ?AST.Expression = null;
        var is_inferred_size = false;
        if (self.match(.left_bracket)) {
            if (self.match(.underscore)) {
                is_inferred_size = true;
            } else {
                array_size = try self.parse_expression();
            }
            _ = try self.expect(.right_bracket);
        }
        var type_annotation: ?AST.Type = null;
        if (self.match(.colon)) {
            if (self.match(.left_paren)) {
                while (!self.match(.right_paren)) {
                    _ = try self.expect(.identifier);
                    if (!self.match(.comma)) break;
                }
                type_annotation = AST.Type{ .named = "tuple" };
            } else {
                var parsed_type = try self.parse_type();
                if (array_size != null or is_inferred_size) {
                    const elem_type_ptr = try self.allocator.create(AST.Type);
                    elem_type_ptr.* = parsed_type;
                    var size_ptr: ?*AST.Expression = null;
                    if (array_size) |size| {
                        const ptr = try self.allocator.create(AST.Expression);
                        ptr.* = size;
                        size_ptr = ptr;
                    } else if (is_inferred_size) {
                        const ptr = try self.allocator.create(AST.Expression);
                        ptr.* = AST.Expression{ .identifier = "_" };
                        size_ptr = ptr;
                    }
                    parsed_type = AST.Type{ .array = AST.ArrayType{ .element_type = elem_type_ptr, .size = size_ptr } };
                }
                if (pointer_in_name) {
                    const ptr_type = try self.allocator.create(AST.Type);
                    ptr_type.* = parsed_type;
                    type_annotation = AST.Type{ .pointer = ptr_type };
                } else {
                    type_annotation = parsed_type;
                }
            }
        }
        var initializer: ?AST.Expression = null;
        if (self.match(.assign)) {
            initializer = try self.parse_expression();
            while (self.match(.comma)) {
                _ = try self.parse_expression();
            }
        }
        _ = self.match(.semicolon);
        return switch (kind_token.type) {
            .kw_var => AST.Declaration{
                .kind = .var_decl,
                .is_mut = is_mut,
                .is_match = false,
                .name = name_token.text,
                .type_annotation = type_annotation,
                .initializer = initializer,
                .fields = null,
                .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                .generic_params = null,
            },
            .kw_let => AST.Declaration{
                .kind = .let_decl,
                .is_mut = is_mut,
                .is_match = false,
                .name = name_token.text,
                .type_annotation = type_annotation,
                .initializer = initializer,
                .fields = null,
                .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                .generic_params = null,
            },
            .kw_const => AST.Declaration{
                .kind = .const_decl,
                .is_mut = is_mut,
                .is_match = false,
                .name = name_token.text,
                .type_annotation = type_annotation,
                .initializer = initializer,
                .fields = null,
                .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                .generic_params = null,
            },
            .kw_process => {
                _ = try self.expect(.left_brace);
                var fields = std.ArrayList(AST.Field).init(self.allocator);
                while (!self.match(.right_brace)) {
                    _ = self.advance(); 
                    const field_name = try self.expect(.identifier);
                    var field_type: AST.Type = AST.Type{ .named = "unknown" };
                    if (self.match(.colon)) {
                        field_type = try self.parse_type();
                    }
                    var field_init: ?AST.Expression = null;
                    if (self.match(.assign)) {
                        field_init = try self.parse_expression();
                    }
                    _ = self.match(.semicolon);
                    try fields.append(AST.Field{
                        .name = field_name.text,
                        .type_annotation = field_type,
                        .initializer = field_init,
                        .visibility = AST.Visibility.private,
                    });
                }
                return AST.Declaration{
                    .kind = .process_decl,
                    .is_mut = is_mut,
                    .is_match = false,
                    .name = name_token.text,
                    .type_annotation = type_annotation,
                    .initializer = initializer,
                    .fields = try fields.toOwnedSlice(),
                    .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                    .generic_params = null,
                };
            },
            .kw_fm => {
                // fm is equivalent to func match
                const is_match_func = true;
                
                var generic_params: ?[][]const u8 = null;
                if (self.match(.double_colon) and self.match(.less)) {
                    var params = std.ArrayList([]const u8).init(self.allocator);
                    const param = try self.expect(.identifier);
                    const param_text = param.text;

                    try params.append(param_text);
                    while (self.match(.comma)) {
                        const next_param = try self.expect(.identifier);
                        const next_param_text = next_param.text;

                        try params.append(next_param_text);
                    }
                    _ = try self.expect(.greater);
                    generic_params = try params.toOwnedSlice();
                }
                _ = try self.expect(.left_paren);
                var params = std.ArrayList(AST.Parameter).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        const param_is_mut = self.match(.kw_mut);
                        
                        // Allow direct values (literals) as parameters
                        var param_name: []const u8 = undefined;
                        const current_token = self.current() orelse return ParseError.UnexpectedEOF;
                        if (current_token.type == .lit_int or current_token.type == .lit_float or current_token.type == .lit_str) {
                            param_name = self.advance().?.text;
                        } else {
                            param_name = (try self.expect(.identifier)).text;
                        }
                        _ = try self.expect(.colon);
                        var param_is_copy = false;
                        var param_is_ref = false;
                        var param_is_ptr = false;
                        if (self.match(.hash)) {
                            param_is_copy = true;
                        }
                        if (self.match(.ampersand)) {
                            param_is_ref = true;
                        }
                        if (self.match(.caret)) {
                            param_is_ptr = true;
                        }
                        const param_type = try self.parse_type();
                        var final_type = param_type;
                        if (param_is_ptr) {
                            const ptr_type = try self.allocator.create(AST.Type);
                            ptr_type.* = param_type;
                            final_type = AST.Type{ .pointer = ptr_type };
                        }
                        try params.append(AST.Parameter{
                            .name = param_name,
                            .param_type = final_type,
                            .is_mut = param_is_mut,
                            .is_ref = param_is_ref,
                            .is_copy = param_is_copy,
                        });
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                const owned_params = try params.toOwnedSlice();
                params.deinit();
                var return_type: ?AST.Type = null;
                if (self.current() != null and 
                    self.current().?.type != .left_brace) {
                    return_type = try self.parse_type();
                }

                if (self.current() == null or self.current().?.type != .left_brace) {
                    self.allocator.free(owned_params);
                    return AST.Declaration{
                        .kind = .fun_decl,
                        .is_mut = is_mut,
                        .is_match = true,
                        .name = name_token.text,
                        .type_annotation = return_type,
                        .initializer = null,
                        .fields = try self.convert_params_to_fields(&[_]AST.Parameter{}, null),
                        .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                        .generic_params = null,
                    };
                }
                _ = self.expect(.left_brace) catch |err| {
                    self.allocator.free(owned_params);
                    return err;
                };
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?AST.Expression = null;
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance(); 
                        const return_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        
                        if (return_type == null) {
                            return_type = self.infer_type_from_expression_with_params(return_expr, owned_params);
                        }
                        
                        body_expr = return_expr;
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                _ = try self.expect(.right_brace);
                if (body_statements.items.len > 0 or body_expr != null) {
                    const block_expr = AST.Expression{ .block_expr = AST.Block{
                        .statements = try body_statements.toOwnedSlice(),
                        .expr = if (body_expr) |expr| blk: {
                            const expr_ptr = try self.allocator.create(AST.Expression);
                            expr_ptr.* = expr;
                            break :blk expr_ptr;
                        } else null,
                    }};
                    body_expr = block_expr;
                }
                const param_fields = try self.allocator.alloc(AST.Field, owned_params.len);
                for (owned_params, 0..) |param, i| {
                    var modifier_expr: ?AST.Expression = null;
                    if (param.is_mut and param.is_ref and param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "mut_ref_copy" };
                    } else if (param.is_mut and param.is_ref) {
                        modifier_expr = AST.Expression{ .identifier = "mut_ref" };
                    } else if (param.is_mut and param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "mut_copy" };
                    } else if (param.is_ref and param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "ref_copy" };
                    } else if (param.is_mut) {
                        modifier_expr = AST.Expression{ .identifier = "mut" };
                    } else if (param.is_ref) {
                        modifier_expr = AST.Expression{ .identifier = "ref" };
                    } else if (param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "copy" };
                    }
                    param_fields[i] = AST.Field{
                        .name = param.name,
                        .type_annotation = param.param_type,
                        .initializer = modifier_expr,
                        .visibility = AST.Visibility.private,
                    };
                }
                if (return_type == null and body_expr != null) {
                    switch (body_expr.?) {
                        .block_expr => |block| {
                            if (block.expr) |expr| {
                                return_type = self.infer_type_from_expression_with_params(expr.*, owned_params);
                            }
                        },
                        else => {
                            return_type = self.infer_type_from_expression_with_params(body_expr.?, owned_params);
                        },
                    }
                }
                
                type_annotation = return_type;
                var final_generic_params: ?[][]const u8 = null;
                if (generic_params != null) {
                    final_generic_params = generic_params;
                }
                self.allocator.free(owned_params);
                return AST.Declaration{
                    .kind = .fun_decl,
                    .is_mut = is_mut,
                    .is_match = is_match_func,
                    .name = name_token.text,
                    .type_annotation = type_annotation,
                    .initializer = body_expr,
                    .fields = param_fields,
                    .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                    .generic_params = final_generic_params,
                };
            },
            .kw_func => {
                const is_match_func = false;
                
                var generic_params: ?[][]const u8 = null;
                if (self.match(.double_colon) and self.match(.less)) {
                    var params = std.ArrayList([]const u8).init(self.allocator);
                    const param = try self.expect(.identifier);
                    const param_text = param.text;

                    try params.append(param_text);
                    while (self.match(.comma)) {
                        const next_param = try self.expect(.identifier);
                        const next_param_text = next_param.text;

                        try params.append(next_param_text);
                    }
                    _ = try self.expect(.greater);
                    generic_params = try params.toOwnedSlice();
                }
                _ = try self.expect(.left_paren);
                var params = std.ArrayList(AST.Parameter).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        const param_is_mut = self.match(.kw_mut);
                        const param_name = try self.expect(.identifier);
                        _ = try self.expect(.colon);
                        var param_is_copy = false;
                        var param_is_ref = false;
                        var param_is_ptr = false;
                        if (self.match(.hash)) {
                            param_is_copy = true;
                        }
                        if (self.match(.ampersand)) {
                            param_is_ref = true;
                        }
                        if (self.match(.caret)) {
                            param_is_ptr = true;
                        }
                        const param_type = try self.parse_type();
                        var final_type = param_type;
                        if (param_is_ptr) {
                            const ptr_type = try self.allocator.create(AST.Type);
                            ptr_type.* = param_type;
                            final_type = AST.Type{ .pointer = ptr_type };
                        }
                        try params.append(AST.Parameter{
                            .name = param_name.text,
                            .param_type = final_type,
                            .is_mut = param_is_mut,
                            .is_ref = param_is_ref,
                            .is_copy = param_is_copy,
                        });
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                const owned_params = try params.toOwnedSlice();
                params.deinit();
                var return_type: ?AST.Type = null;
                if (self.current() != null and 
                    self.current().?.type != .left_brace) {
                    return_type = try self.parse_type();
                }

                if (self.current() == null or self.current().?.type != .left_brace) {
                    self.allocator.free(owned_params);
                    return AST.Declaration{
                        .kind = .fun_decl,
                        .is_mut = is_mut,
                        .is_match = false,
                        .name = name_token.text,
                        .type_annotation = return_type,
                        .initializer = null,
                        .fields = try self.convert_params_to_fields(&[_]AST.Parameter{}, null),
                        .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                        .generic_params = null,
                    };
                }
                _ = self.expect(.left_brace) catch |err| {
                    self.allocator.free(owned_params);
                    return err;
                };
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?AST.Expression = null;
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance(); 
                        const return_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        
                        // If no explicit return type was provided, try to infer it
                        if (return_type == null) {
                            return_type = self.infer_type_from_expression_with_params(return_expr, owned_params);
                        }
                        
                        body_expr = return_expr;
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                _ = try self.expect(.right_brace);
                if (body_statements.items.len > 0 or body_expr != null) {
                    const block_expr = AST.Expression{ .block_expr = AST.Block{
                        .statements = try body_statements.toOwnedSlice(),
                        .expr = if (body_expr) |expr| blk: {
                            const expr_ptr = try self.allocator.create(AST.Expression);
                            expr_ptr.* = expr;
                            break :blk expr_ptr;
                        } else null,
                    }};
                    body_expr = block_expr;
                }
                const param_fields = try self.allocator.alloc(AST.Field, owned_params.len);
                for (owned_params, 0..) |param, i| {
                    var modifier_expr: ?AST.Expression = null;
                    if (param.is_mut and param.is_ref and param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "mut_ref_copy" };
                    } else if (param.is_mut and param.is_ref) {
                        modifier_expr = AST.Expression{ .identifier = "mut_ref" };
                    } else if (param.is_mut and param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "mut_copy" };
                    } else if (param.is_ref and param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "ref_copy" };
                    } else if (param.is_mut) {
                        modifier_expr = AST.Expression{ .identifier = "mut" };
                    } else if (param.is_ref) {
                        modifier_expr = AST.Expression{ .identifier = "ref" };
                    } else if (param.is_copy) {
                        modifier_expr = AST.Expression{ .identifier = "copy" };
                    }
                    param_fields[i] = AST.Field{
                        .name = param.name,
                        .type_annotation = param.param_type,
                        .initializer = modifier_expr,
                        .visibility = AST.Visibility.private,
                    };
                }
                // Ensure we use the inferred return type if available
                if (return_type == null and body_expr != null) {
                    switch (body_expr.?) {
                        .block_expr => |block| {
                            if (block.expr) |expr| {
                                return_type = self.infer_type_from_expression_with_params(expr.*, owned_params);
                            }
                        },
                        else => {
                            return_type = self.infer_type_from_expression_with_params(body_expr.?, owned_params);
                        },
                    }
                }
                
                type_annotation = return_type;
                var final_generic_params: ?[][]const u8 = null;
                if (generic_params != null) {
                    final_generic_params = generic_params;
                }
                self.allocator.free(owned_params);
                return AST.Declaration{
                    .kind = .fun_decl,
                    .is_mut = is_mut,
                    .is_match = is_match_func,
                    .name = name_token.text,
                    .type_annotation = type_annotation,
                    .initializer = body_expr,
                    .fields = param_fields,
                    .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                    .generic_params = final_generic_params,
                };
            },
            .kw_fum => {
                try attributes.append(AST.MacroCall{
                    .name = "fum",
                    .args = &[_]AST.Expression{},
                });
                var generic_params: ?[][]const u8 = null;
                if (self.match(.double_colon) and self.match(.less)) {
                    var params = std.ArrayList([]const u8).init(self.allocator);
                    const param = try self.expect(.identifier);
                    try params.append(param.text);
                    while (self.match(.comma)) {
                        const next_param = try self.expect(.identifier);
                        try params.append(next_param.text);
                    }
                    _ = try self.expect(.greater);
                    generic_params = try params.toOwnedSlice();
                }
                _ = try self.expect(.left_paren);
                var params = std.ArrayList(AST.Parameter).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        var param_name: []const u8 = undefined;
                        const current_token = self.current() orelse return ParseError.UnexpectedEOF;
                        if (current_token.type == .lit_int or current_token.type == .lit_float or current_token.type == .lit_str) {
                            param_name = self.advance().?.text;
                        } else {
                            param_name = (try self.expect(.identifier)).text;
                        }
                        _ = try self.expect(.colon);
                        const param_type = try self.parse_type();
                        try params.append(AST.Parameter{
                            .name = param_name,
                            .param_type = param_type,
                            .is_mut = false,
                            .is_ref = false,
                            .is_copy = false,
                        });
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                var return_type: ?AST.Type = null;
                if (self.current() != null and self.current().?.type != .left_brace) {
                    return_type = try self.parse_type();
                }
                _ = try self.expect(.left_brace);
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?AST.Expression = null;
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance();
                        const return_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        
                        // If no explicit return type was provided, try to infer it
                        if (return_type == null) {
                            return_type = self.infer_type_from_expression_with_params(return_expr, params.items);
                        }
                        
                        body_expr = return_expr;
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                _ = try self.expect(.right_brace);
                const block_expr = AST.Expression{ .block_expr = AST.Block{
                    .statements = try body_statements.toOwnedSlice(),
                    .expr = if (body_expr) |expr| blk: {
                        const expr_ptr = try self.allocator.create(AST.Expression);
                        expr_ptr.* = expr;
                        break :blk expr_ptr;
                    } else null,
                }};
                const param_fields = try self.allocator.alloc(AST.Field, params.items.len);
                for (params.items, 0..) |param, i| {
                    param_fields[i] = AST.Field{
                        .name = param.name,
                        .type_annotation = param.param_type,
                        .initializer = null,
                        .visibility = AST.Visibility.private,
                    };
                }
                // Ensure we use the inferred return type if available
                if (return_type == null) {
                    const temp_params = try self.allocator.alloc(AST.Parameter, params.items.len);
                    for (params.items, 0..) |param, i| {
                        temp_params[i] = param;
                    }
                    switch (block_expr) {
                        .block_expr => |block| {
                            if (block.expr) |expr| {
                                return_type = self.infer_type_from_expression_with_params(expr.*, temp_params);
                            }
                        },
                        else => {},
                    }
                    self.allocator.free(temp_params);
                }
                
                params.deinit();
                return AST.Declaration{
                    .kind = .fun_decl,
                    .is_mut = is_mut,
                    .is_match = false,
                    .name = name_token.text,
                    .type_annotation = return_type,
                    .initializer = block_expr,
                    .fields = param_fields,
                    .attributes = if (attributes.items.len > 0) try attributes.toOwnedSlice() else null,
                    .generic_params = generic_params,
                };
            },
            else => return ParseError.UnexpectedToken,
        };
    }
    fn parse_expression(self: *Parser) ParseError!AST.Expression {
        return self.parse_additive_expression();
    }
    fn parse_additive_expression(self: *Parser) ParseError!AST.Expression {
        var left = try self.parse_primary_expression();
        if (self.current()) |token| {
            if (token.type == .assign) {
                const op_token = self.advance().?;
                const right = try self.parse_expression();
                const left_ptr = try self.allocator.create(AST.Expression);
                const right_ptr = try self.allocator.create(AST.Expression);
                left_ptr.* = left;
                right_ptr.* = right;
                return AST.Expression{ .binary_op = AST.BinaryOp{
                    .left = left_ptr,
                    .operator = op_token.text,
                    .right = right_ptr,
                }};
            }
        }
        while (self.current()) |token| {
            if (token.type == .plus or token.type == .minus or token.type == .multiply or token.type == .divide or token.type == .modulo or token.type == .power or token.type == .equal or token.type == .not_equal or token.type == .less or token.type == .greater or token.type == .less_equal or token.type == .greater_equal or token.type == .piping or token.type == .excl_range or token.type == .incl_range or token.type == .kw_ifErr or token.type == .kw_ifNull) {
                const op_token = self.advance().?;
                const right = try self.parse_primary_expression();
                if (op_token.type == .excl_range or op_token.type == .incl_range) {
                    const left_ptr = try self.allocator.create(AST.Expression);
                    const right_ptr = try self.allocator.create(AST.Expression);
                    left_ptr.* = left;
                    right_ptr.* = right;
                    left = AST.Expression{ .range_expr = AST.RangeExpression{
                        .start = left_ptr,
                        .end = right_ptr,
                        .inclusive = op_token.type == .incl_range, 
                    }};
                } else if (op_token.type == .kw_ifErr) {
                    const left_ptr = try self.allocator.create(AST.Expression);
                    const right_ptr = try self.allocator.create(AST.Expression);
                    left_ptr.* = left;
                    right_ptr.* = right;
                    left = AST.Expression{ .if_err_expr = AST.IfErrExpression{
                        .expr = left_ptr,
                        .fallback = right_ptr,
                    }};
                } else if (op_token.type == .kw_ifNull) {
                    const left_ptr = try self.allocator.create(AST.Expression);
                    const right_ptr = try self.allocator.create(AST.Expression);
                    left_ptr.* = left;
                    right_ptr.* = right;
                    left = AST.Expression{ .if_null_expr = AST.IfNullExpression{
                        .expr = left_ptr,
                        .fallback = right_ptr,
                    }};
                } else {
                    const left_ptr = try self.allocator.create(AST.Expression);
                    const right_ptr = try self.allocator.create(AST.Expression);
                    left_ptr.* = left;
                    right_ptr.* = right;
                    left = AST.Expression{ .binary_op = AST.BinaryOp{
                        .left = left_ptr,
                        .operator = op_token.text,
                        .right = right_ptr,
                    }};
                }
            } else {
                break;
            }
        }
        return left;
    }
    fn parse_primary_expression(self: *Parser) ParseError!AST.Expression {
        const token = self.current() orelse return ParseError.UnexpectedEOF;
        switch (token.type) {
            .lit_int, .lit_float => {
                _ = self.advance();
                const num = std.fmt.parseFloat(f64, token.text) catch 0.0;
                return AST.Expression{ .literal = AST.Literal{ .number = num } };
            },
            .lit_str => {
                _ = self.advance();
                return AST.Expression{ .literal = AST.Literal{ .string = token.text } };
            },
            .lit_bool => {
                _ = self.advance();
                const bool_val = std.mem.eql(u8, token.text, "true");
                return AST.Expression{ .literal = AST.Literal{ .boolean = bool_val } };
            },
            .caret => {
                _ = self.advance();
                const expr = try self.parse_primary_expression();
                const deref_expr = try self.allocator.create(AST.Expression);
                deref_expr.* = expr;
                return AST.Expression{ .dereference = deref_expr };
            },
            .kw_while => {
                _ = self.advance(); 
                _ = try self.expect(.left_paren);
                const condition = try self.parse_expression();
                _ = try self.expect(.right_paren);
                
                // Check for optional after-code block
                var after_code: ?AST.Expression = null;
                if (self.current() != null and self.current().?.type == .left_paren) {
                    _ = self.advance();
                    after_code = try self.parse_expression();
                    _ = try self.expect(.right_paren);
                }
                
                _ = try self.expect(.kw_loop);
                _ = try self.expect(.left_brace);
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?*AST.Expression = null;
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance();
                        if (self.current() != null and self.current().?.type != .semicolon) {
                            const return_expr = try self.parse_expression();
                            const expr_ptr = try self.allocator.create(AST.Expression);
                            expr_ptr.* = return_expr;
                            body_expr = expr_ptr;
                        }
                        _ = self.match(.semicolon);
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var or self.current().?.type == .kw_func) {
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                _ = try self.expect(.right_brace);
                const condition_ptr = try self.allocator.create(AST.Expression);
                condition_ptr.* = condition;
                return AST.Expression{ .while_expr = AST.WhileExpression{
                    .condition = condition_ptr,
                    .body = AST.Block{
                        .statements = try body_statements.toOwnedSlice(),
                        .expr = body_expr,
                    },
                }};
            },
            .kw_for => {
                _ = self.advance(); 
                _ = try self.expect(.left_paren);
                
                // Parse variable names (can be multiple for hashmaps)
                var var_names = std.ArrayList([]const u8).init(self.allocator);
                const first_var = try self.expect(.identifier);
                try var_names.append(first_var.text);
                
                // Check for additional variables (key, value)
                while (self.match(.comma)) {
                    const next_var = try self.expect(.identifier);
                    try var_names.append(next_var.text);
                }
                
                _ = try self.expect(.kw_in); 
                const iterable = try self.parse_expression();
                _ = try self.expect(.right_paren);
                _ = try self.expect(.kw_loop);
                _ = try self.expect(.left_brace);
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?*AST.Expression = null;
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance();
                        if (self.current() != null and self.current().?.type != .semicolon) {
                            const return_expr = try self.parse_expression();
                            const expr_ptr = try self.allocator.create(AST.Expression);
                            expr_ptr.* = return_expr;
                            body_expr = expr_ptr;
                        }
                        _ = self.match(.semicolon);
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var or self.current().?.type == .kw_func) {
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                _ = try self.expect(.right_brace);
                const iterable_ptr = try self.allocator.create(AST.Expression);
                iterable_ptr.* = iterable;
                
                return AST.Expression{ .for_expr = AST.ForExpression{
                    .variables = try var_names.toOwnedSlice(),
                    .iterable = iterable_ptr,
                    .body = AST.Block{
                        .statements = try body_statements.toOwnedSlice(),
                        .expr = body_expr,
                    },
                    .is_inline = false,
                }};
            },
            .kw_loop => {
                _ = self.advance(); 
                _ = try self.expect(.left_brace);
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?*AST.Expression = null;
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance();
                        if (self.current() != null and self.current().?.type != .semicolon) {
                            const return_expr = try self.parse_expression();
                            const expr_ptr = try self.allocator.create(AST.Expression);
                            expr_ptr.* = return_expr;
                            body_expr = expr_ptr;
                        }
                        _ = self.match(.semicolon);
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var or self.current().?.type == .kw_func) {
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                _ = try self.expect(.right_brace);
                return AST.Expression{ .loop_expr = AST.LoopExpression{
                    .body = AST.Block{
                        .statements = try body_statements.toOwnedSlice(),
                        .expr = body_expr,
                    },
                }};
            },
            .kw_if => {
                _ = self.advance();
                const condition = try self.parse_expression();
                var has_braces = false;
                if (self.current() != null and self.current().?.type == .left_brace) {
                    _ = self.advance();
                    has_braces = true;
                }
                var then_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var then_expr: ?AST.Expression = null;
                if (has_braces) {
                    while (self.current() != null and self.current().?.type != .right_brace) {
                        if (self.current().?.type == .kw_return) {
                            _ = self.advance();
                            then_expr = try self.parse_expression();
                            _ = self.match(.semicolon);
                            break;
                        } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                            const decl = try self.parse_declaration();
                            try then_statements.append(AST.Statement{ .declaration = decl });
                        } else {
                            const stmt_expr = try self.parse_expression();
                            _ = self.match(.semicolon);
                            const stmt_ptr = try self.allocator.create(AST.Expression);
                            stmt_ptr.* = stmt_expr;
                            try then_statements.append(AST.Statement{ .expression = stmt_ptr });
                        }
                    }
                    _ = try self.expect(.right_brace);
                } else {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance();
                        then_expr = try self.parse_expression();
                    } else {
                        const stmt_expr = try self.parse_expression();
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try then_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                const then_block = AST.Block{
                    .statements = try then_statements.toOwnedSlice(),
                    .expr = if (then_expr) |expr| blk: {
                        const expr_ptr = try self.allocator.create(AST.Expression);
                        expr_ptr.* = expr;
                        break :blk expr_ptr;
                    } else null,
                };
                var else_block: ?AST.Block = null;
                if (self.current() != null and self.current().?.type == .kw_else) {
                    _ = self.advance(); 
                    if (self.current() != null and self.current().?.type == .kw_if) {
                        _ = self.advance(); 
                        const nested_condition = try self.parse_expression();
                        _ = try self.expect(.left_brace);
                        var nested_then_statements = std.ArrayList(AST.Statement).init(self.allocator);
                        var nested_then_expr: ?AST.Expression = null;
                        while (self.current() != null and self.current().?.type != .right_brace) {
                            if (self.current().?.type == .kw_return) {
                                _ = self.advance();
                                nested_then_expr = try self.parse_expression();
                                _ = self.match(.semicolon);
                                break;
                            } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                                const decl = try self.parse_declaration();
                                try nested_then_statements.append(AST.Statement{ .declaration = decl });
                            } else {
                                const stmt_expr = try self.parse_expression();
                                _ = self.match(.semicolon);
                                const stmt_ptr = try self.allocator.create(AST.Expression);
                                stmt_ptr.* = stmt_expr;
                                try nested_then_statements.append(AST.Statement{ .expression = stmt_ptr });
                            }
                        }
                        _ = try self.expect(.right_brace);
                        const nested_then_block = AST.Block{
                            .statements = try nested_then_statements.toOwnedSlice(),
                            .expr = if (nested_then_expr) |expr| blk: {
                                const expr_ptr = try self.allocator.create(AST.Expression);
                                expr_ptr.* = expr;
                                break :blk expr_ptr;
                            } else null,
                        };
                        var nested_else_block: ?AST.Block = null;
                        if (self.match(.kw_else)) {
                            _ = try self.expect(.left_brace);
                            var nested_else_statements = std.ArrayList(AST.Statement).init(self.allocator);
                            var nested_else_expr: ?AST.Expression = null;
                            while (self.current() != null and self.current().?.type != .right_brace) {
                                if (self.current().?.type == .kw_return) {
                                    _ = self.advance();
                                    nested_else_expr = try self.parse_expression();
                                    _ = self.match(.semicolon);
                                    break;
                                } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                                    const decl = try self.parse_declaration();
                                    try nested_else_statements.append(AST.Statement{ .declaration = decl });
                                } else {
                                    const stmt_expr = try self.parse_expression();
                                    _ = self.match(.semicolon);
                                    const stmt_ptr = try self.allocator.create(AST.Expression);
                                    stmt_ptr.* = stmt_expr;
                                    try nested_else_statements.append(AST.Statement{ .expression = stmt_ptr });
                                }
                            }
                            _ = try self.expect(.right_brace);
                            nested_else_block = AST.Block{
                                .statements = try nested_else_statements.toOwnedSlice(),
                                .expr = if (nested_else_expr) |expr| blk: {
                                    const expr_ptr = try self.allocator.create(AST.Expression);
                                    expr_ptr.* = expr;
                                    break :blk expr_ptr;
                                } else null,
                            };
                        }
                        const nested_condition_ptr = try self.allocator.create(AST.Expression);
                        nested_condition_ptr.* = nested_condition;
                        const nested_if_expr = AST.Expression{ .if_expr = AST.IfExpression{
                            .condition = nested_condition_ptr,
                            .then_block = nested_then_block,
                            .else_block = nested_else_block,
                        }};
                        const nested_if_ptr = try self.allocator.create(AST.Expression);
                        nested_if_ptr.* = nested_if_expr;
                        else_block = AST.Block{
                            .statements = &[_]AST.Statement{},
                            .expr = nested_if_ptr,
                        };
                    } else {
                        _ = try self.expect(.left_brace);
                        var else_statements = std.ArrayList(AST.Statement).init(self.allocator);
                        var else_expr: ?AST.Expression = null;
                        while (self.current() != null and self.current().?.type != .right_brace) {
                        if (self.current().?.type == .kw_return) {
                            _ = self.advance();
                            else_expr = try self.parse_expression();
                            _ = self.match(.semicolon);
                            break;
                        } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                            const decl = try self.parse_declaration();
                            try else_statements.append(AST.Statement{ .declaration = decl });
                        } else {
                            const stmt_expr = try self.parse_expression();
                            _ = self.match(.semicolon);
                            const stmt_ptr = try self.allocator.create(AST.Expression);
                            stmt_ptr.* = stmt_expr;
                            try else_statements.append(AST.Statement{ .expression = stmt_ptr });
                        }
                    }
                    _ = try self.expect(.right_brace);
                    else_block = AST.Block{
                        .statements = try else_statements.toOwnedSlice(),
                        .expr = if (else_expr) |expr| blk: {
                            const expr_ptr = try self.allocator.create(AST.Expression);
                            expr_ptr.* = expr;
                            break :blk expr_ptr;
                        } else null,
                    };
                }
                }
                const condition_ptr = try self.allocator.create(AST.Expression);
                condition_ptr.* = condition;
                return AST.Expression{ .if_expr = AST.IfExpression{
                    .condition = condition_ptr,
                    .then_block = then_block,
                    .else_block = else_block,
                }};
            },
            .kw_match => {
                _ = self.advance();
                const match_target = try self.expect(.identifier);
                const match_expr = AST.Expression{ .identifier = match_target.text };
                _ = try self.expect(.left_brace);
                var arms = std.ArrayList(AST.MatchArm).init(self.allocator);
                while (self.current() != null and self.current().?.type != .right_brace) {
                    var pattern: AST.Pattern = undefined;
                    const current_token = self.current() orelse return ParseError.UnexpectedEOF;
                    if (current_token.type == .underscore) {
                        _ = self.advance();
                        pattern = AST.Pattern{ .wildcard = {} };
                    } else if (current_token.type == .lit_int or current_token.type == .lit_float) {
                        const lit_token = self.advance().?;
                        const num = std.fmt.parseFloat(f64, lit_token.text) catch 0.0;
                        pattern = AST.Pattern{ .literal = AST.Literal{ .number = num } };
                    } else if (current_token.type == .identifier) {
                        const first_token = self.advance().?;
                        if (self.match(.double_colon)) {
                            var path_parts = std.ArrayList([]const u8).init(self.allocator);
                            try path_parts.append(first_token.text);
                            while (true) {
                                const part_token = try self.expect(.identifier);
                                try path_parts.append(part_token.text);
                                if (!self.match(.double_colon)) break;
                            }
                            const variant_name = path_parts.items[path_parts.items.len - 1];
                            if (self.match(.left_paren)) {
                                var params = std.ArrayList([]const u8).init(self.allocator);
                                if (self.current() != null and self.current().?.type != .right_paren) {
                                    while (true) {
                                        const param_token = try self.expect(.identifier);
                                        try params.append(param_token.text);
                                        if (!self.match(.comma)) break;
                                    }
                                }
                                _ = try self.expect(.right_paren);
                                pattern = AST.Pattern{ .enum_variant = AST.EnumVariantPattern{
                                    .variant_name = variant_name,
                                    .params = if (params.items.len > 0) try params.toOwnedSlice() else null,
                                }};
                            } else {
                                pattern = AST.Pattern{ .enum_variant = AST.EnumVariantPattern{
                                    .variant_name = variant_name,
                                    .params = null,
                                }};
                            }
                        } else {
                            pattern = AST.Pattern{ .identifier = first_token.text };
                        }
                    } else {
                        return ParseError.UnexpectedToken;
                    }
                    _ = try self.expect(.arrow);
                    const body_expr = try self.parse_expression();
                    const body_ptr = try self.allocator.create(AST.Expression);
                    body_ptr.* = body_expr;
                    try arms.append(AST.MatchArm{
                        .pattern = pattern,
                        .body = body_ptr,
                    });
                    _ = self.match(.comma);
                }
                _ = try self.expect(.right_brace);
                const match_expr_ptr = try self.allocator.create(AST.Expression);
                match_expr_ptr.* = match_expr;
                return AST.Expression{ .match_expr = AST.MatchExpression{
                    .expr = match_expr_ptr,
                    .arms = try arms.toOwnedSlice(),
                }};
            },
            .kw_inline => {
                _ = self.advance(); 
                if (self.current()) |next_token| {
                    if (next_token.type == .kw_for) {
                        _ = self.advance(); 
                        _ = try self.expect(.left_paren);
                        
                        // Parse variable names
                        var var_names = std.ArrayList([]const u8).init(self.allocator);
                        const first_var = try self.expect(.identifier);
                        try var_names.append(first_var.text);
                        
                        while (self.match(.comma)) {
                            const next_var = try self.expect(.identifier);
                            try var_names.append(next_var.text);
                        }
                        
                        _ = try self.expect(.kw_in);
                        const iterable = AST.Expression{ .identifier = (try self.expect(.identifier)).text };
                        _ = try self.expect(.right_paren);
                        _ = try self.expect(.kw_loop);
                        _ = try self.expect(.left_brace);
                        var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                        var body_expr: ?*AST.Expression = null;
                        while (self.current() != null and self.current().?.type != .right_brace) {
                            if (self.current().?.type == .kw_return) {
                                _ = self.advance();
                                if (self.current() != null and self.current().?.type != .semicolon) {
                                    const return_expr = try self.parse_expression();
                                    const expr_ptr = try self.allocator.create(AST.Expression);
                                    expr_ptr.* = return_expr;
                                    body_expr = expr_ptr;
                                }
                                _ = self.match(.semicolon);
                                break;
                            } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var or self.current().?.type == .kw_func) {
                                const decl = try self.parse_declaration();
                                try body_statements.append(AST.Statement{ .declaration = decl });
                            } else {
                                const stmt_expr = try self.parse_expression();
                                _ = self.match(.semicolon);
                                const stmt_ptr = try self.allocator.create(AST.Expression);
                                stmt_ptr.* = stmt_expr;
                                try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                            }
                        }
                        _ = try self.expect(.right_brace);
                        const iterable_ptr = try self.allocator.create(AST.Expression);
                        iterable_ptr.* = iterable;
                        
                        return AST.Expression{ .for_expr = AST.ForExpression{
                            .variables = try var_names.toOwnedSlice(),
                            .iterable = iterable_ptr,
                            .body = AST.Block{
                                .statements = try body_statements.toOwnedSlice(),
                                .expr = body_expr,
                            },
                            .is_inline = true,
                        }};
                    } else {
                        const func_name = try self.expect(.identifier);
                        _ = try self.expect(.left_paren);
                        var args = std.ArrayList(AST.Expression).init(self.allocator);
                        if (self.current() != null and self.current().?.type != .right_paren) {
                            while (true) {
                                try args.append(try self.parse_expression());
                                if (!self.match(.comma)) break;
                            }
                        }
                        _ = try self.expect(.right_paren);
                        return AST.Expression{ .function_call = AST.FunctionCall{
                            .name = func_name.text,
                            .args = try args.toOwnedSlice(),
                            .is_inline = true,
                            .type_args = null,
                        }};
                    }
                } else {
                    return ParseError.UnexpectedEOF;
                }
            },
            .identifier, .kw_spawn => {
                _ = self.advance();
                var expr = AST.Expression{ .identifier = token.text };
                while (true) {
                    if (self.match(.double_colon)) {
                        const variant_token = try self.expect(.identifier);
                        if (self.match(.left_paren)) {
                            var args = std.ArrayList(AST.Expression).init(self.allocator);
                            while (!self.match(.right_paren)) {
                                try args.append(try self.parse_expression());
                                if (!self.match(.comma)) break;
                            }
                            expr = AST.Expression{ .function_call = AST.FunctionCall{
                                .name = variant_token.text,
                                .args = try args.toOwnedSlice(),
                                .is_inline = false,
                                .type_args = null,
                            }};
                        } else {
                            const full_path = try std.fmt.allocPrint(self.allocator, "{s}::{s}", .{ token.text, variant_token.text });
                            expr = AST.Expression{ .identifier = full_path };
                        }
                    } else if (self.match(.dot)) {
                        const member_token = try self.expect(.identifier);
                        if (self.match(.left_paren)) {
                            var args = std.ArrayList(AST.Expression).init(self.allocator);
                            if (self.current() != null and self.current().?.type != .right_paren) {
                                while (true) {
                                    try args.append(try self.parse_expression());
                                    if (!self.match(.comma)) break;
                                }
                            }
                            _ = try self.expect(.right_paren);
                            const obj_ptr = try self.allocator.create(AST.Expression);
                            obj_ptr.* = expr;
                            expr = AST.Expression{ .method_call = AST.MethodCall{
                                .object = obj_ptr,
                                .method = member_token.text,
                                .args = try args.toOwnedSlice(),
                            }};
                        } else {
                            const obj_ptr = try self.allocator.create(AST.Expression);
                            obj_ptr.* = expr;
                            expr = AST.Expression{ .member_access = AST.MemberAccess{
                                .object = obj_ptr,
                                .member = member_token.text,
                            }};
                        }
                    } else if (self.match(.left_brace)) {
                        var args = std.ArrayList(AST.Expression).init(self.allocator);
                        while (!self.match(.right_brace)) {
                            _ = try self.expect(.identifier); 
                            _ = try self.expect(.colon);
                            const type_expr = try self.parse_type();
                            const type_name = switch (type_expr) {
                                .basic => |name| name,
                                .named => |name| name,
                                else => "unknown",
                            };
                            try args.append(AST.Expression{ .identifier = type_name });
                            if (!self.match(.comma)) break;
                        }
                        expr = AST.Expression{ .function_call = AST.FunctionCall{
                            .name = token.text,
                            .args = try args.toOwnedSlice(),
                            .is_inline = false,
                            .type_args = null,
                        }};
                        break;
                    } else if (self.current() != null and self.current().?.type == .less) {
                        const saved_pos = self.position;
                        _ = self.advance(); 
                        var is_generic = false;
                        var lookahead_pos = self.position;
                        while (lookahead_pos < self.tokens.len) {
                            const lookahead_token = self.tokens[lookahead_pos];
                            if (lookahead_token.type == .greater) {
                                if (lookahead_pos + 1 < self.tokens.len) {
                                    const next_token = self.tokens[lookahead_pos + 1];
                                    if (next_token.type == .double_colon or next_token.type == .left_paren) {
                                        is_generic = true;
                                    }
                                }
                                break;
                            } else if (lookahead_token.type == .identifier or 
                                      lookahead_token.type == .kw_i32 or lookahead_token.type == .kw_f32 or 
                                      lookahead_token.type == .kw_i8 or lookahead_token.type == .kw_i16 or 
                                      lookahead_token.type == .kw_i64 or lookahead_token.type == .kw_u8 or 
                                      lookahead_token.type == .kw_u16 or lookahead_token.type == .kw_u32 or 
                                      lookahead_token.type == .kw_u64 or lookahead_token.type == .comma) {
                                lookahead_pos += 1;
                            } else {
                                break;
                            }
                        }
                        if (is_generic) {
                            var type_args = std.ArrayList(AST.Type).init(self.allocator);
                            if (self.current() != null and self.current().?.type != .greater) {
                                try type_args.append(try self.parse_type());
                                while (self.match(.comma)) {
                                    try type_args.append(try self.parse_type());
                                }
                            }
                            _ = try self.expect(.greater);
                            if (self.current() != null and self.current().?.type == .double_colon) {
                                _ = self.advance(); 
                                const variant_token = try self.expect(.identifier);
                                if (self.match(.left_paren)) {
                                    var args = std.ArrayList(AST.Expression).init(self.allocator);
                                    while (!self.match(.right_paren)) {
                                        try args.append(try self.parse_expression());
                                        if (!self.match(.comma)) break;
                                    }
                                    const enum_variant_name = try std.fmt.allocPrint(self.allocator, "{s}::{s}", .{ token.text, variant_token.text });
                                    expr = AST.Expression{ .function_call = AST.FunctionCall{
                                        .name = enum_variant_name,
                                        .args = try args.toOwnedSlice(),
                                        .is_inline = false,
                                        .type_args = try type_args.toOwnedSlice(),
                                    }};
                                } else {
                                    const enum_variant_name = try std.fmt.allocPrint(self.allocator, "{s}::{s}", .{ token.text, variant_token.text });
                                    expr = AST.Expression{ .function_call = AST.FunctionCall{
                                        .name = enum_variant_name,
                                        .args = &[_]AST.Expression{},
                                        .is_inline = false,
                                        .type_args = try type_args.toOwnedSlice(),
                                    }};
                                }
                                break;
                            } else if (self.current() != null and self.current().?.type == .left_paren) {
                                _ = self.advance(); 
                                var args = std.ArrayList(AST.Expression).init(self.allocator);
                                if (self.current() != null and self.current().?.type != .right_paren) {
                                    while (true) {
                                        try args.append(try self.parse_expression());
                                        if (!self.match(.comma)) break;
                                    }
                                }
                                _ = try self.expect(.right_paren);
                                expr = AST.Expression{ .function_call = AST.FunctionCall{
                                    .name = token.text,
                                    .args = try args.toOwnedSlice(),
                                    .is_inline = false,
                                    .type_args = try type_args.toOwnedSlice(),
                                }};
                                break;
                            } else {
                                break;
                            }
                        } else {
                            self.position = saved_pos;
                            break;
                        }
                    } else if (self.match(.left_paren)) {
                        var args = std.ArrayList(AST.Expression).init(self.allocator);
                        if (self.current() != null and self.current().?.type != .right_paren) {
                            while (true) {
                                try args.append(try self.parse_expression());
                                if (!self.match(.comma)) break;
                            }
                        }
                        _ = try self.expect(.right_paren);
                        expr = AST.Expression{ .function_call = AST.FunctionCall{
                            .name = token.text,
                            .args = try args.toOwnedSlice(),
                            .is_inline = false,
                            .type_args = null,
                        }};
                        break;
                    } else {
                        break;
                    }
                }
                return expr;
            },
            .left_paren => {
                _ = self.advance(); 
                var tuple_elements = std.ArrayList(AST.Expression).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        try tuple_elements.append(try self.parse_expression());
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                return AST.Expression{ .tuple_literal = try tuple_elements.toOwnedSlice() };
            },
            .left_bracket => {
                _ = self.advance();
                var elements = std.ArrayList(AST.Expression).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_bracket) {
                    while (true) {
                        try elements.append(try self.parse_expression());
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_bracket);
                return AST.Expression{ .literal = AST.Literal{ .array = try elements.toOwnedSlice() } };
            },
            .left_brace => {
                _ = self.advance();
                if (self.current()) |current_token| {
                    if (current_token.type == .identifier) {
                        const next_pos = self.position + 1;
                        if (next_pos < self.tokens.len and self.tokens[next_pos].type == .assign) {
                            var fields = std.ArrayList(AST.StructField).init(self.allocator);
                            while (self.current() != null and self.current().?.type != .right_brace) {
                                const field_name = try self.expect(.identifier);
                                _ = try self.expect(.assign);
                                const field_value = try self.parse_expression();
                                try fields.append(AST.StructField{
                                    .name = field_name.text,
                                    .value = field_value,
                                });
                                if (!self.match(.comma)) break;
                            }
                            _ = try self.expect(.right_brace);
                            return AST.Expression{ .struct_literal = AST.StructLiteral{
                                .fields = try fields.toOwnedSlice(),
                            }};
                        }
                    }
                }
                var elements = std.ArrayList(AST.Expression).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_brace) {
                    while (true) {
                        try elements.append(try self.parse_expression());
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_brace);
                return AST.Expression{ .literal = AST.Literal{ .array = try elements.toOwnedSlice() } };
            },
            .at_sign => {
                _ = self.advance(); 
                const macro_name = try self.expect(.identifier);
                _ = try self.expect(.left_paren);
                var args = std.ArrayList(AST.Expression).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        try args.append(try self.parse_expression());
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                return AST.Expression{ .macro_call = AST.MacroCall{
                    .name = macro_name.text,
                    .args = try args.toOwnedSlice(),
                }};
            },
            else => return ParseError.UnexpectedToken,
        }
    }
    fn parse_type(self: *Parser) ParseError!AST.Type {
        const token = self.current() orelse return ParseError.UnexpectedEOF;
        switch (token.type) {
            .left_paren => {
                _ = self.advance(); 
                var tuple_types = std.ArrayList(AST.Type).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        try tuple_types.append(try self.parse_type());
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                return AST.Type{ .tuple = try tuple_types.toOwnedSlice() };
            },
            .caret => {
                _ = self.advance();
                const inner_type = try self.parse_type();
                const ptr_type = try self.allocator.create(AST.Type);
                ptr_type.* = inner_type;
                return AST.Type{ .pointer = ptr_type };
            },
            .ampersand => {
                _ = self.advance();
                const inner_type = try self.parse_type();
                const ref_type = try self.allocator.create(AST.Type);
                ref_type.* = inner_type;
                return AST.Type{ .reference = ref_type };
            },
            .dollar => {
                _ = self.advance();
                const process_name = try self.expect(.identifier);
                return AST.Type{ .process = process_name.text };
            },
            .kw_i8, .kw_i16, .kw_i32, .kw_i64, .kw_i128, .kw_u8, .kw_u16, .kw_u32, .kw_u64, .kw_u128,
            .kw_f16, .kw_f32, .kw_f64, .kw_f128, .kw_bool, .kw_char, .kw_str, .kw_strA, .kw_str16, .kw_str32,
            .kw_string, .kw_stringA, .kw_string16, .kw_string32 => {
                _ = self.advance();
                const base_type = AST.Type{ .basic = token.text };
                
                var has_question = false;
                var has_exclamation = false;
                
                while (true) {
                    if (self.match(.question_mark) and !has_question) {
                        has_question = true;
                    } else if (self.match(.exclamation) and !has_exclamation) {
                        has_exclamation = true;
                    } else {
                        break;
                    }
                }
                
                if (has_question and has_exclamation) {
                    const inner_type = try self.allocator.create(AST.Type);
                    inner_type.* = base_type;
                    return AST.Type{ .nullable_error = inner_type };
                } else if (has_question) {
                    const nullable_type = try self.allocator.create(AST.Type);
                    nullable_type.* = base_type;
                    return AST.Type{ .nullable = nullable_type };
                } else if (has_exclamation) {
                    const error_type = try self.allocator.create(AST.Type);
                    error_type.* = base_type;
                    return AST.Type{ .error_type = error_type };
                }
                
                return base_type;
            },

            .identifier => {
                _ = self.advance();
                var base_type: AST.Type = undefined;
                if (self.match(.less)) {
                    var args = std.ArrayList(AST.Type).init(self.allocator);
                    if (self.current() != null and self.current().?.type != .greater) {
                        while (true) {
                            try args.append(try self.parse_type());
                            if (!self.match(.comma)) break;
                        }
                    }
                    _ = try self.expect(.greater);
                    base_type = AST.Type{ .generic = AST.GenericType{
                        .name = token.text,
                        .args = try args.toOwnedSlice(),
                    }};
                } else {
                    base_type = AST.Type{ .named = token.text };
                }
                
                var has_question = false;
                var has_exclamation = false;
                
                while (true) {
                    if (self.match(.question_mark) and !has_question) {
                        has_question = true;
                    } else if (self.match(.exclamation) and !has_exclamation) {
                        has_exclamation = true;
                    } else {
                        break;
                    }
                }
                
                if (has_question and has_exclamation) {
                    const inner_type = try self.allocator.create(AST.Type);
                    inner_type.* = base_type;
                    return AST.Type{ .nullable_error = inner_type };
                } else if (has_question) {
                    const nullable_type = try self.allocator.create(AST.Type);
                    nullable_type.* = base_type;
                    return AST.Type{ .nullable = nullable_type };
                } else if (has_exclamation) {
                    const error_type = try self.allocator.create(AST.Type);
                    error_type.* = base_type;
                    return AST.Type{ .error_type = error_type };
                }
                
                return base_type;
            },
            else => {
                _ = self.advance();
                return AST.Type{ .named = token.text };
            },
        }
    }
    fn parse_trait(self: *Parser) ParseError!AST.TraitDef {
        _ = try self.expect(.kw_trait);
        const trait_name = try self.expect(.identifier);
        _ = try self.expect(.left_brace);
        var methods = std.ArrayList(AST.FunctionDef).init(self.allocator);
        while (self.current() != null and self.current().?.type != .right_brace) {
            const token = self.current() orelse return ParseError.UnexpectedEOF;
            switch (token.type) {
                .kw_func => {
                    _ = self.advance();
                    const method_name = try self.expect(.identifier);
                    _ = try self.expect(.left_paren);
                    var params = std.ArrayList(AST.Parameter).init(self.allocator);
                    if (self.current() != null and self.current().?.type != .right_paren) {
                        while (true) {
                            const param_name = try self.expect(.identifier);
                            var param_type = AST.Type{ .named = "unknown" };
                            if (self.match(.colon)) {
                                param_type = try self.parse_type();
                            }
                            try params.append(AST.Parameter{
                                .name = param_name.text,
                                .param_type = param_type,
                                .is_mut = false,
                                .is_ref = false,
                                .is_copy = false,
                            });
                            if (!self.match(.comma)) break;
                        }
                    }
                    _ = try self.expect(.right_paren);
                    var return_type: ?AST.Type = null;
                    if (self.current() != null and 
                        self.current().?.type != .left_brace and 
                        self.current().?.type != .semicolon) {
                        return_type = try self.parse_type();
                    }
                    var body: ?AST.Block = null;
                    if (self.match(.left_brace)) {
                        var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                        var body_expr: ?*AST.Expression = null;
                        while (self.current() != null and self.current().?.type != .right_brace) {
                            if (self.current().?.type == .kw_return) {
                                _ = self.advance();
                                if (self.current() != null and self.current().?.type != .semicolon) {
                                    const return_expr = try self.parse_expression();
                                    const expr_ptr = try self.allocator.create(AST.Expression);
                                    expr_ptr.* = return_expr;
                                    body_expr = expr_ptr;
                                }
                                _ = self.match(.semicolon);
                                break;
                            } else {
                                const stmt_expr = try self.parse_expression();
                                _ = self.match(.semicolon);
                                const stmt_ptr = try self.allocator.create(AST.Expression);
                                stmt_ptr.* = stmt_expr;
                                try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                            }
                        }
                        _ = try self.expect(.right_brace);
                        body = AST.Block{
                            .statements = try body_statements.toOwnedSlice(),
                            .expr = body_expr,
                        };
                    } else {
                        _ = self.match(.semicolon);
                    }
                    try methods.append(AST.FunctionDef{
                        .name = method_name.text,
                        .parameters = try params.toOwnedSlice(),
                        .return_type = return_type orelse AST.Type{ .named = "void" },
                        .body = body orelse AST.Block{ .statements = &[_]AST.Statement{}, .expr = null },
                        .match = false,
                    });
                },
                .kw_const => {
                    _ = self.advance();
                    const const_name = try self.expect(.identifier);
                    _ = try self.expect(.colon);
                    const const_type = try self.parse_type();
                    _ = self.match(.semicolon);
                    try methods.append(AST.FunctionDef{
                        .name = const_name.text,
                        .parameters = &[_]AST.Parameter{},
                        .return_type = const_type,
                        .body = AST.Block{ .statements = &[_]AST.Statement{}, .expr = null },
                        .match = false,
                    });
                },
                else => {
                    std.debug.print("Error: Only functions and constants are allowed in trait body, found: {s}\n", .{@tagName(token.type)});
                    return ParseError.UnexpectedToken;
                },
            }
        }
        _ = try self.expect(.right_brace);
        return AST.TraitDef{
            .name = trait_name.text,
            .methods = try methods.toOwnedSlice(),
        };
    }
    fn parse_impl_block(self: *Parser) ParseError!AST.ImplBlock {
        _ = try self.expect(.impl);
        var trait_name: ?[]const u8 = null;
        const first_name = try self.expect(.identifier);
        var type_name: []const u8 = undefined;
        if (self.current() != null and self.current().?.type == .kw_for) {
            _ = self.advance(); 
            trait_name = first_name.text;
            const type_token = try self.expect(.identifier);
            type_name = type_token.text;
        } else {
            type_name = first_name.text;
        }
        _ = try self.expect(.left_brace);
        var methods = std.ArrayList(AST.FunctionDef).init(self.allocator);
        while (!self.match(.right_brace)) {
            const func_def = try self.parse_function_def();
            try methods.append(func_def);
        }
        return AST.ImplBlock{
            .trait_name = trait_name,
            .type_name = type_name,
            .methods = try methods.toOwnedSlice(),
        };
    }
    fn parse_implement_block(self: *Parser) ParseError!AST.ImplBlock {
        _ = try self.expect(.kw_implement);
        const trait_token = try self.expect(.identifier);
        _ = try self.expect(.kw_for);
        const type_token = try self.expect(.identifier);
        _ = try self.expect(.left_brace);
        var methods = std.ArrayList(AST.FunctionDef).init(self.allocator);
        while (!self.match(.right_brace)) {
            const func_def = try self.parse_function_def();
            try methods.append(func_def);
        }
        return AST.ImplBlock{
            .trait_name = trait_token.text,
            .type_name = type_token.text,
            .methods = try methods.toOwnedSlice(),
        };
    }
    fn validate_all_implementations(self: *Parser, program: AST.Program) ParseError!void {
        for (program.items) |item| {
            if (item == .impl_block) {
                try self.validate_trait_implementation(item.impl_block);
            }
        }
    }
    fn validate_fum_grouping(self: *Parser, program: AST.Program) ParseError!void {
        var fum_positions = std.HashMap([]const u8, std.ArrayList(usize), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(self.allocator);
        var fm_positions = std.HashMap([]const u8, std.ArrayList(usize), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(self.allocator);
        defer {
            var iterator = fum_positions.iterator();
            while (iterator.next()) |entry| {
                entry.value_ptr.deinit();
            }
            fum_positions.deinit();
            
            var fm_iterator = fm_positions.iterator();
            while (fm_iterator.next()) |entry| {
                entry.value_ptr.deinit();
            }
            fm_positions.deinit();
        }
        
        for (program.items, 0..) |item, i| {
            if (item == .declaration and item.declaration.kind == .fun_decl) {
                const decl = item.declaration;
                
                // Check for fm functions (is_match = true)
                if (decl.is_match) {
                    const result = fm_positions.getOrPut(decl.name) catch return ParseError.OutOfMemory;
                    if (!result.found_existing) {
                        result.value_ptr.* = std.ArrayList(usize).init(self.allocator);
                    }
                    result.value_ptr.append(i) catch return ParseError.OutOfMemory;
                }
                
                // Check for fum functions (attributes)
                if (decl.attributes) |attrs| {
                    for (attrs) |attr| {
                        if (std.mem.eql(u8, attr.name, "fum")) {
                            const result = fum_positions.getOrPut(decl.name) catch return ParseError.OutOfMemory;
                            if (!result.found_existing) {
                                result.value_ptr.* = std.ArrayList(usize).init(self.allocator);
                            }
                            result.value_ptr.append(i) catch return ParseError.OutOfMemory;
                            break;
                        }
                    }
                }
            }
        }
        
        // Validate fm function grouping
        var fm_iterator = fm_positions.iterator();
        while (fm_iterator.next()) |entry| {
            const positions = entry.value_ptr.items;
            if (positions.len > 1) {
                for (positions[1..], 1..) |pos, idx| {
                    if (pos != positions[idx - 1] + 1) {
                        std.debug.print("Error: FM functions with name '{s}' must be grouped together\n", .{entry.key_ptr.*});
                        return ParseError.UnexpectedToken;
                    }
                }
            }
        }
        
        // Validate fum function grouping
        var iterator = fum_positions.iterator();
        while (iterator.next()) |entry| {
            const positions = entry.value_ptr.items;
            if (positions.len > 1) {
                for (positions[1..], 1..) |pos, idx| {
                    if (pos != positions[idx - 1] + 1) {
                        std.debug.print("Error: Fum functions with name '{s}' must be grouped together\n", .{entry.key_ptr.*});
                        return ParseError.UnexpectedToken;
                    }
                }
            }
        }
    }
    fn validate_trait_implementation(self: *Parser, impl_block: AST.ImplBlock) ParseError!void {
        if (impl_block.trait_name == null) return; 
        var trait_def: ?AST.TraitDef = null;
        for (self.trait_definitions.items) |trait| {
            if (std.mem.eql(u8, trait.name, impl_block.trait_name.?)) {
                trait_def = trait;
                break;
            }
        }
        if (trait_def == null) {
            std.debug.print("Error: Trait '{s}' not found\n", .{impl_block.trait_name.?});
            return ParseError.UnexpectedToken;
        }
        for (trait_def.?.methods) |trait_method| {
            const is_constant = trait_method.parameters.len == 0 and switch (trait_method.return_type) {
                .named => |name| !std.mem.eql(u8, name, "void"),
                else => true,
            };
            if (is_constant) {
                continue; 
            }
            const has_default = trait_method.body.statements.len > 0 or trait_method.body.expr != null;
            if (!has_default) {
                var found = false;
                for (impl_block.methods) |impl_method| {
                    if (std.mem.eql(u8, impl_method.name, trait_method.name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    std.debug.print("Error: Missing implementation for method '{s}' in trait '{s}' for type '{s}'\n", .{ trait_method.name, impl_block.trait_name.?, impl_block.type_name });
                }
            }
        }
    }
    fn parse_struct(self: *Parser) ParseError!AST.StructDef {
        _ = try self.expect(.kw_struct);
        const struct_name = try self.expect(.identifier);
        var generic_params: ?[][]const u8 = null;
        if (self.match(.less)) {
            var params = std.ArrayList([]const u8).init(self.allocator);
            const param = try self.expect(.identifier);
            const param_text = param.text;

            try params.append(param_text);
            while (self.match(.comma)) {
                const next_param = try self.expect(.identifier);
                const next_param_text = next_param.text;

                try params.append(next_param_text);
            }
            _ = try self.expect(.greater);
            generic_params = try params.toOwnedSlice();
        }
        _ = try self.expect(.left_brace);
        var fields = std.ArrayList(AST.Field).init(self.allocator);
        var methods = std.ArrayList(AST.FunctionDef).init(self.allocator);
        
        while (!self.match(.right_brace)) {
            if (self.current()) |token| {
                if (token.type == .kw_func) {
                    const method = try self.parse_function_def();
                    try methods.append(method);
                } else {
                    const is_private = self.match(.kw_priv);
                    const field_name = try self.expect(.identifier);
                    _ = try self.expect(.colon);
                    const field_type = try self.parse_type();
                    _ = self.match(.comma);
                    try fields.append(AST.Field{
                        .name = field_name.text,
                        .type_annotation = field_type,
                        .initializer = null,
                        .visibility = if (is_private) AST.Visibility.private else AST.Visibility.public,
                    });
                }
            } else {
                break;
            }
        }
        
        var implemented_traits: ?[][]const u8 = null;
        if (self.match(.kw_implement)) {
            var traits = std.ArrayList([]const u8).init(self.allocator);
            const first_trait = try self.expect(.identifier);
            try traits.append(first_trait.text);
            
            while (self.match(.comma)) {
                const trait_name = try self.expect(.identifier);
                try traits.append(trait_name.text);
            }
            implemented_traits = try traits.toOwnedSlice();
        }
        
        return AST.StructDef{
            .name = struct_name.text,
            .generic_params = generic_params,
            .fields = try fields.toOwnedSlice(),
            .methods = if (methods.items.len > 0) try methods.toOwnedSlice() else null,
            .implemented_traits = implemented_traits,
        };
    }
    fn parse_union(self: *Parser) ParseError!AST.EnumDef {
        _ = try self.expect(.kw_union);
        const union_name = try self.expect(.identifier);
        var generic_params: ?[][]const u8 = null;
        if (self.match(.less)) {
            var params = std.ArrayList([]const u8).init(self.allocator);
            const param = try self.expect(.identifier);
            try params.append(param.text);
            while (self.match(.comma)) {
                const next_param = try self.expect(.identifier);
                try params.append(next_param.text);
            }
            _ = try self.expect(.greater);
            generic_params = try params.toOwnedSlice();
        }
        _ = try self.expect(.left_brace);
        var variants = std.ArrayList(AST.EnumVariant).init(self.allocator);
        while (!self.match(.right_brace)) {
            const variant_name = try self.expect(.identifier);
            _ = try self.expect(.colon);
            const variant_type = try self.parse_type();
            _ = self.match(.comma);
            const field_types = try self.allocator.alloc(AST.Type, 1);
            field_types[0] = variant_type;
            try variants.append(AST.EnumVariant{
                .name = variant_name.text,
                .fields = field_types,
            });
        }
        return AST.EnumDef{
            .name = union_name.text,
            .generic_params = generic_params,
            .variants = try variants.toOwnedSlice(),
        };
    }
    fn parse_enum(self: *Parser) ParseError!AST.EnumDef {
        _ = try self.expect(.kw_enum);
        const enum_name = try self.expect(.identifier);
        var generic_params: ?[][]const u8 = null;
        if (self.match(.less)) {
            var params = std.ArrayList([]const u8).init(self.allocator);
            const param = try self.expect(.identifier);
            try params.append(param.text);
            while (self.match(.comma)) {
                const next_param = try self.expect(.identifier);
                try params.append(next_param.text);
            }
            _ = try self.expect(.greater);
            generic_params = try params.toOwnedSlice();
        }
        _ = try self.expect(.left_brace);
        var variants = std.ArrayList(AST.EnumVariant).init(self.allocator);
        while (!self.match(.right_brace)) {
            const variant_name = try self.expect(.identifier);
            var fields: ?[]AST.Type = null;
            if (self.match(.left_paren)) {
                var field_types = std.ArrayList(AST.Type).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        if (self.current()) |token| {
                            if (token.type == .identifier) {
                                const next_pos = self.position + 1;
                                if (next_pos < self.tokens.len and self.tokens[next_pos].type == .colon) {
                                    _ = self.advance(); 
                                    _ = self.advance(); 
                                }
                            }
                        }
                        try field_types.append(try self.parse_type());
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                fields = try field_types.toOwnedSlice();
            }
            _ = self.match(.comma);
            try variants.append(AST.EnumVariant{
                .name = variant_name.text,
                .fields = fields,
            });
        }
        return AST.EnumDef{
            .name = enum_name.text,
            .generic_params = generic_params,
            .variants = try variants.toOwnedSlice(),
        };
    }
    fn parse_typealias(self: *Parser) ParseError!AST.Declaration {
        _ = try self.expect(.kw_typealias);
        const alias_name = try self.expect(.identifier);
        const target_type = try self.parse_type();
        _ = self.match(.semicolon);
        return AST.Declaration{
            .kind = .typealias_decl,
            .is_mut = false,
            .is_match = false,
            .name = alias_name.text,
            .type_annotation = target_type,
            .initializer = null,
            .fields = null,
            .attributes = null,
            .generic_params = null,
        };
    }
    fn parse_generic(self: *Parser) ParseError!AST.GenericDef {
        _ = try self.expect(.kw_generic);
        const generic_name = try self.expect(.identifier);
        _ = try self.expect(.left_brace);
        var constraints = std.ArrayList([]const u8).init(self.allocator);
        while (!self.match(.right_brace)) {
            const constraint_token = self.current() orelse return ParseError.UnexpectedEOF;
            const constraint_text = switch (constraint_token.type) {
                .identifier, .kw_i8, .kw_i16, .kw_i32, .kw_i64, .kw_i128,
                .kw_u8, .kw_u16, .kw_u32, .kw_u64, .kw_u128,
                .kw_f16, .kw_f32, .kw_f64, .kw_f128,
                .kw_bool, .kw_char, .kw_str => constraint_token.text,
                else => return ParseError.UnexpectedToken,
            };
            _ = self.advance();
            try constraints.append(constraint_text);
            if (!self.match(.comma)) break;
        }
        return AST.GenericDef{
            .name = generic_name.text,
            .constraints = try constraints.toOwnedSlice(),
        };
    }
    fn convert_params_to_fields(self: *Parser, params: []AST.Parameter, generic_params: ?[][]const u8) ![]AST.Field {
        var all_fields = std.ArrayList(AST.Field).init(self.allocator);
        if (generic_params) |gen_params| {
            for (gen_params) |gen_param| {
                try all_fields.append(AST.Field{
                    .name = gen_param,
                    .type_annotation = AST.Type{ .named = "<generic>" },
                    .initializer = null,
                    .visibility = AST.Visibility.private,
                });
            }
        }
        for (params) |param| {
            try all_fields.append(AST.Field{
                .name = param.name,
                .type_annotation = param.param_type,
                .initializer = null,
                .visibility = AST.Visibility.private,
            });
        }
        return try all_fields.toOwnedSlice();
    }
    fn parse_function_def(self: *Parser) ParseError!AST.FunctionDef {
        _ = try self.expect(.kw_func);
        const name_token = try self.expect(.identifier);
        if (self.match(.less)) {
            var depth: u32 = 1;
            while (self.current() != null and depth > 0) {
                const token = self.advance().?;
                if (token.type == .less) {
                    depth += 1;
                } else if (token.type == .greater) {
                    depth -= 1;
                }
            }
        }
        _ = try self.expect(.left_paren);
        var params = std.ArrayList(AST.Parameter).init(self.allocator);
        if (self.current() != null and self.current().?.type != .right_paren) {
            while (true) {
                const param_name = try self.expect(.identifier);
                var param_type = AST.Type{ .named = "unknown" };
                if (self.match(.colon)) {
                    param_type = try self.parse_type();
                }
                try params.append(AST.Parameter{
                    .name = param_name.text,
                    .param_type = param_type,
                    .is_mut = false,
                    .is_ref = false,
                    .is_copy = false,
                });
                if (!self.match(.comma)) break;
            }
        }
        _ = try self.expect(.right_paren);
        var return_type = AST.Type{ .named = "void" };
        if (self.current() != null and self.current().?.type != .left_brace) {
            return_type = try self.parse_type();
        }
        _ = try self.expect(.left_brace);
        var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
        var body_expr: ?*AST.Expression = null;
        while (self.current() != null and self.current().?.type != .right_brace) {
            if (self.current().?.type == .kw_return) {
                _ = self.advance();
                if (self.current() != null and self.current().?.type != .semicolon) {
                    const return_expr = try self.parse_expression();
                    const expr_ptr = try self.allocator.create(AST.Expression);
                    expr_ptr.* = return_expr;
                    body_expr = expr_ptr;
                }
                _ = self.match(.semicolon);
                break;
            } else {
                const stmt_expr = try self.parse_expression();
                _ = self.match(.semicolon);
                const stmt_ptr = try self.allocator.create(AST.Expression);
                stmt_ptr.* = stmt_expr;
                try body_statements.append(AST.Statement{ .expression = stmt_ptr });
            }
        }
        _ = try self.expect(.right_brace);
        return AST.FunctionDef{
            .name = name_token.text,
            .parameters = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = AST.Block{ 
                .statements = try body_statements.toOwnedSlice(), 
                .expr = body_expr 
            },
            .match = false,
        };
    }
};
pub fn parse(allocator: Allocator, tokens: []const Token) ParseError!AST.Program {
    var parser = Parser.init(tokens, allocator);
    defer parser.trait_definitions.deinit();
    return parser.parse_program();
}
pub fn cleanup_ast(allocator: Allocator, program: AST.Program) void {
    for (program.items) |item| {
        cleanup_ast_node(allocator, item);
    }
    allocator.free(program.items);
}
fn cleanup_ast_node(allocator: Allocator, node: AST.ASTNode) void {
    switch (node) {
        .declaration => |decl| {
            cleanup_type_if_exists(allocator, decl.type_annotation);
            cleanup_expression_if_exists(allocator, decl.initializer);
            if (decl.fields) |fields| {
                for (fields) |field| {
                    cleanup_type(allocator, field.type_annotation);
                    cleanup_expression_if_exists(allocator, field.initializer);
                }
                allocator.free(fields);
            }
            if (decl.generic_params) |generic_params| {
                allocator.free(generic_params);
            }
            if (decl.attributes) |attrs| {
                for (attrs) |attr| {
                    for (attr.args) |arg| {
                        cleanup_expression(allocator, arg);
                    }
                    allocator.free(attr.args);
                }
                allocator.free(attrs);
            }
        },
        .struct_def => |struct_def| {
            if (struct_def.generic_params) |params| {
                for (params) |param| {
                    if (std.mem.indexOf(u8, param, ":")) |_| {
                        allocator.free(param);
                    }
                }
                allocator.free(params);
            }
            for (struct_def.fields) |field| {
                cleanup_type(allocator, field.type_annotation);
                cleanup_expression_if_exists(allocator, field.initializer);
            }
            allocator.free(struct_def.fields);
            if (struct_def.methods) |methods| {
                for (methods) |method| {
                    for (method.parameters) |param| {
                        cleanup_type(allocator, param.param_type);
                    }
                    allocator.free(method.parameters);
                    cleanup_type(allocator, method.return_type);
                    cleanup_block(allocator, method.body);
                }
                allocator.free(methods);
            }
            if (struct_def.implemented_traits) |traits| {
                allocator.free(traits);
            }
        },
        .trait_def => |trait_def| {
            for (trait_def.methods) |method| {
                allocator.free(method.parameters);
                cleanup_block(allocator, method.body);
            }
            allocator.free(trait_def.methods);
        },
        .impl_block => |impl_block| {
            for (impl_block.methods) |method| {
                allocator.free(method.parameters);
                cleanup_block(allocator, method.body);
            }
            allocator.free(impl_block.methods);
        },
        .enum_def => |enum_def| {
            if (enum_def.generic_params) |params| {
                for (params) |param| {
                    if (std.mem.indexOf(u8, param, ":")) |_| {
                        allocator.free(param);
                    }
                }
                allocator.free(params);
            }
            for (enum_def.variants) |variant| {
                if (variant.fields) |fields| {
                    for (fields) |field_type| {
                        cleanup_type(allocator, field_type);
                    }
                    allocator.free(fields);
                }
            }
            allocator.free(enum_def.variants);
        },
        .expression => |expr| {
            cleanup_expression(allocator, expr);
        },
        else => {},
    }
}
fn cleanup_type_if_exists(allocator: Allocator, type_opt: ?AST.Type) void {
    if (type_opt) |t| cleanup_type(allocator, t);
}
fn cleanup_type(allocator: Allocator, type_node: AST.Type) void {
    switch (type_node) {
        .pointer => |ptr| {
            cleanup_type(allocator, ptr.*);
            allocator.destroy(ptr);
        },
        .reference => |ref| {
            cleanup_type(allocator, ref.*);
            allocator.destroy(ref);
        },
        .array => |arr| {
            cleanup_type(allocator, arr.element_type.*);
            allocator.destroy(arr.element_type);
            if (arr.size) |size| {
                cleanup_expression(allocator, size.*);
                allocator.destroy(size);
            }
        },
        .generic => |gen| {
            for (gen.args) |arg| {
                cleanup_type(allocator, arg);
            }
            allocator.free(gen.args);
        },
        .tuple => |types| {
            for (types) |tuple_type| {
                cleanup_type(allocator, tuple_type);
            }
            allocator.free(types);
        },
        .nullable => |nullable| {
            cleanup_type(allocator, nullable.*);
            allocator.destroy(nullable);
        },
        .error_type => |error_type| {
            cleanup_type(allocator, error_type.*);
            allocator.destroy(error_type);
        },
        .nullable_error => |nullable_error| {
            cleanup_type(allocator, nullable_error.*);
            allocator.destroy(nullable_error);
        },
        else => {},
    }
}
fn cleanup_expression_if_exists(allocator: Allocator, expr_opt: ?AST.Expression) void {
    if (expr_opt) |e| cleanup_expression(allocator, e);
}
fn cleanup_expression(allocator: Allocator, expr: AST.Expression) void {
    switch (expr) {
        .binary_op => |binop| {
            cleanup_expression(allocator, binop.left.*);
            cleanup_expression(allocator, binop.right.*);
            allocator.destroy(binop.left);
            allocator.destroy(binop.right);
        },
        .function_call => |call| {
            for (call.args) |arg| {
                cleanup_expression(allocator, arg);
            }
            allocator.free(call.args);
            if (call.type_args) |type_args| {
                for (type_args) |type_arg| {
                    cleanup_type(allocator, type_arg);
                }
                allocator.free(type_args);
            }
        },
        .block_expr => |block| {
            cleanup_block(allocator, block);
        },
        .struct_literal => |struct_lit| {
            for (struct_lit.fields) |field| {
                cleanup_expression(allocator, field.value);
            }
            allocator.free(struct_lit.fields);
        },
        .literal => |lit| {
            switch (lit) {
                .array => |elements| {
                    for (elements) |element| {
                        cleanup_expression(allocator, element);
                    }
                    allocator.free(elements);
                },
                else => {},
            }
        },
        .member_access => |member| {
            cleanup_expression(allocator, member.object.*);
            allocator.destroy(member.object);
        },
        .method_call => |method| {
            cleanup_expression(allocator, method.object.*);
            allocator.destroy(method.object);
            for (method.args) |arg| {
                cleanup_expression(allocator, arg);
            }
            allocator.free(method.args);
        },
        .tuple_literal => |elements| {
            for (elements) |element| {
                cleanup_expression(allocator, element);
            }
            allocator.free(elements);
        },
        .dereference => |deref| {
            cleanup_expression(allocator, deref.*);
            allocator.destroy(deref);
        },
        .if_err_expr => |if_err| {
            cleanup_expression(allocator, if_err.expr.*);
            cleanup_expression(allocator, if_err.fallback.*);
            allocator.destroy(if_err.expr);
            allocator.destroy(if_err.fallback);
        },
        .if_null_expr => |if_null| {
            cleanup_expression(allocator, if_null.expr.*);
            cleanup_expression(allocator, if_null.fallback.*);
            allocator.destroy(if_null.expr);
            allocator.destroy(if_null.fallback);
        },
        .range_expr => |range| {
            cleanup_expression(allocator, range.start.*);
            cleanup_expression(allocator, range.end.*);
            allocator.destroy(range.start);
            allocator.destroy(range.end);
        },
        .while_expr => |while_expr| {
            cleanup_expression(allocator, while_expr.condition.*);
            allocator.destroy(while_expr.condition);
            cleanup_block(allocator, while_expr.body);
        },
        .for_expr => |for_expr| {
            cleanup_expression(allocator, for_expr.iterable.*);
            allocator.destroy(for_expr.iterable);
            allocator.free(for_expr.variables);
            cleanup_block(allocator, for_expr.body);
        },
        .loop_expr => |loop_expr| {
            cleanup_block(allocator, loop_expr.body);
        },
        .if_expr => |if_expr| {
            cleanup_expression(allocator, if_expr.condition.*);
            allocator.destroy(if_expr.condition);
            cleanup_block(allocator, if_expr.then_block);
            if (if_expr.else_block) |else_block| {
                cleanup_block(allocator, else_block);
            }
        },
        .match_expr => |match_expr| {
            cleanup_expression(allocator, match_expr.expr.*);
            allocator.destroy(match_expr.expr);
            for (match_expr.arms) |arm| {
                cleanup_expression(allocator, arm.body.*);
                allocator.destroy(arm.body);
                switch (arm.pattern) {
                    .enum_variant => |variant| {
                        if (variant.params) |params| {
                            allocator.free(params);
                        }
                    },
                    else => {},
                }
            }
            allocator.free(match_expr.arms);
        },
        else => {},
    }
}
fn cleanup_block(allocator: Allocator, block: AST.Block) void {
    for (block.statements) |stmt| {
        switch (stmt) {
            .expression => |stmt_expr| {
                cleanup_expression(allocator, stmt_expr.*);
                allocator.destroy(stmt_expr);
            },
            .declaration => |decl| {
                cleanup_type_if_exists(allocator, decl.type_annotation);
                cleanup_expression_if_exists(allocator, decl.initializer);
            },
            .return_stmt => |ret_expr| {
                if (ret_expr) |expr_ptr| {
                    cleanup_expression(allocator, expr_ptr.*);
                    allocator.destroy(expr_ptr);
                }
            },
        }
    }
    allocator.free(block.statements);
    if (block.expr) |expr_ptr| {
        cleanup_expression(allocator, expr_ptr.*);
        allocator.destroy(expr_ptr);
    }
}
pub fn print_ast(allocator: Allocator, program: AST.Program) !void {
    _ = allocator;
    const file = try std.fs.cwd().createFile("src/parser/ast_output.txt", .{});
    defer file.close();
    const writer = file.writer();
    try writer.print("AST:\n", .{});
    try writer.print("Program {{\n", .{});
    for (program.items, 0..) |item, i| {
        try writer.print("  [{d}] ", .{i});
        try print_ast_node(writer, item, 2);
    }
    try writer.print("}}\n", .{});
}
fn print_ast_node(writer: anytype, node: AST.ASTNode, indent: usize) !void {
    const spaces = " " ** 40;
    const safe_indent = @min(indent, 38);
    const prefix = spaces[0..safe_indent];
    switch (node) {
        .declaration => |decl| {
            try writer.print("Declaration {{\n", .{});
            try writer.print("{s}  kind: {s}\n", .{ prefix, @tagName(decl.kind) });
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, decl.name });
            try writer.print("{s}  is_mut: {any}\n", .{ prefix, decl.is_mut });
            try writer.print("{s}  is_match: {any}\n", .{ prefix, decl.is_match });
            if (decl.attributes) |attrs| {
                try writer.print("{s}  attributes: [\n", .{prefix});
                for (attrs, 0..) |attr, i| {
                    try writer.print("{s}    [{d}] @{s}(", .{ prefix, i, attr.name });
                    for (attr.args, 0..) |arg, j| {
                        if (j > 0) try writer.print(", ", .{});
                        switch (arg) {
                            .literal => |lit| {
                                switch (lit) {
                                    .string => |str| try writer.print("\"{s}\"", .{str}),
                                    else => try writer.print("<literal>", .{}),
                                }
                            },
                            else => try writer.print("<expr>", .{}),
                        }
                    }
                    try writer.print(")\n", .{});
                }
                try writer.print("{s}  ]\n", .{prefix});
            }
            if (decl.type_annotation) |type_ann| {
                try writer.print("{s}  type: ", .{prefix});
                try print_type(writer, type_ann, indent + 2);
            } else {
                try writer.print("{s}  type: null\n", .{prefix});
            }
            if (decl.initializer) |init| {
                try writer.print("{s}  initializer: ", .{prefix});
                try print_expression(writer, init, indent + 2);
            } else {
                try writer.print("{s}  initializer: null\n", .{prefix});
            }
            if (decl.generic_params) |generic_params| {
                try writer.print("{s}  generics: [\n", .{prefix});
                for (generic_params, 0..) |param, i| {
                    try writer.print("{s}    [{d}] \"{s}\"\n", .{ prefix, i, param });
                }
                try writer.print("{s}  ]\n", .{prefix});
            }
            if (decl.fields) |fields| {
                try writer.print("{s}  parameters: [\n", .{prefix});
                for (fields, 0..) |field, i| {
                    try writer.print("{s}    [{d}] {s}: ", .{ prefix, i, field.name });
                    try print_type(writer, field.type_annotation, indent + 6);
                    if (field.initializer) |init| {
                        switch (init) {
                            .identifier => |id| {
                                if (std.mem.eql(u8, id, "mut_ref_copy")) try writer.print("{s}      (mutable reference copy)\n", .{prefix})
                                else if (std.mem.eql(u8, id, "mut_ref")) try writer.print("{s}      (mutable reference)\n", .{prefix})
                                else if (std.mem.eql(u8, id, "mut_copy")) try writer.print("{s}      (mutable copy)\n", .{prefix})
                                else if (std.mem.eql(u8, id, "ref_copy")) try writer.print("{s}      (reference copy)\n", .{prefix})
                                else if (std.mem.eql(u8, id, "mut")) try writer.print("{s}      (mutable)\n", .{prefix})
                                else if (std.mem.eql(u8, id, "ref")) try writer.print("{s}      (reference)\n", .{prefix})
                                else if (std.mem.eql(u8, id, "copy")) try writer.print("{s}      (copy)\n", .{prefix});
                            },
                            else => {},
                        }
                    }
                }
                try writer.print("{s}  ]\n", .{prefix});
            } else {
                try writer.print("{s}  parameters: []\n", .{prefix});
            }
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .expression => |expr| {
            try writer.print("Expression: ", .{});
            try print_expression(writer, expr, indent);
        },
        .function_def => |func| {
            try writer.print("FunctionDef {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, func.name });
            try writer.print("{s}  parameters: [{d}]\n", .{ prefix, func.parameters.len });
            try writer.print("{s}  return_type: ", .{prefix});
            try print_type(writer, func.return_type, indent + 2);
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .struct_def => |struct_def| {
            try writer.print("StructDef {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, struct_def.name });
            if (struct_def.generic_params) |params| {
                try writer.print("{s}  generic_params: [", .{prefix});
                for (params, 0..) |param, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("\"{s}\"", .{param});
                }
                try writer.print("]\n", .{});
            }
            try writer.print("{s}  fields: [\n", .{prefix});
            for (struct_def.fields, 0..) |field, i| {
                try writer.print("{s}    [{d}] Field {{\n", .{ prefix, i });
                try writer.print("{s}      name: \"{s}\"\n", .{ prefix, field.name });
                try writer.print("{s}      type: ", .{prefix});
                try print_type(writer, field.type_annotation, indent + 6);
                try writer.print("{s}      visibility: {s}\n", .{ prefix, @tagName(field.visibility) });
                try writer.print("{s}    }}\n", .{prefix});
                if (field.initializer) |init| {
                    switch (init) {
                        .identifier => |id| {
                            if (std.mem.eql(u8, id, "mut")) try writer.print("{s}      (mutable)\n", .{prefix})
                            else if (std.mem.eql(u8, id, "ref")) try writer.print("{s}      (reference)\n", .{prefix})
                            else if (std.mem.eql(u8, id, "mut_ref")) try writer.print("{s}      (mutable reference)\n", .{prefix});
                        },
                        else => {},
                    }
                }
            }
            try writer.print("{s}  ]\n", .{prefix});
            if (struct_def.methods) |methods| {
                try writer.print("{s}  methods: [\n", .{prefix});
                for (methods, 0..) |method, i| {
                    try writer.print("{s}    [{d}] FunctionDef {{\n", .{ prefix, i });
                    try writer.print("{s}      name: \"{s}\"\n", .{ prefix, method.name });
                    try writer.print("{s}      parameters: [{d}]\n", .{ prefix, method.parameters.len });
                    try writer.print("{s}      return_type: ", .{prefix});
                    try print_type(writer, method.return_type, indent + 6);
                    try writer.print("{s}    }}\n", .{prefix});
                }
                try writer.print("{s}  ]\n", .{prefix});
            }
            if (struct_def.implemented_traits) |traits| {
                try writer.print("{s}  implemented_traits: [", .{prefix});
                for (traits, 0..) |trait_name, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("\"{s}\"", .{trait_name});
                }
                try writer.print("]\n", .{});
            }
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .trait_def => |trait_def| {
            try writer.print("TraitDef {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, trait_def.name });
            try writer.print("{s}  methods: [\n", .{prefix});
            for (trait_def.methods, 0..) |method, i| {
                try writer.print("{s}    [{d}] FunctionDef {{\n", .{ prefix, i });
                try writer.print("{s}      name: \"{s}\"\n", .{ prefix, method.name });
                try writer.print("{s}      parameters: [\n", .{prefix});
                for (method.parameters, 0..) |param, j| {
                    try writer.print("{s}        [{d}] {s}: ", .{ prefix, j, param.name });
                    try print_type(writer, param.param_type, safe_indent + 10);
                }
                try writer.print("{s}      ]\n", .{prefix});
                try writer.print("{s}      return_type: ", .{prefix});
                try print_type(writer, method.return_type, safe_indent + 6);
                try writer.print("{s}      body: Block {{\n", .{prefix});
                try writer.print("{s}        statements: [{d}]\n", .{ prefix, method.body.statements.len });
                if (method.body.expr) |body_expr| {
                    try writer.print("{s}        return: ", .{prefix});
                    try print_expression(writer, body_expr.*, safe_indent + 8);
                } else {
                    try writer.print("{s}        return: void\n", .{prefix});
                }
                try writer.print("{s}      }}\n", .{prefix});
                try writer.print("{s}    }}\n", .{prefix});
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .generic_def => |generic_def| {
            try writer.print("GenericDef {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, generic_def.name });
            try writer.print("{s}  constraints: [", .{prefix});
            for (generic_def.constraints, 0..) |constraint, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("\"{s}\"", .{constraint});
            }
            try writer.print("]\n", .{});
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .impl_block => |impl_block| {
            try writer.print("ImplBlock {{\n", .{});
            if (impl_block.trait_name) |trait| {
                try writer.print("{s}  trait: \"{s}\"\n", .{ prefix, trait });
            }
            try writer.print("{s}  type: \"{s}\"\n", .{ prefix, impl_block.type_name });
            try writer.print("{s}  methods: [\n", .{prefix});
            for (impl_block.methods, 0..) |method, i| {
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                try writer.print("FunctionDef {{\n", .{});
                try writer.print("{s}      name: \"{s}\"\n", .{ prefix, method.name });
                try writer.print("{s}      parameters: [\n", .{prefix});
                for (method.parameters, 0..) |param, j| {
                    try writer.print("{s}        [{d}] {s}: ", .{ prefix, j, param.name });
                    try print_type(writer, param.param_type, safe_indent + 10);
                }
                try writer.print("{s}      ]\n", .{prefix});
                try writer.print("{s}      return_type: ", .{prefix});
                try print_type(writer, method.return_type, safe_indent + 6);
                try writer.print("{s}      body: Block {{\n", .{prefix});
                try writer.print("{s}        statements: [\n", .{prefix});
                for (method.body.statements, 0..) |stmt, k| {
                    try writer.print("{s}          [{d}] ", .{ prefix, k });
                    switch (stmt) {
                        .expression => |stmt_expr| {
                            try writer.print("ExpressionStatement: ", .{});
                            try print_expression(writer, stmt_expr.*, safe_indent + 12);
                        },
                        .declaration => |decl| {
                            try writer.print("Declaration: {s}\n", .{decl.name});
                        },
                        .return_stmt => |ret_expr| {
                            if (ret_expr) |expr| {
                                try writer.print("ReturnStatement: ", .{});
                                try print_expression(writer, expr.*, safe_indent + 12);
                            } else {
                                try writer.print("ReturnStatement: void\n", .{});
                            }
                        },
                    }
                }
                try writer.print("{s}        ]\n", .{prefix});
                if (method.body.expr) |body_expr| {
                    try writer.print("{s}        return: ", .{prefix});
                    try print_expression(writer, body_expr.*, safe_indent + 8);
                } else {
                    try writer.print("{s}        return: void\n", .{prefix});
                }
                try writer.print("{s}      }}\n", .{prefix});
                try writer.print("{s}    }}\n", .{prefix});
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .enum_def => |enum_def| {
            try writer.print("EnumDef {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, enum_def.name });
            if (enum_def.generic_params) |params| {
                try writer.print("{s}  generic_params: [", .{prefix});
                for (params, 0..) |param, i| {
                    if (i > 0) try writer.print(", ", .{});
                    try writer.print("\"{s}\"", .{param});
                }
                try writer.print("]\n", .{});
            }
            try writer.print("{s}  variants: [\n", .{prefix});
            for (enum_def.variants, 0..) |variant, i| {
                try writer.print("{s}    [{d}] {s}", .{ prefix, i, variant.name });
                if (variant.fields) |fields| {
                    try writer.print(": [", .{});
                    for (fields, 0..) |field_type, j| {
                        if (j > 0) try writer.print(", ", .{});
                        try print_type_inline(writer, field_type);
                    }
                    try writer.print("]", .{});
                }
                try writer.print("\n", .{});
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        else => {
            try writer.print("{s}\n", .{@tagName(node)});
        },
    }
}
fn print_type(writer: anytype, type_node: AST.Type, indent: usize) !void {
    _ = indent;
    switch (type_node) {
        .basic => |name| {
            try writer.print("BasicType(\"{s}\")\n", .{name});
        },
        .named => |name| {
            try writer.print("NamedType(\"{s}\")\n", .{name});
        },
        .struct_type => |name| {
            try writer.print("StructType(\"{s}\")\n", .{name});
        },
        .enum_type => |name| {
            try writer.print("EnumType(\"{s}\")\n", .{name});
        },
        .generic => |gen| {
            try writer.print("GenericType(\"{s}\", {d} args)\n", .{ gen.name, gen.args.len });
        },
        .pointer => |ptr| {
            try writer.print("PointerType(", .{});
            try print_type(writer, ptr.*, 0);
            try writer.print(")\n", .{});
        },
        .reference => |ref| {
            try writer.print("ReferenceType(", .{});
            try print_type(writer, ref.*, 0);
            try writer.print(")\n", .{});
        },
        .process => |name| {
            try writer.print("ProcessType(\"{s}\")\n", .{name});
        },
        .tuple => |types| {
            try writer.print("TupleType(", .{});
            for (types, 0..) |tuple_type, i| {
                if (i > 0) try writer.print(", ", .{});
                try print_type_inline(writer, tuple_type);
            }
            try writer.print(")\n", .{});
        },
        .function => |func| {
            try writer.print("FunctionType(params: [{d}], return: ", .{func.params.len});
            try print_type_inline(writer, func.return_type.*);
            try writer.print(")\n", .{});
        },
        .array => |arr| {
            try writer.print("ArrayType(", .{});
            try print_type_inline(writer, arr.element_type.*);
            if (arr.size) |size| {
                switch (size.*) {
                    .identifier => |id| {
                        if (std.mem.eql(u8, id, "_")) {
                            try writer.print(", size: inferred", .{});
                        } else {
                            try writer.print(", size: ", .{});
                            try print_expression_inline(writer, size.*);
                        }
                    },
                    else => {
                        try writer.print(", size: ", .{});
                        try print_expression_inline(writer, size.*);
                    },
                }
            }
            try writer.print(")\n", .{});
        },
        .nullable => |nullable| {
            try writer.print("NullableType(", .{});
            try print_type_inline(writer, nullable.*);
            try writer.print(")\n", .{});
        },
        .error_type => |error_type| {
            try writer.print("ErrorType(", .{});
            try print_type_inline(writer, error_type.*);
            try writer.print(")\n", .{});
        },
        .nullable_error => |nullable_error| {
            try writer.print("NullableErrorType(", .{});
            try print_type_inline(writer, nullable_error.*);
            try writer.print(")\n", .{});
        },
    }
}
fn print_type_inline(writer: anytype, type_node: AST.Type) !void {
    switch (type_node) {
        .basic => |name| {
            try writer.print("{s}", .{name});
        },
        .named => |name| {
            try writer.print("{s}", .{name});
        },
        .struct_type => |name| {
            try writer.print("{s}", .{name});
        },
        .enum_type => |name| {
            try writer.print("{s}", .{name});
        },
        .generic => |gen| {
            try writer.print("{s}<...>", .{gen.name});
        },
        .pointer => |ptr| {
            try writer.print("^", .{});
            try print_type_inline(writer, ptr.*);
        },
        .reference => |ref| {
            try writer.print("&", .{});
            try print_type_inline(writer, ref.*);
        },
        .process => |name| {
            try writer.print("${s}", .{name});
        },
        .tuple => |types| {
            try writer.print("(", .{});
            for (types, 0..) |tuple_type, i| {
                if (i > 0) try writer.print(", ", .{});
                try print_type_inline(writer, tuple_type);
            }
            try writer.print(")", .{});
        },
        .function => |func| {
            try writer.print("fn(", .{});
            for (func.params, 0..) |param_type, i| {
                if (i > 0) try writer.print(", ", .{});
                try print_type_inline(writer, param_type);
            }
            try writer.print(") -> ", .{});
            try print_type_inline(writer, func.return_type.*);
        },
        .array => |arr| {
            try writer.print("[", .{});
            try print_type_inline(writer, arr.element_type.*);
            if (arr.size) |size| {
                try writer.print("; ", .{});
                switch (size.*) {
                    .identifier => |id| {
                        if (std.mem.eql(u8, id, "_")) {
                            try writer.print("_", .{});
                        } else {
                            try print_expression_inline(writer, size.*);
                        }
                    },
                    else => {
                        try print_expression_inline(writer, size.*);
                    },
                }
            }
            try writer.print("]", .{});
        },
        .nullable => |nullable| {
            try print_type_inline(writer, nullable.*);
            try writer.print("?", .{});
        },
        .error_type => |error_type| {
            try print_type_inline(writer, error_type.*);
            try writer.print("!", .{});
        },
        .nullable_error => |nullable_error| {
            try print_type_inline(writer, nullable_error.*);
            try writer.print("?!", .{});
        },
    }
}
fn print_expression_inline(writer: anytype, expr: AST.Expression) !void {
    switch (expr) {
        .literal => |lit| {
            switch (lit) {
                .number => |num| {
                    if (num == @floor(num)) {
                        try writer.print("{d}", .{@as(i64, @intFromFloat(num))});
                    } else {
                        try writer.print("{d}", .{num});
                    }
                },
                .string => |str| try writer.print("\"{s}\"", .{str}),
                .boolean => |b| try writer.print("{any}", .{b}),
                else => try writer.print("{s}", .{@tagName(lit)}),
            }
        },
        .identifier => |id| try writer.print("{s}", .{id}),
        else => try writer.print("<expr>", .{}),
    }
}
fn print_expression(writer: anytype, expr: AST.Expression, indent: usize) !void {
    const spaces = " " ** 40;
    const safe_indent = @min(indent, 38);
    const prefix = spaces[0..safe_indent];
    switch (expr) {
        .literal => |lit| {
            switch (lit) {
                .number => |num| {
                    if (num == @floor(num)) {
                        try writer.print("Literal(int: {d})\n", .{@as(i64, @intFromFloat(num))});
                    } else {
                        try writer.print("Literal(float: {d})\n", .{num});
                    }
                },
                .string => |str| try writer.print("Literal(string: \"{s}\")\n", .{str}),
                .boolean => |b| try writer.print("Literal(bool: {any})\n", .{b}),
                .array => |elements| {
                    try writer.print("ArrayLiteral([{d}]: [", .{elements.len});
                    for (elements, 0..) |element, i| {
                        if (i > 0) try writer.print(", ", .{});
                        switch (element) {
                            .literal => |element_lit| {
                                switch (element_lit) {
                                    .number => |num| {
                                        if (num == @floor(num)) {
                                            try writer.print("{d}", .{@as(i64, @intFromFloat(num))});
                                        } else {
                                            try writer.print("{d}", .{num});
                                        }
                                    },
                                    .string => |str| try writer.print("\"{s}\"", .{str}),
                                    .boolean => |b| try writer.print("{any}", .{b}),
                                    else => try writer.print("{s}", .{@tagName(element_lit)}),
                                }
                            },
                            .identifier => |id| try writer.print("{s}", .{id}),
                            else => try writer.print("<expr>", .{}),
                        }
                    }
                    try writer.print("])\n", .{});
                },
                else => try writer.print("Literal({s})\n", .{@tagName(lit)}),
            }
        },
        .identifier => |id| {
            try writer.print("Identifier(\"{s}\")\n", .{id});
        },
        .function_call => |call| {
            try writer.print("FunctionCall {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, call.name });
            try writer.print("{s}  is_inline: {any}\n", .{ prefix, call.is_inline });
            if (call.type_args) |type_args| {
                try writer.print("{s}  type_args: [\n", .{prefix});
                for (type_args, 0..) |type_arg, i| {
                    try writer.print("{s}    [{d}] ", .{ prefix, i });
                    try print_type(writer, type_arg, indent + 6);
                }
                try writer.print("{s}  ]\n", .{prefix});
            }
            try writer.print("{s}  args: [\n", .{prefix});
            for (call.args, 0..) |arg, i| {
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                try print_expression(writer, arg, indent + 6);
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .dereference => |deref| {
            try writer.print("Dereference(", .{});
            try print_expression(writer, deref.*, indent);
            try writer.print(")\n", .{});
        },
        .binary_op => |binop| {
            try writer.print("BinaryOp {{\n", .{});
            try writer.print("{s}  left: ", .{prefix});
            try print_expression(writer, binop.left.*, indent + 2);
            try writer.print("{s}  operator: \"{s}\"\n", .{ prefix, binop.operator });
            try writer.print("{s}  right: ", .{prefix});
            try print_expression(writer, binop.right.*, indent + 2);
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .match_expr => |match_expr| {
            try writer.print("MatchExpression {{\n", .{});
            try writer.print("{s}  expr: ", .{prefix});
            try print_expression(writer, match_expr.expr.*, indent + 2);
            try writer.print("{s}  arms: [\n", .{prefix});
            for (match_expr.arms, 0..) |arm, i| {
                try writer.print("{s}    [{d}] MatchArm {{\n", .{ prefix, i });
                try writer.print("{s}      pattern: ", .{prefix});
                switch (arm.pattern) {
                    .wildcard => try writer.print("Wildcard\n", .{}),
                    .identifier => |id| try writer.print("Identifier(\"{s}\")\n", .{id}),
                    .literal => |lit| {
                        switch (lit) {
                            .number => |num| {
                                if (num == @floor(num)) {
                                    try writer.print("Literal(int: {d})\n", .{@as(i64, @intFromFloat(num))});
                                } else {
                                    try writer.print("Literal(float: {d})\n", .{num});
                                }
                            },
                            else => try writer.print("Literal\n", .{}),
                        }
                    },
                    .enum_variant => |variant| {
                        try writer.print("EnumVariant(\"{s}\"", .{variant.variant_name});
                        if (variant.params) |params| {
                            try writer.print(", bindings: [", .{});
                            for (params, 0..) |param, j| {
                                if (j > 0) try writer.print(", ", .{});
                                try writer.print("\"{s}\"", .{param});
                            }
                            try writer.print("])", .{});
                        } else {
                            try writer.print(")", .{});
                        }
                        try writer.print("\n", .{});
                    },
                }
                try writer.print("{s}      body: ", .{prefix});
                try print_expression(writer, arm.body.*, indent + 6);
                try writer.print("{s}    }}\n", .{prefix});
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .block_expr => |block| {
            try writer.print("BlockExpression {{\n", .{});
            try writer.print("{s}  statements: [\n", .{prefix});
            for (block.statements, 0..) |stmt, i| {
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                switch (stmt) {
                    .expression => |stmt_expr| {
                        try writer.print("ExpressionStatement: ", .{});
                        try print_expression(writer, stmt_expr.*, indent + 6);
                    },
                    .declaration => |decl| {
                        try writer.print("Declaration {{\n", .{});
                        try writer.print("{s}      kind: {s}\n", .{ prefix, @tagName(decl.kind) });
                        try writer.print("{s}      name: \"{s}\"\n", .{ prefix, decl.name });
                        if (decl.initializer) |init| {
                            try writer.print("{s}      initializer: ", .{prefix});
                            try print_expression(writer, init, indent + 8);
                        } else {
                            try writer.print("{s}      initializer: null\n", .{prefix});
                        }
                        try writer.print("{s}    }}\n", .{prefix});
                    },
                    else => try writer.print("{s}\n", .{@tagName(stmt)}),
                }
            }
            try writer.print("{s}  ]\n", .{prefix});
            if (block.expr) |return_expr| {
                try writer.print("{s}  return: ", .{prefix});
                try print_expression(writer, return_expr.*, indent + 2);
            }
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .member_access => |member| {
            try writer.print("MemberAccess {{\n", .{});
            try writer.print("{s}  object: ", .{prefix});
            try print_expression(writer, member.object.*, indent + 2);
            try writer.print("{s}  member: \"{s}\"\n", .{ prefix, member.member });
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .method_call => |method| {
            try writer.print("MethodCall {{\n", .{});
            try writer.print("{s}  object: ", .{prefix});
            try print_expression(writer, method.object.*, indent + 2);
            try writer.print("{s}  method: \"{s}\"\n", .{ prefix, method.method });
            try writer.print("{s}  args: [\n", .{prefix});
            for (method.args, 0..) |arg, i| {
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                try print_expression(writer, arg, indent + 6);
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .range_expr => |range| {
            try writer.print("RangeExpression {{\n", .{});
            try writer.print("{s}  start: ", .{prefix});
            try print_expression(writer, range.start.*, safe_indent + 2);
            try writer.print("{s}  end: ", .{prefix});
            try print_expression(writer, range.end.*, safe_indent + 2);
            try writer.print("{s}  inclusive: {any}\n", .{ prefix, range.inclusive });
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .while_expr => |while_expr| {
            try writer.print("WhileExpression {{\n", .{});
            try writer.print("{s}  condition: ", .{prefix});
            try print_expression(writer, while_expr.condition.*, safe_indent + 2);
            try writer.print("{s}  body: ", .{prefix});
            try print_expression(writer, AST.Expression{ .block_expr = while_expr.body }, safe_indent + 2);
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .for_expr => |for_expr| {
            try writer.print("ForExpression {{\n", .{});
            try writer.print("{s}  variables: [", .{prefix});
            for (for_expr.variables, 0..) |var_name, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("\"{s}\"", .{var_name});
            }
            try writer.print("]\n", .{});
            try writer.print("{s}  is_inline: {any}\n", .{ prefix, for_expr.is_inline });
            try writer.print("{s}  iterable: ", .{prefix});
            try print_expression(writer, for_expr.iterable.*, safe_indent + 2);
            try writer.print("{s}  body: ", .{prefix});
            try print_expression(writer, AST.Expression{ .block_expr = for_expr.body }, safe_indent + 2);
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .loop_expr => |loop_expr| {
            try writer.print("LoopExpression {{\n", .{});
            try writer.print("{s}  body: ", .{prefix});
            try print_expression(writer, AST.Expression{ .block_expr = loop_expr.body }, safe_indent + 2);
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .tuple_literal => |elements| {
            try writer.print("TupleLiteral {{\n", .{});
            try writer.print("{s}  elements: [\n", .{prefix});
            for (elements, 0..) |element, i| {
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                try print_expression(writer, element, safe_indent + 6);
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .struct_literal => |struct_lit| {
            try writer.print("StructLiteral {{\n", .{});
            try writer.print("{s}  fields: [\n", .{prefix});
            for (struct_lit.fields, 0..) |field, i| {
                try writer.print("{s}    [{d}] {s} = ", .{ prefix, i, field.name });
                try print_expression(writer, field.value, safe_indent + 6);
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..@max(2, safe_indent) - 2]});
        },
        .if_expr => |if_expr| {
            try writer.print("IfExpression {{\n", .{});
            try writer.print("{s}  condition: ", .{prefix});
            try print_expression(writer, if_expr.condition.*, indent + 2);
            try writer.print("{s}  then: {{\n", .{prefix});
            try writer.print("{s}    statements: [{d}]\n", .{ prefix, if_expr.then_block.statements.len });
            if (if_expr.then_block.expr) |_| {
                try writer.print("{s}    return: <expression>\n", .{prefix});
            }
            try writer.print("{s}  }}\n", .{prefix});
            if (if_expr.else_block) |else_block| {
                if (else_block.statements.len == 0 and else_block.expr != null) {
                    if (else_block.expr.?.* == .if_expr) {
                        try writer.print("{s}  else if: ", .{prefix});
                        try print_expression(writer, else_block.expr.?.*, indent + 2);
                        return;
                    }
                }
                try writer.print("{s}  else: {{\n", .{prefix});
                try writer.print("{s}    statements: [{d}]\n", .{ prefix, else_block.statements.len });
                if (else_block.expr) |_| {
                    try writer.print("{s}    return: <expression>\n", .{prefix});
                }
                try writer.print("{s}  }}\n", .{prefix});
            }
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .macro_call => |macro| {
            try writer.print("MacroCall {{\n", .{});
            try writer.print("{s}  name: \"@{s}\"\n", .{ prefix, macro.name });
            try writer.print("{s}  args: [\n", .{prefix});
            for (macro.args, 0..) |arg, i| {
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                try print_expression(writer, arg, indent + 6);
            }
            try writer.print("{s}  ]\n", .{prefix});
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        else => {
            try writer.print("{s}\n", .{@tagName(expr)});
        },
    }
}
