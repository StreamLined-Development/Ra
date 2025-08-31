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
    
    fn init(tokens: []const Token, allocator: Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .position = 0,
            .allocator = allocator,
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
                .kw_var, .kw_let, .kw_const, .kw_process, .kw_fun => {
                    const decl = self.parse_declaration() catch |err| {
                        std.debug.print("Declaration parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        // Skip to next statement on error
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
                    try items.append(AST.ASTNode{ .enum_def = union_def }); // Reuse enum_def for unions
                },
                .kw_trait => {
                    const trait_def = self.parse_trait() catch |err| {
                        std.debug.print("Trait parse failed: {s} at token {d}\n", .{ @errorName(err), self.position });
                        self.skip_to_brace();
                        continue;
                    };
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
                .kw_match => {
                    const match_expr = try self.parse_expression();
                    try items.append(AST.ASTNode{ .expression = match_expr });
                },
                .identifier => {
                    // Try to parse as expression, skip on error
                    const expr = self.parse_expression() catch {
                        _ = self.advance();
                        continue;
                    };
                    _ = self.match(.semicolon);
                    try items.append(AST.ASTNode{ .expression = expr });
                },
                else => {
                    // Skip unknown tokens
                    _ = self.advance();
                },
            }
        }
        
        return AST.Program{ .items = try items.toOwnedSlice() };
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
    
    fn parse_declaration(self: *Parser) ParseError!AST.Declaration {
        const kind_token = self.advance().?;
        
        const is_mut = self.match(.kw_mut);
        
        // Handle pointer declaration: ^identifier
        const has_pointer_prefix = self.match(.caret);
        const name_token = try self.expect(.identifier);
        
        // If we had ^identifier syntax, we need to adjust the type parsing
        const pointer_in_name = has_pointer_prefix;
        
        var type_annotation: ?AST.Type = null;
        if (self.match(.colon)) {
            // Handle tuple type: (x, y)
            if (self.match(.left_paren)) {
                // Skip tuple type parameters for now
                while (!self.match(.right_paren)) {
                    _ = try self.expect(.identifier);
                    if (!self.match(.comma)) break;
                }
                type_annotation = AST.Type{ .named = "tuple" };
            } else {
                const parsed_type = try self.parse_type();
                // If we had ^identifier syntax, wrap the type in a pointer
                if (pointer_in_name) {
                    const ptr_type = try self.allocator.create(AST.Type);
                    ptr_type.* = parsed_type;
                    type_annotation = AST.Type{ .pointer = ptr_type };
                } else {
                    type_annotation = parsed_type;
                }
            }
        }
        
        // Handle array syntax: name[size]
        if (self.match(.left_bracket)) {
            _ = try self.parse_expression(); // array size
            _ = try self.expect(.right_bracket);
        }
        
        var initializer: ?AST.Expression = null;
        if (self.match(.assign)) {
            initializer = try self.parse_expression();
            // Handle comma-separated values (tuple literals)
            while (self.match(.comma)) {
                _ = try self.parse_expression();
            }
        }
        
        // Semicolon is optional
        _ = self.match(.semicolon);
        
        return switch (kind_token.type) {
            .kw_var => AST.Declaration{
                .kind = .var_decl,
                .is_mut = is_mut,
                .name = name_token.text,
                .type_annotation = type_annotation,
                .initializer = initializer,
                .fields = null,
            },
            .kw_let => AST.Declaration{
                .kind = .let_decl,
                .is_mut = is_mut,
                .name = name_token.text,
                .type_annotation = type_annotation,
                .initializer = initializer,
                .fields = null,
            },
            .kw_const => AST.Declaration{
                .kind = .const_decl,
                .is_mut = is_mut,
                .name = name_token.text,
                .type_annotation = type_annotation,
                .initializer = initializer,
                .fields = null,
            },
            .kw_process => {
                _ = try self.expect(.left_brace);
                var fields = std.ArrayList(AST.Field).init(self.allocator);
                
                while (!self.match(.right_brace)) {
                    // Parse field declaration manually to avoid recursion issues
                    _ = self.advance(); // skip var/let/const
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
                    });
                }
                
                return AST.Declaration{
                    .kind = .process_decl,
                    .is_mut = is_mut,
                    .name = name_token.text,
                    .type_annotation = type_annotation,
                    .initializer = initializer,
                    .fields = try fields.toOwnedSlice(),
                };
            },

            .kw_fun => {
                // Parse optional generic parameters: fun name<T: Trait, U>
                var generic_params: ?[][]const u8 = null;
                if (self.match(.less)) {
                    var params = std.ArrayList([]const u8).init(self.allocator);
                    const param = try self.expect(.identifier);
                    var param_text = param.text;
                    
                    // Check for trait bound: T: TraitName
                    if (self.match(.colon)) {
                        const trait_name = try self.expect(.identifier);
                        // Combine param and trait bound
                        const combined = try std.fmt.allocPrint(self.allocator, "{s}: {s}", .{ param.text, trait_name.text });
                        param_text = combined;
                    }
                    
                    try params.append(param_text);
                    while (self.match(.comma)) {
                        const next_param = try self.expect(.identifier);
                        var next_param_text = next_param.text;
                        
                        // Check for trait bound on next parameter
                        if (self.match(.colon)) {
                            const next_trait_name = try self.expect(.identifier);
                            const next_combined = try std.fmt.allocPrint(self.allocator, "{s}: {s}", .{ next_param.text, next_trait_name.text });
                            next_param_text = next_combined;
                        }
                        
                        try params.append(next_param_text);
                    }
                    _ = try self.expect(.greater);
                    generic_params = try params.toOwnedSlice();
                }
                
                _ = try self.expect(.left_paren);
                
                // Parse parameters: (x: i32, y: i32)
                var params = std.ArrayList(AST.Parameter).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        // Check for mut keyword
                        const param_is_mut = self.match(.kw_mut);
                        
                        // Check for pointer prefix: ^name
                        const param_is_ptr = self.match(.caret);
                        
                        const param_name = try self.expect(.identifier);
                        _ = try self.expect(.colon);
                        const param_type = try self.parse_type();
                        // Wrap type in pointer if ^name syntax was used
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
                            .is_ref = false,
                        });
                        if (!self.match(.comma)) break;
                    }
                }
                _ = try self.expect(.right_paren);
                
                // Parse return type: -> Type
                var return_type: ?AST.Type = null;
                if (self.match(.arrow)) {
                    return_type = try self.parse_type();
                }
                
                _ = try self.expect(.left_brace);
                
                // Parse function body
                var body_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var body_expr: ?AST.Expression = null;
                
                while (self.current() != null and self.current().?.type != .right_brace) {
                    if (self.current().?.type == .kw_return) {
                        _ = self.advance(); // consume 'return'
                        const return_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        body_expr = return_expr;
                        break;
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var) {
                        // Parse variable declaration inside function
                        const decl = try self.parse_declaration();
                        try body_statements.append(AST.Statement{ .declaration = decl });
                    } else {
                        // Parse assignment or expression statement
                        const stmt_expr = try self.parse_expression();
                        _ = self.match(.semicolon);
                        const stmt_ptr = try self.allocator.create(AST.Expression);
                        stmt_ptr.* = stmt_expr;
                        try body_statements.append(AST.Statement{ .expression = stmt_ptr });
                    }
                }
                
                _ = try self.expect(.right_brace);
                
                // Store function body in initializer as block expression
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
                
                // Store parameters in fields for detailed info
                const param_fields = try self.allocator.alloc(AST.Field, params.items.len);
                for (params.items, 0..) |param, i| {
                    var modifier_expr: ?AST.Expression = null;
                    if (param.is_mut) {
                        modifier_expr = AST.Expression{ .identifier = "mut" };
                    }
                    
                    param_fields[i] = AST.Field{
                        .name = param.name,
                        .type_annotation = param.param_type,
                        .initializer = modifier_expr,
                    };
                }
                
                // Store return type in type_annotation
                type_annotation = return_type;
                
                // Store generic parameters by prepending them to the fields array
                var final_fields: []AST.Field = param_fields;
                if (generic_params) |gen_params| {
                    var all_fields = std.ArrayList(AST.Field).init(self.allocator);
                    
                    // Add generic parameters as special fields with "generic" type
                    for (gen_params) |gen_param| {
                        try all_fields.append(AST.Field{
                            .name = gen_param,
                            .type_annotation = AST.Type{ .named = "<generic>" },
                            .initializer = null,
                        });
                    }
                    
                    // Add regular parameters
                    for (param_fields) |field| {
                        try all_fields.append(field);
                    }
                    
                    final_fields = try all_fields.toOwnedSlice();
                }
                
                return AST.Declaration{
                    .kind = .fun_decl,
                    .is_mut = is_mut,
                    .name = name_token.text,
                    .type_annotation = type_annotation,
                    .initializer = body_expr,
                    .fields = final_fields,
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
        
        // Check for assignment: left = right
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
            if (token.type == .plus or token.type == .minus or token.type == .multiply or token.type == .divide or token.type == .modulo or token.type == .power or token.type == .equal or token.type == .not_equal or token.type == .less or token.type == .greater or token.type == .less_equal or token.type == .greater_equal or token.type == .piping or token.type == .excl_range or token.type == .incl_range) {
                const op_token = self.advance().?;
                const right = try self.parse_primary_expression();
                
                // Handle range expressions specially
                if (op_token.type == .excl_range or op_token.type == .incl_range) {
                    const left_ptr = try self.allocator.create(AST.Expression);
                    const right_ptr = try self.allocator.create(AST.Expression);
                    left_ptr.* = left;
                    right_ptr.* = right;
                    
                    left = AST.Expression{ .range_expr = AST.RangeExpression{
                        .start = left_ptr,
                        .end = right_ptr,
                        .inclusive = op_token.type == .incl_range, // ... is inclusive, .. is exclusive
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
                _ = self.advance(); // consume 'while'
                const condition = try self.parse_expression();
                _ = try self.expect(.left_brace);
                
                // Parse block body using existing block parsing logic
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
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var or self.current().?.type == .kw_fun) {
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
                _ = self.advance(); // consume 'for'
                const var_name = try self.expect(.identifier);
                _ = try self.expect(.kw_in); // expect 'in' keyword
                const iterable = try self.parse_expression();
                _ = try self.expect(.left_brace);
                
                // Parse block body using existing block parsing logic
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
                    } else if (self.current().?.type == .kw_let or self.current().?.type == .kw_var or self.current().?.type == .kw_fun) {
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
                    .variable = var_name.text,
                    .iterable = iterable_ptr,
                    .body = AST.Block{
                        .statements = try body_statements.toOwnedSlice(),
                        .expr = body_expr,
                    },
                }};
            },
            .kw_if => {
                _ = self.advance();
                const condition = try self.parse_expression();
                _ = try self.expect(.left_brace);
                
                // Parse then block statements
                var then_statements = std.ArrayList(AST.Statement).init(self.allocator);
                var then_expr: ?AST.Expression = null;
                
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
                
                const then_block = AST.Block{
                    .statements = try then_statements.toOwnedSlice(),
                    .expr = if (then_expr) |expr| blk: {
                        const expr_ptr = try self.allocator.create(AST.Expression);
                        expr_ptr.* = expr;
                        break :blk expr_ptr;
                    } else null,
                };
                
                // Parse optional else block
                var else_block: ?AST.Block = null;
                if (self.current() != null and self.current().?.type == .kw_else) {
                    _ = self.advance(); // consume 'else'
                    // Check for 'else if' syntax
                    if (self.current() != null and self.current().?.type == .kw_if) {
                        // Parse 'else if' as nested if expression
                        _ = self.advance(); // consume 'if'
                        const nested_condition = try self.parse_expression();
                        _ = try self.expect(.left_brace);
                        
                        // Parse nested if then block
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
                        
                        // Check for nested else
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
                        
                        // Create nested if expression
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
                const match_expr = try self.parse_expression();
                _ = try self.expect(.left_brace);
                
                var arms = std.ArrayList(AST.MatchArm).init(self.allocator);
                
                while (!self.match(.right_brace)) {
                    // Parse pattern (could be literal, identifier, or wildcard)
                    var pattern: AST.Pattern = undefined;
                    if (self.current()) |current_token| {
                        if (current_token.type == .identifier and std.mem.eql(u8, current_token.text, "_")) {
                            _ = self.advance();
                            pattern = AST.Pattern{ .wildcard = {} };
                        } else if (current_token.type == .lit_int or current_token.type == .lit_float) {
                            const lit_token = self.advance().?;
                            const num = std.fmt.parseFloat(f64, lit_token.text) catch 0.0;
                            pattern = AST.Pattern{ .literal = AST.Literal{ .number = num } };
                        } else {
                            const pattern_token = try self.expect(.identifier);
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
                                    .variant_name = pattern_token.text,
                                    .params = if (params.items.len > 0) try params.toOwnedSlice() else null,
                                }};
                            } else {
                                pattern = AST.Pattern{ .identifier = pattern_token.text };
                            }
                        }
                    } else {
                        return ParseError.UnexpectedEOF;
                    }
                    
                    _ = try self.expect(.arrow);
                    const body_expr = try self.parse_expression();
                    _ = self.match(.semicolon);
                    
                    const body_ptr = try self.allocator.create(AST.Expression);
                    body_ptr.* = body_expr;
                    
                    try arms.append(AST.MatchArm{
                        .pattern = pattern,
                        .body = body_ptr,
                    });
                }
                
                const match_expr_ptr = try self.allocator.create(AST.Expression);
                match_expr_ptr.* = match_expr;
                
                return AST.Expression{ .match_expr = AST.MatchExpression{
                    .expr = match_expr_ptr,
                    .arms = try arms.toOwnedSlice(),
                }};
            },
            .identifier, .kw_spawn => {
                _ = self.advance();
                var expr = AST.Expression{ .identifier = token.text };
                
                // Handle member access: obj.method(), function calls, or enum variants Type::Variant
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
                            }};
                        } else {
                            expr = AST.Expression{ .identifier = variant_token.text };
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
                            
                            // Create method call expression
                            const obj_ptr = try self.allocator.create(AST.Expression);
                            obj_ptr.* = expr;
                            expr = AST.Expression{ .method_call = AST.MethodCall{
                                .object = obj_ptr,
                                .method = member_token.text,
                                .args = try args.toOwnedSlice(),
                            }};
                        } else {
                            // Create member access expression
                            const obj_ptr = try self.allocator.create(AST.Expression);
                            obj_ptr.* = expr;
                            expr = AST.Expression{ .member_access = AST.MemberAccess{
                                .object = obj_ptr,
                                .member = member_token.text,
                            }};
                        }
                    } else if (self.match(.left_brace)) {
                        // Generic function call: func{T: i32, U: f32}
                        var args = std.ArrayList(AST.Expression).init(self.allocator);
                        
                        while (!self.match(.right_brace)) {
                            _ = try self.expect(.identifier); // param_name
                            _ = try self.expect(.colon);
                            const type_expr = try self.parse_type();
                            
                            // Create a type expression as identifier for now
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
                        }};
                        break;
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
                        }};
                        break;
                    } else {
                        break;
                    }
                }
                
                return expr;
            },
            .left_paren => {
                _ = self.advance(); // consume '('
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
            .left_brace => {
                _ = self.advance();
                // Parse object literal: { key = value, ... } or { key: value, ... }
                while (!self.match(.right_brace)) {
                    // Parse field assignment
                    _ = try self.expect(.identifier); // field name
                    _ = try self.expect(.assign);
                    _ = try self.parse_expression(); // value
                    // Optional comma or newline
                    _ = self.match(.comma);
                }
                return AST.Expression{ .identifier = "struct_literal" };
            },
            else => return ParseError.UnexpectedToken,
        }
    }
    
    fn parse_type(self: *Parser) ParseError!AST.Type {
        const token = self.current() orelse return ParseError.UnexpectedEOF;
        
        switch (token.type) {
            .left_paren => {
                _ = self.advance(); // consume '('
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
            .kw_i8, .kw_i16, .kw_i32, .kw_i64, .kw_u8, .kw_u16, .kw_u32, .kw_u64,
            .kw_f32, .kw_f64, .kw_bool, .kw_char, .kw_str, .kw_strA, .kw_str16, .kw_str32,
            .kw_string, .kw_stringA, .kw_string16, .kw_string32 => {
                _ = self.advance();
                return AST.Type{ .basic = token.text };
            },
            .identifier => {
                _ = self.advance();
                // Handle generic types: name<type1, type2> - simplified
                if (self.match(.less)) {
                    // Skip everything until >
                    var depth: u32 = 1;
                    while (self.current()) |current_token| {
                        _ = self.advance();
                        if (current_token.type == .less) {
                            depth += 1;
                        } else if (current_token.type == .greater) {
                            depth -= 1;
                            if (depth == 0) break;
                        }
                    }
                    return AST.Type{ .named = token.text };
                }
                return AST.Type{ .named = token.text };
            },
            else => {
                // Treat unknown tokens as named types for robustness
                _ = self.advance();
                return AST.Type{ .named = token.text };
            },
        }
    }
    
    fn parse_trait(self: *Parser) ParseError!AST.TraitDef {
        _ = try self.expect(.kw_trait);
        const trait_name = try self.expect(.identifier);
        _ = try self.expect(.left_brace);
        
        var methods = std.ArrayList(AST.TraitMethod).init(self.allocator);
        
        while (!self.match(.right_brace)) {
            _ = try self.expect(.kw_fun);
            const method_name = try self.expect(.identifier);
            _ = try self.expect(.left_paren);
            
            // Parse parameters
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
                    });
                    if (!self.match(.comma)) break;
                }
            }
            _ = try self.expect(.right_paren);
            
            // Parse return type
            var return_type: ?AST.Type = null;
            if (self.match(.arrow)) {
                return_type = try self.parse_type();
            }
            
            _ = self.match(.semicolon);
            
            try methods.append(AST.TraitMethod{
                .name = method_name.text,
                .parameters = try params.toOwnedSlice(),
                .return_type = return_type,
            });
        }
        
        return AST.TraitDef{
            .name = trait_name.text,
            .methods = try methods.toOwnedSlice(),
        };
    }
    
    fn parse_impl_block(self: *Parser) ParseError!AST.ImplBlock {
        _ = try self.expect(.impl);
        
        // Check for trait implementation: impl TraitName for TypeName
        var trait_name: ?[]const u8 = null;
        const first_name = try self.expect(.identifier);
        
        var type_name: []const u8 = undefined;
        if (self.current() != null and self.current().?.type == .kw_for) {
            _ = self.advance(); // consume 'for'
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
    
    fn parse_struct(self: *Parser) ParseError!AST.StructDef {
        _ = try self.expect(.kw_struct);
        const struct_name = try self.expect(.identifier);
        
        // Handle generic parameters: struct Name<T>
        var generic_params: ?[][]const u8 = null;
        if (self.match(.less)) {
            var params = std.ArrayList([]const u8).init(self.allocator);
            const param = try self.expect(.identifier);
            try params.append(param.text);
            // Handle multiple parameters separated by commas
            while (self.match(.comma)) {
                const next_param = try self.expect(.identifier);
                try params.append(next_param.text);
            }
            _ = try self.expect(.greater);
            generic_params = try params.toOwnedSlice();
        }
        
        _ = try self.expect(.left_brace);
        
        var fields = std.ArrayList(AST.Field).init(self.allocator);
        
        while (!self.match(.right_brace)) {
            const field_name = try self.expect(.identifier);
            _ = try self.expect(.colon);
            const field_type = try self.parse_type();
            _ = self.match(.comma);
            
            try fields.append(AST.Field{
                .name = field_name.text,
                .type_annotation = field_type,
                .initializer = null,
            });
        }
        
        return AST.StructDef{
            .name = struct_name.text,
            .generic_params = generic_params,
            .fields = try fields.toOwnedSlice(),
        };
    }
    
    fn parse_union(self: *Parser) ParseError!AST.EnumDef {
        _ = try self.expect(.kw_union);
        const union_name = try self.expect(.identifier);
        
        // Handle generic parameters: union Name<T>
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
            
            // Store union field as enum variant with single type
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
        
        // Handle generic parameters: enum Name<T>
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
            
            // Handle variant with parameters: Variant(Type1, Type2) or Variant(name: Type)
            var fields: ?[]AST.Type = null;
            if (self.match(.left_paren)) {
                var field_types = std.ArrayList(AST.Type).init(self.allocator);
                if (self.current() != null and self.current().?.type != .right_paren) {
                    while (true) {
                        // Skip optional field name: "name:"
                        if (self.current()) |token| {
                            if (token.type == .identifier) {
                                const next_pos = self.position + 1;
                                if (next_pos < self.tokens.len and self.tokens[next_pos].type == .colon) {
                                    _ = self.advance(); // field name
                                    _ = self.advance(); // colon
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
            .name = alias_name.text,
            .type_annotation = target_type,
            .initializer = null,
            .fields = null,
        };
    }
    
    fn parse_generic(self: *Parser) ParseError!AST.GenericDef {
        _ = try self.expect(.kw_generic);
        const generic_name = try self.expect(.identifier);
        _ = try self.expect(.left_brace);
        
        var constraints = std.ArrayList([]const u8).init(self.allocator);
        while (!self.match(.right_brace)) {
            const constraint = try self.expect(.identifier);
            try constraints.append(constraint.text);
            if (!self.match(.comma)) break;
        }
        
        return AST.GenericDef{
            .name = generic_name.text,
            .constraints = try constraints.toOwnedSlice(),
        };
    }
    
    fn parse_function_def(self: *Parser) ParseError!AST.FunctionDef {
        _ = try self.expect(.kw_fun);
        const name_token = try self.expect(.identifier);
        
        // Parse optional generic parameters with trait bounds: <T: Printable>
        if (self.match(.less)) {
            // Skip generic parameters for now in FunctionDef
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
        
        // Parse parameters
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
                });
                if (!self.match(.comma)) break;
            }
        }
        _ = try self.expect(.right_paren);
        
        // Parse return type
        var return_type = AST.Type{ .named = "void" };
        if (self.match(.arrow)) {
            return_type = try self.parse_type();
        }
        
        _ = try self.expect(.left_brace);
        
        // Parse function body
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
        };
    }
};

pub fn parse(allocator: Allocator, tokens: []const Token) ParseError!AST.Program {
    var parser = Parser.init(tokens, allocator);
    return parser.parse_program();
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
            
            if (decl.fields) |fields| {
                // Separate generic parameters from regular parameters
                var generic_count: usize = 0;
                var regular_start: usize = 0;
                
                // Count generic parameters (they have type "<generic>")
                for (fields) |field| {
                    if (field.type_annotation == .named and std.mem.eql(u8, field.type_annotation.named, "<generic>")) {
                        generic_count += 1;
                    } else {
                        break;
                    }
                }
                regular_start = generic_count;
                
                // Print generic parameters
                if (generic_count > 0) {
                    try writer.print("{s}  generics: <", .{prefix});
                    for (fields[0..generic_count], 0..) |field, i| {
                        if (i > 0) try writer.print(", ", .{});
                        try writer.print("{s}", .{field.name});
                    }
                    try writer.print(">\n", .{});
                }
                
                // Print regular parameters
                if (regular_start < fields.len) {
                    try writer.print("{s}  parameters: [\n", .{prefix});
                    for (fields[regular_start..], 0..) |field, i| {
                        try writer.print("{s}    [{d}] {s}: ", .{ prefix, i, field.name });
                        try print_type(writer, field.type_annotation, indent + 6);
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
                } else {
                    try writer.print("{s}  parameters: []\n", .{prefix});
                }
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
                try writer.print("{s}    [{d}] {s}: ", .{ prefix, i, field.name });
                try print_type(writer, field.type_annotation, indent + 6);
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
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        .trait_def => |trait_def| {
            try writer.print("TraitDef {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, trait_def.name });
            try writer.print("{s}  methods: [\n", .{prefix});
            for (trait_def.methods, 0..) |method, i| {
                try writer.print("{s}    [{d}] {s}(", .{ prefix, i, method.name });
                for (method.parameters, 0..) |param, j| {
                    if (j > 0) try writer.print(", ", .{});
                    try writer.print("{s}: ", .{param.name});
                    try print_type_inline(writer, param.param_type);
                }
                try writer.print(")",.{});
                if (method.return_type) |ret_type| {
                    try writer.print(" -> ", .{});
                    try print_type_inline(writer, ret_type);
                }
                try writer.print("\n", .{});
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
                // Print FunctionDef
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
        else => {
            try writer.print("{s}\n", .{@tagName(type_node)});
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
        else => {
            try writer.print("{s}", .{@tagName(type_node)});
        },
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
                    // Check if it's an integer or float
                    if (num == @floor(num)) {
                        try writer.print("Literal(int: {d})\n", .{@as(i64, @intFromFloat(num))});
                    } else {
                        try writer.print("Literal(float: {d})\n", .{num});
                    }
                },
                .string => |str| try writer.print("Literal(string: \"{s}\")\n", .{str}),
                .boolean => |b| try writer.print("Literal(bool: {any})\n", .{b}),
                else => try writer.print("Literal({s})\n", .{@tagName(lit)}),
            }
        },
        .identifier => |id| {
            try writer.print("Identifier(\"{s}\")\n", .{id});
        },
        .function_call => |call| {
            try writer.print("FunctionCall {{\n", .{});
            try writer.print("{s}  name: \"{s}\"\n", .{ prefix, call.name });
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
                try writer.print("{s}    [{d}] ", .{ prefix, i });
                switch (arm.pattern) {
                    .wildcard => try writer.print("_ => ", .{}),
                    .identifier => |id| try writer.print("{s} => ", .{id}),
                    .literal => |lit| {
                        switch (lit) {
                            .number => |num| try writer.print("{d} => ", .{num}),
                            else => try writer.print("literal => ", .{}),
                        }
                    },
                    .enum_variant => |variant| {
                        try writer.print("{s}(", .{variant.variant_name});
                        if (variant.params) |params| {
                            for (params, 0..) |param, j| {
                                if (j > 0) try writer.print(", ", .{});
                                try writer.print("{s}", .{param});
                            }
                        }
                        try writer.print(") => ", .{});
                    },
                }
                try print_expression(writer, arm.body.*, indent + 4);
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
            try writer.print("{s}  variable: \"{s}\"\n", .{ prefix, for_expr.variable });
            try writer.print("{s}  iterable: ", .{prefix});
            try print_expression(writer, for_expr.iterable.*, safe_indent + 2);
            try writer.print("{s}  body: ", .{prefix});
            try print_expression(writer, AST.Expression{ .block_expr = for_expr.body }, safe_indent + 2);
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
                // Check if this is an else-if (empty statements + if expression)
                if (else_block.statements.len == 0 and else_block.expr != null) {
                    if (else_block.expr.?.* == .if_expr) {
                        try writer.print("{s}  else if: ", .{prefix});
                        try print_expression(writer, else_block.expr.?.*, indent + 2);
                        return;
                    }
                }
                // Regular else block
                try writer.print("{s}  else: {{\n", .{prefix});
                try writer.print("{s}    statements: [{d}]\n", .{ prefix, else_block.statements.len });
                if (else_block.expr) |_| {
                    try writer.print("{s}    return: <expression>\n", .{prefix});
                }
                try writer.print("{s}  }}\n", .{prefix});
            }
            try writer.print("{s}}}\n", .{prefix[0..indent-2]});
        },
        else => {
            try writer.print("{s}\n", .{@tagName(expr)});
        },
    }
}

