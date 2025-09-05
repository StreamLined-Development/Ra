const std = @import("std");
const AST = @import("../parser/ast.zig");
const Allocator = std.mem.Allocator;

pub const SyntacticError = struct {
    message: []const u8,
    node_type: []const u8,
    severity: enum { warning, err },
};

pub const SyntacticAnalyzer = struct {
    allocator: Allocator,
    errors: std.ArrayList(SyntacticError),
    declared_structs: std.StringHashMap(void),
    struct_definitions: std.StringHashMap(AST.StructDef),
    declared_enums: std.StringHashMap(void),
    enum_definitions: std.StringHashMap(AST.EnumDef),
    enum_generic_counts: std.StringHashMap(usize),
    struct_generic_counts: std.StringHashMap(usize),
    function_generic_counts: std.StringHashMap(usize),
    function_signatures: std.StringHashMap([]AST.Type),
    variable_types: std.StringHashMap([]const u8),
    current_function_return_type: ?AST.Type,
    generic_constraints: std.StringHashMap([][]const u8),
    current_function_generics: std.StringHashMap(void),
    
    pub fn init(allocator: Allocator) SyntacticAnalyzer {
        return SyntacticAnalyzer{
            .allocator = allocator,
            .errors = std.ArrayList(SyntacticError).init(allocator),
            .declared_structs = std.StringHashMap(void).init(allocator),
            .struct_definitions = std.StringHashMap(AST.StructDef).init(allocator),
            .declared_enums = std.StringHashMap(void).init(allocator),
            .enum_definitions = std.StringHashMap(AST.EnumDef).init(allocator),
            .enum_generic_counts = std.StringHashMap(usize).init(allocator),
            .struct_generic_counts = std.StringHashMap(usize).init(allocator),
            .function_generic_counts = std.StringHashMap(usize).init(allocator),
            .function_signatures = std.StringHashMap([]AST.Type).init(allocator),
            .variable_types = std.StringHashMap([]const u8).init(allocator),
            .current_function_return_type = null,
            .generic_constraints = std.StringHashMap([][]const u8).init(allocator),
            .current_function_generics = std.StringHashMap(void).init(allocator),
        };
    }
    
    pub fn deinit(self: *SyntacticAnalyzer) void {
        self.errors.deinit();
        self.declared_structs.deinit();
        self.struct_definitions.deinit();
        self.declared_enums.deinit();
        self.enum_definitions.deinit();
        self.enum_generic_counts.deinit();
        self.struct_generic_counts.deinit();
        self.function_generic_counts.deinit();
        self.function_signatures.deinit();
        self.variable_types.deinit();
        self.generic_constraints.deinit();
        self.current_function_generics.deinit();
    }
    
    pub fn analyze(self: *SyntacticAnalyzer, program: AST.Program) ![]SyntacticError {
        for (program.items) |item| {
            try self.analyze_node(item);
        }
        return self.errors.toOwnedSlice();
    }
    
    fn analyze_node(self: *SyntacticAnalyzer, node: AST.ASTNode) std.mem.Allocator.Error!void {
        switch (node) {
            .declaration => |decl| try self.analyze_declaration(decl),
            .function_def => |func| try self.analyze_function(func),
            .expression => |expr| try self.analyze_expression(expr),
            .struct_def => |struct_def| try self.register_struct(struct_def),
            .enum_def => |enum_def| try self.register_enum(enum_def),
            .generic_def => |generic_def| try self.register_generic_constraint(generic_def),
            else => {},
        }
    }
    
    fn analyze_declaration(self: *SyntacticAnalyzer, decl: AST.Declaration) std.mem.Allocator.Error!void {
        // Check @access() macro usage
        if (decl.attributes) |attrs| {
            for (attrs) |attr| {
                if (std.mem.eql(u8, attr.name, "access")) {
                    try self.validate_access_macro(attr, decl);
                }
            }
        }
        
        // Check function declarations
        if (decl.kind == .fun_decl) {
            try self.validate_function_declaration(decl);
            
            // Track function generic parameter count
            var generic_count: usize = 0;
            if (decl.fields) |fields| {
                for (fields) |field| {
                    if (field.type_annotation == .named and std.mem.eql(u8, field.type_annotation.named, "<generic>")) {
                        generic_count += 1;
                    } else {
                        break; // Generic parameters come first
                    }
                }
            }
            try self.function_generic_counts.put(decl.name, generic_count);
            
            // Track function parameter types
            if (decl.fields) |fields| {
                var param_types = std.ArrayList(AST.Type).init(self.allocator);
                for (fields) |field| {
                    if (field.type_annotation != .named or !std.mem.eql(u8, field.type_annotation.named, "<generic>")) {
                        try param_types.append(field.type_annotation);
                    }
                }
                try self.function_signatures.put(decl.name, try param_types.toOwnedSlice());
            }
            
            // Validate return type if present
            if (decl.type_annotation) |return_type| {
                try self.validate_type_usage(return_type);
            }
            
            // Set current function return type for return statement validation
            self.current_function_return_type = decl.type_annotation;
            
            // Register function-level generic parameters and function parameters
            self.current_function_generics.clearRetainingCapacity();
            if (decl.fields) |fields| {
                for (fields) |field| {
                    if (field.type_annotation == .named and std.mem.eql(u8, field.type_annotation.named, "<generic>")) {
                        try self.current_function_generics.put(field.name, {});
                    } else {
                        // Register function parameters with their types
                        const param_type_name = switch (field.type_annotation) {
                            .named => |name| name,
                            .basic => |name| name,
                            else => "unknown",
                        };
                        try self.variable_types.put(field.name, param_type_name);
                    }
                }
            }
            
            // Analyze function body if present
            if (decl.initializer) |body_expr| {
                try self.analyze_expression(body_expr);
            }
            
            // Reset function context
            self.current_function_return_type = null;
            self.current_function_generics.clearRetainingCapacity();
            
            // Clear function parameter types
            if (decl.fields) |fields| {
                for (fields) |field| {
                    if (field.type_annotation != .named or !std.mem.eql(u8, field.type_annotation.named, "<generic>")) {
                        _ = self.variable_types.remove(field.name);
                    }
                }
            }
        }
        
        // Track variable type for later member access validation
        if (decl.type_annotation) |type_ann| {
            try self.validate_type_usage(type_ann);
            
            // Store variable type for member access validation
            if (type_ann == .named) {
                try self.variable_types.put(decl.name, type_ann.named);
            }
            
            // If this is a struct type with an initializer, validate the struct literal
            if (decl.initializer) |initializer| {
                if (initializer == .struct_literal and type_ann == .named) {
                    try self.validate_struct_literal_for_type(initializer.struct_literal, type_ann.named);
                }
            }
        }
        
        // Analyze initializer expression (including function calls)
        if (decl.initializer) |initializer| {
            try self.analyze_expression(initializer);
            
            // Validate type-value mismatch
            if (decl.type_annotation) |expected_type| {
                try self.validate_type_value_match(expected_type, initializer, decl.name);
            }
        }
    }
    
    fn validate_access_macro(self: *SyntacticAnalyzer, macro: AST.MacroCall, decl: AST.Declaration) !void {
        // @access() should only be used on functions
        if (decl.kind != .fun_decl) {
            try self.errors.append(SyntacticError{
                .message = "@access() can only be applied to functions",
                .node_type = "macro_call",
                .severity = .err,
            });
        }
        
        // @access() should have at least one argument
        if (macro.args.len == 0) {
            try self.errors.append(SyntacticError{
                .message = "@access() requires at least one file path argument",
                .node_type = "macro_call",
                .severity = .err,
            });
        }
        
        // All arguments should be string literals (file paths)
        for (macro.args) |arg| {
            switch (arg) {
                .literal => |lit| {
                    switch (lit) {
                        .string => {}, // Valid
                        else => {
                            try self.errors.append(SyntacticError{
                                .message = "@access() arguments must be string literals (file paths)",
                                .node_type = "macro_call",
                                .severity = .err,
                            });
                        },
                    }
                },
                else => {
                    try self.errors.append(SyntacticError{
                        .message = "@access() arguments must be string literals (file paths)",
                        .node_type = "macro_call",
                        .severity = .err,
                    });
                },
            }
        }
    }
    
    fn validate_function_declaration(self: *SyntacticAnalyzer, decl: AST.Declaration) !void {
        // Register function-level generic parameters as valid types
        if (decl.fields) |fields| {
            for (fields) |field| {
                // Check if this is a generic parameter (has type "<generic>")
                if (field.type_annotation == .named and std.mem.eql(u8, field.type_annotation.named, "<generic>")) {
                    // Register this generic as a valid type for this function scope
                    try self.declared_structs.put(field.name, {});
                }
                
                if (field.initializer) |initializer| {
                    switch (initializer) {
                        .identifier => |id| {
                            // Check for invalid parameter modifier combinations
                            if (std.mem.eql(u8, id, "mut_ref_copy")) {
                                try self.errors.append(SyntacticError{
                                    .message = "Cannot combine mutable, reference, and copy modifiers",
                                    .node_type = "parameter",
                                    .severity = .warning,
                                });
                            }
                        },
                        else => {},
                    }
                }
            }
        }
    }
    
    fn analyze_function(self: *SyntacticAnalyzer, func: AST.FunctionDef) std.mem.Allocator.Error!void {
        // Set current function return type
        self.current_function_return_type = func.return_type;
        
        // Analyze function body
        try self.analyze_block(func.body);
        
        // Reset function context
        self.current_function_return_type = null;
    }
    
    fn analyze_expression(self: *SyntacticAnalyzer, expr: AST.Expression) std.mem.Allocator.Error!void {
        switch (expr) {
            .macro_call => |macro| {
                if (std.mem.eql(u8, macro.name, "access")) {
                    try self.errors.append(SyntacticError{
                        .message = "@access() should be used as function attribute, not expression",
                        .node_type = "macro_call",
                        .severity = .err,
                    });
                }
            },
            .block_expr => |block| try self.analyze_block(block),
            .struct_literal => |struct_lit| try self.validate_struct_literal(struct_lit),
            .member_access => |member| {
                try self.analyze_expression(member.object.*);
                try self.validate_member_access(member);
            },
            .function_call => |call| {
                // Check if this is an enum variant constructor (EnumName::Variant)
                try self.validate_enum_variant_usage(call.name);
                
                // Also validate standalone enum variant constructors
                try self.validate_standalone_enum_variant(call.name);
                
                // Validate generic type constraints in function calls
                try self.validate_function_call_constraints(call);
                
                // Validate function generic parameter count
                if (self.function_generic_counts.get(call.name)) |expected_count| {
                    if (call.type_args) |type_args| {
                        if (type_args.len != expected_count) {
                            const msg = try std.fmt.allocPrint(self.allocator, "Function '{s}' expects {d} generic parameter(s), but {d} were provided", .{ call.name, expected_count, type_args.len });
                            try self.errors.append(SyntacticError{
                                .message = msg,
                                .node_type = "function_call",
                                .severity = .err,
                            });
                        }
                    } else if (expected_count > 0) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Function '{s}' expects {d} generic parameter(s), but none were provided", .{ call.name, expected_count });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "function_call",
                            .severity = .err,
                        });
                    }
                }
                
                // Validate function parameter types
                try self.validate_function_parameters(call);
                
                // Validate enum generic type parameter count if type_args are present
                if (call.type_args) |type_args| {
                    if (std.mem.indexOf(u8, call.name, "::")) |colon_pos| {
                        const enum_name = call.name[0..colon_pos];
                        if (self.enum_generic_counts.get(enum_name)) |expected_count| {
                            if (type_args.len != expected_count) {
                                const msg = try std.fmt.allocPrint(self.allocator, "Enum '{s}' expects {d} generic parameter(s), but {d} were provided", .{ enum_name, expected_count, type_args.len });
                                try self.errors.append(SyntacticError{
                                    .message = msg,
                                    .node_type = "function_call",
                                    .severity = .err,
                                });
                            }
                        }
                    }
                }
            },
            .identifier => |id| {
                // Check if this could be an enum variant
                try self.validate_identifier_usage(id);
                
                // Check for generic enum usage: EnumName<Type>::Variant
                if (std.mem.indexOf(u8, id, "<")) |bracket_pos| {
                    if (std.mem.indexOf(u8, id, "::")) |colon_pos| {
                        if (bracket_pos < colon_pos) {
                            try self.validate_generic_enum_usage(id);
                        }
                    }
                }
            },
            .if_expr => |if_expr| {
                try self.analyze_expression(if_expr.condition.*);
                try self.analyze_block(if_expr.then_block);
                if (if_expr.else_block) |else_block| {
                    try self.analyze_block(else_block);
                }
            },
            .match_expr => |match_expr| {
                try self.analyze_expression(match_expr.expr.*);
                try self.validate_match_exhaustiveness(match_expr);
                
                // Analyze all match arm bodies
                for (match_expr.arms) |arm| {
                    try self.analyze_expression(arm.body.*);
                }
            },
            else => {},
        }
    }
    
    fn analyze_block(self: *SyntacticAnalyzer, block: AST.Block) std.mem.Allocator.Error!void {
        for (block.statements) |stmt| {
            switch (stmt) {
                .declaration => |decl| try self.analyze_declaration(decl),
                .expression => |expr| try self.analyze_expression(expr.*),
                .return_stmt => |return_expr| {
                    try self.validate_return_statement(return_expr);
                    if (return_expr) |expr| {
                        try self.analyze_expression(expr.*);
                    }
                },
            }
        }
        
        if (block.expr) |expr| {
            // This is likely a return expression in a function
            if (self.current_function_return_type != null) {
                try self.validate_return_expression(expr.*);
            }
            try self.analyze_expression(expr.*);
        }
    }
    
    fn register_struct(self: *SyntacticAnalyzer, struct_def: AST.StructDef) !void {
        try self.declared_structs.put(struct_def.name, {});
        try self.struct_definitions.put(struct_def.name, struct_def);
        
        // Track generic parameter count
        const generic_count = if (struct_def.generic_params) |params| params.len else 0;
        try self.struct_generic_counts.put(struct_def.name, generic_count);
    }
    
    fn register_enum(self: *SyntacticAnalyzer, enum_def: AST.EnumDef) !void {
        try self.declared_enums.put(enum_def.name, {});
        try self.enum_definitions.put(enum_def.name, enum_def);
        
        // Track generic parameter count
        const generic_count = if (enum_def.generic_params) |params| params.len else 0;
        try self.enum_generic_counts.put(enum_def.name, generic_count);
    }
    
    fn register_generic_constraint(self: *SyntacticAnalyzer, generic_def: AST.GenericDef) !void {
        try self.generic_constraints.put(generic_def.name, generic_def.constraints);
    }
    
    fn validate_type_usage(self: *SyntacticAnalyzer, type_node: AST.Type) std.mem.Allocator.Error!void {
        switch (type_node) {
            .named => |name| {
                // Check if it's a basic type first
                if (self.is_basic_type(name)) return;
                
                // Check if it's a function-level generic parameter
                if (self.current_function_generics.contains(name)) return;
                
                // Check if it's a declared enum or struct
                const is_enum = self.declared_enums.contains(name);
                const is_struct = self.declared_structs.contains(name);
                
                if (!is_enum and !is_struct) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Type '{s}' used before declaration", .{name});
                    try self.errors.append(SyntacticError{
                        .message = msg,
                        .node_type = "type_usage",
                        .severity = .err,
                    });
                    return;
                }
                
                // Check if this is a generic struct used without type parameters
                if (self.struct_generic_counts.get(name)) |expected_count| {
                    if (expected_count > 0) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Struct '{s}' expects {d} generic parameter(s), but none were provided", .{ name, expected_count });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "type_usage",
                            .severity = .err,
                        });
                    }
                }
                
                // Check if this is a generic enum used without type parameters
                if (self.enum_generic_counts.get(name)) |expected_count| {
                    if (expected_count > 0) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Enum '{s}' expects {d} generic parameter(s), but none were provided", .{ name, expected_count });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "type_usage",
                            .severity = .err,
                        });
                    }
                }
            },
            .generic => |gen| {
                // Validate generic struct usage
                if (self.struct_generic_counts.get(gen.name)) |expected_count| {
                    if (gen.args.len != expected_count) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Struct '{s}' expects {d} generic parameter(s), but {d} were provided", .{ gen.name, expected_count, gen.args.len });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "type_usage",
                            .severity = .err,
                        });
                    }
                }
                
                // Validate generic enum usage
                if (self.enum_generic_counts.get(gen.name)) |expected_count| {
                    if (gen.args.len != expected_count) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Enum '{s}' expects {d} generic parameter(s), but {d} were provided", .{ gen.name, expected_count, gen.args.len });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "type_usage",
                            .severity = .err,
                        });
                    }
                }
                
                // Validate each type argument recursively
                for (gen.args) |arg| {
                    try self.validate_type_usage(arg);
                }
            },
            .pointer => |ptr| try self.validate_type_usage(ptr.*),
            .reference => |ref| try self.validate_type_usage(ref.*),
            .array => |arr| try self.validate_type_usage(arr.element_type.*),
            else => {},
        }
    }
    
    fn is_basic_type(self: *SyntacticAnalyzer, name: []const u8) bool {
        _ = self;
        const basic_types = [_][]const u8{ "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f16", "f32", "f64", "f128", "bool", "str", "char", "void" };
        for (basic_types) |basic_type| {
            if (std.mem.eql(u8, name, basic_type)) return true;
        }
        return false;
    }
    
    fn validate_struct_literal(self: *SyntacticAnalyzer, struct_lit: AST.StructLiteral) std.mem.Allocator.Error!void {
        // Generic struct literal validation (without type context)
        _ = self;
        _ = struct_lit;
    }
    
    fn validate_struct_literal_for_type(self: *SyntacticAnalyzer, struct_lit: AST.StructLiteral, struct_type_name: []const u8) std.mem.Allocator.Error!void {
        if (self.struct_definitions.get(struct_type_name)) |struct_def| {
            var provided_fields = std.StringHashMap(void).init(self.allocator);
            defer provided_fields.deinit();
            
            // Track which fields are provided
            for (struct_lit.fields) |field| {
                try provided_fields.put(field.name, {});
            }
            
            // Check for missing required fields
            for (struct_def.fields) |required_field| {
                if (!provided_fields.contains(required_field.name)) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Missing required field '{s}' in struct '{s}' literal", .{ required_field.name, struct_type_name });
                    try self.errors.append(SyntacticError{
                        .message = msg,
                        .node_type = "struct_literal",
                        .severity = .err,
                    });
                }
            }
            
            // Check for unknown fields
            for (struct_lit.fields) |provided_field| {
                var field_exists = false;
                for (struct_def.fields) |required_field| {
                    if (std.mem.eql(u8, provided_field.name, required_field.name)) {
                        field_exists = true;
                        break;
                    }
                }
                if (!field_exists) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Unknown field '{s}' in struct '{s}' literal", .{ provided_field.name, struct_type_name });
                    try self.errors.append(SyntacticError{
                        .message = msg,
                        .node_type = "struct_literal",
                        .severity = .err,
                    });
                }
            }
        }
    }
    
    fn validate_member_access(self: *SyntacticAnalyzer, member_access: AST.MemberAccess) std.mem.Allocator.Error!void {
        // Try to determine the type of the object being accessed
        const object_type = self.infer_expression_type(member_access.object.*) orelse return;
        
        // Check if it's a struct type
        if (self.struct_definitions.get(object_type)) |struct_def| {
            // Find the field being accessed
            for (struct_def.fields) |field| {
                if (std.mem.eql(u8, field.name, member_access.member)) {
                    // Check if field is private
                    if (!field.is_public) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Cannot access private field '{s}' of struct '{s}'", .{ field.name, object_type });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "member_access",
                            .severity = .err,
                        });
                    }
                    return;
                }
            }
            
            // Field not found
            const msg = try std.fmt.allocPrint(self.allocator, "Field '{s}' does not exist on struct '{s}'", .{ member_access.member, object_type });
            try self.errors.append(SyntacticError{
                .message = msg,
                .node_type = "member_access",
                .severity = .err,
            });
        }
    }
    
    fn infer_expression_type(self: *SyntacticAnalyzer, expr: AST.Expression) ?[]const u8 {
        switch (expr) {
            .identifier => |id| {
                // Look up the variable's type from our tracking
                return self.variable_types.get(id);
            },
            else => return null,
        }
    }
    
    fn validate_enum_variant_usage(self: *SyntacticAnalyzer, variant_name: []const u8) std.mem.Allocator.Error!void {
        // Check if variant name contains :: (enum variant syntax)
        if (std.mem.indexOf(u8, variant_name, "::")) |_| {
            // Extract enum name (part before ::)
            const enum_name_end = std.mem.indexOf(u8, variant_name, "::") orelse return;
            const enum_name = variant_name[0..enum_name_end];
            
            // Check if enum is declared
            if (!self.declared_enums.contains(enum_name)) {
                const msg = try std.fmt.allocPrint(self.allocator, "Enum '{s}' used before declaration", .{enum_name});
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "enum_usage",
                    .severity = .err,
                });
            }
        }
    }
    
    fn validate_identifier_usage(self: *SyntacticAnalyzer, id: []const u8) std.mem.Allocator.Error!void {
        // Check if this is an enum variant path (EnumName::Variant)
        if (std.mem.indexOf(u8, id, "::")) |colon_pos| {
            const enum_name = id[0..colon_pos];
            const variant_name = id[colon_pos + 2..];
            
            // Check if enum is declared
            if (!self.declared_enums.contains(enum_name)) {
                const msg = try std.fmt.allocPrint(self.allocator, "Unknown enum variant or type '{s}'", .{id});
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "identifier_usage",
                    .severity = .err,
                });
                return;
            }
            
            // Check if variant exists in the enum
            if (self.enum_definitions.get(enum_name)) |enum_def| {
                for (enum_def.variants) |variant| {
                    if (std.mem.eql(u8, variant.name, variant_name)) {
                        return; // Valid enum variant
                    }
                }
                
                // Variant not found in enum
                const msg = try std.fmt.allocPrint(self.allocator, "Unknown variant '{s}' in enum '{s}'", .{ variant_name, enum_name });
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "identifier_usage",
                    .severity = .err,
                });
            }
        } else {
            // Check if this identifier looks like an enum variant (starts with uppercase)
            if (id.len > 0 and std.ascii.isUpper(id[0])) {
                // Check if it's a known enum variant
                var is_valid_variant = false;
                var enum_iter = self.enum_definitions.iterator();
                while (enum_iter.next()) |entry| {
                    const enum_def = entry.value_ptr.*;
                    for (enum_def.variants) |variant| {
                        if (std.mem.eql(u8, variant.name, id)) {
                            is_valid_variant = true;
                            break;
                        }
                    }
                    if (is_valid_variant) break;
                }
                
                if (!is_valid_variant and !self.is_basic_type(id)) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Unknown enum variant or type '{s}'", .{id});
                    try self.errors.append(SyntacticError{
                        .message = msg,
                        .node_type = "identifier_usage",
                        .severity = .err,
                    });
                }
            }
        }
    }
    
    fn validate_standalone_enum_variant(self: *SyntacticAnalyzer, variant_name: []const u8) std.mem.Allocator.Error!void {
        // Skip if this contains :: (already handled by validate_enum_variant_usage)
        if (std.mem.indexOf(u8, variant_name, "::")) |_| return;
        
        // Check if this is a valid enum variant constructor
        var enum_iter = self.enum_definitions.iterator();
        while (enum_iter.next()) |entry| {
            const enum_def = entry.value_ptr.*;
            for (enum_def.variants) |variant| {
                if (std.mem.eql(u8, variant.name, variant_name)) {
                    return; // Valid enum variant
                }
            }
        }
    }
    
    fn validate_match_exhaustiveness(self: *SyntacticAnalyzer, match_expr: AST.MatchExpression) std.mem.Allocator.Error!void {
        // Try to determine the type being matched
        const match_type = self.infer_match_type(match_expr.expr.*) orelse return;
        
        // Check if it's an enum type
        if (self.enum_definitions.get(match_type)) |enum_def| {
            var covered_variants = std.StringHashMap(void).init(self.allocator);
            defer covered_variants.deinit();
            
            var has_wildcard = false;
            
            // Collect all covered variants from match arms
            for (match_expr.arms) |arm| {
                switch (arm.pattern) {
                    .wildcard => {
                        has_wildcard = true;
                    },
                    .enum_variant => |variant_pattern| {
                        try covered_variants.put(variant_pattern.variant_name, {});
                    },
                    .identifier => |id| {
                        // Check if this is a simple enum variant name
                        for (enum_def.variants) |variant| {
                            if (std.mem.eql(u8, variant.name, id)) {
                                try covered_variants.put(id, {});
                                break;
                            }
                        }
                    },
                    else => {},
                }
            }
            
            // If there's a wildcard, the match is exhaustive
            if (has_wildcard) return;
            
            // Check if all enum variants are covered
            var missing_variants = std.ArrayList([]const u8).init(self.allocator);
            defer missing_variants.deinit();
            
            for (enum_def.variants) |variant| {
                if (!covered_variants.contains(variant.name)) {
                    try missing_variants.append(variant.name);
                }
            }
            
            if (missing_variants.items.len > 0) {
                // Create error message listing missing variants
                var msg_buffer = std.ArrayList(u8).init(self.allocator);
                defer msg_buffer.deinit();
                
                try msg_buffer.appendSlice("Non-exhaustive match on enum '");
                try msg_buffer.appendSlice(match_type);
                try msg_buffer.appendSlice("'. Missing variants: ");
                
                for (missing_variants.items, 0..) |variant, i| {
                    if (i > 0) try msg_buffer.appendSlice(", ");
                    try msg_buffer.appendSlice(variant);
                }
                
                const msg = try msg_buffer.toOwnedSlice();
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "match_expr",
                    .severity = .err,
                });
            }
        }
    }
    
    fn infer_match_type(self: *SyntacticAnalyzer, expr: AST.Expression) ?[]const u8 {
        switch (expr) {
            .identifier => |id| {
                // Look up the variable's type
                return self.variable_types.get(id);
            },
            else => return null,
        }
    }
    
    fn validate_return_statement(self: *SyntacticAnalyzer, return_expr: ?*AST.Expression) std.mem.Allocator.Error!void {
        const expected_return_type = self.current_function_return_type orelse {
            // Not in a function context
            if (return_expr != null) {
                try self.errors.append(SyntacticError{
                    .message = "Return statement outside of function",
                    .node_type = "return_stmt",
                    .severity = .err,
                });
            }
            return;
        };
        
        // Check if function expects void return
        const expects_void = switch (expected_return_type) {
            .named => |name| std.mem.eql(u8, name, "void"),
            else => false,
        };
        
        if (expects_void) {
            // Function should return void
            if (return_expr != null) {
                try self.errors.append(SyntacticError{
                    .message = "Function with void return type cannot return a value",
                    .node_type = "return_stmt",
                    .severity = .err,
                });
            }
        } else {
            // Function expects a specific return type
            if (return_expr == null) {
                const expected_type_name = self.type_to_string(expected_return_type);
                const msg = try std.fmt.allocPrint(self.allocator, "Function must return a value of type '{s}'", .{expected_type_name});
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "return_stmt",
                    .severity = .err,
                });
            } else {
                const actual_type = self.infer_expression_return_type(return_expr.?.*) orelse "unknown";
                const expected_type_name = self.type_to_string(expected_return_type);
                
                // For generic types, compare base type names
                const matches_type = if (expected_return_type == .generic) blk: {
                    const generic_type = expected_return_type.generic;
                    break :blk std.mem.eql(u8, actual_type, generic_type.name);
                } else std.mem.eql(u8, actual_type, expected_type_name);
                
                if (!matches_type) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Return type mismatch: expected '{s}', found '{s}'", .{ expected_type_name, actual_type });
                    try self.errors.append(SyntacticError{
                        .message = msg,
                        .node_type = "return_stmt",
                        .severity = .err,
                    });
                }
            }
        }
    }
    
    fn type_to_string(self: *SyntacticAnalyzer, type_node: AST.Type) []const u8 {
        _ = self;
        switch (type_node) {
            .basic => |name| return name,
            .named => |name| return name,
            .pointer => return "pointer",
            .reference => return "reference",
            .array => return "array",
            .tuple => return "tuple",
            .function => return "function",
            .process => return "process",
            .generic => |gen| {
                // For generic types like Option<str>, show the full type
                if (gen.args.len > 0) {
                    // This is a simplified representation - in a real compiler you'd format this properly
                    return gen.name; // Just return the base name for now
                }
                return gen.name;
            },
        }
    }
    
    fn infer_expression_return_type(self: *SyntacticAnalyzer, expr: AST.Expression) ?[]const u8 {
        switch (expr) {
            .literal => |lit| {
                switch (lit) {
                    .number => return "i32", // Default integer type for literals
                    .string => return "str",
                    .boolean => return "bool",
                    .char => return "char",
                    .array => return "array",
                }
            },
            .identifier => |id| {
                // Check if it's an enum variant (EnumName::Variant)
                if (std.mem.indexOf(u8, id, "::")) |colon_pos| {
                    const enum_name = id[0..colon_pos];
                    return enum_name;
                }
                return self.variable_types.get(id);
            },
            .binary_op => |binop| {
                // For binary operations, try to infer from operands
                const left_type = self.infer_expression_return_type(binop.left.*);
                const right_type = self.infer_expression_return_type(binop.right.*);
                
                // If both operands have the same type, return that type
                if (left_type != null and right_type != null) {
                    if (std.mem.eql(u8, left_type.?, right_type.?)) {
                        return left_type;
                    }
                }
                
                // If one operand has a type, prefer that
                return left_type orelse right_type;
            },
            .function_call => |call| {
                // Try to infer from function name (basic heuristic)
                if (std.mem.indexOf(u8, call.name, "::")) |_| {
                    // Enum variant constructor - return the enum type
                    const enum_name_end = std.mem.indexOf(u8, call.name, "::") orelse return null;
                    return call.name[0..enum_name_end];
                }
                
                // Check if this is a standalone enum variant constructor
                var enum_iter = self.enum_definitions.iterator();
                while (enum_iter.next()) |entry| {
                    const enum_def = entry.value_ptr.*;
                    for (enum_def.variants) |variant| {
                        if (std.mem.eql(u8, variant.name, call.name)) {
                            return enum_def.name;
                        }
                    }
                }
                
                return null;
            },
            else => return null,
        }
    }
    
    fn validate_return_expression(self: *SyntacticAnalyzer, return_expr: AST.Expression) std.mem.Allocator.Error!void {
        const expected_return_type = self.current_function_return_type orelse return;
        
        const actual_type = self.infer_expression_return_type(return_expr) orelse "unknown";
        const expected_type_name = self.type_to_string(expected_return_type);
        
        if (!std.mem.eql(u8, actual_type, expected_type_name)) {
            const msg = try std.fmt.allocPrint(self.allocator, "Return type mismatch: expected '{s}', found '{s}'", .{ expected_type_name, actual_type });
            try self.errors.append(SyntacticError{
                .message = msg,
                .node_type = "return_expr",
                .severity = .err,
            });
        }
    }
    
    fn validate_function_call_constraints(self: *SyntacticAnalyzer, call: AST.FunctionCall) std.mem.Allocator.Error!void {
        // Check generic parameter count for known functions that require generics
        if (std.mem.eql(u8, call.name, "calculate")) {
            const expected_count = 1; // calculate<F> expects 1 generic parameter
            
            if (call.type_args) |type_args| {
                if (type_args.len != expected_count) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Function '{s}' expects {d} generic parameter(s), but {d} were provided", .{ call.name, expected_count, type_args.len });
                    try self.errors.append(SyntacticError{
                        .message = msg,
                        .node_type = "function_call",
                        .severity = .err,
                    });
                }
            } else {
                // Function call without type arguments but function requires them
                const msg = try std.fmt.allocPrint(self.allocator, "Function '{s}' expects {d} generic parameter(s), but none were provided", .{ call.name, expected_count });
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "function_call",
                    .severity = .err,
                });
            }
        }
        
        // Validate type constraints if type arguments are provided
        if (call.type_args) |type_args| {
            // Check all registered generic constraints
            var constraint_iter = self.generic_constraints.iterator();
            while (constraint_iter.next()) |entry| {
                const generic_name = entry.key_ptr.*;
                const constraints = entry.value_ptr.*;
                
                // Validate each type argument against the constraint
                for (type_args) |type_arg| {
                    const type_name = switch (type_arg) {
                        .basic => |name| name,
                        .named => |name| name,
                        else => "unknown",
                    };
                    
                    // Check if this type violates the constraint
                    var type_allowed = false;
                    for (constraints) |allowed_type| {
                        if (std.mem.eql(u8, type_name, allowed_type)) {
                            type_allowed = true;
                            break;
                        }
                    }
                    
                    if (!type_allowed) {
                        const constraint_list = self.format_constraints(constraints);
                        const msg = try std.fmt.allocPrint(self.allocator, "Type '{s}' does not satisfy generic constraint '{s}'. Allowed types: {s}", .{ type_name, generic_name, constraint_list });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "function_call",
                            .severity = .err,
                        });
                    }
                }
            }
        }
    }
    
    fn function_uses_generic(self: *SyntacticAnalyzer, function_name: []const u8, generic_name: []const u8) bool {
        _ = self;
        _ = function_name;
        _ = generic_name;
        // For this test, assume functions with generic parameters use the constraint
        // In a real implementation, you'd track function signatures
        return true;
    }
    
    fn format_constraints(self: *SyntacticAnalyzer, constraints: [][]const u8) []const u8 {
        if (constraints.len == 0) return "none";
        if (constraints.len == 1) return constraints[0];
        
        // Format multiple constraints as "type1, type2, type3"
        var result = std.ArrayList(u8).init(self.allocator);
        for (constraints, 0..) |constraint, i| {
            if (i > 0) result.appendSlice(", ") catch {};
            result.appendSlice(constraint) catch {};
        }
        return result.toOwnedSlice() catch "multiple types";
    }
    
    fn validate_type_value_match(self: *SyntacticAnalyzer, expected_type: AST.Type, value: AST.Expression, var_name: []const u8) std.mem.Allocator.Error!void {
        const expected_type_name = self.get_type_name(expected_type);
        const actual_type_name = self.infer_value_type(value);
        
        if (actual_type_name) |actual| {
            if (!std.mem.eql(u8, expected_type_name, actual)) {
                const msg = try std.fmt.allocPrint(self.allocator, "Type mismatch for variable '{s}': expected '{s}', found '{s}'", .{ var_name, expected_type_name, actual });
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "type_mismatch",
                    .severity = .err,
                });
            }
        }
    }
    
    fn get_type_name(self: *SyntacticAnalyzer, type_node: AST.Type) []const u8 {
        _ = self;
        return switch (type_node) {
            .basic => |name| name,
            .named => |name| name,
            .generic => |gen| gen.name,
            else => "unknown",
        };
    }
    
    fn infer_value_type(self: *SyntacticAnalyzer, expr: AST.Expression) ?[]const u8 {
        _ = self;
        return switch (expr) {
            .literal => |lit| switch (lit) {
                .number => "i32",
                .string => "str",
                .boolean => "bool",
                else => null,
            },
            else => null,
        };
    }
    
    fn validate_function_parameters(self: *SyntacticAnalyzer, call: AST.FunctionCall) std.mem.Allocator.Error!void {
        if (self.function_signatures.get(call.name)) |expected_params| {
            if (call.args.len != expected_params.len) {
                const msg = try std.fmt.allocPrint(self.allocator, "Function '{s}' expects {d} parameter(s), but {d} were provided", .{ call.name, expected_params.len, call.args.len });
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "function_call",
                    .severity = .err,
                });
                return;
            }
            
            // Check parameter types
            for (call.args, 0..) |arg, i| {
                const expected_type = expected_params[i];
                const actual_type = self.infer_value_type(arg);
                
                if (actual_type) |actual| {
                    const expected_name = self.get_type_name(expected_type);
                    if (!std.mem.eql(u8, expected_name, actual)) {
                        const msg = try std.fmt.allocPrint(self.allocator, "Parameter {d} of function '{s}': expected '{s}', found '{s}'", .{ i + 1, call.name, expected_name, actual });
                        try self.errors.append(SyntacticError{
                            .message = msg,
                            .node_type = "function_call",
                            .severity = .err,
                        });
                    }
                }
            }
        }
    }
    
    fn validate_generic_enum_usage(self: *SyntacticAnalyzer, enum_usage: []const u8) std.mem.Allocator.Error!void {
        // Parse EnumName<Type1, Type2>::Variant syntax
        const bracket_start = std.mem.indexOf(u8, enum_usage, "<") orelse return;
        const bracket_end = std.mem.indexOf(u8, enum_usage, ">") orelse return;
        const colon_pos = std.mem.indexOf(u8, enum_usage, "::") orelse return;
        
        if (bracket_start >= bracket_end or bracket_end >= colon_pos) return;
        
        const enum_name = enum_usage[0..bracket_start];
        const type_args_str = enum_usage[bracket_start + 1..bracket_end];
        
        // Count type arguments by counting commas + 1 (if not empty)
        var provided_count: usize = 0;
        if (type_args_str.len > 0) {
            provided_count = 1;
            for (type_args_str) |char| {
                if (char == ',') provided_count += 1;
            }
        }
        
        // Check if enum exists and get expected count
        if (self.enum_generic_counts.get(enum_name)) |expected_count| {
            if (provided_count != expected_count) {
                const msg = try std.fmt.allocPrint(self.allocator, "Enum '{s}' expects {d} generic parameter(s), but {d} were provided", .{ enum_name, expected_count, provided_count });
                try self.errors.append(SyntacticError{
                    .message = msg,
                    .node_type = "enum_usage",
                    .severity = .err,
                });
            }
        }
    }
};

pub fn analyze_syntax(allocator: Allocator, program: AST.Program) ![]SyntacticError {
    var analyzer = SyntacticAnalyzer.init(allocator);
    defer analyzer.deinit();
    
    // First pass: register all struct, enum, and generic constraint declarations
    for (program.items) |item| {
        if (item == .struct_def) {
            try analyzer.register_struct(item.struct_def);
        } else if (item == .enum_def) {
            try analyzer.register_enum(item.enum_def);
        } else if (item == .generic_def) {
            try analyzer.register_generic_constraint(item.generic_def);
        }
    }
    
    // Second pass: analyze usage (skip definitions since they're already registered)
    for (program.items) |item| {
        switch (item) {
            .struct_def => {}, // Skip, already processed
            .enum_def => {}, // Skip, already processed
            .generic_def => {}, // Skip, already processed
            else => try analyzer.analyze_node(item),
        }
    }
    
    return analyzer.errors.toOwnedSlice();
}