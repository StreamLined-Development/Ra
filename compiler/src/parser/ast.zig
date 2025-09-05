const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ASTNode = union(enum) {
    program: Program,
    declaration: Declaration,
    function_def: FunctionDef,
    expression: Expression,
    statement: Statement,
    type_def: Type,
    impl_block: ImplBlock,
    struct_def: StructDef,
    enum_def: EnumDef,
    generic_def: GenericDef,
    trait_def: TraitDef,
};

pub const EnumDef = struct {
    name: []const u8,
    generic_params: ?[][]const u8,
    variants: []EnumVariant,
};

pub const EnumVariant = struct {
    name: []const u8,
    fields: ?[]Type,
};

pub const StructDef = struct {
    name: []const u8,
    generic_params: ?[][]const u8,
    fields: []Field,
};

pub const Program = struct {
    items: []ASTNode,
};

pub const Declaration = struct {
    kind: enum { var_decl, let_decl, const_decl, process_decl, fun_decl, struct_decl, typealias_decl },
    is_mut: bool,
    name: []const u8,
    type_annotation: ?Type,
    initializer: ?Expression,
    fields: ?[]Field,
    attributes: ?[]MacroCall,
};

pub const Field = struct {
    name: []const u8,
    type_annotation: Type,
    initializer: ?Expression,
    is_public: bool,
};

pub const ImplBlock = struct {
    trait_name: ?[]const u8, // None for inherent impl, Some for trait impl
    type_name: []const u8,
    methods: []FunctionDef, // Store full function definitions
};

pub const MessageMethod = struct {
    name: []const u8,
    parameters: []Parameter,
    return_type: ?Type,
    body: []Statement,
};

pub const FunctionDef = struct {
    name: []const u8,
    parameters: []Parameter,
    return_type: Type,
    body: Block,
};

pub const Parameter = struct {
    name: []const u8,
    param_type: Type,
    is_mut: bool,
    is_ref: bool,
    is_copy: bool,
};

pub const Expression = union(enum) {
    literal: Literal,
    identifier: []const u8,
    function_call: FunctionCall,
    method_call: MethodCall,
    binary_op: BinaryOp,
    unary_op: UnaryOp,
    if_expr: IfExpression,
    match_expr: MatchExpression,
    block_expr: Block,
    dereference: *Expression,
    member_access: MemberAccess,
    tuple_literal: []Expression,
    struct_literal: StructLiteral,
    range_expr: RangeExpression,
    for_expr: ForExpression,
    while_expr: WhileExpression,
    loop_expr: LoopExpression,
    macro_call: MacroCall,
};

pub const RangeExpression = struct {
    start: *Expression,
    end: *Expression,
    inclusive: bool, // true for ..., false for ..
};

pub const ForExpression = struct {
    variable: []const u8,
    iterable: *Expression,
    body: Block,
    is_inline: bool,
};

pub const WhileExpression = struct {
    condition: *Expression,
    body: Block,
};

pub const LoopExpression = struct {
    body: Block,
};

pub const MacroCall = struct {
    name: []const u8,
    args: []Expression,
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
    char: u8,
    boolean: bool,
    array: []Expression,
};

pub const FunctionCall = struct {
    name: []const u8,
    args: []Expression,
    is_inline: bool,
    type_args: ?[]Type,
};

pub const BinaryOp = struct {
    left: *Expression,
    operator: []const u8,
    right: *Expression,
};

pub const UnaryOp = struct {
    operator: []const u8,
    operand: *Expression,
};

pub const IfExpression = struct {
    condition: *Expression,
    then_block: Block,
    else_block: ?Block,
};

pub const MatchExpression = struct {
    expr: *Expression,
    arms: []MatchArm,
};

pub const MatchArm = struct {
    pattern: Pattern,
    body: *Expression,
};

pub const Pattern = union(enum) {
    literal: Literal,
    identifier: []const u8,
    wildcard,
    enum_variant: EnumVariantPattern,
};

pub const EnumVariantPattern = struct {
    variant_name: []const u8,
    params: ?[][]const u8,
};

pub const Block = struct {
    statements: []Statement,
    expr: ?*Expression,
};

pub const Statement = union(enum) {
    declaration: Declaration,
    expression: *Expression,
    return_stmt: ?*Expression,
};

pub const Type = union(enum) {
    basic: []const u8,
    named: []const u8,
    generic: GenericType,
    array: ArrayType,
    function: FunctionType,
    process: []const u8,
    pointer: *Type,
    reference: *Type,
    tuple: []Type,
};

pub const GenericType = struct {
    name: []const u8,
    args: []Type,
};

pub const ArrayType = struct {
    element_type: *Type,
    size: ?*Expression,
};

pub const FunctionType = struct {
    params: []Type,
    return_type: *Type,
};

pub const MemberAccess = struct {
    object: *Expression,
    member: []const u8,
};

pub const MethodCall = struct {
    object: *Expression,
    method: []const u8,
    args: []Expression,
};

pub const StructLiteral = struct {
    fields: []StructField,
};

pub const StructField = struct {
    name: []const u8,
    value: Expression,
};

pub const GenericDef = struct {
    name: []const u8,
    constraints: [][]const u8,
};

pub const TraitDef = struct {
    name: []const u8,
    methods: []TraitMethod,
};

pub const TraitMethod = struct {
    name: []const u8,
    parameters: []Parameter,
    return_type: ?Type,
};