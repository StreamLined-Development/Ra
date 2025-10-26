const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "parser",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);

    const analyzer_exe = b.addExecutable(.{
        .name = "analyzer",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/analyzer_main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(analyzer_exe);

    const run_step = b.step("run", "Run parser then analyzer");
    const analyze_step = b.step("analyze", "Run syntax analyzer only");

    const run_cmd = b.addRunArtifact(exe);
    const analyze_cmd = b.addRunArtifact(analyzer_exe);

    // Make analyzer depend on parser completion
    analyze_cmd.step.dependOn(&run_cmd.step);

    // Run step runs both parser and analyzer
    run_step.dependOn(&analyze_cmd.step);

    // Analyze step runs only analyzer
    analyze_step.dependOn(&analyze_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());
    analyze_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });

    const run_exe_tests = b.addRunArtifact(exe_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_exe_tests.step);
}
