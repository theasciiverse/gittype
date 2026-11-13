const config = @import("config.zig");
const GameState = @import("gamestate.zig").GameState;
const Move = @import("gamestate.zig").Move;
const VerifiedMove = @import("gamestate.zig").VerifiedMove;
const mibu = @import("mibu");
const events = mibu.events;
const std = @import("std");
const buildopts = @import("buildopts");
const Display = @import("display.zig").Display;
const clock = @import("clock.zig");

var prng: std.Random.Xoshiro256 = undefined;
var rand: std.Random = undefined;
var randInited = false;

// Seeded on first use, not when the player is made: players are made before
// the command line is read, so -s was not known yet, and the clock had only
// just started, so every run without -s got the same seed.
fn getRand() std.Random {
    if (!randInited) {
        randInited = true;
        if (config.RANDOMSEED) |seed| {
            prng = std.Random.DefaultPrng.init(@intCast(seed));
        } else if (buildopts.web) {
            prng = std.Random.DefaultPrng.init(@intCast(clock.millis()));
        } else {
            prng = std.Random.DefaultPrng.init(@truncate(@as(u128, @bitCast(std.time.nanoTimestamp()))));
        }
        rand = prng.random();
    }
    return rand;
}

const UiState = enum {
    Idle,
    Processing,
    Completed,
};

pub const UiAgentRandom = struct {
    const Self = @This();
    state: UiState,
    nextMove: VerifiedMove,

    pub fn init() Self {
        return Self{
            .state = .Idle,
            .nextMove = undefined,
        };
    }

    pub fn paint(self: *Self, display: *Display) !void {
        _ = self;
        _ = display;
    }

    pub fn getAnyLegalMove(self: *const Self, gs: *const GameState, pi: usize) !VerifiedMove{
        _ = self;
        while(true) {
            const move = switch(getRand().int(usize) % 3) {
                0 => Move{ .pawn = .{ .x = @intCast(getRand().int(usize)%9), .y = @intCast(getRand().int(usize)%9) } },
                1 => Move{ .fence = .{ .pos = .{ .x = @intCast(getRand().int(usize)%8), .y = @intCast(getRand().int(usize)%8) }, .dir = .vert } },
                2 => Move{ .fence = .{ .pos = .{ .x = @intCast(getRand().int(usize)%8), .y = @intCast(getRand().int(usize)%8) }, .dir = .horz } },
                else => unreachable,
            };
            const vm = try gs.verifyMove(pi, move);
            if (vm.legal) {
                return vm;
            }
        }
    }

    pub fn handleEvent(self: *Self, event: ?events.Event, gs: *const GameState, pi: usize) !bool {
        _ = gs;
        _ = pi;
        _ = self;
        _ = event;
        return false;
    }

    pub fn process(self: *Self, gs: *const GameState, pi: usize) !void {
        switch (self.state) {
            .Idle, .Completed => {},
            .Processing => { // generating a move
                self.nextMove = try self.getAnyLegalMove(gs, pi);
                self.state = .Completed;
            },
        }
    }

    pub fn selectMoveInteractive(self: *Self, gs: *const GameState, pi: usize) !void {
        _ = gs;
        _ = pi;
        self.state = .Processing;
    }

    pub fn getCompletedMove(self: *Self) ?VerifiedMove {
        switch (self.state) {
            .Completed => return self.nextMove,
            else => return null,
        }
    }
};


