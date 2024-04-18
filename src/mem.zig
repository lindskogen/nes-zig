pub const Mem = struct {
  internal: [65_536]u8,


  pub fn write(self: *Mem, k: u16, v: u8) void {
    self.internal[k] = v;
  }

  pub fn read(self: *Mem, k: u16) u8 {
    return self.internal[k];
  }
};
