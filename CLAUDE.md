# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

NES emulator written in Zig 0.14.0. Emulates the 6502 CPU and 2C02 PPU with Mapper 0 (NROM) support. Uses Fenster (vendored C library) for cross-platform windowing.

## Build Commands

```bash
zig build              # Build library and executable
zig build run          # Run the emulator (loads roms/nestest.nes by default)
zig build run -- <rom> # Run with a specific ROM
zig build run -- disasm <rom>            # Disassemble a ROM
zig build run -- screenshot <rom> [N]    # Headless render N frames (default 120), outputs framebuffer.ppm
zig build test         # Run all unit tests (lib + exe modules)
zig build -Dno-bin     # Build without emitting binaries
```

Tests are inline within source files (Zig convention). CPU tests use embedded ROMs: `roms/6502_functional_test.bin` and `roms/nestest.nes`.

## Architecture

The emulator follows standard NES architecture with these core components connected through a central bus:

- **Bus** (`src/bus.zig`) - Central interconnect. Implements the CPU memory map ($0000-$FFFF), routes reads/writes to RAM/PPU/ROM, handles controller input and OAM DMA. Owns the clock: PPU ticks every cycle, CPU every 3rd cycle. Detects PPU vblank to trigger NMI on CPU.

- **CPU** (`src/cpu.zig`) - 6502 processor with registers (A, X, Y, SP, PC, status flags), ~80+ opcodes, cycle counting, and IRQ/NMI interrupt handling.

- **PPU** (`src/ppu.zig`) - Picture Processing Unit. Manages VRAM (pattern tables, nametables, palette), OAM (64 sprites), and renders 256x240 frames. Handles background tiles, 8x8 sprites with flipping/priority, sprite 0 hit detection, and vblank NMI signaling.

- **ROM** (`src/rom.zig`) - iNES format parser. Loads PRG/CHR ROM banks. Only Mapper 0 implemented; PRG mirrored if 16KB, CHR RAM used when chr_rom_size is 0.

- **Addressing** (`src/addr.zig`) - Union type for 12 6502 addressing modes.

- **Debug** (`src/debug.zig`) - Opcode table (256 entries with mnemonics) and disassembler.

- **Main** (`src/main.zig`) - Entry point. GUI mode renders at 2x scale (512x480) at 60fps. Maps keyboard to NES controller (Z=A, X=B, Shift=Select, Enter=Start, arrows=D-pad, Esc=quit).

## Platform Dependencies

Linux requires libX11-dev. macOS links Cocoa. Windows links gdi32.
