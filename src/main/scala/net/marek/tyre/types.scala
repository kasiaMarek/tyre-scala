package net.marek.tyre

// Type conventions and aliases

// R - result type of parsing - parse tree shape
// IS - input stack for routine
// OS - output stack of routine
// S - stack without input/output meaning
// E - single element on stack
// T - elementary TyRE type

type Nix = EmptyTuple
type RS = [R] =>> R *: Nix
