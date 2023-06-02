// Type conventions and aliases

// R, R1, R2 - result type of parsing - parse tree shape
// IS - input stack for routine
// OS - output stack of routine
// S - stack without input/output meaning
// E - single element on stack

type Nix = EmptyTuple
type RS = [R] =>> R *: Nix
