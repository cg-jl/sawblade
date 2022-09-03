# register allocation

- What to do to conserve spec based allocations? maybe keeping a
hashset per block to know which resolve allocations?

- for callee/caller saved differences:
 - use a scratch (caller-saved) register when the value doesn't live across a call
 - otherwise when it does


- need a way to make the architecture communicate whether it allows loads
directly from memory/have a (preferred) scratch register. If not, I'll
have to choose one scratch register (probably the last item of the
scratch register range?) 
