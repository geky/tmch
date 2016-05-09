#ifndef TMACHINE_H
#define TMACHINE_H

#include "mem.h"
#include <stdbool.h>

// Terse machine state
struct tmch {
    unsigned bits;
    bool nz;
    unsigned regs[4];
    struct mem mem;
};

// Machine lifetime
void tmch_create(struct tmch *tmch, unsigned bits);
void tmch_destroy(struct tmch *tmch);

// Step the machine
void tmch_step(struct tmch *tmch);

// Check if machine has been halted
bool tmch_halted(struct tmch *tmch);

#endif
