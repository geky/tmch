#ifndef TMACHINE_H
#define TMACHINE_H

#include "mem.h"
#include <stdbool.h>

// Terse machine state
struct tmachine {
    unsigned bits;
    unsigned regs[4];
    struct mem mem;
};

// Machine lifetime
void tmachine_create(struct tmachine *tm, unsigned bits);
void tmachine_destroy(struct tmachine *tm);

// Step the machine
void tmachine_step(struct tmachine *tm);

// Check if machine has been halted
bool tmachine_halted(struct tmachine *tm);

#endif
