#include <stdio.h>

#include "mem.h"
#include "tmch.h"


struct tmch tm;

int main() {
    tmch_create(&tm, 12);
    mem_read(&tm.mem, stdin);

    while (!tmch_halted(&tm)) {
        tmch_step(&tm);
        printf("\r[%03x %03x %03x %03x]",
            tm.regs[0], tm.regs[1], tm.regs[2], tm.regs[3]);
        fflush(stdout);
    }

    tmch_destroy(&tm);
    printf("\n");
}
