#include <stdio.h>

#include "mem.h"
#include "tmachine.h"


struct tmachine tm;

int main() {
    tmachine_create(&tm, 12);
    mem_read(&tm.mem, stdin);

    while (!tmachine_halted(&tm)) {
        tmachine_step(&tm);
        printf("\r[%03x %03x %03x %03x]",
            tm.regs[0], tm.regs[1], tm.regs[2], tm.regs[3]);
        fflush(stdout);
    }

    tmachine_destroy(&tm);
    printf("\n");
}
