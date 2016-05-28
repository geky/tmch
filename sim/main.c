#include <stdio.h>

#include "mem.h"
#include "tmch.h"


struct tmch tm;

uint8_t io_read(void *p, unsigned addr) {
    return fgetc((FILE *)p);
}

void io_write(void *p, unsigned addr, uint8_t data) {
    fputc(data, (FILE *)p);
}

int main() {
    tmch_create(&tm, 12);
    mem_load(&tm.mem, stdin);
    mem_read_hook(&tm.mem, 0x100, io_read, stdin);
    mem_write_hook(&tm.mem, 0x101, io_write, stdout);
    mem_write_hook(&tm.mem, 0x102, io_write, stderr);

    while (!tmch_halted(&tm)) {
        tmch_step(&tm);
//        printf("\r[%03x %03x %03x %03x]",
//            tm.regs[0], tm.regs[1], tm.regs[2], tm.regs[3]);
//        fflush(stdout);
    }

//    printf("\n");
//    mem_dump(&tm.mem, stdout);
//    printf("\n");
    tmch_destroy(&tm);
}
