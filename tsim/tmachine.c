#include "tmachine.h"
#include <string.h>

static inline unsigned mask(struct tmachine *tm, unsigned v) {
    return v & ((unsigned)-1 >> (8*sizeof(unsigned) - tm->bits));
}

void tmachine_create(struct tmachine *tm, unsigned bits) {
    memset(tm, 0, sizeof(struct tmachine));
    tm->bits = bits;
    mem_create(&tm->mem, bits);
}

void tmachine_destroy(struct tmachine *tm) {
    mem_destroy(&tm->mem);
}

bool tmachine_halted(struct tmachine *tm) {
    return (mem_load(&tm->mem, tm->regs[3]) == 0xff &&
            mem_load(&tm->mem, mask(tm, tm->regs[3]+1)) == 0x02);
}

enum tmachine_ops {
    OP_MV  = 0 << 1,  // rd = ra                     rd = rd<<8 | mem[ra++]
    OP_ST  = 1 << 1,  // mem[--rd], ra = ra, ra>>8   mem[--rd] = mem[ra++]

    OP_CZ  = 2 << 1, // rd = rd - ra if ==0         rd = rd - mem[ra++] if ==0
    OP_CNZ = 3 << 1, // rd = rd - ra if !=0         rd = rd - mem[ra++] if !=0

    OP_AND = 4 << 1, // rd = rd & ra                rd = rd & mem[ra++]
    OP_XOR = 5 << 1, // rd = rd ^ ra                rd = rd ^ mem[ra++]
    OP_ADD = 6 << 1, // rd = rd + ra                rd = rd + mem[ra++]
    OP_SUB = 7 << 1, // rd = rd - ra                rd = rd - mem[ra++]
};

void tmachine_step(struct tmachine *tm) {
    uint8_t ins = mem_load(&tm->mem, tm->regs[3]);
    tm->regs[3] = mask(tm, tm->regs[3]+1);

    uint8_t op = (0xf0 & ins) >> 4;
    uint8_t rd = (0x0c & ins) >> 2;
    uint8_t ra = (0x03 & ins) >> 0;

    switch (op) {
        case OP_MV | 0: {
            tm->regs[rd] = tm->regs[ra];
        } break;

        case OP_MV | 1: {
            uint8_t t = mem_load(&tm->mem, tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
            tm->regs[rd] = mask(tm, (tm->regs[rd]<<8) | t);
        } break;

        case OP_ST | 0: {
            tm->regs[rd] = mask(tm, tm->regs[rd]-1);
            mem_store(&tm->mem, tm->regs[rd], tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra] >> 8);
        } break;

        case OP_ST | 1: {
            uint8_t t = mem_load(&tm->mem, tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
            tm->regs[rd] = mask(tm, tm->regs[rd]-1);
            mem_store(&tm->mem, tm->regs[rd], t);
        } break;

        case OP_CZ | 0: {
            if (!tm->nz) {
                tm->regs[rd] = mask(tm, tm->regs[rd] - tm->regs[ra]);
            }
        } break;

        case OP_CZ | 1: {
            if (!tm->nz) {
                int8_t t = mem_load(&tm->mem, tm->regs[ra]);
                tm->regs[ra] = mask(tm, tm->regs[ra]+1);
                tm->regs[rd] = mask(tm, tm->regs[rd] - t);
            }
        } break;

        case OP_CNZ | 0: {
            if (tm->nz) {
                tm->regs[rd] = mask(tm, tm->regs[rd] - tm->regs[ra]);
            }
        } break;

        case OP_CNZ | 1: {
            if (tm->nz) {
                int8_t t = mem_load(&tm->mem, tm->regs[ra]);
                tm->regs[ra] = mask(tm, tm->regs[ra]+1);
                tm->regs[rd] = mask(tm, tm->regs[rd] - t);
            }
        } break;

        case OP_AND | 0: {
            tm->regs[rd] = tm->regs[rd] & tm->regs[ra];
            tm->nz = tm->regs[rd];
        } break;

        case OP_AND | 1: {
            int8_t t = mem_load(&tm->mem, tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
            tm->regs[rd] = tm->regs[rd] & t;
            tm->nz = tm->regs[rd];
        } break;

        case OP_XOR | 0: {
            tm->regs[rd] = tm->regs[rd] ^ tm->regs[ra];
            tm->nz = tm->regs[rd];
        } break;

        case OP_XOR | 1: {
            int8_t t = mem_load(&tm->mem, tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
            tm->regs[rd] = tm->regs[rd] ^ t;
            tm->nz = tm->regs[rd];
        } break;

        case OP_ADD | 0: {
            tm->regs[rd] = mask(tm, tm->regs[rd] + tm->regs[ra]);
            tm->nz = tm->regs[rd];
        } break;

        case OP_ADD | 1: {
            int8_t t = mem_load(&tm->mem, tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
            tm->regs[rd] = mask(tm, tm->regs[rd] + t);
            tm->nz = tm->regs[rd];
        } break;

        case OP_SUB | 0: {
            tm->regs[rd] = mask(tm, tm->regs[rd] - tm->regs[ra]);
            tm->nz = tm->regs[rd];
        } break;

        case OP_SUB | 1: {
            int8_t t = mem_load(&tm->mem, tm->regs[ra]);
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
            tm->regs[rd] = mask(tm, tm->regs[rd] - t);
            tm->nz = tm->regs[rd];
        } break;
    }
}

