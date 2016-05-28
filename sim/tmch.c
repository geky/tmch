#include "tmch.h"
#include <string.h>

static inline unsigned mask(struct tmch *tmch, unsigned v) {
    return v & ((unsigned)-1 >> (8*sizeof(unsigned) - tmch->bits));
}

void tmch_create(struct tmch *tmch, unsigned bits) {
    memset(tmch, 0, sizeof(struct tmch));
    tmch->bits = bits;
    mem_create(&tmch->mem, bits);
}

void tmch_destroy(struct tmch *tmch) {
    mem_destroy(&tmch->mem);
}

bool tmch_halted(struct tmch *tmch) {
    return (mem_read(&tmch->mem, tmch->regs[3]) == 0xff &&
            mem_read(&tmch->mem, mask(tmch, tmch->regs[3]+1)) == 0x02);
}

enum tmch_ops {
    OP_MV  = 0 << 1,  // rd = ra                     rd = rd<<8 | mem[ra++]
    OP_ST  = 1 << 1,  // mem[--rd], ra = ra, ra>>8   mem[--rd] = mem[ra++]

    OP_CZ  = 2 << 1, // rd = rd - ra if ==0         rd = rd - mem[ra++] if ==0
    OP_CNZ = 3 << 1, // rd = rd - ra if !=0         rd = rd - mem[ra++] if !=0

    OP_AND = 4 << 1, // rd = rd & ra                rd = rd & mem[ra++]
    OP_XOR = 5 << 1, // rd = rd ^ ra                rd = rd ^ mem[ra++]
    OP_ADD = 6 << 1, // rd = rd + ra                rd = rd + mem[ra++]
    OP_SUB = 7 << 1, // rd = rd - ra                rd = rd - mem[ra++]
};

void tmch_step(struct tmch *tmch) {
    uint8_t ins = mem_read(&tmch->mem, tmch->regs[3]);
    tmch->regs[3] = mask(tmch, tmch->regs[3]+1);

    uint8_t op = (0xf0 & ins) >> 4;
    uint8_t rd = (0x0c & ins) >> 2;
    uint8_t ra = (0x03 & ins) >> 0;

    switch (op) {
        case OP_MV | 0: {
            tmch->regs[rd] = tmch->regs[ra];
        } break;

        case OP_MV | 1: {
            uint8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            tmch->regs[rd] = mask(tmch, (tmch->regs[rd]<<8) | t);
        } break;

        case OP_ST | 0: {
            tmch->regs[rd] = mask(tmch, tmch->regs[rd]-1);
            mem_write(&tmch->mem, tmch->regs[rd], tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra] >> 8);
        } break;

        case OP_ST | 1: {
            uint8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            tmch->regs[rd] = mask(tmch, tmch->regs[rd]-1);
            mem_write(&tmch->mem, tmch->regs[rd], t);
        } break;

        case OP_CZ | 0: {
            int8_t t = tmch->regs[ra];
            if (!(uint8_t)tmch->regs[0]) {
                tmch->regs[rd] = mask(tmch, tmch->regs[rd] - t);
            }
        } break;

        case OP_CZ | 1: {
            int8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            if (!(uint8_t)tmch->regs[0]) {
                tmch->regs[rd] = mask(tmch, tmch->regs[rd] - t);
            }
        } break;

        case OP_CNZ | 0: {
            int8_t t = tmch->regs[ra];
            if ((uint8_t)tmch->regs[0]) {
                tmch->regs[rd] = mask(tmch, tmch->regs[rd] - t);
            }
        } break;

        case OP_CNZ | 1: {
            int8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            if ((uint8_t)tmch->regs[0]) {
                tmch->regs[rd] = mask(tmch, tmch->regs[rd] - t);
            }
        } break;

        case OP_AND | 0: {
            uint8_t t = tmch->regs[ra];
            tmch->regs[rd] = tmch->regs[rd] & (((unsigned)-1 << 8) | t);
        } break;

        case OP_AND | 1: {
            uint8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            tmch->regs[rd] = tmch->regs[rd] & (((unsigned)-1 << 8) | t);
        } break;

        case OP_XOR | 0: {
            uint8_t t = tmch->regs[ra];
            tmch->regs[rd] = tmch->regs[rd] ^ t;
        } break;

        case OP_XOR | 1: {
            uint8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            tmch->regs[rd] = tmch->regs[rd] ^ t;
        } break;

        case OP_ADD | 0: {
            int8_t t = tmch->regs[ra];
            tmch->regs[rd] = mask(tmch, tmch->regs[rd] + t);
        } break;

        case OP_ADD | 1: {
            int8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            tmch->regs[rd] = mask(tmch, tmch->regs[rd] + t);
        } break;

        case OP_SUB | 0: {
            int8_t t = tmch->regs[ra];
            tmch->regs[rd] = mask(tmch, tmch->regs[rd] - t);
        } break;

        case OP_SUB | 1: {
            int8_t t = mem_read(&tmch->mem, tmch->regs[ra]);
            tmch->regs[ra] = mask(tmch, tmch->regs[ra]+1);
            tmch->regs[rd] = mask(tmch, tmch->regs[rd] - t);
        } break;
    }
}

