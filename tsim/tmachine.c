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
    OP_MV,      // rd = ra
    OP_SW,      // rd, ra = ra, rd
    OP_ST,      // mem[--ra], rd = rd, rd >> 8
    OP_LD,      // rd = rd << 8 | mem[ra++]

    OP_BEQ,     // rd = rd - mem[pc++] if ra == 0
    OP_BNE,     // rd = rd - mem[pc++] if ra != 0
    OP_BGT,     // rd = rd - mem[pc++] if ra > 0
    OP_BLT,     // rd = rd - mem[pc++] if ra < 0

    OP_AND,     // rd = rd & ra
    OP_XOR,     // rd = rd ^ ra
    OP_ADD,     // rd = rd + ra
    OP_SUB,     // rd = rd - ra

    OP_ANDI,    // rd = rd & mem[ra++]
    OP_XORI,    // rd = rd ^ mem[ra++]
    OP_ADDI,    // rd = rd + mem[ra++]
    OP_SUBI,    // rd = rd - mem[ra++]
};

void tmachine_step(struct tmachine *tm) {
    uint8_t ins = mem_load(&tm->mem, tm->regs[3]);
    tm->regs[3] = mask(tm, tm->regs[3]+1);

    uint8_t op = 0xf & (ins >> 4);
    uint8_t rd = 0x3 & (ins >> 2);
    uint8_t ra = 0x3 & (ins >> 0);

    switch (op) {
        case OP_MV: {
            tm->regs[rd] = tm->regs[ra];
        } break;

        case OP_SW: {
            unsigned temp = tm->regs[rd];
            tm->regs[rd] = tm->regs[ra];
            tm->regs[ra] = temp;
        } break;

        case OP_ST: {
            tm->regs[ra] = mask(tm, tm->regs[ra]-1);
            mem_store(&tm->mem, tm->regs[ra], tm->regs[rd]);
            tm->regs[rd] = mask(tm, tm->regs[rd] >> 8);
        } break;

        case OP_LD: {
            tm->regs[rd] = mask(tm, (tm->regs[rd] << 8) |
                    mem_load(&tm->mem, tm->regs[ra]));
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
        } break;

        case OP_BEQ: {
            if (tm->regs[ra] == 0) {
                tm->regs[rd] = mask(tm, tm->regs[rd] -
                        mem_load(&tm->mem, tm->regs[3]));
                tm->regs[3] = mask(tm, tm->regs[3]+1);
            }
        } break;

        case OP_BNE: {
            if (tm->regs[ra] != 0) {
                tm->regs[rd] = mask(tm, tm->regs[rd] -
                        mem_load(&tm->mem, tm->regs[3]));
                tm->regs[3] = mask(tm, tm->regs[3]+1);
            }
        } break;

        case OP_BGT: {
            if (tm->regs[ra] && !(tm->regs[ra] & (1 << (tm->bits-1)))) {
                tm->regs[rd] = mask(tm, tm->regs[rd] -
                        mem_load(&tm->mem, tm->regs[3]));
                tm->regs[3] = mask(tm, tm->regs[3]+1);
            }
        } break;

        case OP_BLT: {
            if (tm->regs[ra] & (1 << (tm->bits-1))) {
                tm->regs[rd] = mask(tm, tm->regs[rd] -
                        mem_load(&tm->mem, tm->regs[3]));
                tm->regs[3] = mask(tm, tm->regs[3]+1);
            }
        } break;

        case OP_AND: {
            tm->regs[rd] = (tm->regs[rd] & ~0xff) |
                    (tm->regs[rd] & (uint8_t)tm->regs[ra]);
        } break;

        case OP_XOR: {
            tm->regs[rd] = (tm->regs[rd] & ~0xff) |
                    (tm->regs[rd] ^ (uint8_t)tm->regs[ra]);
        } break;

        case OP_ADD: {
            tm->regs[rd] = mask(tm, tm->regs[rd] +
                    (uint8_t)tm->regs[ra]);
        } break;

        case OP_SUB: {
            tm->regs[rd] = mask(tm, tm->regs[rd] -
                    (uint8_t)tm->regs[ra]);
        } break;

        case OP_ANDI: {
            tm->regs[rd] = (tm->regs[rd] & ~0xff) |
                    (tm->regs[rd] & mem_load(&tm->mem, tm->regs[ra]));
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
        } break;

        case OP_XORI: {
            tm->regs[rd] = (tm->regs[rd] & ~0xff) |
                    (tm->regs[rd] ^ mem_load(&tm->mem, tm->regs[ra]));
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
        } break;

        case OP_ADDI: {
            tm->regs[rd] = mask(tm, tm->regs[rd] + 
                    mem_load(&tm->mem, tm->regs[ra]));
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
        } break;

        case OP_SUBI: {
            tm->regs[rd] = mask(tm, tm->regs[rd] -
                    mem_load(&tm->mem, tm->regs[ra]));
            tm->regs[ra] = mask(tm, tm->regs[ra]+1);
        } break;
    }
}

