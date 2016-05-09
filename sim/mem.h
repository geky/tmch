#ifndef MEM_H
#define MEM_H

#include <stdint.h>
#include <stdio.h>

struct mem {
    unsigned bits;
    void *page;
};

// Memory lifetime
void mem_create(struct mem *mem, unsigned bits);
void mem_destroy(struct mem *mem);

// Loading and storing bytes
uint8_t mem_load(struct mem *mem, unsigned addr);
void mem_store(struct mem *mem, unsigned addr, uint8_t data);

// Dumping to streams in hex format
void mem_read(struct mem *mem, FILE *f);
void mem_write(struct mem *mem, FILE *f);

#endif
