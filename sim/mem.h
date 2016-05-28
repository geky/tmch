#ifndef MEM_H
#define MEM_H

#include <stdint.h>
#include <stdio.h>

struct mem {
    unsigned bits;

    union mem_page {
        struct {
            struct mem_hooks {
                uint8_t (*read_hook)(void*, unsigned addr);
                void *read_data;
                void (*write_hook)(void*, unsigned addr, uint8_t data);
                void *write_data;
            } *hooks;
            uint8_t bytes[256];
        };
        union mem_page *pages[256];
    } *page;
};

// Memory lifetime
void mem_create(struct mem *mem, unsigned bits);
void mem_destroy(struct mem *mem);

// Loading and storing bytes
uint8_t mem_read(struct mem *mem, unsigned addr);
void mem_write(struct mem *mem, unsigned addr, uint8_t data);

void mem_read_hook(struct mem *mem, unsigned addr,
        uint8_t (*read_hook)(void*, unsigned addr),
        void *read_data);
void mem_write_hook(struct mem *mem, unsigned addr,
        void (*write_hook)(void*, unsigned addr, uint8_t data),
        void *read_data);

// Dumping to streams in hex format
void mem_load(struct mem *mem, FILE *f);
void mem_dump(struct mem *mem, FILE *f);

#endif
