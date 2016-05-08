#include "mem.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>


void mem_create(struct mem *mem, unsigned bits) {
    mem->bits = bits;
    mem->page = 0;
}

static void mem_page_destroy(void *page, unsigned bits) {
    if (page && bits > 8) {
        for (int i = 0; i < (1 << ((bits-1)%8+1)); i++) {
            mem_page_destroy(((void**)page)[i], bits-((bits-1)%8+1));
        }
    }

    free(page);
}

void mem_destroy(struct mem *mem) {
    mem_page_destroy(mem->page, mem->bits);
}

static uint8_t mem_page_load(void *page, unsigned bits, unsigned addr) {
    if (!page) {
        return 0;
    }

    unsigned prefix = (addr >> 8*((bits-1)/8)) & ((1 << ((bits-1)%8+1))-1);

    if (bits > 8) {
        return mem_page_load(((void**)page)[prefix],
                bits-((bits-1)%8+1), addr);
    } else {
        return ((uint8_t*)page)[prefix];
    }
}

uint8_t mem_load(struct mem *mem, unsigned addr) {
    return mem_page_load(mem->page, mem->bits, addr);
}

static void mem_page_store(void **page,
        unsigned bits, unsigned addr, uint8_t data) {
    if (!*page) {
        if (bits > 8) {
            *page = malloc((1 << ((bits-1)%8+1))*sizeof(void*));
            memset(*page, 0, (1 << ((bits-1)%8+1))*sizeof(void*));
        } else {
            *page = malloc(1 << ((bits-1)%8+1));
            memset(*page, 0, 1 << ((bits-1)%8+1));
        }
    }

    unsigned prefix = (addr >> 8*((bits-1)/8)) & ((1 << ((bits-1)%8+1))-1);

    if (bits > 8) {
        mem_page_store(&((void **)*page)[prefix],
                bits-((bits-1)%8+1), addr, data);
    } else {
        ((uint8_t*)*page)[prefix] = data;
    }
}


void mem_store(struct mem *mem, unsigned addr, uint8_t data) {
    return mem_page_store(&mem->page, mem->bits, addr, data);
}

void mem_read(struct mem *mem, FILE *f) {
    char *buffer = malloc(256);

    while (true) {
        if (!fgets(buffer, 256, f)) {
            return;
        }

        unsigned off;
        unsigned addr;
        if (sscanf(buffer, "%x%*[: ]%n", &addr, &off) < 1) {
            continue;
        }

        while (true) {
            uint8_t data;
            unsigned noff;
            if (sscanf(&buffer[off], "%2hhx%n", &data, &noff) < 1) {
                break;
            }

            mem_store(mem, addr++, data);
            off += noff;
        }
    }
}

static void mem_page_write(void *page, 
        unsigned bits, unsigned addr, FILE *f, unsigned size) {
    if (!page) {
        return ;
    }

    if (bits > 8) {
        for (int i = 0; i < (1 << ((bits-1)%8+1)); i++) {
            mem_page_write(((void**)page)[i], bits-((bits-1)%8+1),
                    (addr<<8) + i, f, size);
        }
    } else {
        for (int i = 0; i < (1 << ((bits-1)%8+1)); i += 16) {
            fprintf(f, "%0*x:", size, (addr<<8) + i);
            for (int j = 0; j < 16 && (i+j) < (1 << ((bits-1)%8+1)); j++) {
                fprintf(f, " %02x", ((uint8_t*)page)[i+j]);
            }
            fprintf(f, " |");
            for (int j = 0; j < 16 && (i+j) < (1 << ((bits-1)%8+1)); j++) {
                char c = ((char*)page)[i+j];
                fprintf(f, "%c", (c >= ' ' && c <= '~') ? c : '.');
            }
            fprintf(f, "|\n");
        }
    }
}

void mem_write(struct mem *mem, FILE *f) {
    mem_page_write(mem->page, mem->bits, 0, f, (mem->bits-1)/4+1);
}
