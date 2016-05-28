#include "mem.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>


void mem_create(struct mem *mem, unsigned bits) {
    mem->bits = bits;
    mem->page = 0;
}

static void mem_page_create(union mem_page **page, unsigned bits) {
    if (bits > 8) {
        *page = malloc((1 << ((bits-1)%8+1))*sizeof(union mem_page*));
        memset(*page, 0, (1 << ((bits-1)%8+1))*sizeof(union mem_page*));
    } else {
        *page = malloc((1 << ((bits-1)%8+1))+sizeof(struct mem_hooks*));
        memset(*page, 0, (1 << ((bits-1)%8+1))+sizeof(struct mem_hooks*));
    }
}

static void mem_hooks_create(struct mem_hooks **hooks, unsigned bits) {
    *hooks = malloc((1 << ((bits-1)%8+1))*sizeof(struct mem_hooks));
    memset(*hooks, 0, (1 << ((bits-1)%8+1))*sizeof(struct mem_hooks));
}

static void mem_page_destroy(union mem_page *page, unsigned bits) {
    if (!page) {
        return;
    }

    if (bits > 8) {
        for (int i = 0; i < (1 << ((bits-1)%8+1)); i++) {
            mem_page_destroy(page->pages[i], bits-((bits-1)%8+1));
        }
    } else {
        free(page->hooks);
    }

    free(page);
}

void mem_destroy(struct mem *mem) {
    mem_page_destroy(mem->page, mem->bits);
}

static uint8_t mem_page_read(union mem_page *page,
        unsigned bits, unsigned addr) {
    if (!page) {
        return 0;
    }

    unsigned prefix = (addr >> 8*((bits-1)/8)) & ((1 << ((bits-1)%8+1))-1);

    if (bits > 8) {
        return mem_page_read(page->pages[prefix],
                bits-((bits-1)%8+1), addr);
    } else if (page->hooks && page->hooks[prefix].read_hook) {
        return page->hooks[prefix].read_hook(
                page->hooks[prefix].read_data, addr);
    } else {
        return page->bytes[prefix];
    }
}

uint8_t mem_read(struct mem *mem, unsigned addr) {
    return mem_page_read(mem->page, mem->bits, addr);
}

static void mem_page_write(union mem_page **page,
        unsigned bits, unsigned addr, uint8_t data) {
    if (!*page) {
        mem_page_create(page, bits);
    }

    unsigned prefix = (addr >> 8*((bits-1)/8)) & ((1 << ((bits-1)%8+1))-1);

    if (bits > 8) {
        mem_page_write(&(*page)->pages[prefix],
                bits-((bits-1)%8+1), addr, data);
    } else if ((*page)->hooks && (*page)->hooks[prefix].write_hook) {
        (*page)->hooks[prefix].write_hook(
                (*page)->hooks[prefix].write_data, addr, data);
    } else {
        (*page)->bytes[prefix] = data;
    }
}

void mem_write(struct mem *mem, unsigned addr, uint8_t data) {
    return mem_page_write(&mem->page, mem->bits, addr, data);
}

void mem_page_read_hook(union mem_page **page,
        unsigned bits, unsigned addr, 
        uint8_t (*read_hook)(void*, unsigned addr),
        void *read_data) {
    if (!*page) {
        mem_page_create(page, bits);
    }

    unsigned prefix = (addr >> 8*((bits-1)/8)) & ((1 << ((bits-1)%8+1))-1);

    if (bits > 8) {
        mem_page_read_hook(&(*page)->pages[prefix],
                bits-((bits-1)%8+1), addr, read_hook, read_data);
    } else {
        if (!(*page)->hooks) {
            mem_hooks_create(&(*page)->hooks, bits);
        }
        (*page)->hooks[prefix].read_hook = read_hook;
        (*page)->hooks[prefix].read_data = read_data;
    }
}

void mem_read_hook(struct mem *mem, unsigned addr,
        uint8_t (*read_hook)(void*, unsigned addr),
        void *read_data) {
    return mem_page_read_hook(&mem->page, mem->bits, addr,
            read_hook, read_data);
}

void mem_page_write_hook(union mem_page **page,
        unsigned bits, unsigned addr, 
        void (*write_hook)(void*, unsigned addr, uint8_t data),
        void *write_data) {
    if (!*page) {
        mem_page_create(page, bits);
    }

    unsigned prefix = (addr >> 8*((bits-1)/8)) & ((1 << ((bits-1)%8+1))-1);

    if (bits > 8) {
        mem_page_write_hook(&(*page)->pages[prefix],
                bits-((bits-1)%8+1), addr, write_hook, write_data);
    } else {
        if (!(*page)->hooks) {
            mem_hooks_create(&(*page)->hooks, bits);
        }
        (*page)->hooks[prefix].write_hook = write_hook;
        (*page)->hooks[prefix].write_data = write_data;
    }
}

void mem_write_hook(struct mem *mem, unsigned addr,
        void (*write_hook)(void*, unsigned addr, uint8_t data),
        void *write_data) {
    return mem_page_write_hook(&mem->page, mem->bits, addr,
            write_hook, write_data);
}

void mem_load(struct mem *mem, FILE *f) {
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

            mem_write(mem, addr++, data);
            off += noff;
        }
    }
}

static void mem_page_dump(union mem_page *page, 
        unsigned bits, unsigned addr, FILE *f, unsigned size) {
    if (!page) {
        return;
    }

    if (bits > 8) {
        for (int i = 0; i < (1 << ((bits-1)%8+1)); i++) {
            mem_page_dump(page->pages[i], bits-((bits-1)%8+1),
                    (addr<<8) + i, f, size);
        }
    } else {
        for (int i = 0; i < (1 << ((bits-1)%8+1)); i += 16) {
            fprintf(f, "%0*x:", size, (addr<<8) + i);
            for (int j = 0; j < 16 && (i+j) < (1 << ((bits-1)%8+1)); j++) {
                fprintf(f, " %02x", page->bytes[i+j]);
            }
            fprintf(f, " |");
            for (int j = 0; j < 16 && (i+j) < (1 << ((bits-1)%8+1)); j++) {
                char c = page->bytes[i+j];
                fprintf(f, "%c", (c >= ' ' && c <= '~') ? c : '.');
            }
            fprintf(f, "|\n");
        }
    }
}

void mem_dump(struct mem *mem, FILE *f) {
    mem_page_dump(mem->page, mem->bits, 0, f, (mem->bits-1)/4+1);
}
