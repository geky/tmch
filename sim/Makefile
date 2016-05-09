TARGET = tmch-sim

CC = gcc
AR = ar
SIZE = size

SRC += $(wildcard *.c)
OBJ := $(SRC:.c=.o)
DEP := $(SRC:.c=.d)
ASM := $(SRC:.c=.s)

ifdef DEBUG
CFLAGS += -O0 -g3
else
CFLAGS += -O3
endif
CFLAGS += -std=c99
CFLAGS += -Wall -Winline


all: $(TARGET)

asm: $(ASM)

size: $(OBJ)
	$(SIZE) -t $^

-include $(DEP)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) $^ $(LFLAGS) -o $@

%.o: %.c
	$(CC) -c -MMD $(CFLAGS) $< -o $@

%.s: %.c
	$(CC) -S $(CFLAGS) $< -o $@

clean:
	rm -f $(TARGET)
	rm -f $(OBJ)
	rm -f $(DEP)
	rm -f $(ASM)
