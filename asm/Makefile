TARGET = tmch-asm

HC = ghc
HI = ghci
HFLAGS += -W
HFLAGS += -XLambdaCase -XRankNTypes -XScopedTypeVariables -XTupleSections -XFlexibleInstances -XFlexibleContexts -XConstraintKinds

MAIN = Main.hs
SRC += $(wildcard *.hs)


all: $(TARGET)

$(TARGET): $(SRC)
	$(HC) $(HFLAGS) $(MAIN) -o $@

%.o: %.hs
	$(HC) $(HFLAGS) $^

repl: $(SRC)
	$(HI) $(HFLAGS) $(MAIN)

clean:
	rm -rf $(SRC:.hs=.o)
	rm -rf $(SRC:.hs=.hi)
	rm -f $(TARGET)
