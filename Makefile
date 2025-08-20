EMACS ?= emacs
CC = gcc
INDIGO_DIR = indigo-install
EMACS_SRC_DIR = $(shell $(EMACS) -Q --batch --eval '(princ (expand-file-name "../src"))')
CFLAGS = -fPIC -I$(EMACS_SRC_DIR) -I$(INDIGO_DIR)/include
LDFLAGS = -shared -L$(INDIGO_DIR)/lib -lindigo

all: indigo-module.so test-indigo

indigo-module.so: src/indigo-module.c
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Test program to verify Indigo installation
test-indigo: test-indigo.c
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -lindigo

.PHONY: all clean test

test: test-indigo
	./test-indigo

clean:
	rm -f indigo-module.so test-indigo
