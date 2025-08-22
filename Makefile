EMACS ?= emacs
CC = gcc
INDIGO_DIR = indigo-install
EMACS_SRC_DIR = $(shell $(EMACS) -Q --batch --eval '(princ (expand-file-name "../src"))')
CFLAGS = -fPIC -I$(EMACS_SRC_DIR) -I$(INDIGO_DIR)/include
LDFLAGS = -shared -L$(INDIGO_DIR)/lib -lindigo

all: build/indigo-module.so build/test-indigo

build/indigo-module.so: src/indigo-module.c src/indigo-stateless-utils.c src/indigo-stateless-ops.c src/indigo-stateless-wrappers.c src/indigo-stateful-ops.c src/indigo-stateful-wrappers.c | build
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Test program to verify Indigo installation
build/test-indigo: test/test-indigo.c | build
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -lindigo

build:
	mkdir -p build

.PHONY: all clean test

test: build/test-indigo
	./build/test-indigo

clean:
	rm -rf build
