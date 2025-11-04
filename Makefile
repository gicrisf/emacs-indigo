EMACS ?= emacs
CC = gcc
INDIGO_DIR = indigo-install
EMACS_SRC_DIR = $(shell $(EMACS) -Q --batch --eval '(princ (expand-file-name "../src"))')
CFLAGS = -fPIC -I$(EMACS_SRC_DIR) -I$(INDIGO_DIR)/include
LDFLAGS = -shared -L$(INDIGO_DIR)/lib -lindigo -lindigo-renderer

all: build/indigo-module.so build/test-indigo

build/indigo-module.so: src/indigo-module.c src/indigo-stateless-utils.c src/indigo-stateless-ops.c src/indigo-stateless-wrappers.c src/indigo-system-ops.c src/indigo-system-wrappers.c src/indigo-io-ops.c src/indigo-io-wrappers.c src/indigo-molecular-ops.c src/indigo-molecular-wrappers.c src/indigo-iterators-ops.c src/indigo-iterators-wrappers.c src/indigo-reactions-ops.c src/indigo-reactions-wrappers.c src/indigo-rendering-ops.c src/indigo-rendering-wrappers.c | build
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Test program to verify Indigo installation
build/test-indigo: test/test-indigo.c | build
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -lindigo

# Test program to check renderer availability
build/test-renderer-availability: test/test-renderer-availability.c | build
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -lindigo -lindigo-renderer || \
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -lindigo

build:
	mkdir -p build

.PHONY: all clean test test-renderer

test: build/test-indigo
	./build/test-indigo

test-renderer: build/test-renderer-availability
	./build/test-renderer-availability

clean:
	rm -rf build
