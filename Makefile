EMACS ?= emacs
CC = gcc
INDIGO_DIR = indigo-install

# Find emacs-module.h
# Use EMACS_MODULE_HEADER env var if set (e.g., from shell.nix), otherwise search common locations
ifdef EMACS_MODULE_HEADER
  EMACS_MODULE_H := $(EMACS_MODULE_HEADER)
else
  EMACS_MODULE_H := $(shell \
    for dir in \
      /usr/include \
      /usr/local/include \
      "$$($(EMACS) -Q --batch --eval '(princ (expand-file-name \"src\" data-directory))')" \
      "$$($(EMACS) -Q --batch --eval '(princ (expand-file-name \"../src\" data-directory))')" \
      "$$($(EMACS) -Q --batch --eval '(princ (expand-file-name \"../include\" invocation-directory))')"; do \
      if [ -f "$$dir/emacs-module.h" ]; then echo "$$dir"; break; fi; \
    done)
endif

ifeq ($(EMACS_MODULE_H),)
  $(error emacs-module.h not found. Use nix-shell or set EMACS_MODULE_HEADER, or install Emacs development headers)
endif

CFLAGS = -fPIC -I$(EMACS_MODULE_H) -I$(INDIGO_DIR)/include
LDFLAGS = -shared -L$(INDIGO_DIR)/lib -Wl,--start-group -lindigo-static -lindigo-renderer-static -Wl,--end-group -lstdc++ -lm -lz -ltinyxml -linchi

all: build/indigo-module.so build/test-indigo

build/indigo-module.so: src/indigo-module.c src/indigo-stateless-utils.c src/indigo-stateless-ops.c src/indigo-stateless-wrappers.c src/indigo-system-ops.c src/indigo-system-wrappers.c src/indigo-io-ops.c src/indigo-io-wrappers.c src/indigo-molecular-ops.c src/indigo-molecular-wrappers.c src/indigo-iterators-ops.c src/indigo-iterators-wrappers.c src/indigo-reactions-ops.c src/indigo-reactions-wrappers.c src/indigo-rendering-ops.c src/indigo-rendering-wrappers.c | build
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Test program to verify Indigo installation
build/test-indigo: test/test-indigo.c | build
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -Wl,--start-group -lindigo-static -Wl,--end-group -lstdc++ -lm -lz -ltinyxml -linchi

# Test program to check renderer availability
build/test-renderer-availability: test/test-renderer-availability.c | build
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -Wl,--start-group -lindigo-static -lindigo-renderer-static -Wl,--end-group -lstdc++ -lm -lz -ltinyxml -linchi || \
	$(CC) -I$(INDIGO_DIR)/include -o $@ $< -L$(INDIGO_DIR)/lib -Wl,--start-group -lindigo-static -Wl,--end-group -lstdc++ -lm -lz -ltinyxml -linchi

build:
	mkdir -p build

.PHONY: all clean test test-renderer

test: build/test-indigo
	./build/test-indigo

test-renderer: build/test-renderer-availability
	./build/test-renderer-availability

clean:
	rm -rf build
