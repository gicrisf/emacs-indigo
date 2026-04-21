{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Build tools
    gcc
    gnumake
    pkg-config

    # Dependencies (used by Indigo)
    zlib
    tinyxml
    inchi

    # For downloading Indigo
    wget
    dpkg  # for extracting .deb

    # Emacs with module support
    emacs
  ];

  shellHook = ''
    # Point to system zlib/tinyxml instead of building from source
    export CFLAGS="-I${pkgs.zlib.dev}/include -I${pkgs.tinyxml}/include"
    export LDFLAGS="-L${pkgs.zlib}/lib -L${pkgs.tinyxml}/lib -L${pkgs.inchi}/lib"
    export PATH="$HOME/.local/bin:$PATH"

    # Emacs module header for building native modules
    export EMACS_MODULE_HEADER="${pkgs.emacs}/include"

    echo "Emacs Indigo development shell"
    echo ""
    echo "Build steps:"
    echo "  1. bash ./install-indigo.sh  # Download Indigo library"
    echo "  2. make                       # Build the module"
    echo "  3. eldev test                 # Run tests"
  '';
}
