{
  description = "Emacs Indigo - Emacs bindings for the Indigo cheminformatics toolkit";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Indigo cheminformatics library extracted from Ubuntu .deb
        indigo = pkgs.stdenv.mkDerivation rec {
          pname = "indigo";
          version = "1.2.3";

          src = pkgs.fetchurl {
            url = "http://archive.ubuntu.com/ubuntu/pool/universe/i/indigo/libindigo-dev_1.2.3-3.1build1_amd64.deb";
            sha256 = "0f2hj850kq6pgifhs8hr4ghr78p76zhb48jnmd5qqdrd15jh21pp";
          };

          nativeBuildInputs = [ pkgs.dpkg ];

          unpackPhase = ''
            dpkg-deb -x $src .
          '';

          installPhase = ''
            mkdir -p $out/{include,lib}
            cp usr/include/indigo*.h $out/include/
            cp usr/lib/*.a $out/lib/

            # Create symlinks expected by the linker
            ln -s libindigo-static.a $out/lib/libindigo.a
            ln -s libindigo-renderer-static.a $out/lib/libindigo-renderer.a
          '';
        };

        # All libraries needed to link against Indigo
        indigoDeps = with pkgs; [ zlib tinyxml inchi ];

      in {
        # Development shell for building the module
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gcc
            gnumake
            pkg-config
            emacs
          ] ++ indigoDeps ++ [ indigo ];

          shellHook = ''
            export PATH="$HOME/.local/bin:$PATH"

            # Set up paths for the Indigo library
            export INDIGO_DIR="${indigo}"
            export CFLAGS="-I${indigo}/include -I${pkgs.zlib.dev}/include -I${pkgs.tinyxml}/include"
            export LDFLAGS="-L${indigo}/lib -L${pkgs.zlib}/lib -L${pkgs.tinyxml}/lib -L${pkgs.inchi}/lib"

            echo "Emacs Indigo development shell (flake)"
            echo ""
            echo "Indigo library: ${indigo}"
            echo ""
            echo "Build: make INDIGO_DIR=${indigo}"
            echo "Test:  eldev test"
            echo ""
            echo "Or use 'nix build' to build the package directly."
          '';
        };

        # Build the Emacs module as a Nix package
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "emacs-indigo";
          version = "0.10.1";

          src = ./.;

          nativeBuildInputs = with pkgs; [ gcc gnumake ];
          buildInputs = [ pkgs.emacs indigo ] ++ indigoDeps;

          # Get Emacs source dir for emacs-module.h
          EMACS_SRC_DIR = "${pkgs.emacs}/include";

          buildPhase = ''
            # Build the module with all required libraries
            mkdir -p build
            gcc -fPIC \
              -I${pkgs.emacs}/include \
              -I${indigo}/include \
              -o build/indigo-module.so \
              src/indigo-module.c \
              src/indigo-stateless-utils.c \
              src/indigo-stateless-ops.c \
              src/indigo-stateless-wrappers.c \
              src/indigo-system-ops.c \
              src/indigo-system-wrappers.c \
              src/indigo-io-ops.c \
              src/indigo-io-wrappers.c \
              src/indigo-molecular-ops.c \
              src/indigo-molecular-wrappers.c \
              src/indigo-iterators-ops.c \
              src/indigo-iterators-wrappers.c \
              src/indigo-reactions-ops.c \
              src/indigo-reactions-wrappers.c \
              src/indigo-rendering-ops.c \
              src/indigo-rendering-wrappers.c \
              -shared \
              -L${indigo}/lib \
              -Wl,--start-group -lindigo -lindigo-renderer -Wl,--end-group \
              -lstdc++ -lm \
              -L${pkgs.zlib}/lib -lz \
              -L${pkgs.tinyxml}/lib -ltinyxml \
              -L${pkgs.inchi}/lib -linchi
          '';

          installPhase = ''
            mkdir -p $out/share/emacs/site-lisp/indigo/{build,indigo-install}
            cp build/indigo-module.so $out/share/emacs/site-lisp/indigo/build/
            cp *.el $out/share/emacs/site-lisp/indigo/
            # Create empty indigo-install dir to satisfy the check in indigo.el
            touch $out/share/emacs/site-lisp/indigo/indigo-install/.nixbuild
          '';

          meta = with pkgs.lib; {
            description = "Emacs bindings for the Indigo cheminformatics toolkit";
            license = licenses.gpl3Plus;
            platforms = platforms.linux;
          };
        };
      }
    );
}
