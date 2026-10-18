{
  description = "chem-mode - generate chemfig and SVG chemical structures in Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # The PyPI wheel for epam.indigo is ABI independent (py3-none) and
        # bundles the Indigo shared library, so it works with any Python 3.
        epam-indigo = pkgs.python3Packages.buildPythonPackage rec {
          pname = "epam.indigo";
          version = "1.46.0";
          format = "wheel";

          src = pkgs.fetchurl {
            url = "https://files.pythonhosted.org/packages/62/bd/c81abffe374e1b2854c9f978efa13f7cbb38f1349fe95e0eb66a0e6d9edb/epam_indigo-1.46.0-py3-none-manylinux1_x86_64.whl";
            sha256 = "205415900ed919bfbf2a951af01f52cbdcc7ab43559143a31be9b9c7e0bfd79a";
          };

          dontCheck = true;
        };

        # Python environment used to run the reference mol2chemfigPy3
        # implementation and (re)generate the golden test files.
        reference-python = pkgs.python3.withPackages (_: [ epam-indigo ]);

        # Indigo is statically linked into emacs-indigo's dynamic module, but
        # the module still needs these at runtime.
        indigo-runtime = with pkgs; [ tinyxml inchi zlib freetype fontconfig ];

      in {
        packages.epam-indigo = epam-indigo;
        packages.reference-python = reference-python;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.emacs
            pkgs.gcc
            pkgs.gnumake
            pkgs.pkg-config
            pkgs.freetype
            pkgs.fontconfig
            reference-python
          ] ++ indigo-runtime;

          shellHook = ''
            export PATH="$HOME/.local/bin:$PATH"
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath indigo-runtime}:$LD_LIBRARY_PATH"

            echo "chem-mode development shell"
            echo ""
            echo "  Reference python : ${reference-python}/bin/python"
            echo "  Regenerate goldens: tools/gen-goldens.sh"
            echo "  Run tests          : eldev test (requires eldev on PATH)"
            echo ""
          '';
        };
      });
}
