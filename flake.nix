{
  description = "futhark-presents: a PDF slide viewer in Standard ML and Futhark";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  # The effects use language features newer than the Futhark in nixpkgs.
  # ...and smlfut must be new enough to understand its manifests.
  inputs.futhark.url = "github:diku-dk/futhark";
  inputs.smlfut = {
    url   = "github:diku-dk/smlfut";
    flake = false;
  };

  outputs = { self, nixpkgs, futhark, smlfut }:
    let
      system   = "x86_64-linux";
      pkgs     = nixpkgs.legacyPackages.${system};
      futhark' = futhark.packages.${system}.futhark;
      smlfut'  = pkgs.smlfut.overrideAttrs {
        version = "git";
        src     = smlfut;
        doCheck = false;
      };
    in {

      # `nix develop` — everything needed to run `make`
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          pkgs.mlkit
          pkgs.smlpkg
          futhark'
          smlfut'
          pkgs.poppler-utils   # pdftoppm
          pkgs.libx11
          pkgs.libGL
          pkgs.libGLU
          pkgs.gcc
          pkgs.gnumake
        ];
      };

      # `nix build` — produces result/bin/futhark-presents
      packages.${system}.default =
        pkgs.stdenv.mkDerivation {
          pname   = "futhark-presents";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = [
            pkgs.mlkit
            futhark'
            smlfut'
            pkgs.makeWrapper
          ];

          buildInputs = [
            pkgs.libx11
            pkgs.libGL
            pkgs.libGLU
          ];

          # The SML (sml/lib/) and Futhark (fut/lib/) dependencies are vendored
          # in the repository, so no package syncing is needed here.
          buildPhase = ''
            export HOME=$TMPDIR
            make
          '';

          # pdftoppm is invoked at startup to rasterise the PDF.
          installPhase = ''
            mkdir -p $out/bin
            cp futhark-presents $out/bin/
            wrapProgram $out/bin/futhark-presents \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.poppler-utils ]}
          '';

          meta = {
            description = "PDF slide viewer with keyboard navigation and Futhark effects";
            platforms   = [ "x86_64-linux" ];
            mainProgram = "futhark-presents";
          };
        };
    };
}
