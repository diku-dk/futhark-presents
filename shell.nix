with import <nixpkgs> {};
pkgs.stdenv.mkDerivation {
  name = "futhark-presents";
  buildInputs =
    with pkgs;
    [
      python3Packages.pyopencl
      python3Packages.numpy
      python3Packages.pysdl2
      python3Packages.imageio
      python3Packages.pypng
    ];
}
