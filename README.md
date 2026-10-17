# Futhark Presents!

A simple slide viewer written in Futhark and Standard ML.

Supports fancy effects!

The slide viewer works by converting PDFs to images on startup with `pdftoppm`,
so you need that available at runtime. That also means visual fidelity probably
isn't as great as proper PDF viewers.

### Controls

* **Right arrow, space, pagedown, return**: next slide.

* **Left arrow, backspace, pageup**: previous slide.

* **P**: *n*-body particle effect.

* **R**: randomise particles.

* **O**: revert particle effect.

* **D**: falling sand effect.

## Installation

You need [MLKit](https://elsman.com/mlkit/),
[Futhark](https://futhark-lang.org), and
[smlfut](https://github.com/diku-dk/smlfut). If you have Nix, you can run `nix
build` to build everything. Otherwise install the dependencies and run `make`.
Optionally, pass `FUTHARK_BACKEND=foo` to use the `foo` backend for Futhark. The
default is `multicore`.
