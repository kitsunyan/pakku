# pakku

Pakku is a pacman wrapper with additional features, such as AUR support.
Stable release is available in [AUR](https://aur.archlinux.org/packages/pakku).

## Description

There are dozens of AUR helpers, but all of them have a fatal flaw:
I didn't write them! So I made another useless AUR helper.
Say hello to stillborn AUR helper written in stillborn programming language!

Basically, pakku supports the following features:

- Installing packages from AUR
- Viewing files and changes between builds
- Building packages from official repositories
- Removing make dependencies after building
- Searching and querying AUR packages
- Reading comments for AUR packages
- PKGBUILD retrieving
- Pacman integration

In other words, it does the same things any AUR helper capable of.

The following principles were the basis of the program:

- Pacman-like user interface
- Pacman options support (`--asdeps`, `--needed`, etc)
- Pacman configuration support (output settings, ignored packages, etc)
- Download, ask all questions, and only after that start building
- No PKGBUILD sourcing

## Examples

- Build packages from sources: `pakku -S --build linux linux-headers`
- Query all "dependency islands": `pakku -Qdttt`
