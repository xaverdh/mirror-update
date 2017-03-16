# mirrorupdate

## What it is

This is a script for updating the (arch linux) pacman mirrorlist.

## Prerequisites and Dependencies (for building)
  * Arch linux
  * ghc

## Building

```sh
git clone https://gitlab.com/xaverdh/mirrorupdate
cd mirror-update && cabal install --ghc-options="-O2 -j$(nproc)"
```

