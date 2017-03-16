# mirror-update

## What it is

This is a script for updating the (arch linux) pacman mirrorlist.

## Prerequisites and Dependencies (for building)
  * arch linux
  * ghc

## Building

```sh
git clone https://gitlab.com/xaverdh/mirror-update
cd mirror-update
cabal sandbox init
cabal install --ghc-options="-O2 -j$(nproc)"
```
The generated binary will be
"dist/dist-sandbox-XXX/build/mirrorupdate/mirrorupdate"
where XXX is some hash value.

To automate the update process, place the binary in "/usr/bin"
and the "mirror-update.hook" file in "/etc/pacman.d/hooks/".
