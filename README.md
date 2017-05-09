# mirror-update

## What it is

This is a script for updating the (arch linux) pacman mirrorlist.
It also pre-filters the servers by country.

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


## Usage

To automate the update process, place the binary in "/usr/bin"
and the "mirror-update.hook" file in "/etc/pacman.d/hooks/".

You also want to adjust the list of countries in the
mirror-update.hook file to suit your needs.


## Legal
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    
    See LICENSE for more details.

