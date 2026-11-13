#!/bin/sh
# Builds asciiverse/samples/: the code av's gittype types when it is started
# from its install folder (av starts every title there). It is source from
# games in the Asciiverse catalog, each folder with its own licence, taken
# with git archive from our checkouts at the commits named below, so the
# set can be rebuilt exactly. Run from the gittype folder, with the
# catalog's repositories checked out beside it (games/, av/):
#
#   sh asciiverse/make-samples.sh
set -eu
cd "$(dirname "$0")/.."
out=asciiverse/samples
games=..
rm -rf "$out" && mkdir -p "$out"

take() {   # take NAME REPO REV PATH...
  name=$1 repo=$2 rev=$3; shift 3
  mkdir -p "$out/$name"
  git -C "$repo" archive "$rev" "$@" | tar -x -C "$out/$name"
}

take four-in-a-row "$games/four-in-a-row" 13b2e65 LICENSE src
take av "$games/../av" d5c7e78 LICENSE src
take tinycols "$games/tinycols" fd2ea7d LICENSE src include
take galaxis "$games/galaxis" 11fda83 COPYING galaxis.c
take blackjack "$games/blackjack-tui" f3f0a84 LICENSE $(git -C "$games/blackjack-tui" ls-tree --name-only f3f0a84 | grep '\.go$')
take zoridor "$games/zoridor" d49ec51 LICENSE.md $(git -C "$games/zoridor" ls-tree -r --name-only d49ec51 src | grep '\.zig$')
take intricacy "$games/intricacy" d570802 COPYING $(git -C "$games/intricacy" ls-tree --name-only d570802 | grep '\.hs$')
take robotfindskitten "$games/robotfindskitten" 891f8ab COPYING src/robotfindskitten.c
cat > "$out/README.md" <<'TXT'
# Code to type

Started from its own folder (av starts every title there), gittype types
the code in this folder: source from games in the Asciiverse catalog.
Start it in a project of your own to type your own code instead.

Each folder keeps its own licence:

- `four-in-a-row/`: Four in a Row, Rust, MIT (LICENSE), github.com/theasciiverse/four-in-a-row
- `av/`: av, the Asciiverse launcher, Rust, MIT (LICENSE), github.com/theasciiverse/av
- `tinycols/`: TinyCols by Rigel Di Scala, C, BSD-3-Clause (LICENSE), github.com/zedr/tinycols
- `galaxis/`: Galaxis by Eric S. Raymond, C, BSD-2-Clause (COPYING), gitlab.com/esr/galaxis
- `blackjack/`: blackjack-tui by MarcusBilo, Go, BSD-3-Clause (LICENSE), github.com/MarcusBilo/blackjack-tui
- `zoridor/`: Zoridor by Toby Jaffey (Ringtail Software), Zig, MIT (LICENSE.md), github.com/ringtailsoftware/zoridor
- `intricacy/`: Intricacy by Martin Bays, Haskell, GPL-3.0-or-later (COPYING), mbays.sdf.org/intricacy
- `robotfindskitten/`: robotfindskitten by Leonard Richardson and contributors, C, GPL-2.0-or-later (COPYING), codeberg.org/robotfindskitten/robotfindskitten

Each is taken from the Asciiverse edition at github.com/theasciiverse/<name>
(av's own repository for av), at the commit named in
asciiverse/make-samples.sh.
TXT
du -sh "$out"
