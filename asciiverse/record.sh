#!/bin/sh
# Records asciiverse/gittype.cast, the catalog preview, at 100x30:
#
#   asciiverse/record.sh path/to/gittype
#
# Lays the program out as av installs it (the binary with samples/ beside
# it, in a throwaway folder, with a throwaway home), then asciinema rec
# runs it from that folder in a detached 100x30 tmux session while
# asciiverse/drive.py types through the pane. The game is stopped with
# SIGKILL, so the cast ends on the game, its last frame held for two
# seconds.
set -eu
cd "$(dirname "$0")/.."
bin=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
out=$PWD/asciiverse/gittype.cast
S=gittype-preview-$$
home=$(mktemp -d)
trap 'tmux -L "$S" kill-server 2>/dev/null; pkill -KILL -f "asciinema rec.*$out" 2>/dev/null; rm -rf "$home"' EXIT
mkdir -p "$home/av"
cp "$bin" "$home/av/gittype"
cp -r asciiverse/samples "$home/av/samples"
tmux -L "$S" new-session -d -s "$S" -x 100 -y 30 -c "$home/av" \
  "env -u TMUX -u XDG_STATE_HOME HOME=$home TERM=xterm-256color LANG=C.UTF-8 USER=player LOGNAME=player \
   asciinema rec --overwrite -q --window-size 100x30 --idle-time-limit 2 --title gittype \
     --capture-env TERM,LANG -c 'exec ./gittype' $out"
TMUX_SOCKET=$S python3 asciiverse/drive.py "$S"
pids=$(tmux -L "$S" display-message -p -t "$S" '#{pane_pid}')
for _ in 1 2 3; do
  pids="$pids $(for p in $pids; do pgrep -P "$p" || true; done)"
done
for p in $(echo $pids | tr ' ' '\n' | sort -u); do
  [ "$(ps -o comm= -p "$p" 2>/dev/null)" = gittype ] && kill -KILL "$p"
done
for _ in $(seq 1 20); do
  tmux -L "$S" has-session -t "$S" 2>/dev/null || break
  sleep 0.5
done
python3 - "$out" <<'PY'
import json, sys
lines = open(sys.argv[1]).read().splitlines()
keep = [lines[0]]
for line in lines[1:]:
    ev = json.loads(line)
    if ev[1] == "x":   # gittype leaves and re-enters the alternate
        break           # screen between screens, so only the exit ends it
    keep.append(line)
keep.append(json.dumps([2.0, "o", ""]))
open(sys.argv[1], "w").write("\n".join(keep) + "\n")
PY
echo "recorded $out"
