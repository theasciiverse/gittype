"""drive.py SESSION: plays gittype in a 100x30 tmux pane for the catalog
preview (asciiverse/record.sh), as a quick typist.

From the title screen it starts a session on Easy. In a challenge it reads
the code from the Code box (numbered lines, a return arrow where the line
ends), skips each line's indentation as gittype does, and types the rest
at a varied pace, with the odd slip, pressing Enter at each arrow. Space
moves on from each stage's result. Plays for SECONDS_LIMIT (default 45)."""
import os, random, re, subprocess, sys, time

S = sys.argv[1]
secs = float(os.environ.get("SECONDS_LIMIT", "45"))
TMUX = ["tmux", "-L", os.environ.get("TMUX_SOCKET", "default")]
rng = random.Random(7)
LINE = re.compile(r" │\s+(\d+) │ (.*?)\s*│$")


def send(*k):
    subprocess.run(TMUX + ["send-keys", "-t", S, *k])


def screen():
    out = subprocess.run(TMUX + ["capture-pane", "-p", "-t", S],
                         capture_output=True, text=True).stdout
    return out.split("\n")


def code_lines(rows):
    """The challenge's lines; a long line wraps onto unnumbered rows."""
    lines, inside = [], False
    for r in rows:
        if "Code" in r and "\u250c" in r:
            inside = True
            continue
        if inside and "\u2514" in r:
            break
        if not inside:
            continue
        m = LINE.match(r)
        if m:
            lines.append(m.group(2))
        elif lines and not lines[-1].endswith("\u21b5"):
            more = re.match(r" \u2502 (.*?)\s*\u2502$", r)
            if more and more.group(1).strip():
                lines[-1] += more.group(1)
    return lines


def type_code(lines, t0):
    for line in lines:
        text = line.rstrip("↵").lstrip(" ")
        for ch in text:
            if time.time() - t0 > secs:
                return
            if ch.isalpha() and rng.random() < 0.025:
                send("-l", rng.choice("asdfjkl"))   # a slip
                time.sleep(0.12)
            send("-l", ch)
            time.sleep(rng.uniform(0.09, 0.2) + (0.25 if ch in " (" and rng.random() < 0.1 else 0))
        if line.endswith("↵"):
            send("Enter")
            time.sleep(rng.uniform(0.15, 0.3))


def main():
    t0 = time.time()
    done = None
    time.sleep(2.5)
    while time.time() - t0 < secs:
        rows = screen()
        text = "\n".join(rows)
        lines = code_lines(rows)
        header = next((r for r in rows if "[Easy]" in r or "[Normal]" in r), "")
        if lines and "Time:" in text and header != done:
            time.sleep(0.3)
            type_code(code_lines(screen()) or lines, t0)
            done = header
            time.sleep(0.6)
        elif "[SPACE] Continue" in text or "[SPACE] Start" in text or "Press [SPACE] to start" in text:
            time.sleep(1.2)
            send("Space")
            time.sleep(0.8)
        else:
            time.sleep(0.2)


main()
