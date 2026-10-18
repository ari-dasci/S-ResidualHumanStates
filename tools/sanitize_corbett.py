"""Remove direct identifiers and credentials from the Corbett & Tangen Qualtrics exports.

Run on the extracted csvs/ folder before any analysis:
    python3 -I tools/sanitize_corbett.py data/corbett_tangen/csvs

* Link recovery: a follow-up response with an empty PROLIFIC_PID whose self-reported
  Prolific ID (Q48, "What is your Prolific ID?") matches a participant of the same
  condition's first session is linked to that participant. Such rows are flagged in a
  new column PID_source = "Q48" (otherwise "embedded"). In the released data this
  affects one completed two-month response (Neutral AI).
* Blanks: IPAddress, LocationLatitude, LocationLongitude, RecipientLastName,
  RecipientFirstName, RecipientEmail, ExternalReference, OpenRouterAPIKey and Q48.
* Replaces PROLIFIC_PID by pseudonymous codes (S0001, ...) that are consistent across
  the nine files and preserve the sort order of the original IDs.
* Post-check: aborts if any Prolific-style ID (24 hexadecimal characters) or API
  credential remains anywhere in the response rows.
The three Qualtrics header rows are kept. Files are rewritten in place.
"""
import csv, glob, os, re, sys

BLANK = {"IPAddress", "LocationLatitude", "LocationLongitude", "RecipientLastName",
         "RecipientFirstName", "RecipientEmail", "ExternalReference", "OpenRouterAPIKey", "Q48"}
PID_RE = re.compile(r"\b[0-9a-fA-F]{24}\b")
KEY_RE = re.compile(r"sk-or-|Bearer\s", re.I)

folder = sys.argv[1]
files = sorted(glob.glob(os.path.join(folder, "*.csv")))
if len(files) != 9:
    sys.exit(f"expected 9 CSV files in {folder}, found {len(files)}")

tables = {}
for f in files:
    raw = open(f, "rb").read()
    bom = raw.startswith(b"\xef\xbb\xbf")
    rows = list(csv.reader(raw.decode("utf-8-sig").splitlines(keepends=True)))
    tables[f] = (bom, rows)

def col(rows, name):
    return rows[0].index(name)

# 1. recover missing embedded IDs from the self-reported ID (same condition, first session)
recovered = 0
known = {}
for f, (bom, rows) in tables.items():
    if "time1" in os.path.basename(f):
        known[f] = {r[col(rows, "PROLIFIC_PID")] for r in rows[3:] if r[col(rows, "PROLIFIC_PID")]}
for f, (bom, rows) in tables.items():
    ids = known[f.replace("time2", "time1").replace("time3", "time1")]
    pid, q48 = col(rows, "PROLIFIC_PID"), col(rows, "Q48")
    for r in rows[:3]:
        r.append("PID_source")
    for r in rows[3:]:
        src = "embedded"
        if not r[pid] and r[q48].strip() in ids:
            cand = r[q48].strip()
            # the self-reported ID must not already have an embedded response in this file
            if any(x[pid] == cand for x in rows[3:]):
                sys.exit(f"ambiguous link in {os.path.basename(f)}; aborting")
            r[pid] = cand; src = "Q48"; recovered += 1
        r.append(src)

if recovered != 1:
    sys.exit(f"expected exactly one linked follow-up response in the released data, found {recovered}")

# 2. pseudonymise IDs (order-preserving) and blank identifiers
pids = set()
for bom, rows in tables.values():
    i = col(rows, "PROLIFIC_PID")
    pids.update(r[i] for r in rows[3:] if r[i])
code = {p: "S%04d" % (k + 1) for k, p in enumerate(sorted(pids))}

for f, (bom, rows) in tables.items():
    h = rows[0]
    blank_idx = [h.index(c) for c in BLANK if c in h]
    pid = h.index("PROLIFIC_PID")
    for r in rows[3:]:
        for j in blank_idx:
            if j < len(r):
                r[j] = ""
        if r[pid]:
            r[pid] = code[r[pid]]
        for cell in r:  # 3. post-check (before anything is written)
            if PID_RE.search(cell) or KEY_RE.search(cell):
                sys.exit(f"identifier or credential left in {os.path.basename(f)}; aborting")

for f, (bom, rows) in tables.items():
    with open(f, "w", encoding="utf-8-sig" if bom else "utf-8", newline="") as out:
        csv.writer(out, quoting=csv.QUOTE_MINIMAL).writerows(rows)
print(f"sanitised {len(files)} files; {len(code)} participant IDs pseudonymised; "
      f"{recovered} follow-up response(s) linked through the self-reported ID")
