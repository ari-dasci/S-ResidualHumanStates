#!/usr/bin/env bash
# Downloads the public source data used by the reanalyses and removes direct identifiers.
# Requires git, curl, wget, unzip, python3, sha256sum (and gdown or a manual zip for Study 2).
#
# Study 2: the authors' open data are in a Google Drive folder. They are fetched with `gdown`
# (pip install gdown) or, if that is not available, from a manually downloaded zip of the
# folder passed as CABITZA_DATA_ZIP.
# Study 4 alternative: if the OSF download is not possible, set CORBETT_DATA_ZIP to a local
# copy of the OSF "Data" component zip (the one that contains csvs.zip).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$ROOT/data" && cd "$ROOT/data"

# --- Study 2: Cabitza & Vicente (2026), Google Drive open data folder ------
CABITZA_URL="https://drive.google.com/drive/folders/1E-4MMbgPMU1t2jz7bWK9Gu5drc_XhRLF"
CABITZA_SHA256="d73ec24f0721bb541c8c68da1e1749511a28aafeaaf5b2cc7e5ea652e2dc1154"  # MachineMentoring_DataHD.csv
mkdir -p cabitza_vicente
if [ -n "${CABITZA_DATA_ZIP:-}" ]; then
  unzip -o -q "$CABITZA_DATA_ZIP" 'Data/*' -d cabitza_vicente
elif command -v gdown >/dev/null 2>&1; then
  (cd cabitza_vicente && gdown --folder --quiet "$CABITZA_URL" -O .)
  rm -rf "cabitza_vicente/Script blood smears"   # images and notebook, not needed
  [ -d cabitza_vicente/Data ] || { echo "Study 2: expected a Data/ subfolder from Google Drive" >&2; exit 1; }
else
  echo "Study 2: install gdown or download the Drive folder as a zip and set CABITZA_DATA_ZIP" >&2; exit 1
fi
(cd cabitza_vicente/Data && echo "$CABITZA_SHA256  MachineMentoring_DataHD.csv" | sha256sum -c -)

# --- Study 3: Bastani et al. (2025), GitHub, frozen commit -----------------
if [ ! -d GenAICanHarmLearning ]; then
  git clone https://github.com/obastani/GenAICanHarmLearning.git
fi
git -C GenAICanHarmLearning checkout 2f63dae1a01d51453826fe07ef5cf6678e339588

# --- Study 4: Corbett & Tangen (2026), OSF component "Data" ------------------
VO="bba17d9c74ca4dfca02a716cb2ed21f6"   # view-only token from the article's data statement
CSVS_SHA256="99603ed85ce618bcfd2693bd2361a52d6a195dd75662902c8d98c939987e1463"  # csvs.zip used in the article
mkdir -p corbett_tangen && cd corbett_tangen
# never leave raw (identifiable) files behind if anything below fails
cleanup_raw() { rm -rf csvs Data.zip csvs.zip; echo "Study 4: failed; raw files removed" >&2; }
trap cleanup_raw ERR
if [ -n "${CORBETT_DATA_ZIP:-}" ]; then
  cp "$CORBETT_DATA_ZIP" Data.zip
else
  DATA_ID=$(curl -fsS "https://api.osf.io/v2/nodes/wseq3/children/?view_only=$VO" \
    | python3 -I -c 'import sys,json; print([d["id"] for d in json.load(sys.stdin)["data"] if d["attributes"]["title"]=="Data"][0])')
  wget -q -O Data.zip "https://files.osf.io/v1/resources/$DATA_ID/providers/osfstorage/?zip=&view_only=$VO"
fi
unzip -o -q Data.zip csvs.zip
echo "$CSVS_SHA256  csvs.zip" | sha256sum -c -   # stops if the OSF file has changed
unzip -o -q csvs.zip -x '__MACOSX/*'

# The OSF exports contain IP addresses, geolocation, Prolific IDs (embedded and
# self-reported) and an API credential. They are removed here (the script checks for
# exactly nine files and aborts if any identifier remains); only sanitised files are kept.
python3 -I "$ROOT/tools/sanitize_corbett.py" csvs
rm -f Data.zip csvs.zip
trap - ERR
