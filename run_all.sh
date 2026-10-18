#!/usr/bin/env bash
# Runs the three reanalyses from the repository root. Results and logs go to $OUT_DIR (default output/),
# with one subfolder per study (P2/, P3/, P4/).
set -euo pipefail
cd "$(dirname "$0")"
BASE="${OUT_DIR:-output}"
mkdir -p "$BASE/P2" "$BASE/P3" "$BASE/P4"
OUT_DIR="$BASE/P2" Rscript P2_cabitza/P2_policy_contrasts.R > "$BASE/P2_log.txt" 2>&1
OUT_DIR="$BASE/P3" Rscript P3_bastani/P3_cluster_robust.R > "$BASE/P3_log.txt" 2>&1
OUT_DIR="$BASE/P4" Rscript P4_corbett/P4_longitudinal.R  > "$BASE/P4_log.txt" 2>&1
echo "Done. Compare $BASE/ with expected_output/."
