#!/bin/bash

# batch_profile.sh - Generate data profile reports for all CSV files in a directory
# Usage: ./batch_profile.sh <directory> [qmd_file]

DIR="${1:?Error: Please provide directory containing CSV files}"
QMD="${2:-./data_profile_report.qmd}"

for csv in "$DIR"/*.csv; do
    [[ -e "$csv" ]] || continue
    name=$(basename "$csv" .csv)
    echo "Processing: $name.csv"
    quarto render "$QMD" -P "filepath:$csv" -o "${name}_info.html" --output-dir "$DIR"
done

echo "Done."