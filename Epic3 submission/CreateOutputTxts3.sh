#!/bin/bash

WORK_DIR="$(pwd)"   # Variable path for if/when you want to run script outside of working folder
SOURCE_FOLDER="$WORK_DIR/Epic3 submission folders/Epic3-Storyx-Test-Input"
DEST_FOLDER="$WORK_DIR/Epic3 submission folders/Epic3-Storyx-Test-Output"

if [[ ! -d "$SOURCE_FOLDER" ]]; then
    echo "Source folder does not exist."
    exit 1
fi
if [[ ! -d "$DEST_FOLDER" ]]; then
    mkdir -p "$DEST_FOLDER"
fi

for file in "$SOURCE_FOLDER"/*.txt; do
    [[ -e "$file" ]] || continue  # Skip if no .txt files
    BASENAME="$(basename "$file" .txt)"

    # Remove lines starting with "##" and copy to current directory as InCollege-Input.txt
    sed '/^##/d' "$file" > "$WORK_DIR/InCollege-Input.txt"

    # Remove storage files and run executable (faster than make, which also compiles)
    rm -f $WORK_DIR/Accounts.txt
    rm -f $WORK_DIR/Profiles.txt*
    $WORK_DIR/Incollege.exe > "term_output.txt" 2>&1

    if cmp -s "$WORK_DIR/InCollege-Output.txt" "term_output.txt"; then
        echo "${BASENAME} terminal output matches."
    else
        echo "${BASENAME} terminal output does not match!!!"
        diff --side-by-side --suppress-common-lines $WORK_DIR/InCollege-Output.txt term_output.txt | head -10
    fi

    # Copy and rename InCollege-Output.txt to destination folder with same basename
    if [[ -f "$WORK_DIR/InCollege-Output.txt" ]]; then
        cp "$WORK_DIR/InCollege-Output.txt" "$DEST_FOLDER/${BASENAME}-Output.txt"
    else
        echo "Warning: InCollege-Output.txt not found after processing $file."
    fi
done
