#!/bin/bash

WORK_DIR="$(pwd)"    # Variable path for if/when you want to run script outside of working folder
SOURCE_FOLDER="$WORK_DIR/Epic3 submission folders/Epic3-Storyx-Test-Input"
CHECK_FOLDER="$WORK_DIR/Epic3 submission folders/Epic3-Storyx-Test-Output"

if [[ ! -d "$SOURCE_FOLDER" ]]; then
    echo "Source folder does not exist."
    exit 1
fi
if [[ ! -d "$CHECK_FOLDER" ]]; then
    echo "Check folder does not exist."
    exit 1
fi

for file in "$SOURCE_FOLDER"/*.txt; do
    [[ -e "$file" ]] || continue  # Skip if no .txt files
    BASENAME="$(basename "$file" .txt)"

    # Remove lines starting with "##" and copy to current directory as InCollege-Input.txt
    sed '/^##/d' "$file" > "$WORK_DIR/InCollege-Input.txt"

    # Remove storage files and run executable (faster than make, which also compiles)
    rm -f Accounts.txt
    rm -f Profiles.txt*
    $WORK_DIR/Incollege.exe > "term_output.txt" 2>&1

    if cmp -s "$WORK_DIR/InCollege-Output.txt" "term_output.txt"; then
        echo "${BASENAME} terminal output matches."
    else
        echo "${BASENAME} terminal output does not match!!!"
        diff --side-by-side --suppress-common-lines $WORK_DIR/InCollege-Output.txt term_output.txt | head -10
    fi

    if cmp -s "$WORK_DIR/InCollege-Output.txt" "$CHECK_FOLDER/${BASENAME}-Output.txt"; then
        echo "${BASENAME} output file matches."
    else
        echo "${BASENAME} does not match!!!"
        diff --side-by-side --suppress-common-lines $WORK_DIR/InCollege-Output.txt "$CHECK_FOLDER/${BASENAME}-Output.txt" | head -10
    fi

done
