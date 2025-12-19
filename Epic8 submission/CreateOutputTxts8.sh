#!/bin/bash

#===================================================================================
#
# FILE: CreateOutputTxts.sh
#
# USAGE (Cygwin): bash .\CreateOutputTxts.sh
#
# DESCRIPTION: Put this script in the same directory as InCollege.cob and the Makefile.
#   Set the FOLDER variables appropriately beforehand.  Script will run all input.txt
#   skipping cleaning storage files if filename
#   has "-{cont}" postfix.  Will check if terminal output matches output.txt, then
#   force-copy output txt to DEST_FOLDER.  Cleans up produced files afterwards.
#===================================================================================

WORK_DIR="$(pwd)"   # Variable path for if/when you want to run script outside of working folder
SOURCE_FOLDER="$WORK_DIR/Epic8 submission/Epic8-Storyx-Test-Input"
DEST_FOLDER="$WORK_DIR/Epic8 submission/Epic8-Storyx-Test-Output"

# Compile the all-important InCollege
make -s -C $WORK_DIR compile

# Handle missing folders
if [ ! -d "$SOURCE_FOLDER" ]; then
    echo "Source folder does not exist."
    exit 1
fi
if [ ! -d "$DEST_FOLDER" ]; then
    mkdir -p "$DEST_FOLDER"
fi

for file in "$SOURCE_FOLDER"/*.txt; do
    [ -e "$file" ] || continue  # Skip if no .txt files
    BASENAME="$(basename "$file" .txt)"

    # Remove lines starting with "##" and copy to current directory as InCollege-Input.txt
    sed '/^##/d' "$file" > "$WORK_DIR/InCollege-Input.txt"

    # If filename ends in {cont}, don't remove storage files before running next input text
    if [[ ! "$file" == *"{cont}.txt" ]]; then
        # Remove storage files before running executable (faster than make, which also compiles)
        make -s -C $WORK_DIR cleanstorage
    else
        echo "Skipping clearing storage"
    fi

    # One or the other depending on whether Windows or Linux environment
    if [ -e "Incollege.exe" ]; then
        $WORK_DIR/InCollege.exe > "term_output.txt" 2>&1
    elif [ -e "InCollege" ]; then
        $WORK_DIR/InCollege > "term_output.txt" 2>&1
    fi

    # Check terminal output is exactly the same as output.txt
    if diff -q --strip-trailing-c "$WORK_DIR/InCollege-Output.txt" "term_output.txt"; then
        echo "${BASENAME} terminal output matches."
    else
        echo "${BASENAME} terminal output does not match!!!"
        diff --side-by-side --suppress-common-lines --strip-trailing-cr $WORK_DIR/InCollege-Output.txt term_output.txt | head -10
    fi

    # Copy and rename InCollege-Output.txt to destination folder with same basename
    if [[ -f "$WORK_DIR/InCollege-Output.txt" ]]; then
        cp "$WORK_DIR/InCollege-Output.txt" "$DEST_FOLDER/${BASENAME}-Output.txt"
    else
        echo "Warning: InCollege-Output.txt not found after processing $file."
    fi
done

# Cleanup
make -s -C $WORK_DIR clean
rm -f term_output.txt