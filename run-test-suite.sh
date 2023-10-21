#!/bin/bash

# This script is used to run the test suite for the compiler.

# Run the test suite for the compiler.
echo "==========================================================================="
echo "Running the test suite for the compiler."
echo "This suite contains both valid and invalid programs."
echo "It only checks whether they compile or not, not whether they run correctly."
echo "The latter should be done by hand."
echo "==========================================================================="

compile="./gracec.sh"

baddir="./test_programs/error_programs"
gooddir="./test_programs/valid_programs"

# create the error log files
errorlogbad="./errors_bad.log"
errorloggood="./errors_good.log"
touch $errorlogbad
touch $errorloggood

GREEN='\033[0;32m'
RED='\033[0;31m'
RESET='\033[0m'

echo "==========================================================================="
echo "Testing Invalid Programs."
echo "==========================================================================="

for file in $baddir/*.grc;
do
    # keep the filename without the path
    filename=$(basename "$file")
    # the compiler should return a non-zero exit code
    echo "Compiling $filename..."
    echo "error output for $filename:" >> $errorlogbad
    $compile $file >> $errorlogbad
    status=$?
    echo "error output for $filename done." >> $errorlogbad
    if [ $status -eq 0 ]
    then
        echo -e "${RED}Test failed${RESET}: $filename should return a non-zero exit code."
    else
        echo -e "${GREEN}Test passed${RESET}: $filename passes."
    fi
done

echo "==========================================================================="
echo "Testing Valid Programs."
echo "==========================================================================="

for file in $gooddir/*.grc
do
    filename=$(basename "$file")
    echo "Compiling $filename..."
    echo "error output for $filename:" >> $errorloggood
    $compile $file >> $errorloggood
    status=$?
    echo "error output for $filename done." >> $errorloggood
    if [ $status -eq 0 ]
    then
        echo -e "${GREEN}Test passed${RESET}: $filename passes."
    else
        echo -e "${RED}Test failed${RESET}: $filename should return a zero exit code."
    fi
done

echo "==========================================================================="
echo "Test suite complete."
echo "==========================================================================="
echo "Do you want to cleanup the output files of the test suite? (y/n)"
read cleanup
if [ $cleanup == "y" ]
then
    echo "remove logs? (y/n)"
    read removelogs
    if [ $removelogs == "y" ]
    then
        rm $errorlogbad
        rm $errorloggood
    fi
    echo "remove .asm files? (y/n)"
    read removeasm
    if [ $removeasm == "y" ]
    then
        if ls $baddir/*.asm 1> /dev/null 2>&1; then
            rm $baddir/*.asm
        fi
        if ls $gooddir/*.asm 1> /dev/null 2>&1; then
            rm $gooddir/*.asm
        fi
    fi
    echo "remove .imm files? (y/n)"
    read removeimm
    if [ $removeimm == "y" ]
    then
        if ls $baddir/*.imm 1> /dev/null 2>&1; then
            rm $baddir/*.imm
        fi
        if ls $gooddir/*.imm 1> /dev/null 2>&1; then
            rm $gooddir/*.imm
        fi
    fi
    echo "remove executable .out files? (y/n)"
    read removeout
    if [ $removeout == "y" ]
    then
        if ls $baddir/*.out 1> /dev/null 2>&1; then
            rm $baddir/*.out
        fi
        if ls $gooddir/*.out 1> /dev/null 2>&1; then
            rm $gooddir/*.out
        fi
    fi
    echo "Cleanup complete."
else
    echo "Cleanup skipped."
fi

