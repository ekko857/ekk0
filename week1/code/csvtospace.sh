#!/bin/bash
##1. check whether the input file was provided
if [ $# -ne 2 ];then
    echo 'usage : $0 <input_file.csv'
    exit 1
fi

##3.check whether the input file exists
if [ ! -f "$1" ]; then
    echo "Input file not found: $1"
    exit 1
fi

###4.csv to space
awk -F ',' -v OFS=' ' '{$1=$1}1' "$1" > "$2"

echo "Conversion complete. Space-separated values saved to $2"



