#!/bin/bash
if [$# -ne 3];then
    echo ' input fail'
    exit 1
fi

if [ ! -f "$1 $2 $3 " ]; then
    echo "Input file and output not found"
    exit 1
fi

cat $1 > $3
cat $2 >> $3
echo Merged File is 
cat $3
