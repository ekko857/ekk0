#!/bin/sh
#Author: Yijie Jiao yj2323@ic.ac.uk
#Script: tabtocsv.sh
#Description: substitude the tabs in the files with commas
#
#Saves the outout in to a .csv file
#Arguments: 1 -> tab delimited file
#Date: Oct 2023
if [$# -ne 1];then
    echo ' input fail'
    exit 1
fi

if [ ! -f "$1" ]; then
    echo "Input file not found: $1"
    exit 1
fi

echo "creating a commac delimited version of $1 ... "
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit 0