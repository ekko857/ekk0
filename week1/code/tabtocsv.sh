#!/bin/sh
#Author: Yijie Jiao yj2323@ic.ac.uk
#Script: tabtocsv.sh
#Description: substitude the tabs in the files with commas
#
#Saves the outout in to a .csv file
#Arguments: 1 -> tab delimited file
#Date: Oct 2023

echo "creating a commac delimited version of $1 ... "
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
echo -e "test \t\t test" >> ../sandbox/test.txt
exit