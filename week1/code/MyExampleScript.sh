#!/bin/sh

MSG1="Hello"
MSG2=$USER
echo "$MSG1 $MSG2"
echo "Hello $USER"
echo

echo  Remove    excess      spaces.| tr -s " "

echo "remove all the a's" | tr -d "a"

echo set to uppercase | tr [:lower:] [:upper:]

echo "10.00 only numbers 1.33" | tr -d [:alpha:] | tr -s " " ","