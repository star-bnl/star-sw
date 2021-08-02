#!/bin/bash 

#name of the program is first parameter
#$number = 0

number=`ps -A | grep $1 | grep -v rep | grep $2 |wc -l`

#env



echo   $number

exit $number
