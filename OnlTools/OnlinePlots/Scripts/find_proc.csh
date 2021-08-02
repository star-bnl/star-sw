#!/bin/bash 

#name of the program is first parameter
#$number = 0

number=`ps -u $LOGNAME  | grep -i $1 | grep -v rep | wc -l`

#env



echo   $number

exit $number
