#! /bin/tcsh -f
ls -1d [0-2]*_[1-9].root | awk -F\_ '{print $1/1000}' | awk -F\. '{print "string:"$1}' | sort -u 
