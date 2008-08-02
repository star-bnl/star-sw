#!/bin/bash

old=$1
new=$2

for file in $(find ./ -regex '.*\.\(cxx\|C\|hh?\)' -print); do
    found=$(cat $file | sed -n -e "/\([ <:(~]\|^\)$old\([ >&*;:(),]\)/p")
    if [ -n "$found" ]; then
	echo "sed -e \"s/\([ <:(~]\|^\)$old\([ >&*;:(),]\)/\1$new\2/g\" $file > ${file}_"
	echo "mv -f ${file}_ $file"
    fi
done
