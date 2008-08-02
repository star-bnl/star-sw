#!/bin/bash

old=$1
new=$2

oldbase=$(basename $old)
newbase=$(basename $new)

for file in $(find ./ -regex '.*\.\(cxx\|C\|hh?\)' -print); do
    included=$(cat $file | sed -n -e "/#include.*$oldbase/p")
    if [ -n "$included" ]; then
	if [ "$file" = "$old" ]; then
	    file=$new
	fi
	echo "sed -e \"/#include.*$oldbase/s/$oldbase/$newbase/g\" $file > ${file}_"
	echo "mv -f ${file}_ $file"
    fi
done
