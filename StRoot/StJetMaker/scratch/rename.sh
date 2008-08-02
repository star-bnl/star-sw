#!/bin/bash

toRemove=""
toAdd=""
for file in *.h *.cxx; do
    new=$(grep "^${file%.*} " cha.txt | cut -d " " -f 2)
    if [ ! -z $new ]; then
	new=$new\.${file#*.}
	echo "mv $file $new"
	toRemove="$toRemove $file"
	toAdd="$toAdd $new"
    fi
done
echo "cvs rm $toRemove"
echo "cvs add $toAdd"
