#!/bin/bash

for file in *.cxx *.h; do
    olds=$(cat $file | sed -n -e '/#include/s/^.*["<]\(.*\)[">]/\1/p')
    if [ -n "$olds" ]; then
	sedcom=""
	for old in $olds; do
	    new=$(grep "^${old%.*} " cha.txt | cut -d " " -f 2)
	    if [ ! -z $new ]; then
		new=$new\.${old#*.}
		sedcom=$(echo $sedcom "/#include/s/${old%.*}/${new%.*}/g")
	    fi
	done
	if [ -n "$sedcom" ]; then
	    sedcom=$(echo $sedcom | tr " " "\n")
	    sed -e "$sedcom" $file > ${file}_
	    mv ${file}_ $file
	fi
    fi
done
