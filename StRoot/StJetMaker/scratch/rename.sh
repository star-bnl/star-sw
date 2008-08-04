#!/bin/bash

old=$1
new=$2

oldhdr=$(find ./ -regex ".*$old\.hh?" -print)
oldsrc=$(find ./ -regex ".*$old\.\(cxx\|C\)" -print)

newhdr=$(echo $oldhdr | sed -e "s/$old/$new/g")
newsrc=$(echo $oldsrc | sed -e "s/$old/$new/g")

./scratch/renameInclude.sh $oldhdr $newhdr
./scratch/renameClass.sh $old $new

echo "mv $oldhdr $newhdr"
echo "mv $oldsrc $newsrc"
echo "cvs rm $oldhdr $oldsrc"
echo "cvs add $newhdr $newsrc"
