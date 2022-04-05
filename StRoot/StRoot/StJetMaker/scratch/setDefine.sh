#!/bin/bash

for file in $*; do
    define=$(echo $(basename $file) | tr [a-z] [A-Z] | sed -e "s/\./_/g")
    sed -e "s/^#ifndef.*$/#ifndef $define/g" \
	-e "s/^#define.*$/#define $define/g" \
	-e "s/^#endif.*$/#endif \/\/ $define/g" $file > ${file}_
    mv -f ${file}_ $file
done

