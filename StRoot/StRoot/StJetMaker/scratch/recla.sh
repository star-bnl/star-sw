#!/bin/bash

for file in *.cxx *.h; do
    cat $file | sed -f sedscr.txt > ${file}_
    mv -f ${file}_ $file
done
