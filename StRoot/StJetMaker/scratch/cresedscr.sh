#!/bin/bash

while read old new;do
    echo 's/\([ <:(~]\|^\)'$old'\([ >&*;:(),]\)/\1'$new'\2/g'
done < cha.txt

