#!/bin/sh
# generate zeroed tray entries

for module in `seq 1 32`; do
  for cell in `seq 1 6`; do
   printf "%4.3s%4.3s%4.3s\n" $1 $module $cell
   printf "%4.3s\n" 39

   for i in `seq 1 40`; do
    printf "%16.1f" 0
   done
   printf "\n"
   for i in `seq 1 40`; do
    printf "%16.1f" 0
   done
   printf "\n"
  done
done
