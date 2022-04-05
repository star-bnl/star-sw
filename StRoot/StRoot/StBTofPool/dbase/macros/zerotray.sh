#!/bin/sh
# generate zeroed tray entries

case "$1" in

[0-9]*)
     # cell-based calibration parameters
     if [ $2 == 'c' ]; then
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

     # board-based calibration parameters
     elif [ $2 == 'b' ]; then
            for board in `seq 1 8`; do
               printf "%4.3s%4.3s%4.3s\n" $1 $board
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
     fi
     ;;

*)
     # Print quick usage report
     echo "Usage: $0 <tray> <c|b> > <tray>.txt"
    ;;

esac
