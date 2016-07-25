#! /usr/local/bin/tcsh -f
busers all | awk '{if ($4>0) print}'
