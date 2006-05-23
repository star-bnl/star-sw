#!/usr/local/bin/tcsh
mysql -h heston.star.bnl.gov -u StarLogger -plogger --exec="\. $1"
