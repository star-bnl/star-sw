#!/usr/bin/perl

if($#ARGV==-1) {print "writestat.pl <run> <show|insert>\n";}

$run=$ARGV[0];
$opt=$ARGV[1];

$option=0;
if($opt eq "insert") {$option=1;}

$unixtime=`runtime $run`; $unixtime=~s/\n//g; $unixtime-=10;
$_=`date -u -d \"UTC 1970-01-01 $unixtime secs\" +\"%Y-%m-%d %k:%M:%S\"`; 
$_=~s/\n//g;
@dt=split;
print("run$run unixtime=$unixtime date=$dt[0] time=$dt[1]\n");

$cmd="root4star -b -q write_fgt_status.C\'($run,\"$dt[0]\",\"$dt[1]\",$option)\'";
print("$cmd\n");
$out = `$cmd`;
print("$out");
