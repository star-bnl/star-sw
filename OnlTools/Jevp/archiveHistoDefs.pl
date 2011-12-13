#!/usr/bin/perl

$mypath = $ARGV[0];
$myfile = $ARGV[1];

#printf $mypath."\n";

$tm = `date +-%Y-%m-%d-%k:%M:%S`;
chomp $tm;

$newfile = $mypath."/".$myfile;
$oldfile = $mypath."/".$myfile.".tmp";
$archivefile = $mypath."/display_archive/".$myfile.$tm;

# printf "new: $newfile\n";
# printf "old: $oldfile\n";

system("diff $oldfile $newfile > /dev/null");

$x = $?;

if($x == 0) {
    #printf "files the same...no archive\n";
    exit;
} 

#printf("archivefile: $archivefile\n");
#printf "files different... archive\n";
system("cp $newfile $archivefile");

system("cp $newfile $oldfile");

#printf("done...\n");
