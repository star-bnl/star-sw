#!/usr/bin/perl

$mypath = $ARGV[0];
$myfile = $ARGV[1];

#printf $mypath."\n";

$tm = `date +-%Y-%m-%d-%H:%M:%S`;
chomp $tm;
chomp $myfile;

$newfile = $mypath."/".$myfile;
$oldfile = $mypath."/".$myfile.".tmp";
$archivefile = $mypath."/display_archive/".$myfile.$tm;

# printf "new: $newfile\n";
# printf "old: $oldfile\n";

system("diff $oldfile $newfile > /dev/null");

$x = $?;

if($x == 0) {
    printf "files (old=$oldfile new=$newfile)the same...no archive\n";
    exit;
} 

#printf("archivefile: $archivefile\n");
printf "files different... archiving to $archivefile\n";
system("cp $newfile $archivefile");

system("cp $newfile $oldfile");

#printf("done...\n");
