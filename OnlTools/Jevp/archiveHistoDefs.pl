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
    system("rtsLog -d JEFF -p 8004 -c archiveHistoDefs.pl \"$oldfile == $newfile:  no archive\"");
    exit;
} 

system("rtsLog -d JEFF -p 8004 -c archiveHistoDefs.pl \"$oldfile != $newfile:  archive\"");
#printf("archivefile: $archivefile\n");
#printf "files different... archiving to $archivefile\n";
system("cp $newfile $archivefile");

system("cp $newfile $oldfile");

#printf("done...\n");
