#!/usr/local/bin/perl
#
# $Id: diskUsers.pl,v 1.2 2000/01/22 02:21:42 wenaus Exp $
#
# $Log: diskUsers.pl,v $
# Revision 1.2  2000/01/22 02:21:42  wenaus
# no 'strict'
#
# Revision 1.1  1999/09/16 16:43:25  wenaus
# cron scripts for home dir usage summaries
#
#
######################################################################
#
# diskUsers.pl
#
# Author: K. Olchanski
#   Adapted for STAR by T. Wenaus
#
# Report disk usage for everyone on a home directory disk
#
# Usage: diskUsers.pl path
#

$dir = shift @ARGV;
$verbose = shift @ARGV;

$dir = `pwd` if ! defined $dir;
$verbose = 0 if ! defined $verbose;

$verbose = int $verbose;

$| = 1;

print "Scanning: [$dir], verbose: [$verbose]\n" if $verbose;

chdir $dir;

$cmd = "/bin/ls -RAso";

open(LS,"$cmd |") || die "Cannot fork $cmd : $!\n";

while (<LS>)
  {
    chop;
    $line = $_;

    if (/^\.\//)
      {
	  print "Scanning: $line\n" if $verbose;
      }
    elsif (/^total/)
      {
	# do nothing...
      }
    else
      {
#	print "Line: $line\n";

	($nblocks,$perm,$nlinks,$user,$bytes) = split(' ',$line);

	$inblocks = int($nblocks);
	$inlinks = int($nlinks);

	next if $inblocks == 0;
	next if $inlinks == 0;

	$user = substr($user."        ",0,8);

#	print "$inblocks  $inlinks  $user\n";	

	$used{$user} += ($inblocks)/($inlinks);
      }
  }

close LS;

@report = ();

$totalkbytes = 0;

foreach $user (keys %used)
  {
     $nblocks = $used{$user};
     $nkbytes = int (($nblocks+1)/2);

     next if $nblocks eq 0;

     push @report,(sprintf(" %8d     %s\n",$nkbytes,$user));

     $totalkbytes += $nkbytes;
  }

$timenow = scalar localtime(time());
print "Report on [$dir] at $timenow, $totalkbytes Kbytes used:\n";
print "   Kbytes     User\n";

print sort {$b cmp $a} @report;

exit 123;

open (SORT,"| sort -r");

foreach $user (keys %used)
  {
     $nblocks = $used{$user};
     $nkbytes = ($nblocks+1)/2;

     next if $nblocks eq 0;

     printf SORT " %8d     %s\n",$nkbytes,$user;
  }

close SORT;

#end file
