#!/usr/bin/env perl

#
# This script takes care of saturating the queue
# with the appropriate number of jobs.
#
# The variable $again is set to 1 when there is a
# need to retry i.e. we have submitted at least one
# file or there are no slots in the chosen queue.
#
# The presence of a file name  submit.quit  will
# make this procedure quit at the next cycle.
#
# This script now takes arguments as well (not mandatory)
# as follow : QueueNumber
#             Pattern
#             Sorting mode
#             Drop value
#

use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use CRSQueues;  

#
# '4 -3' is = 2 jobs in all qeues from 4 -> (4-3)=1
#
$QUEUE = 4;            		# Queue number to submit to
$DROP  = 2;             	# drop value (if any). 
$PRIO  = 50;           		# default priority
$PATT  = "*st_physics*";	# pattern for job files
$SLTIME=  2;           		# sleep time between job submission   was 15
$SLTIM2= 30;           		# sleep time between queue inspection was 60
$ARCH  = "../archive"; 		# Archive directory path
$SORT  = 1;			# 1 sort in list of files descending order
$MAXJB = 25;                    # Maximum number of jobs in one loop  was 15


# Grab the queue number, pattern and sorting from the command
# line. 
$QUEUE = shift(@ARGV) if( @ARGV);
$PATT  = shift(@ARGV) if( @ARGV);
$SORT  = shift(@ARGV) if( @ARGV);
$DROP  = shift(@ARGV) if( @ARGV);


if( $PATT !~ m/\.\*/){
    $PATT  =~ s/\*/.*/g;   # backward support shell pattern
}

print "Starting with pattern '$PATT' Queue=$QUEUE Sorting=$SORT Drop=$DROP\n";
do {
   # If this file exists, it will quit now ...
   if( -e "submit.quit"){
     print "I have been asked to quit on ".localtime()."\n";
     unlink("submit.quit");
     exit;
   }

   # glob() is unstable. Use readdir() instead.
   # In addition, we can use perl patterns 
   opendir(DIR,".");
   @TMP = grep { /$PATT/ } readdir(DIR);
   closedir(DIR);
   $again = ($#TMP != 0);

   
   # Sort if requested
   if($SORT==1){
       @all = sort {$b cmp $a} @TMP;
   } elsif ($SORT == 3){
       # Random sorting will span a set jobs in the queue
       # and evenly distribute things
       undef(@all);
       while ($#TMP != -1){
           $i = rand($#TMP+1);
           $s = splice(@TMP,$i,1); #print "Removing $s\n";
           if ($s =~ /\.lock/){ next;}
           push(@all,$s);
       }
       
   } else {
       @all = sort {$a cmp $b} @TMP;
   }


   $TOT = CRSQ_getcnt($QUEUE,$DROP,$PATT,1);
   if($TOT > 0){
       for($i=0 ; $i < $TOT && $i <= $#all && $i < $MAXJB ; $i++){
	   $jfile = $all[$i];
	 
	   if( $jfile !~ m/\.lock/ ){
	       if( -e $jfile.".lock"){
		   #print "Lock file exists for $jfile\n";
	       } elsif( ! -e $jfile){
		   print "Oups ! $jfile is gone ...\n";
	       } else {
		   $lockf = $jfile.".lock";
		   open(FO,">$lockf"); 
		   close(FO);
		   if ( ($q = CRSQ_submit($jfile,$PRIO,$QUEUE,$DROP)) ){
		       if( -e "$ARCH/$jfile"){ unlink("$ARCH/$jfile");}
		       rename($jfile,"$ARCH/$jfile");
		       
		       print "Successful submit of $jfile in queue $q\n";
		   }
		   sleep($SLTIME);
		   unlink($lockf);
	       }
	   } else {
	       #print "Skipping lock file $jfile\n";
	   }
       }
       print "Waiting until next loop ".localtime()."\n" ;
   }

   sleep($SLTIM2);

} while ($again);

print "It appears that all jobs are submitted\n";
