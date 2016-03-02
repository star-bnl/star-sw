#!/opt/star/bin/perl -w

#
# Add a run or run sequence to the .lis file.
#

print qq~
  Use an empty line to stop adding runs (i.e. press return)
  Syntax at the prompt is : 
    runnumber                 submits all file sequence for this run

    runnumber Chain           submits all file sequence for this run 
                              using 'Chain'. Note that within this
                              form, records for 'runnumber' are reset
                              (all status are considered new)

    runumber #FSeq Chain      submits up to 'Fseq' file sequence for
                              this run. Chain is MANDATORY.

    runumber #FSeq Chain Pat  As previous but only files matching pattern 
                              'pat' are submitted, the rest is skipped
                              Chain is MANDATORY.

 You can also use a semi column separated list of the above.

 NOTES:
    Pat is used to only submit files with this pattern (example st_WE
    would select '#FSeq' files but submit only the st_WE)

    When you specify the form 'runumber #FSeq Chain', up to '#Fseq' file 
    sequence means not ALREADY submitted or skipped. Only the form 
   'runnumber Chain' will reset FIRST the status of each file for the run 
    and then re-submit.

    If '#FSeq' == 0 or -1 then all files (that have not been submitted
    yet) will be selected. 'Pat' may still apply.

    If '#FSeq' < -1, then all filess (bot yet submitted) will be processed
    as for == 0 and the MAXEVENT will be ignored (all events will be processed). 
    Use with care (some files have a lot of events).
~;
print "\n";

$ver = "";
$ver = $ARGV[0] if (@ARGV);

do {
    print "Sequence : ";
    chomp($line = <STDIN>);
    @items= split(";",$line);
    foreach $seq (@items){
	push(@TOADD,$seq);
    }
} while ($line ne "");

$flag =1;
while (-e "FastOff.lock"){
    print "Lock file exists, waiting ... ".localtime()."\n" if ($flag);
    $flag = 0;
    sleep(5);
}


if ( open(FO,">>JobSubmit$ver.lis") ){
    foreach $seq (@TOADD){
	print "Adding [$seq]\n";
	print FO "$seq\n";
    }
    close(FO);
} else {
    print "Could not open JobSubmit$ver.lis (protection issue? wrong account? space?)\n";
}



