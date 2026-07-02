#!/usr/bin/perl

use strict;
use warnings;
use Cwd qw(abs_path);
use Getopt::Long qw(GetOptions);
use Getopt::Long qw(HelpMessage);
use Pod::Usage;

=pod

=head1 NAME

Filter - Handy script for filtering a file list with some specific criteria. For example, generating a new list such that the new list has at least one file from every runnumber and is not empty. Or generating many list files into directory filtering on run number and some "suitable" number of events to process for each job

=head1 SYNOPSIS

Filter.pl E<lt>-i I<file>E<gt>  [-f I<file{stdout}>] [-v I<level{0}>] [-h]

  --input, -i    Input file with list of runs to filter
  --file, -f     File or existing folder to write output to (default is to write to stdout)
  --verobse, -v  Verbose output level
  --help, -h     Print this help and exit

=head1 VERSION

0.0.1

=head1 DESCRIPTION

This program is meant to be a quick and dirty solution to be able to generate a new run list from the full list of Run22 production files. It was needed so that I can submit jobs with a single run number that can then be used to fill the spin dataabase. As such its options are limited. Later a new functionality was addd such that to take the full input file list and generate a folder containing many smaller list files tied to a particular runnumber. It may generate multiple list files for a given runnumber with each list file containing a subset of those runs based on some "good" criteria of how many events to process per job.

=head1 CAVEATS

Any extra filters need to implemented directly in the source code as the purpose of this is singular. Also, input file list expected to be in the form "/path/file/events/size"

=head1 AUTHOR

David Kapukchyan

=cut

=begin comment

LOG

@[September 6,2024](David Kapukchyan)
> First instance

@[September 9, 2024](David Kapukchyan)
> Sorted File Hash by runnumber (keys)

@[October 7, 2024](David Kapukchyan)
> Added an extra check to only accept runs greater than or equal to 23005043 since that is the start of FCS calibrated run data

@[October 22, 2024](David Kapukchyan)
> Added some code to print the run number sorted filelist to a file. It is commented out because it is not a permanent feature yet.

@[January 17, 2025](David Kapukchyan)
> Changed it so that it generates several file lists separated by run number and maxing each list at 500k events

@[February 24, 2025](David Kapukchyan)
> Fixed eventcounter and number of files per job counter to properly reset on every new file list. Found a bad runnumber=23108012 and added a special case for this debugging this runnumber. See note below

@[June 30, 2025](David Kapukchyan)
> Fixed code so that it will handle either writing to a single file or a directory. It figures it out at runtime based on argument passed on the command line. If there is a trailing slash then it will assume directory and if it can't find it will spit out an error. If it is an existing directory don't need trailing slash. All other arguments will be assumed to be file names

@[May 14, 2026](David Kapukchyan)
> Fixed the regex expression that matches MuDst files

=cut

my $INPUTFILE = "";
my $OUTPUTFILE = "";
my $VERBOSE = 0;

GetOptions(
    'input|i=s'   => \$INPUTFILE,
    'file|f=s'    => \$OUTPUTFILE,
    'verbose|v=i' => \$VERBOSE,
    'help|h'      => sub { HelpMessage(0) }
    ) or HelpMessage(1);

if( $INPUTFILE eq "" ){ print "ERROR:No input file given: $!\n"; HelpMessage(0); }
$INPUTFILE = abs_path($INPUTFILE);
open( my $inputfh, '<', $INPUTFILE ) or die "Could not open file '$INPUTFILE': $!";
if( $VERBOSE>=1 ){ print "Opened $INPUTFILE for reading\n"; }

my $nlines = 0;
my %SortedFiles; #Hash where key is runnumber and value is comma separated list of all files with that runnumber
while( my $line = <$inputfh> ){
    #if( $nlines>=10000 ){ last; }
    if( $VERBOSE>=4 ){ print "$line\n"; }
    
    if( $line =~ m/\/\w*_(2[23]\d{6})_\w*_(\d{7})\.MuDst\.root\/?(\d*)\/?(\d*)/ ){
	my $runnum = $1;    #STAR RunNumber
	if( $VERBOSE>=4 ){ print " + |runnum:${runnum}\n"; }
	chomp $line;
	if( exists $SortedFiles{$runnum} ){ $SortedFiles{$runnum} = $SortedFiles{$runnum}.",".$line; }
	else{ $SortedFiles{$runnum} = $line; }
    }
    $nlines++;
}
close $inputfh;

#Sort hash by keys then get an array of its values by the sorted keys
#[September 9, 2024] > https://stackoverflow.com/questions/10901084/how-can-i-sort-a-perl-hash-on-values-and-order-the-keys-correspondingly-in-two
my @SortedFileKeys = sort { $a <=> $b } keys(%SortedFiles);
my @SortedFileVals = @SortedFiles{@SortedFileKeys};

my @SortedMaxFiles;
my @MissingRunnum;
#open( my $sortedoutputfh, '>', "Sorted.list" ) or die "Could not open file 'Sorted.list': $!";
foreach my $csvfiles (@SortedFileVals){
    if( $VERBOSE>=4 ){ print "$csvfiles\n"; }
    my @files = split( ',',  $csvfiles);
    my $maxsize = 0;
    my $maxevents = 0;
    my $maxfile = "";
    my $runnum = "";
    foreach my $file (@files){
	if( $VERBOSE>=2 ){ print "$file\n"; }
	if( $file =~ m/\/\w*_(2[23]\d{6})_\w*_(\d{7})\.MuDst\.root\/?(\d*)\/?(\d*)/ ){
	    $runnum = $1;       #STAR RunNumber
	    my $segment = $2;   #Segment number for a given STAR file with a given RunNumber
	    my $nevents = $3;   #Number of events in file
	    my $size = $4;      #Size of file in bytes
	    if( $VERBOSE>=2 ){ print " + |runnum:${runnum}|segment:${segment}|nevents:${nevents}|size:${size}\n"; }
	    if( $nevents>$maxevents ){
		if( $size>$maxsize ){
		    $maxevents = $nevents;
		    $maxsize = $size;
		    $maxfile = $file;
		}
	    }
	    #if( ${runnum} > 23005043 ){ print $sortedoutputfh "$file\n"; }
	}
    }
    if( $maxsize==0 || $maxevents==0 ){
	print "WARNING - Could not find a valid file for runnumber $runnum\n";
	push( @MissingRunnum, $runnum );
    }
    else{
	if( ${runnum} > 23005043 ){
	    if( $VERBOSE>=2 ){ print " * |maxevents:$maxevents|maxsize:$maxsize\n"; }
	    push (@SortedMaxFiles, $maxfile);
	}
    }
}

#@[June 30, 2025] > This figures out if the argument to -f was a file or directory and sets the flag '$writetofile' appropriately to be handled by the code later to write to a single file or make multiple files in a directory
my $outputfh;
my $writetofile = 1;     # -1 means don't do anything, 0 means print, 1 means single file (default), 2 means write many files to a directory.
if( $OUTPUTFILE ne "" ){
    #print(" - Before:$OUTPUTFILE \n");
    $OUTPUTFILE = abs_path($OUTPUTFILE);  #abs_path gets rid of trailing slash if directory. If directory does not exist variable becomes unintialized so code will not run
    #print(" - After:$OUTPUTFILE \n");
    if( -d $OUTPUTFILE ){ $writetofile = 2; }
    if( $writetofile==1 && -f $OUTPUTFILE ){
	print "WARNING - File '${OUTPUTFILE}' exists overwrite (cannot be undone) Y/n/(q)uit?: ";
	my $verify = <STDIN>; chomp $verify;
	if( $verify eq "Y" )   { $writetofile = 1; }
	elsif( $verify eq "q" ){ $writetofile = -1; print "Skipping writing output\n"; }
	else                   { $writetofile = 0; print "Will not overwrite file '${OUTPUTFILE}' will print to stdout\n"; }
    }
    if( $writetofile==1 ){
	open( $outputfh, '>', $OUTPUTFILE ) or die "Could not open file '$OUTPUTFILE': $!";
	if( $VERBOSE>=1 ){ print "Opened $OUTPUTFILE for writing\n"; }
    }
    if( $writetofile==2 ){
        print "WARNING - Files with names matching 'Files_\\d{8}_\\d+.list' in '${OUTPUTFILE}' will be overwritten.\nContinue (cannot be undone) Y/n?: ";
	my $verify = <STDIN>; chomp $verify;
	if( $verify ne "Y" ){ $writetofile=-1; print "Aborting writing files"; }
    }
}

#@[June 30, 2025] > This is for writing each xrd file to a single file with the filter applied, if not writing print all the files
if( $writetofile==0 || $writetofile==1 ){
    foreach my $sortedfile (@SortedMaxFiles){
	if( $outputfh ){
	    print $outputfh "$sortedfile\n";
	    if( $VERBOSE>=1 ){ print "|BestFile:$sortedfile\n"; }
	}
	else{ print "|BestFile:$sortedfile\n"; }
	if( $sortedfile =~ m/st_fwd_(\d{8})_/ ){
	    #print $runoutputfh "$1\n";
	}
    }
}

#@[June 30, 2025] > Here '$OUTPUTFILE' is a directory to store the individual file lists
if( $writetofile==2 ){
    my $outfilename;
    my $MAXEVENTS = 250000; #Seperate jobs by max number of events (100k too small), 1000 events takes about a minute
    my $nfilesperjob = 0;
    my $avg_nfilesperjob = 0;
    my $sum_avg_nfilesperjob = 0;
    $OUTPUTFILE = abs_path($OUTPUTFILE);
    my $ncount = 0;
    foreach my $key (@SortedFileKeys){
	#    if( $ncount > 10 ){ last; }
	#if( !(23052047<=int($key) && int($key)<=23052062) ){ next; } #This is fill 33115 
	++$ncount;
	my $filecounter = 0; # Each run number has its own file counter
	$outfilename = "${OUTPUTFILE}/Files_${key}_${filecounter}.list";
	if( $VERBOSE>=1 ){ print "$outfilename\n"; }
	if( $outputfh ){ close $outputfh; }
	open( $outputfh, '>', $outfilename ) or die "Could not open file '$outfilename' for writing: $!";
	my @files = split( ',',  $SortedFiles{$key} );
	my $sum_nfilesperjob = 0;
	my $nfilesperjob = 0;
	my $eventcounter = 0;       #Number of events counter for the file list
	foreach my $file (@files){
	    if( $file =~ m/\/\w*_(2[23]\d{6})_\w*_(\d{7}).MuDst.root\/?(\d*)\/?(\d*)/ ){
		my $runnum  = $1;      #STAR RunNumber
		my $segment = $2;      #Segment number for a given STAR file with a given RunNumber
		my $nevents = $3;      #Number of events in file
		my $size    = $4;      #Size of file in bytes
		if( $eventcounter >= $MAXEVENTS ){
		    #if( $runnum==23108012 ){
		    #print "====================\n";
		    #print "$SortedFiles{$key}\n";
		    #print "====================\n";
		    #}
		    $eventcounter = 0;
		    $sum_nfilesperjob += $nfilesperjob;
		    $nfilesperjob = 0;
		    ++$filecounter;
		    $outfilename = "${OUTPUTFILE}/Files_${key}_${filecounter}.list";
		    if( $VERBOSE>=1 ){ print "$outfilename\n"; }
		    open( $outputfh, '>', $outfilename ) or die "Could not open file '$outfilename' for writing: $!";
		}
		#@[Feb 10, 2025] > For some reason second file for this run number had no files, the bug is that when the number of events exceeds the limit on the last file a new file is still created. Also run log shows 486.7 K events but my file list is giving 700 K events and I'm only missing one (maybe two) files. Also, add check for duplicate files. Changed it so #eventcounter is being reset on every new file list
		$eventcounter += $nevents;
		#if( $runnum==23108012 ){ print "|seg:${segment}|nevt:${nevents}|totevt:${eventcounter}\n"; }
		print $outputfh "$file\n";
		++$nfilesperjob;
	    }
	}
	$sum_nfilesperjob += $nfilesperjob;
	$avg_nfilesperjob = $sum_nfilesperjob/($filecounter+1);
	$sum_avg_nfilesperjob += $avg_nfilesperjob;
	#print "$key average number of files in job: ${sum_nfilesperjob}/",${filecounter}+1," = $avg_nfilesperjob\n";
    }

    if( $outputfh ){ close $outputfh; }
    my $avg_avg_nfilesperjob = $sum_avg_nfilesperjob/($ncount+1);
    print "Average number of the average number of files per job: $sum_avg_nfilesperjob/", $ncount+1, " = $avg_avg_nfilesperjob\n";
}

if( $outputfh ){ close $outputfh; }

print "Total Files Processed: $nlines\n";
print "Total Unique Runnumbers: ", scalar(keys %SortedFiles), "\n";
print "Total Files After Filtering: ",scalar(@SortedMaxFiles),"\n";
print "Number of Missing Runnumbers: ",scalar(@MissingRunnum),"\n";
foreach my $missing (@MissingRunnum){
    print " + $missing=$SortedFiles{$missing}\n";
}
print "\n";
