#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use Getopt::Long qw(HelpMessage);
use Cwd qw(abs_path);
use Pod::Usage;

=pod

=head1 NAME

Merge - Handy script for merging ROOT files from condor jobs generated from *MakeJob.pl*

=head1 SYNOPSIS

Merge.pl [option] [value] ...

  --dir1, -d, -d1    (requried) First directory to compare
  --dir2, -d2        (required) Second directory to compare
  --filter, -f       Name of file to match (search pattern will be filter_\d{8}_(?:raw_)?\d+.root)
  --nfiles, -n       Number of files to merge at once with hadd (no-op with -r)
  --outname, -o      Name of final output which will be [outname].root (default is "All")
  --runnum, -r       Flag to indicate only files with matching runnumber should be merged; no overall merge performed
  --copydir, -c      Specify a folder to copy to before the merge
  --test, -t         Test only checks directories and arguments; does not merge. Use with verbose to get a listing
  --verbose, -v      Set the printout level (default is 1)
  --help, -h         Print this help

=head1 VERSION

1.2

=head1 PURPOSE

Merge output ROOT output files from running *MakeJob.pl* with the *runMuDst.C* in this folder. It can also be used to merge files in another directory that ran the same job with a different set of makers for comparison purposes.

=head2 DESCRIPTION

This program will merge the output ROOT files from one or two different jobs created from *MakeJob.pl* that ran *runMuDst.C* in this folder that. Single merging is useful because it allows to combine the multiple output files from a single job into a single output file for analysis. Double merging is useful becuase it allows to compare two jobs that ran a different analysis but had an unequal aomount of output files. The double merge will only merge those files from both jobs that were successful. The purpose for double merging is to synchorize the events that were analyzed so that we are comparing the same events. This was neccessary at first for comparing the different algorithms from *StFcsWaveformFitMaker*. Since not all jobs will complete normally it is needed to sync which files were processed and which were not. It assumes all the processed data is from the same run number. Directories given must be where the job output the ROOT files.

To use specify the job output folder where the ROOT files are located with the -d option. Specify what file names to look for with the -f option. Specify any other options as needed which can be checked with -h.

If you want to merge only those files with matching run numbers use option --runnum(-r) 

=cut

=begin comment
LOG
@[November 14, 2022]
> First instance

@[January 18, 2023]
> Added ability to merge a single directory

@[July 1, 2024]
> Got rid of specific use cases where search string has a particular value and now does a slightly more generic search. This was done by adding an option called "filter" which will be used in place of the specific search string to process files with any kind of prefix in its name. Also added option to specify how many files to merge at once. Also got rid of specific names in temp merge files and now merely calls them "Merge".

@[July 25, 2024]
> Fix for #Filter which wasn't working correctly because perl thought '_' was part of variable name so added '{}'

@[July 30, 2024]
> Added a command line option #RUNNUM (-r) which is a flag to indicate only merge files with matching run numbers. Only merges single directories then stops.

@[January 31, 2025]
> Since processing *multimudst* mode jobs gives "iteration" numbers not equal to 7, changed regex search to look for any number of digits.

@[March 20, 2025]
> Created a sortable hash by making the key include the iteration number as a decimal point. Added a check for small files and builds a separate list. It will loop over this small list and ask if you want to remove these small files. You can skip this removal by setting '$NOSKIP' to false. This is not a command line option so needs to be modified in the code or use option "s" when prompted to skip removal small files and merge everything.

@[August 11, 2025] > Added a hack to be able to merge root files with only the runnumber

@[May 30, 2026] > Added an additional argument (copydir|c) that can be used to handle specifying a copy directory before merging. This could help in those instances where the pwg disk is working slow. A copy should still be relativiely fast compared to running **hadd** directly on the disk

@[June 1, 2026] > After some testing it seems like copying to SCRATCH disk and then running merge does not significantly impact the merge speed. Also, using hadd option -n did not really improve the speed the much either. I should still implement it just in case the pwg disk experiences slow downs?
=cut

my $DEBUG = 0;                           #Debug mode for printing only
#my $LOC = $ENV{'PWD'};
#my $DIR1 = "$LOC/DB414CE/Output";
#my $DIR2 = "$LOC/B8ACDD1/Output";

#my $DIR1 = "$LOC/028EB68/Output";
#my $DIR2 = "$LOC/124FDD3/Output";

#my $DIR1 = "$ENV{'HOME'}/pwg_disk/TestNewWff/DB414CE/Output";$ENV{'HOME'}/pwg_disk/TestNewWff/DB414CE/Output";
#my $DIR2 = "$ENV{'HOME'}/pwg_disk/TestNewWff/35E5751/Output";$ENV{'HOME'}/pwg_disk/TestNewWff/35E5751/Output";

#Here is the setup to check if the submit option was given
#my $LOC = $ENV{'PWD'};
my $DIR1;          #Main/First directory to merge
my $DIR2;          #Second directory to merge
my $FILTER = "";   #Name of input file (assumes files end in _runnumber_iteration.root)
my $NAME = "All";  #Name of final output file
my $TEST;          #Boolean test mode to check what program will do without doing it
my $NFILES = 50;   #Number of files to merge at once
my $RUNNUM = 0;    #Option to merge files with same runnumber together. Only merges same runnumbers does not do overall merge
my $COPYDIR = "";  #Location to copy files before merging. Argument will always be a subdirectory of SCRATCH. May help speed things up when the pwg disk is wokring slow
my $VERBOSE = 1;

GetOptions(
    'dir1|d|d1=s' => \$DIR1,
    'dir2|d2=s'   => \$DIR2,
    'filter|f=s'  => \$FILTER,
    'nfiles|n=i'  => \$NFILES,
    'outname|o=s' => \$NAME,
    'runnum|r'    => \$RUNNUM,
    'copydir|c=s' => \$COPYDIR,
    'verbose|v=i' => \$VERBOSE,
    'test|t'      => \$TEST,
    'help|h'      => sub { HelpMessage(0) }
    ) or HelpMessage(1);

if( $NAME =~ m/Merge\d+/ ){ print "Error:outname cannot be 'Merge' followed by digits\n"; HelpMessage(0); }
if( $FILTER eq "" ){
    print "WARNING:No filter specified on file name\n";
    print "Please Specify Filter (Q/q to quit)?:";
    my $input = <STDIN>;
    chomp $input;
    if( $input eq "Q" || $input eq "q" || $input eq "" ){ print "Quitting\n"; HelpMessage(0); }
    else{ $FILTER = $input; }
}

if( ! $DIR1 ){ print "Error:No directory 1 given\n"; HelpMessage(0); }
my $char = chop $DIR1; #Get last character of DIR1
while( $char eq "/" ){$char = chop $DIR1;} #Remove all '/'
$DIR1 = $DIR1.$char;     #Append removed character which was not a '/'
$DIR1 = abs_path($DIR1); #Get absolute path
if( ! -d $DIR1 ){ die "'$DIR1' is not a directory '$!'"; }
if( $VERBOSE>=2 ){ print "Dir1:$DIR1\n"; }

my $numfiles = 0;
my $mergefiles1 = "";
my $finalmergefiles1 = "";
my %RunnumMerge;

if( ! $DIR2 ){ print "Warning:No directory 2 given merging only $DIR1\n"; }
else{
    $char = chop $DIR2; #Get last character of DIR2
    while( $char eq "/" ){$char = chop $DIR2;} #Remove all '/'
    $DIR2 = $DIR2.$char;     #Append removed character which was not a '/'
    $DIR2 = abs_path($DIR2); #Get absolute path
    if( ! -d $DIR2 ){ die "'$DIR2' is not a directory '$!'"; }
    if( $VERBOSE>=2 ){ print "Dir2:$DIR2\n"; }
}

if( $COPYDIR ne "" ){
    if( $VERBOSE>2 ){ print "Copy folder to be created in \$SCRATCH: $COPYDIR\n"; }
    $COPYDIR = $ENV{'SCRATCH'} . "/". $COPYDIR;
    $char = chop $COPYDIR; #Get last character of COPYDIR
    while( $char eq "/" ){$char = chop $COPYDIR;} #Remove all '/'
    $COPYDIR = $COPYDIR.$char;     #Append removed character which was not a '/'
    if( ! -d $COPYDIR ){
	print "'$COPYDIR' does not exist or is not a directory\n";
	print "Create (Y/n):";
	my $input = <STDIN>;
	chomp $input;
	if( $input eq "Y" ){
	    if( $VERBOSE>1 ){ print "Creating $COPYDIR\n"; }
	    system("/bin/mkdir -p $COPYDIR")==0 or die "Unable to create '$COPYDIR': $!";
	    $COPYDIR = abs_path($COPYDIR); #Get absolute path
	    if( $VERBOSE>2 ){ print "Created: $COPYDIR\n"; }
	}
	else{
	    print "Skipping copy\n";
	    $COPYDIR="";   # Set to empty/false so below it is not used
	}
    }
    else{
	print "'$COPYDIR' exists!\nIt is recomended to delete its contents before merging\n";
	print "Continue with merge? (Y)es/(q)uit/[s]kip:";
	my $input = <STDIN>;
	chomp $input;
	if( $input eq "Y" ){ print "Continuing without deletion\n"; }
	elsif( $input eq "Q" || $input eq "q" ){ print "Quitting\n"; HelpMessage(0); }
	else{ print "Skipping copy\n"; $COPYDIR=""; }
    }
}


my %Compare;

opendir my $dh1, $DIR1 or die "Could not open '$DIR1' for reading '$!'";

#if( $TEST && ! $DIR2 ){ exit(0); }

if( $VERBOSE>=1 ){ print "Reading $DIR1\n"; }
my %SmallFiles;   #Hash to keep track of files with small sizes and skip over them
while( my $mudstfile = readdir $dh1 ){
    #@[July 30, 2024] > The (?:raw_) is optional group matching syntax, may need testing. Discovered when asking Google's Gemini how to match exact strings
    #if( $mudstfile =~ m/${FILTER}_(\d{8})_(?:raw_)?(\d+).root/ ){
    if( $mudstfile =~ m/${FILTER}_(\d{8})_?(?:raw_)?(\d+)?.root/ ){ #@[August 11, 2025] > Hack to be able to merge root files with only the runnumber set
	my $runnum = $1;
	my $iternum = $2;
	if( ! defined $iternum ){ $iternum = 0; } #@[August 11, 2025] > Hack to be able to merge root files with only the runnumber set
	my $fullpath = "$DIR1/$mudstfile";
	my $filesize = -s $fullpath;
	if( $VERBOSE>=3 ){print " - |runnum:$runnum|iternum:$iternum|$mudstfile\n"; }
	#my $iternum = $mudstfile;
	#print "B: $iternum\n";
	#$iternum =~ s/${FILTER}_//;
	#$iternum =~ s/.root//;
	#print "A: $iternum\n";
	if( $filesize<1000 ){
	    my $key = $runnum . "." . $iternum; #By treating the "iteration" number as a decimal point I can sort it numerically
	    $SmallFiles{$key} = $mudstfile;
	    next;
	}
	$Compare{$iternum} = 1; #1 (true) since it is in DIR1
	if( ! $DIR2 ){
	    if( $RUNNUM ){
		if( ! $RunnumMerge{$runnum} ){ $RunnumMerge{$runnum} = "$DIR1/$mudstfile"; $numfiles++; }
		else{ $RunnumMerge{$runnum} = $RunnumMerge{$runnum} . " $DIR1/$mudstfile"; $numfiles++; }
	    }
	    else{
		$mergefiles1 = $mergefiles1." $DIR1/$mudstfile";
		$numfiles++;
		if( $numfiles % $NFILES==0 ){
		    #print "hadd StFcsPi0invariantmass$numfiles.root$mergefiles\n";
		    #print "$numfiles\n";
		    $finalmergefiles1 = $finalmergefiles1 . " $DIR1/Merge$numfiles.root";
		    if( !$TEST ){ system( "hadd -f $DIR1/Merge$numfiles.root$mergefiles1" ); }# or die "Unable to hadd: $!";
		    $mergefiles1 = "";
		}
	    }
	}
    }
}

closedir $dh1;

if( $RUNNUM ){
    my $NOSKIP = 1;
    my @SortedSmall = sort { $a <=> $b } keys(%SmallFiles);
    foreach my $smallfile_rn (@SortedSmall){
	#print "FileTooSmallSkip|runnum:$smallfile_rn\n";
	my $fullpath = "$DIR1/$SmallFiles{$smallfile_rn}";
	my $filesize = -s $fullpath;
	print "FileTooSmallSkip|runnum:$smallfile_rn|size:$filesize|$SmallFiles{$smallfile_rn}\n";
	if( $NOSKIP ){
	    print "\tRemove file (Can't be undone) (Y)es/(n)o/(s)kip all/(q)uit: ";
	    my $input = <STDIN>; chomp($input);
	    if( $input eq "Y" ){
		if( $VERBOSE>=1 ){ print "\tRemoving file: $fullpath\n"; }
		system("/usr/bin/rm $fullpath");
	    }
	    if( $input eq "Q" || $input eq "q" ){ exit(0); }
	    if( $input eq "S" || $input eq "s" ){ $NOSKIP = 0; }
	}
    }
    my $nruns = 0;
    my @SortedRunnumMerge = sort { $a <=> $b } keys(%RunnumMerge);
    for my $runnum ( @SortedRunnumMerge ){
	if( ($TEST && $VERBOSE>=1) || $VERBOSE>=2 ){
	    print "Merging all runs with run number:$runnum\n";
	}
	my @runnumfiles = split( / /, $RunnumMerge{$runnum} );
	my $nummergefiles1 = 0;
	if( ($TEST && $VERBOSE>=1) || $VERBOSE>=2 ){
	    print( "Total files for run $runnum = ", scalar @runnumfiles, "\n");
	    foreach my $file ( @runnumfiles ){
		$nummergefiles1++;
		print( " ${nummergefiles1}. $file\n" );
	    }
	}
	if( !$TEST ){
	    my $mergefiles1 = "";
	    my $finalmergefiles1 = "";
	    $nummergefiles1 = 0;
	    foreach my $file (@runnumfiles){
		$mergefiles1 = $mergefiles1." $file";
		$nummergefiles1++;
		if( $nummergefiles1 % $NFILES==0 ){
		    $finalmergefiles1 = $finalmergefiles1 . " $DIR1/TempMerge_${runnum}_$nummergefiles1.root";
 		    if( $VERBOSE>=3 ){ print "hadd -f $DIR1/TempMerge_${runnum}_$nummergefiles1.root $mergefiles1\n"; }
		    system( "hadd -f $DIR1/TempMerge_${runnum}_$nummergefiles1.root $mergefiles1" ); #  or die "Unable to hadd: $!";
		    $mergefiles1 = "";
		}
	    }
	    if( $nummergefiles1 % $NFILES != 0 ){
		$finalmergefiles1 = $finalmergefiles1 . " $DIR1/TempMerge_${runnum}_$nummergefiles1.root";
		if( $VERBOSE>=3 ){ print "hadd -f $DIR1/TempMerge_${runnum}_$nummergefiles1.root $mergefiles1\n"; }
		system( "hadd -f ${DIR1}/TempMerge_${runnum}_${nummergefiles1}.root $mergefiles1" );#  or die "Unable to hadd: $!";
	    }
	    if( $VERBOSE>=3 ){ print "hadd ${NAME}_$runnum.root $finalmergefiles1\n";
			       print "Continue (y/n):";
			       my $input = <STDIN>;
			       chomp $input;
			       if( $input ne "y" ){ exit(0); }
	    }
	    system( "hadd $DIR1/../${NAME}_$runnum.root $finalmergefiles1" );#  or die "Unable to hadd: $!";
	}
    }
    print "Total files processed: $numfiles\n";
    exit(0);
}

if( ! $DIR2 ){
    if( $numfiles % $NFILES != 0 ){
	#print "hadd StFcsPi0invariantmass$numfiles.root$mergefiles\n";
	$finalmergefiles1 = $finalmergefiles1 . " $DIR1/Merge$numfiles.root";
	if( !$TEST ){ system( "hadd -f $DIR1/Merge$numfiles.root$mergefiles1" ); }# or die "Unable to hadd: $!";
    }
    
    if( !$TEST ){ system( "hadd -f $DIR1/../${NAME}.root${finalmergefiles1}" ); }
    print "Merged $numfiles files in $DIR1 to ../${NAME}.root\n";
    exit(0);
}

if( $DIR2 ){
    opendir my $dh2, $DIR2 or die "Could not open '$DIR2' for reading '$!'";
    if( $VERBOSE>=1 ){ print "Reading $DIR2\n"; }
    while( my $mudstfile = readdir $dh2 ){
	if( $mudstfile =~ m/${FILTER}_(\d{8})_raw_(\d+).root/ ){
	    my $runnum = $1;
	    my $iternum = $2;
	    #my $iternum = $mudstfile;
	    #print "B: $iternum\n";
	    #$iternum =~ s/${FILTER}_//;
	    #$iternum =~ s/.root//;
	    #print "A: $iternum\n";
	    if( exists $Compare{$iternum} ){
		$Compare{$iternum} = 0; #0 (false) if in both
		if( $VERBOSE>=3 ){ print "$iternum exists in both directories\n"; }
	    }
	    else{
		$Compare{$iternum} = 2;  #2 (true) since it is in DIR2
		if( $VERBOSE>=2 ){print "'$iternum' is missing from dir1:$DIR1\n";}
	    }
	}
    }
    closedir $dh2;
}

if( $TEST ){ exit(0); }

if( $VERBOSE>=1 ){ print "Merging files in '$DIR1' and '$DIR2'\n"; }
$numfiles = 0;
my $mergefiles2 = "";
my $finalmergefiles2 = "";
foreach my $key (keys %Compare){
    #if( $Compare{$key} ){ print "$key:Dir$Compare{$key}\n"; }
    if( ! $Compare{$key} ){
	$mergefiles1 = $mergefiles1." $DIR1/${FILTER}_$key.root";
	$mergefiles2 = $mergefiles2." $DIR2/${FILTER}_$key.root";
	$numfiles++;
    }
    if( $numfiles % $NFILES==0 ){
	#print "hadd StFcsPi0invariantmass$numfiles.root$mergefiles\n";
	#print "$numfiles\n";
	$finalmergefiles1 = $finalmergefiles1 . " $DIR1/../Merge$numfiles.root";
	$finalmergefiles2 = $finalmergefiles2 . " $DIR2/../Merge$numfiles.root";
	if( !$TEST ){ system( "hadd -f $DIR1/../Merge$numfiles.root$mergefiles1" ); }# or die "Unable to hadd: $!";
	if( !$TEST ){ system( "hadd -f $DIR2/../Merge$numfiles.root$mergefiles2" ); }# or die "Unable to hadd: $!";
	$mergefiles1 = "";
	$mergefiles2 = "";
    }
    #if( $numfiles==101 ){last;}
}

if( $numfiles % $NFILES != 0 ){
    #print "hadd StFcsPi0invariantmass$numfiles.root$mergefiles\n";
    $finalmergefiles1 = $finalmergefiles1 . " $DIR1/../Merge$numfiles.root";
    $finalmergefiles2 = $finalmergefiles2 . " $DIR2/../Merge$numfiles.root";
    if( !$TEST ){ system( "hadd -f $DIR1/../Merge$numfiles.root$mergefiles1" ); }# or die "Unable to hadd: $!";
    if( !$TEST ){ system( "hadd -f $DIR2/../Merge$numfiles.root$mergefiles2" ); }# or die "Unable to hadd: $!";
}

if( !$TEST ){ system( "hadd -f $DIR1/../${NAME}.root${finalmergefiles1}" ); }
if( !$TEST ){ system( "hadd -f $DIR2/../${NAME}.root${finalmergefiles2}" ); }

print "Total files processed: $numfiles\n";

