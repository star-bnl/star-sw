#!/usr/local/bin/perl -w

#
# This script was developped to provide a quick and easy 
# test running interface. Sub-directories will be auto-created
# according to test chain. 
# For Insure, there is no real need to run multiple times the 
# same chain since the intent is NOT to test the physics results 
# but the code sanity ...
#
# Written J.Lauret Apr  3 2001. 
# History
#  Apr 13 2001 ; JL added real events chain.
#  Feb  1 2002 ; JL added profiling support
#
# Note that the curent version does everything within one
# script so we are only a few steps away from being able 
# to submit the tests in batch ...
#
# -prof   do profiling tests
# -valg   do valgrind check
# -inter  run interrcatively (the default)
# -batch  use batch
#
use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use ABUtils;



# All tests may be declared here in this array. The first element is the chain
# the seconde a list of files to work on. Note that the array will be later
# sorted so no need to try to put the chain in a different order, hoping
# this script will do something different.
%TESTS=IUTests();
$SRCDIR=IUTestDir();
$DESTDIR=IUHtmlPub();

# Number of events ton run in mode
$NEVT[0]= 2;    # Insure, valgrind mode
$NEVT[1]=10;    # Jprofile

# Beware that non-interactive would mean having
# a token as per being able to copy the results
# back to target.
$INTER = 0;


# ------ No Changes below this line -------------------------------------
$PROF = 0;
$VALG = 0;

# Parse arguments if any
for($i=0 ; $i <= $#ARGV ; $i++){
    $arg = $ARGV[$i];
    if( substr($ARGV[$i],0,1) eq "-"){
	# consider it an option
	if ($arg eq "-batch"){
	    $INTER = 0;
	} elsif ($arg eq "-inter"){
	    $INTER = 1;
	} elsif ($arg eq "-d"){
	    # Delete all directories option. Maintainance
	    print "Are you sure you do delete all directories in $SRCDIR ? ";
	    chomp($ans = <STDIN>); if($ans eq ""){ $ans = "no";}
	    if($ans =~ m/y/i){
		chomp(@dirs = `cd $SRCDIR ; find . -type d`);
		foreach $el (@dirs){
		    if($el ne "."){
			print "Deleting $el\n";
			system("cd $SRCDIR ; rm -rf $el");
		    }
		}
	    }
	    exit;


	} elsif (lc($arg) eq "-prof"){
	    # Turn on profiling instead
	    $PROF = 1;

	} elsif (lc($arg) eq "-valg"){
	    # Turn on valgrind
	    $VALG = 1;
	} elsif ($arg eq "-1"){
	    # separate option ...
	    push(@ARG,$arg);
	    next;
	} else {
	    # an unkown - options
	    die "Don't know what to do with option [$arg]\n";
	}
    } else {
	push(@ARG,$arg);
    }
}
undef(@ARGV);


#
# Sort array now, transfer i into another associative array.
#
foreach $el (keys %TESTS){
    # chain argument may be post-fixed by a tag
    @items = split(";",$el);
    if ($#items > 0){
	$chst = pop(@items);
    } else {
	$chst = "";
    }
    $chain = $items[0];
    
    # chain argument need to be sorted
    @items = split(" ",$chain);
    @items = sort(@items);
    $chain = join(" ",@items);

    push(@CHAINS,$chain);
    push(@CHAINST,$chst);
    $STESTS{$chain} = $TESTS{$el};
}
undef(%TESTS);


#
# We are ready to start
#
if($#ARG == -1){
    print 
	"You may enter several tests separated by space or one\n",
	"per line. Press return to end input.\n";
    do {
	for($i=0 ; $i <= $#CHAINS ; $i++){
	    printf "%4d --> %s %s\n",$i,$CHAINS[$i],($CHAINST[$i] ne ""?"(".$CHAINST[$i].")":"");
	}
	print "Test number : ";
	chomp($choice = <STDIN>);
	if($choice ne ""){ push(@ARG,split(" ",$choice));}
    } while($choice ne "");
}


# test all choices
print "You chose test(s) [".join(" ",@ARG)."]\n";
if ($#ARG == -1){ exit;}

# If choice was -1, run all tests
if ($ARG[0] eq -1){     for ($i=0 ; $i <= $#CHAINS ; $i++){ $ARG[$i] = $i;}}

# Loop over choices
foreach $choice (@ARG){
    # Now we know
    $chain = $CHAINS[$choice];

    # trying to trick me ??
    if( ! defined($chain) ){    
	print "Illegal choice $choice ...\n";
	next;
    }

    # else, multiple files may be used. Several tests will follow
    print "\n*** $chain ***\n";

    if ($CHAINST[$choice] ne ""){
	$dir   = $CHAINST[$choice];
    } else {
	$dir   = $chain;
    }
    $dir   =~ s/[ -]/_/g;
    @files = split(" ",$STESTS{$chain});
    for($i=0 ; $i <= $#files ; $i++){
	$file = $files[$i];

	if(! -e $file){  
	    print "$file cannot be seen from ".`hostname`."\n";
	    next;
	} else {  
	    system("/bin/touch $file");
	}

        #
        # Prepare lock file for this session 
        #
	IULockPrepare($SRCDIR,"$INTER $PROF $VALG $chain");
	if ( ! IULockCheck(129600) ){ 
	    print "Skipping [$chain] on $file\n";
	    next;
	} else {
	    IULockWrite("$0 Interactive=$INTER Profiling=$PROF has started");
	    print "Doing [$chain] on $file\n";
	}

	# Ready to produce a running script
	# Create directory
	if( ! chdir($SRCDIR) ){ &Die("Cannot change directory to $SRCDIR\n");}
	
	if(! -d "$dir"){ 
	    print " - Directory $dir created\n";
	    mkdir($dir,0755); 
	}
	print " - Changing directory to $dir\n";
	if( ! chdir($dir) ){ &Die("Could not change directory to $dir\n");}


	# Pre-script creation tasks/open file
	if ($PROF){
	    $script = "Pscript$i.csh";
	} elsif ($VALG){
	    $script = "Vscript$i.csh";
	} else {
	    IUresource("$SRCDIR/$dir/insure$i.log"," - creating .psrc file");
	    $script = "Iscript$i.csh";
	}
	print " - Creating a script file\n";
	if( -e $script){ unlink($script);}
	open(FO,">$script") || &Die("Could not open file for write\n");


	# Generate the script now
	print FO 
	    "#\!/bin/csh\n",
	    "# Script created on ".localtime()."\n",
	    "# by $0. Written J.Lauret\n",
	    "source ~/.cshrc\n",
	    "unset noclobber\n";

	if($PROF){
	    print FO JPLoad()."\n" ;  # Command to load the Profiling env
	    $flnm = "Prof-$dir-$i.html";
	} elsif ($VALG){
	    print FO VLGLoad()."\n"; # there is nothing to do to load valgrind
	    $flnm = "Valg-$dir-$i.html";
	} else {
	    print FO IULoad()."\n" ;  # command to load the Insure env
	    $flnm = "Ins-$dir-$i.html";
	}

	print FO
	    "cd $SRCDIR/$dir\n",
	    "\n",
	    "# Display result\n",
	    IUCheckFile(3,"$DESTDIR/$flnm"),
	    "set ROOT4STAR=`which root4star`\n",
	    "echo \"Path   = \$PATH\"\n",
	    "echo \"LDPath = \$LD_LIBRARY_PATH\"\n",
	    "echo \"STAR   = \$STAR\"\n",
	    "echo \"root4* = \" \$ROOT4STAR\n",
	    "echo \"CDir   = \" `pwd`\n",
	    "\n",
	    "setenv StarEndMakerShell\n",
	    "rm -f *.root\n",
	    "\n";

	if($PROF){
	    print FO 
		"rm -f *.C\n",
		"echo 'gSystem->Setenv(\"JPROF_FLAGS\", \"JP_START JP_PERIOD=0.001\");' >tmp.C\n",
		"echo 'gSystem->Load(\"libJprof\");'      >>tmp.C\n",
		"echo '.x bfc.C($NEVT[1],\"$chain\",\"$file\");' >>tmp.C\n",
		"\$ROOT4STAR -b < tmp.C\n",
		"\n",
		IUCheckFile(0,"$SRCDIR/$flnm"),
		JPRFFormat()." \$ROOT4STAR $SRCDIR/$dir/jprof-log >$SRCDIR/$flnm\n";
	} elsif ($VALG){
	    print FO 
		"valgrind --tool=memcheck --leak-check=yes --error-limit=no \$ROOT4STAR -b -q 'bfc.C($NEVT[1],\"$chain\",\"$file\")' >&valg$i.log\n",
		"\n",
		IUCheckFile(0,"$SRCDIR/$flnm"),
		VLGFormat()." $SRCDIR/$dir/valg$i.log >$SRCDIR/$flnm\n";
	} else {
	    print FO 
		"\$ROOT4STAR -b -q 'bfc.C($NEVT[0],\"$chain\",\"$file\")'\n",
		"\n",
		IUCheckFile(0,"$SRCDIR/$flnm"),
		IURTFormat()." $SRCDIR/$dir/insure$i.log >$SRCDIR/$flnm\n";
	}
	print FO IUMoveFile(0,"$SRCDIR/$flnm","$DESTDIR/$flnm",10)."\n";
	close(FO);
	chmod(0770,"$SRCDIR/$dir/$script");

	if ($INTER){
	    print " - Running it now\n";
	    # we can also trapped the returned error
	    system("$SRCDIR/$dir/$script"); 
	} else {
	    print " - Submitting it now\n";
	    IUSubmit("$SRCDIR/$dir/$script",1);
	}
	IULockDelete();

    }
}



# --- Sub intercepting for IULockDelete()
sub Die
{
    my($msgs)=@_;
    IULockDelete();
    die $msgs;
    
}



