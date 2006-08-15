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
$NEVT[0]=2;    # Insure mode
$NEVT[1]=10;   # Jprofile


# ------ No Changes below this line -------------------------------------
$PROF = 0;

# Parse arguments if any
for($i=0 ; $i <= $#ARGV ; $i++){
    $arg = $ARGV[$i];
    if( substr($ARGV[$i],0,1) eq "-"){
	# consider it an option
	if ($arg eq "-d"){
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


	} elsif ($arg eq "-prof"){
	    # Turn on profiling instead
	    $PROF = 1;
	}
    } else {
	push(@ARG,$arg);
    }
}
undef(@ARGV);





# Sort array now, transfer i into another associative array.
foreach $el (keys %TESTS){
    @items = split(" ",$el);
    @items = sort(@items);
    $chain = join(" ",@items);
    push(@CHAINS,$chain);
    $STESTS{$chain} = $TESTS{$el};
}
undef(%TESTS);



if($#ARG == -1){
    print 
	"You may enter several tests separated by space or one\n",
	"per line. Press return to end input.\n";
    do {
	for($i=0 ; $i <= $#CHAINS ; $i++){
	    printf "%4d --> %s\n",$i,$CHAINS[$i];
	}
	print "Test number : ";
	chomp($choice = <STDIN>);
	if($choice ne ""){ push(@ARG,split(" ",$choice));}
    } while($choice ne "");
}


# test all choices
print "You chose test(s) [".join(" ",@ARG)."]\n";
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

    $dir   = $chain;
    $dir   =~ s/[ -]/_/g;
    @files = split(" ",$STESTS{$chain});
    for($i=0 ; $i <= $#files ; $i++){
	$file = $files[$i];
	if(! -e $file){  die "$file cannot be seen from ".`hostname`."\n";}
	print "Doing [$chain] on $file\n";
	# Ready to produce a running script
	# Create directory
	if( ! chdir($SRCDIR) ){ die "Cannot change directory to $SRCDIR\n";}
	
	if(! -d "$dir"){ 
	    print " - Directory $dir created\n";
	    mkdir($dir,0755); 
	}
	print " - Changing directory to $dir\n";
	if( ! chdir($dir) ){ die "Could not change directory to $dir\n";}


	# Pre-script creation tasks/open file
	if ($PROF){
	    $script = "Pscript$i.csh";
	} else {
	    IUresource("$SRCDIR/$dir/insure$i.log"," - creating .psrc file");
	    $script = "Iscript$i.csh";
	}
	print " - Creating a script file\n";
	if( -e $script){ unlink($script);}
	open(FO,">$script") || die "Could not open file for write\n";


	# Generate the script now
	print FO 
	    "#\!/bin/csh\n",
	    "# Script created on ".localtime()."\n",
	    "# by $0. Written J.Lauret\n",
	    "source ~/.cshrc\n";

	if($PROF){
	    print FO JPLoad()."\n" ;  # Command to load the Profiling env
	} else {
	    print FO IULoad()."\n" ;  # command to load the Insure env
	}

	print FO
	    "cd $SRCDIR/$dir\n",
	    "\n",
	    "# Display result\n",
	    "set ROOT4STAR=`which root4star`\n",
	    "echo \"Path   = \$PATH\"\n",
	    "echo \"LDPath = \$LD_LIBRARY_PATH\"\n",
	    "echo \"STAR   = \$STAR\"\n",
	    "echo \"root4* = \" \$ROOT4STAR\n",
	    "echo \"CDir   = \" `pwd`\n",
	    "\n",
	    "setenv StarEndMakerShell\n",
	    "unset noclobber\n",
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
		IUCheckFile(0,"$SRCDIR/Prof-$dir-$i.html"),
		JPRFFormat()." \$ROOT4STAR jprof-log >$SRCDIR/Prof-$dir-$i.html\n",
		IUMoveFile(0,"$SRCDIR/Prof-$dir-$i.html","$DESTDIR/Prof-$dir-$i.html",10);
	} else {
	    print FO 
		"\$ROOT4STAR -b -q 'bfc.C($NEVT[0],\"$chain\",\"$file\")'\n",
		"\n",
		IUCheckFile(0,"$SRCDIR/Ins-$dir-$i.html"),
		IURTFormat()." $SRCDIR/$dir/insure$i.log >$SRCDIR/Ins-$dir-$i.html\n",
		IUMoveFile(0,"$SRCDIR/Ins-$dir-$i.html","$DESTDIR/Ins-$dir-$i.html",10);
	}

	close(FO);
	chmod(0770,"$SRCDIR/$dir/$script");

	#print " - Running it now\n";
	#system("$SRCDIR/$dir/$script"); # we can also trapped the returned error
	print " - Submitting it now\n";
	IUSubmit("$SRCDIR/$dir/$script",1);

    }
}




