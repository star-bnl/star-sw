#!/opt/star/bin/perl -w

# Written by J.Lauret on Tue Mar 27 2001
# This script takes care of Insure++ compilation and output 
# formatting in HTML format. It will later be extended. See history ...
# use -h for help.
#
# Note : This script is also menat to be usable by users so NO default
#        file names with location in specific area should be hardcoded.
#        use options to overwrite default instead.
# 
# History :
#   Creation of an empty file if insure does not report any problems.
#   This will lead to an empty HTML file formatting instead of abort.
#
use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use ABUtils;



# List of sub-systems to skip
@SKIP=IUExcluded();


# Name of the Insure++ output file. If exists, will be parsed, code leading
# to errors will find their object file removed, then this output will be
# deleted for a new fresh one. Please read further for applicability rule.
$FILOUT="insure.txt";

# At the end, an HTML file will be produced. This is its name
#$FLNM="InsureComp.html";
$FLNM="/afs/rhic.bnl.gov/star/doc/www/comp/prod/Sanity/InsureComp.html";

# To format in HTML, this script will be used. Expected arguments are
# RawInputFile OutputFile
$FRMTPRGM="/afs/rhic.bnl.gov/star/packages/scripts/inssort.pl";

# Loading insure environment requires this command. If does not exists,
# this script will fail.
$INSCMD="setup INSURE";

# Compiling will use this command.
$CMPLCMD="cons -k INSURE=yes";

# For the obj file to be deleted, we need to know :
# The directory pattern where they are stored
$DIRPAT="IOBJ";
# the object file extension (may be sustem/make dependant)
$FIND="/usr/bin/find";
$GREP="/bin/grep";
$OBJ=".o";

# Not used for now
$SILENT=1==0;
$COMPIL=1==0;

# Quick argument parsing (dirty)
for($i=0 ; $i <= $#ARGV ; $i++){
    $arg = $ARGV[$i];
    if( substr($arg,0,1) eq "-"){
	# yeap. We consider this as an argument.
	# I know we can use this package but why ?
	if($arg eq "-x"){
	    # Exclude this
	    push(@SKIP,$ARGV[++$i]);
	} elsif ($arg eq "-i"){
	    # Overwrite input
	    $FILOUT = $ARGV[++$i];
	} elsif ($arg eq "-o"){
	    # Overwrite output
	    $FLNM   = $ARGV[++$i];
	} elsif ($arg eq "-c"){
	    $COMPIL=1==1;
	} elsif ($arg eq "-h" || $arg eq "--help"){
	    &lhelp();
	    exit;
	} elsif ($arg eq "-s"){
	    $SILENT=1==1;
	} else {
	    print "Unkown argument $arg\n";
	}
    } else {
	&lhelp();
	die "Check syntax.\n";
    }
}


# No more variables needed. From now on, we will assume that the user is in
# the proper directory and we will NOT assume any platform. We will assume
# that the logical/env variable SKIP_DIRS rule applies.
# Now, we will check that the above are all OK configuration.
if( ! -e $FRMTPRGM){ 
    print 
	"Warn : $FRMTPRGM script does not exists !!\n",
	"         We will not be able to produce $FLNM\n";
}
$cmd = (split(" ",$INSCMD))[0];
if( `which $cmd` =~ m/Command\snot\sfound/i){
    die 
	"Error: $INSCMD is unknown or not setup.\n",
	"       Correct this and start again. Hint : your OS is $^O\n";
}
$cmd = (split(" ",$CMPLCMD))[0];
if( `which $cmd` =~ m/Command\snot\sfound/i){
    die 
	"Error: $CMPLCMD is unknown or not setup.\n",
	"       Correct this and start again. Hint : your OS is $^O\n";
}


# Now we start
chomp($dir = `pwd`);



$file= join(" ",@SKIP);
print 
    "\n",
    " The output file will be         : $FILOUT\n",
    " The final html file will be     : $FLNM\n",
    " Object files will be post-fixed : $OBJ\n",
    " Object file will be searched in : ...$DIRPAT...\n",
    " Compilation will use            : $CMPLCMD\n",
    " The setup will be done using    : $INSCMD\n\n",
    " Those module will be skipped    : $file\n",
    " You are now in directory        : $dir\n\n";

# Manage lock file
IULockPrepare($dir,"$INSCMD $CMPLCMD $dir");
if ( ! IULockCheck(129600) ){ exit;}
IULockWrite("Insure++ compilation has started");


if(! $SILENT){ 
    print " Is this correct y/[n] ";
    chomp($ans = <STDIN>);
    if($ans eq ""){ $ans = "n";} 
} else {
    $ans = "y";
}

if($ans !~ /^\s*y/i){ 
    &Die("Hum... OK. Aborting then ...\n");
} 




# Check if old file is present. Read it if so ...
# We will actually build a list of obj files in a file referenced
# as ~/.insbld for faster processing since searching the tree may
# be lengthy.
$HOME = $ENV{HOME};
if( -e "$HOME/.insbld"){
    if( open(FI,"$HOME/.insbld") ){
	print " - Reading $HOME/.insbld\n";
	while( defined($line = <FI>) ){
	    chomp($line);
	    $line =~ m/(.*)\s(.*)/;
	    $PATH{$1} = $2;
	}
	close(FI);
    } else {
	print " * Access to $HOME/.insbld has failed\n";
    }
}

if( -e $FILOUT){
    print " - Old report $FILOUT exists. Reading it (may take a while).\n";
    open(FI,$FILOUT) || &Die("Could not open $FLNM as read.\n");
    while( defined($line = <FI>) ){
	chomp($line);
	if ($line =~ m/(\[.*\]\s)(.*)/ ){
	    # That's a code name. May be unique or not but we don't care
	    # since we are using associative array
	    $tmp = $1; 
	    if ( $tmp =~ m/\[(.*)(\..*):\d*\]/ ){
		$obj = $1.$OBJ;
		if( ! defined($PATH{$obj}) ){
		    print " + Searching for $obj --> ";
		    chomp($res = `$FIND . -name $obj | $GREP $DIRPAT`);

		    $res =~ s/$dir//;
		    if($res eq ""){
			print " *** Not found ***\n";
		    } else {
			$PATH{$obj} = $res; 
			print "$res\n";
		    }
		} else {
		    $res = $PATH{$obj};
		    if( ! defined($DELETE{$res}) ){
			$res = $PATH{$obj};
			print " + $obj is in $res\n";
		    }
		}
		#print "DEBUG :: [$res] ";
		$DELETE{$res} = $obj;
	    } else {
		print "There was no match for [$tmp] extracted from [$line]\n";
	    }
	}
    }
    close(FI);
}

if (open(FO,">$HOME/.insbld")){
    print " - Saving list in $HOME/.insbld\n";
    foreach $line (sort keys %PATH){
	print FO "$line $PATH{$line}\n";
    }
    close(FO);
} else {
    print " * Problem accessing $HOME/.insbld as write\n";
}

if( -e $FILOUT){
    print " - Deleting old report ";
    if( unlink($FILOUT) ){
	print " OK.\n";
    } else {
	print " ***> FAILED\n";
    }
}

print " - We will now delete the old objects to ensure full rebuild\n";
foreach $line (keys %DELETE){
    if( -e $line ){
	print " + deleting $line";
	if( unlink($line) ){
	    print " OK\n";
	} else {
	    print " ***> FAILED\n";
	}
    } else {
	print " + $line already deleted\n";
    }
}

# Swell. We can start now. We will do that by using system and
# exception test.
if($COMPIL){
    IUresource("$dir/$FILOUT"," - .psrc file created");

    $tmp = "/tmp/InsureComp".time().".com";
    open(FO,">$tmp") || 
	&Die("Could not open file for write in /tmp\n");
    print FO 
	"#\!/bin/csh\n",
	"cd $dir\n",
	"$INSCMD\n",
	"setenv SKIP_DIRS \"$file\"\n",
	"$CMPLCMD\n";
    close(FO);
    chmod(0770,$tmp);
    print " - We will now start the compilation. Excluded = $file\n";

    IULockWrite("Executing $tmp");
    $rc = 0xffff & system($tmp);
    if($rc != 0){
	print " * Failure. Returned status is $rc\n";
    } 

    # The above formatting may fail but we decided to proceed
    # anyway and attempt it.
    if( ! -e "$dir/$FILOUT"){
	# create a dummy file
	open(FO,">$FILOUT") || &Die("Could not create empty file\n");
	close(FO);
    }
    print " - Formatting now ...\n"; 
    # delay was added due to not-that-rare AFS fluke
    # making the -e test fail somehow. May be fixed later
    system("$FRMTPRGM $dir/$FILOUT $FLNM ; sleep 5");
    if( -e "$FLNM"){
	print " - All done. $FLNM is ready\n";
    } else {
	print 
	    " * Problem : Previous action did not create $FLNM\n",
	    " * Run $FRMTPRGM $dir/$FILOUT $FLNM by hand\n";
    }

} else {
    print " - Compilation skipped. Used '-c' to compile.\n";
}
IULockDelete();


# --- subs ---

sub Die
{
    my($msgs)=@_;
    IULockDelete();
    die $msgs;
}

sub lhelp
{
    my($excl)=join(" ",@SKIP);
    print qq~

 Usage is : $0 [options...]
 Where options is one of
  -x ABC      exclude ABC from list. Default = $excl
  -i xxx      insure output file xxx. Default is $FILOUT
  -o xxx      HTML output result to xxx. Default is $FLNM
  -c          compile. Currently = $COMPIL
  -s          silent mode (do not ask for confirmation)
              Currently = $SILENT 
  
  -h or --help  
              Display this help.

		  ~;
    print "\n";
}
