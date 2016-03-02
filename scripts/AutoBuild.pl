#!/usr/bin/env perl

# $Id: AutoBuild.pl,v 1.54 2014/10/29 15:48:52 jeromel Exp $
# This script was written to perform an automatic compilation
# with cvs co and write some html page related to it afterward.
# Written J.Lauret Apr 6 2001
# Need to extend it ? (some internal rules)
# - Menu reference is automatically build if a %%REF%% is found
#   the menu will appear on top of the report.
#
#
use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use ABUtils;

# Source, library, directory
$LIBRARY = "adev";                     # Default library version to work with
$TRGTDIR = IUHtmlDir();                # Dir where the reports will be at the end
$COMPDIR = "";                         # Compilation directory (later assigned)


# Cvs command and directory
#if ( -x "/usr/bin/cvs"){
#    $CVSCMD = "/usr/bin/cvs";
#} else {
    $CVSCMD = "cvs";
#}
$CVSCMDT = "$CVSCMD -n -q checkout";   # check differences and updates
$CVSCMDR = "$CVSCMD -q checkout";      # first timer directory or real checkout
$CVSUPDC = "$CVSCMD update -P";        # update command
$BY      = 50;                         # checkout by
@DIRS    = IUSourceDirs();             # default relative directories to checkout


# tasks to peform after compilation
$DFILE   = "RELEASE.date";
@POSTSKS = ("/bin/rm -f $DFILE",
	    "/bin/date >$DFILE",
	    );

# An associatative array for recovery of failures
# - First element (key) is a pattern triggering the recovery
#   It will be used in a m/$PAT/i context
# - Second element is an 'action' executed in the compile directory
#
# For now, we have only one case but there may be many.
#
%RECOVER = ("Disk quota exceeded",     "mgr/CleanLibs");

	    #"no newline at end of file",
	    #"mgr/CleanLibs && /usr/bin/find /tmp -type f -user \$USER -exec /bin/rm -f {} \\;");

#
# Patterns to exclude from reporting ...
# Please, only use this for auto-generated sources as those
# patterns will be matched globally from Execute().
#
@IGNUNKNOWN=("StarVMC\/StarGeometry","StarVMC\/xgeometry");


# this a counter for each recoverrable errors
foreach $re (keys %RECOVER){   $GRECOVER{$re} = 0;}
# we will attempt to recover each errors only MAXRECOVERY times
$MAXRECOVERY= 1;



# Compilation will be made with the following commands. If
# the associative array has for value 1, the command MUST
# succeed in order to continue. Otherwise, the return result
# is irrelevant ...
#
# There are some assumptions made :
# 0 - csh is used to execute the commands.
# 1 - the SKIP_DIRS rule will be respected
# 2 - $CHENV{A|B} command will be executed after/before library version change
# 3 - $CHVER command will be issued before compilation
# 4 - The compilation command MUST be the last item in a serie of "&&"
#     related commands. This assumption is used in recovery procedures
#
# BTW : the echo command is a stupid trick. The keys
# are returned in reverse order (??) somehow and I
# want them to be in this SAME exact order.
#
%COMPILC = (
    "echo 1 && %%CHENVB%% && unsetenv NODEBUG   && %%CHVER%% && %%CHENVA%% && cons ", 1,
    "echo 2 && %%CHENVB%% && setenv NODEBUG yes && %%CHVER%% && %%CHENVA%% && cons ", 1);
@SKIP    = IUExcluded();
$OPTCONS = "";



# --- Miscellaneous
# name of the HTML output report. ONLY the main name
# is expected here. Extension .html will be added.
$FLNM    = "AutoBuild";

# file to recover the STAR_HOST_SYS version
$RECSTARLVL= "/tmp/$FLNM.STAR_HOST_SYS.$$";

# HTML colors for warnings. Experimental
# Used to display error messages.
@COLORS  = ("#D3D3D3","#EEEEEE");

# keeps compilation ans post-tasks status.
@STATUS  = ();

# Global error string
$ERRSTR  = "";

# Post-fix
$POST    = "";



# Arguments taking. Also controled via command line options ;
# Check the help for a better view of what you can do since
# some of those variables are dependant on others.
$FIRSTPASS = 1==0;       # First pass compilation only
$SILENT    = 1==0;       # Ask for confirmation or not
$NIGNOR    = 1==1;       # Compile without update
$CVSUPD    = 1==0;       # Use cvs update
$CVSCOU    = 1==1;       # Use cvs check-out
$DEBUG     = 1==0;       # Debug mode (i.e. no post-tasks)
$TRASH     = 1==0;       # trash code cvs finds conflicting
$NOTIFY    = 1==1;       # notify managers if problems
$FILO      = STDOUT;     # Default Output file
$RELCODE   = 1==0;       # Default is not to release code
$RESETST   = 1==1;       # reset sticky tag i.e. use/append -A

# All arguments will be kept for checksum purposes
$ALLARGS   = "$^O";      # platform will be kept in for sure
$CHENVA    = $CHENVB = "echo noop";# possible additional environment change command


# display arguments for debugging
print $FILO "The arguments passed are [".join(" ",@ARGV)."]\n";

# Quick argument parsing (dirty)
for ($i=0 ; $i <= $#ARGV ; $i++){
    $arg      = $ARGV[$i];
    if( substr($arg,0,1) eq "-"){
	# yeap. We consider this as an argument.
	# I know we can use this package but why ?
	if($arg eq "-x"){
	    # Exclude this
	    push(@SKIP,$ARGV[++$i]);
	} elsif($arg eq "-f"){
	    undef(@SKIP);

	} elsif($arg eq "-a"){
	    $CHENVA   = $ARGV[++$i];
	    $ALLARGS .= " $CHENVA";
	} elsif($arg eq "-b"){
	    $CHENVB   = $ARGV[++$i];
	    $ALLARGS .= " $CHENVB";
	} elsif($arg eq "-B"){
	    # Grab it from ENV
	    $CHENVB   = $ENV{AutoBuild_setup_cmd};
	    if ( defined($CHENVB) ){
		$ALLARGS .= " $CHENVB";
	    } else {
		print $FILO "Warning -B used but AutoBuild_setup_cmd is not defined\n";
	    }

	} elsif($arg eq "-A"){
	    my(@LL) = sort keys %COMPILC;
	    my($LIGNE,$cmd);

	    $LIGNE    = $LL[0];

	    $cmd      = $ARGV[++$i];
	    print $FILO "Considering $cmd\n";
	    $LIGNE    =~ s/%%CHENVB%%/$cmd/;

	    $cmd      = "echo ".($#LL+1+1);
	    $LIGNE    =~ s/echo 1/$cmd/;
	    $COMPILC{$LIGNE} = 1;


	} elsif($arg eq "-o"){
	    $FILO= $ARGV[++$i];
	    ABUnlink($FILO);
	    if(open(FILO,">$FILO")){
		$FILO = FILO;
	    } else {
		$FILO = STDOUT;
	    }
	} elsif($arg eq "-t"){
	    # tag output file with OS name
	    $FLNM .= "-$^O" if($FLNM !~ $^O);
	    $POST .= "-$^O";
	} elsif($arg eq "-T"){
	    # tag output file with arbitrary tag
	    $FLNM .= "-".$ARGV[++$i];
	    $POST .= "-".$ARGV[$i];

	} elsif($arg eq "-1"){
	    # first compilation pass only
	    $ALLARGS .= " $arg";
	    $FIRSTPASS= 1==1;
	} elsif($arg eq "-2"){
	    # both compilations passes
	    $ALLARGS .= " $arg";
	    $FIRSTPASS= 1==0;

	} elsif($arg eq "-i"){
	    # interractive that is no cvs
	    # operations
	    $NIGNOR   = 1==0;
	    $SILENT   = 1==1;

	} elsif($arg eq "-u"){
	    $CVSUPD   = 1==1;
	    $CVSCOU   = 1==0;
	    $SILENT   = 1==1;
	} elsif($arg eq "-c"){
	    $CVSUPD   = 1==0;
	    $CVSCOU   = 1==1;
	    $SILENT   = 1==1;

	} elsif($arg eq "-pst"){
	    $RESETST  = 1==0;

	} elsif($arg eq "-R"){
	    $RELCODE  = 1==1;

	} elsif($arg eq "-s"){
	    $NOTIFY   = 1==0;
	} elsif($arg eq "-d"){
	    $DEBUG    = 1==1;
	} elsif($arg eq "-k"){
	    $OPTCONS .= "-k ";
	} elsif($arg eq "-v"){
	    $LIBRARY  = $ARGV[++$i];
	    $ALLARGS .= " $LIBRARY";
	} elsif($arg eq "-p"){
	    $COMPDIR  = $ARGV[++$i];
	    # $DEBUG    = 1==1;
	    $ALLARGS .= " $COMPDIR";
	} elsif ($arg eq "-h" || $arg eq "--help"){
	    &lhelp();
	    exit;
	} else {
	    print $FILO "Unkown argument $arg\n";
	}
    } else {
	&lhelp();
	die "Check syntax.\n";
    }
}


# Check stickey tag flag - default will be to destroy the sticky tag
# by using -A option. User -pst (preserve stick tag) to bypass the default. 
if ( $RESETST ){
    $CVSCMDT .= " -A";
    $CVSCMDR .= " -A";
    $CVSUPDC .= " -A";
}
print $FILO 
    "Our CVS commands will be\n",
    " - Diff check [$CVSCMDT]\n",
    " - Checkout   [$CVSCMDR]\n",
    " - Update     [$CVSUPDC]\n";

# Massage parameters
if($LIBRARY =~ m/dev/i || $LIBRARY =~ m/new/i ||
   $LIBRARY =~ m/pro/i || $LIBRARY =~ m/old/i ||
   $LIBRARY =~ m/\.dev/i ){
    $CHVER = "star".lc($LIBRARY);
} else {
    $CHVER = "starver $LIBRARY";
}

# Directory this script will work in
$COMPDIR=IUCompDir($LIBRARY) if ( $COMPDIR eq "");

# A temp script will be created with this name (number will be
# appended at the end).
$TMPNM   = "$COMPDIR/.AutoBuild".$POST;
$RECVF   = "/tmp/.AutoBuild".$POST;
# stdout/errors will be re-directed to a file with this main
# name. No extension please.
$FLNMSG  = "$COMPDIR/Execute".$POST;
# name of an eventual resource file
$FLNMRC  = "$COMPDIR/.ABrc_$^O";


# Lock file automated handling
IULockPrepare($COMPDIR,$POST.$ALLARGS);     # prepare create one
if ( ! IULockCheck(129600) ){ exit;}        # Check lock file lifetime


#
# --- We start here
#
if( ! chdir($COMPDIR) ){
    die "Could not change directory to $COMPDIR\n";
}

# else
IULockWrite("We are now in $COMPDIR");
print $FILO " - We are now in $COMPDIR\n";
$fail = 0;


#
# If a config file exists, read it
#
if( -e $FLNMRC){
    if (open(FI,$FLNMRC)){
	IULockWrite("Reading configuration $FLNMRC");
	print $FILO " - Reading configuration\n";
	while( defined($line = <FI>) ){
	    @items=split("#",$line); # comment lines
	    $line =  $items[0];
	    if($line ne ""){
		$line =~ s/^\s*(.*?)\s*$/$1/;
		@items = split("=",$line);
		chomp(@items);
		if( uc($items[0]) eq "SKIP_DIRS"){
		    push(@SKIP,$items[1]);
		} elsif ( uc($items[0]) eq "EX_DIRS"){
		    push(@EXDIRS,$items[1]);
		} elsif ( uc($items[0]) eq "CO_DIRS"){
		    push(@CODIRS,$items[1]);
		}
	    }
	}
	close(FI);
    }
}


#
# Do the CVS fake-checkout or checkout of directories
# declared in @DIRS . "fake" simply means that this
# is done using the -n option which will only list
# what needs update (and not really do anything).
# Note that @DIRS contains base level directories so
# the CODIRS have to be treated sperately.
#
if($NIGNOR){
    foreach $dir (@DIRS){
	IULockWrite(" Inspecting $dir");
	print $FILO " + Inspecting $dir\n";
	if( -d $dir ){
	    $cmd = "$CVSCMDT $dir";
	} else {
	    $cmd = "$CVSCMDR $dir";
	}
	@res = `$cmd`;

	if($? != 0){ 
	    print $FILO  "Failed with error=$? [$!] while performing [$cmd]\n";             
	    push(@REPORT,"Failed with error=$? [$!] while performing [$cmd]<br>");
	    foreach $line (@res){  
		if ($line !~ m/^\? /){
		    if ($line =~ m/^C /){
			print $FILO "\t--> $line   <== THIS FILE CREATES A CONFLICT";     
			push(@REPORT,"\t--> $line  &gt;== THIS FILE CREATES A CONFLICT<br>");
		    } else {
			print $FILO "\t--> $line";     
			push(@REPORT,"\t--> $line<br>");
		    }
		}
	    }
	    $fail++;
	}
	foreach $line (@res){
	    if($line =~ m/^U /){
		push(@UPDATES,(split(" ",$line))[1]);
	    } elsif ($line =~ m/^M /){
		push(@MERGED,(split(" ",$line))[1]);
	    } elsif ($line !~ m/^\? /){
		push(@DONOTKNOW,$line);
	    }
	}
    }
    # the code is somewhat cleare with 2 loops but could
    # benefit from a double loop for easier maintainance
    # as there are code replication here ...
    foreach $dir (@CODIRS){
	if ( $dir =~ m/^\s*$/){
	    IULockWrite(" Fully Inspecting -- got empty string (bogus) [$dir]");
	    next;
	}
	if ( defined($ENV{STAR_HOST_SYS}) ){
	    # expand to be safer (i.e. avoid syntax mistakes leading to
	    # disaster)
	    my(@dirst) = glob(".".$ENV{STAR_HOST_SYS}."/*/$dir");
	    if ($#dirst != -1){
		foreach my $dd (@dirst){
		    # Don't do it for now
		    push(@PRECOMPILE,"cd $COMPDIR && /bin/rm -fr $dd");
		}
	    }
	}
	IULockWrite(" Fully Inspecting for new code $dir");
	print $FILO " + Fully Inspecting for new code $dir\n";
	@res = `$CVSCMDR $dir`;
	if($? != 0){ 
	    print $FILO  "Failed with error=$? [$!] while performing [$CVSCMDR $dir]"; 
	    push(@REPORT,"Failed with error=$? [$!] while performing [$CVSCMDR $dir]<br>");

	    foreach $line (@res){ 
		if ($line !~ m/^\? /){ 
		    if ($line =~ m/^C /){
			print $FILO "\t--> $line   <== THIS FILE CREATES A CONFLICT";     
			push(@REPORT,"\t--> $line  &gt;== THIS FILE CREATES A CONFLICT<br>");
		    } else {
			print $FILO "\t--> $line";     
			push(@REPORT,"\t--> $line<br>");
		    }
		}
	    }
	    $fail++;
	}
	foreach $line (@res){
	    if($line =~ m/^U /){
		push(@UPDATES,(split(" ",$line))[1]);
	    } elsif ($line =~ m/^M /){
		push(@MERGED,(split(" ",$line))[1]);
	    } elsif ($line !~ m/^\? /){
		push(@DONOTKNOW,$line);
	    }
	}
    }


    if($fail != 0){
	print $FILO "$CVSCMDT problems prevents continuation ...\n";
	push(@REPORT,"$CVSCMDT problems prevents continuation ...");
	&Exit($fail);
    }

}




# Output this out. Note that we will update ONLY what was
# caught in the above search. This will avoid interim commit
# to get on the way ...
$tmp  = "";
$fail = 0;

#
# Code 'U'pdates has happened, parsing result in this block
# for report purposes.
#
IULockWrite("Updating code");
if($#UPDATES != -1){
    print $FILO
	"\n",
	" - List of updates will follow ...\n";
    push(@REPORT,"%%REF%%<H2>List of new codes</H2>&nbsp;<I>".localtime()."</I>");
    push(@REPORT,"<UL>");
    foreach $line (@UPDATES){
	$dir = (split("/",$line))[0];
	if($dir ne $tmp){
	    push(@REPORT," </OL>") if($tmp ne "");
	    push(@REPORT,"<LI>$dir");
	    push(@REPORT," <OL>");
	    print $FILO "   For $dir ...\n";
	    $tmp = $dir;
	}
	push(@REPORT," <LI><TT>".&IUCVSRef($line)."</TT>");
	print $FILO "     $line\n";
    }
    push(@REPORT," </OL>") if($tmp ne "");
    push(@REPORT,"</UL>");
}


#
# Displaying code which needs to be 'M' moved happened already.
#
IULockWrite("Dealing with merging");
if($#MERGED != -1){
    print $FILO
	"\n",
	" - List of un-comitted code will follow ...\n";
    push(@REPORT,"%%REF%%<H2>List of code found with conflicting CVS version.</H2>");
    push(@REPORT,"Those re codes modified on disk and NOT commited ");
    push(@REPORT,"in the repository. This may be a mistake or an experimental ");
    push(@REPORT,"version. Please, <U>delete or commit</U> them now !! ");
    push(@REPORT,"<UL>");
    foreach $line (@MERGED){
	push(@REPORT," <LI><TT>$line</TT>");
	print $FILO "   $line\n";
    }
    push(@REPORT,"</UL>");
}

#
# Debug those out (who knows what it may contain)
#
IULockWrite("Dealing with unknown CVS reports");
if ($#DONOTKNOW != -1){
    print $FILO
	"\n",
	" - List of unknown problems will follow ...\n";
    push(@REPORT,"%%REF%%<H2>List of unknown CVS problems</H2>");
    push(@REPORT,"We will consider this as a failure and at least, ".
	         "remove the conflicting files. If the removal is ".
	         "a success, the next pass will self-heal. Other errors ".
	         "are not treated. Please, consider repairing/verifying ".
	         "by hand.<br>\n");
    push(@REPORT,"<UL>");
    foreach $line (@DONOTKNOW){
	push(@REPORT," <LI><TT>$line</TT>");
	print $FILO "   $line\n";
	if ( $line =~ /(^C )(.*)/){
	    $line = $2;
	    if ( ABUnlink($line) ){
		# if we succeed to delete, it will be self-healing
		# and the next pass will be fine
		push(@REPORT," - $line file deleted");
	    } else {
		push(@REPORT," - could not delete $line");
	    }
	}
    }
    push(@REPORT,"</UL>");
    $fail++;
}



#
# Ask for confirmation or proceed depending on interractive
# mode or not. Also displays the compilation commmands which
# would occur so the librarian can decide if it is correct.
#
print $FILO " - Compilation will exclude [".join(" ",@SKIP)."]\n";

if ($#PRECOMPILE != -1){
    foreach $line (@PRECOMPILE){
	print $FILO " - Pre-Compilation commands includes [$line]\n"
    }
}

print $FILO " - Compilation commands will be \n";
foreach $line (sort keys %COMPILC){
    print $FILO "   ".($COMPILC{$line}?"MANDATORY":"OPTIONAL ")." '$line'\n";
}
print $FILO "\n";


if(! $SILENT){
    print $FILO " Is this OK ? y/u/i/[n] ";
    chomp($ans = <STDIN>);
    $ans = "no" if ($ans eq "");
} else {
    if($CVSUPD){     $ans = "u";}   # update
    elsif($CVSCOU){  $ans = "y";}   # yes => checkout (default)
    elsif($NIGNOR){  $ans = "i";}   # ignore i.e. compile now without updating the tree
    else {           $ans = "n";}   # absolutly not. Stop ASAP.
}




#
# On "yes" answer, performs the real update
#
if ($ans =~ /^\s*y/i){
    # Now, the answer was YES. We need to cvs co for real.
    # We can checkout by bunch of $BY to avoid the usual unix
    # limitation on the number of arguments on the command line.
    print $FILO " - cvs operation status\n";
    push(@REPORT,"%%REF%%<H2>cvs operation status</H2>");
    if($#UPDATES == -1 && $#MERGED == -1){
	if( -e "$COMPDIR/$DFILE" && $DEBUG){
	    open(FI,"$COMPDIR/$DFILE");
	    chomp($tmp = <FI>);
	    close(FI);
	    $tmp = "No action requested. Taking the latest $tmp version";
	} else {
	    $tmp = "No action required";
	}

	push(@REPORT,
	     "<BLOCKQUOTE>\n<I>$tmp</I>\n</BLOCKQUOTE>");

    } else {
	if($#UPDATES != -1){
	    push(@REPORT,"<OL>");
	    do {
		@items = splice(@UPDATES,0,$BY);
		$tmp = "$CVSCMDR ".join(" ",@items);
		push(@REPORT,"<LI><TT>$tmp</TT><BR>");
		$fail += &Execute($tmp);
	    } while ($#UPDATES != -1);
	    push(@REPORT,"</OL>");
	    #undef(@UPDATES);
	}

	# Now the so-called merged. Those needs to be removed
	if($#MERGED != -1){
	    push(@REPORT,"<OL>");
	    foreach $file (@MERGED){
		push(@REPORT,"<LI><TT>$file</TT>");
		if(-e $file){
		    if(! $TRASH){
			push(@REPORT," found and preserved as-is");
		    } else {
			if( ABUnlink($file)){
			    push(@REPORT,"deleted");
			    $fail += &Execute("$CVSCMDR $file");
			} else {
			    push(@REPORT,"<U>*** Could not delete ***</U>");
			    push(@REPORT,
				 "Build will continue but manual intervention is required");
			}
		    }
		} else {
		    push(@REPORT,"<I>File not found</I>");
		    $fail += &Execute("$CVSCMDR $file");
		}
	    }
	    push(@REPORT,"</OL>");
	    #undef(@MERGED);
	}
    }
} elsif ($ans =~ /^\s*u/i){
    # update was requested instead of
    # checkout.
    push(@REPORT,"%%REF%%<H2>cvs operation status</H2>");
    push(@REPORT,"<BLOCKQUOTE>\n<I>Update was required</I>");
    push(@REPORT,"<UL>");
    foreach $dir (@DIRS){
	# Add a list entry for the report
	push(@REPORT,"<LI><TT>$dir</TT>");

	# before excuting this, take care of EXDIRS if found
	# will take care of them by renaming
	foreach $exd (@EXDIRS){
	    if ( -e "$dir/$exd"){
		push(@REPORT,"<BR><FONT COLOR=\"#00FF00\"><B>WARNING</B></FONT>".
		     " $dir/$exd is being excluded from updates");
		ABRename("$dir/$exd","$dir/$exd.EXCLUDED");
		#} else {
		#push(@REPORT,"<BR><I>(Did not find $exd to exclude in $dir</I>");
	    }
	}

	# Execute the update. $dir will be things defined in DIRS 
	# as returned and defined by IUSourceDirs()
	&Execute("$CVSUPDC $dir",1);

	# Restore exclusions
	foreach $exd (@EXDIRS){
	    if ( -e "$dir/$exd.EXCLUDED"){
		# allow for files based exclusion code i.e. if file
		# exists aftre update, delete and update
		if ( -d "$dir/$exd" ){  
		    push(@REPORT,"<BR><FONT COLOR=\"#FF0000\"><B>WARNING</B></FONT>".
			 " $dir/$exd was a file which re-appeared after update, ".
			 "now replaced by older version");
		    unlink("$dir/$exd");
		}
		ABRename("$dir/$exd.EXCLUDED","$dir/$exd");
	    }
	}
    }
    push(@REPORT,"</UL></BLOCKQUOTE>");
} elsif ($ans =~ /^\s*i/i){
    push(@REPORT,"%%REF%%<H2>no cvs operation performed</H2>");
} else {
    print $FILO "OK. Goodbye !...\n";
    &Exit(-1);
}




#
# If failure at this stage, DO NOT continue (it certainly
# requires a manual intervention and/or carefull inspection)
#
if($fail != 0){ &Exit($fail); }





#
# Compilation will now take place. Note that recent inclusion
# of a recovery procedure (2004) makes use of pass message
# and pass number. We will try to recover only once.
#
# Recovery is assumed to be at compilation level ONLY.
#

$PASSM   = "";   # pass message
$PASSN   = 0;    # pass number


#
# pre-compilation is needed once only
#
if ($#PRECOMPILE != -1){
    push(@REPORT,"%%REF%%<H2>Pre-compilation command were auto-detected</H2>");
    push(@REPORT,"<UL>");
    foreach $line (@PRECOMPILE){
	#push(@REPORT,"<LI><TT>$line</TT>");
	if ( &Execute($line) == 0){
	    push(@REPORT,"<LI><TT>".IUl2pre($line)."</TT> ".&STRsts(0,$line));
	} else {
	    push(@REPORT,"<LI><TT>".IUl2pre($line)."</TT> ".&STRsts(1,$line));
	}
    }
    push(@REPORT,"</UL>");
}


COMPILE_BEGIN:
{
    # a global flag will also be used and incremented in Execute()
    $FRECOVER   = 0;
    foreach $re (keys %RECOVER){   $CRECOVER{$re} = 0;}

    # Pass number information
    if ($PASSN != 0){  $PASSM = "(pass # $PASSN)";}

    push(@REPORT,"%%REF%%<H2>Compilation report (pass $PASSN)</H2>");
    push(@REPORT,"<UL>");

    # hum ! @ = keys did not put it in the same order.
    $i = 0;
    foreach $line (sort keys %COMPILC){
	if( ! open(FO,">$TMPNM$i") ){
	    print $FILO " * Could not open file $TMPNM$i for write\n";
	    push(@REPORT,"<LI><U>Failure</U> to open temporary file $TMPNM\n");
	    goto End;
	}

	$iline= $line;
	$line =~ s/%%CHVER%%/$CHVER/;
	$line =~ s/%%CHENVB%%/$CHENVB/;
	$line =~ s/%%CHENVA%%/$CHENVA/;
	print $FILO " - Preparing command [$line]\n";
	print FO
	    "#!/bin/csh\n",
	    "# Change directory to target\n",
	    "cd $COMPDIR\n",
	    "\n",
	    "# Set SKIP_DIRS according to config\n",
	    "setenv SKIP_DIRS \"".join(" ",@SKIP)."\"\n",
	    "# Compilation command execution\n",
	    "$line $OPTCONS\n",
	    "\n",
	    "# below a few lines to handle return status and CHVER\n",
	    "set sts=\$status\n",
	    "test -e $RECSTARLVL && /bin/rm -f $RECSTARLVL\n",
	    "echo \$STAR_HOST_SYS >$RECSTARLVL\n",
	    "exit \$sts\n";


	close(FO);
	chmod(0770,"$TMPNM$i");
	#&DumpContent($FILO,"$TMPNM$i");

	push(@REPORT,"<P>");
	push(@REPORT,"%%REF%%<LI>$line<BR>");
	$fail = $COMPILC{$iline}*(&Execute("$TMPNM$i"));
	push(@STATUS,"$i-$fail"."-0");

	#print "DEBUG:: Status is $i-$fail ... FRECOVER=$FRECOVER\n"       if( $DEBUG);
	if($fail != 0 && $FRECOVER != 0){      # FRECOVER starts at 0 and incremented in Execute()
	    foreach $re (keys %CRECOVER){      # but since we do not know which error was detected
		if ( $CRECOVER{$re} != 0 ){    # CRECOVER containing error specific counter is used
		    $GRECOVER{$re} += 1;       # This pass should recover only from previous failure
		    if ($GRECOVER{$re} <= $MAXRECOVERY){
			# We recover only $MAXRECOVERY for each one
			#print "DEBUG:: trying to open $RECVF$i.recover\n" if( $DEBUG);
			if( open(FO,">$RECVF$i.recover") ){
			    print $FILO " * Failure detected at this stage. Recovery procedure for [$re]\n";
			    @recover_env = split("&&",$line);
			    pop(@recover_env);

			    print FO
				"#!/bin/csh\n",
				"cd $COMPDIR\n",
				"\n",
				join("&&",@recover_env)."\n",
				$RECOVER{$re}."\n";
			    close(FO);
			    chmod(0770,"$RECVF$i.recover");

			    push(@REPORT,"<P>");
			    if( &Execute("$RECVF$i.recover") == 0){
				push(@REPORT,"%%REF%%<LI>Error Recovery was a SUCCESS for [$re] - ".
				     "If subsequent pass succeeds, ignore previous failure<BR>");
				#push(@REPORT,"%%REF%%<LI>");
				# we recovered successfully, start all over again
				$PASSN++;                     # increment counter
				ABUnlink("$TMPNM$i");          # clean-up compile pass exec
				ABUnlink("$RECVF$i.recover");  # clean up recovery exec
				push(@STATUS,"$i-$fail"."-1");
				$fail = 0;
				push(@REPORT,"</UL>");        # </UL> is terminated in the next block
				goto COMPILE_BEGIN;           # but need to be here as well if recovery
			    } else {
				#print "DEBUG: Failed to execute $TMPNM$i.recover\n";
			    }
			} else {
			    #print "DEBUG: Failed to open $TMPNM$i.recover\n";
			}
		    } else {
			# More than $MAXRECOVERY made and still did not help
			# We Must abort at this point ... which will be taken
			# care of by the "last" command below but ...
			push(@REPORT,
			     "<BR>%%REF%%Failure recovery for [$re] did not succeed (the error persists)<BR>");
		    }
		}
	    }

	    last;

	} else {
	    # in case of failure, we purposadly leave the executor file
	    # in place and do not delete. It allows for executing by hand
	    # for debugging purposes.
	    ABUnlink("$TMPNM$i");
	    $i++;
	}
	if($FIRSTPASS){ last;}
    }
}


# The post tasks are being done only if the preceeding succeeded.
# In principle, some of those tests will also modify the $fail
# variable (if mandatory) so we do not want to merge this with
# the Exit() routine.
if($fail == 0){
    # Other stuff here (test this version, tree copy, send Email,
    # prepare the coffee etc ...
    push(@REPORT,"</UL>");
    push(@REPORT,"%%REF%%<H2>Post-compilation tasks</H2>");
    push(@REPORT,"<UL>");
    foreach $tmp (@POSTSKS){
	if (&Execute($tmp) == 0){
	    push(@REPORT,"<LI><TT>".IUl2pre($tmp)."</TT> ".&STRsts(0,$tmp));
	} else {
	    push(@REPORT,"<LI><TT>".IUl2pre($tmp)."</TT> ".&STRsts(1,$tmp));
	}
    }
}


End:
push(@REPORT,"</UL>");
&Exit($fail);



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

sub DumpContent
{
    my($FO,$flnm)=@_;
    my($line);

    if ( open(FIDC,">$flnm") ){
	print $FO "# begin content of $flnm --------------------->\n";
	while ( defined($line = <FIDC>) ){  print $FO $line;}
	print $FO "# <-------------- end content of $flnm\n";
    } else {
	print $FO "DumpContent: Could not find $flnm";
    }

}


#
# Exit routine dump the report table and
# leave.
#
sub Exit()
{
    my($sts)=@_;
    my($tmp,$rtsts);

    # Delete the lock file
    IULockDelete();

    # this was added as a bypass to delete the lock
    # while using the Exit()
    if ( $sts < 0){ exit;}

    # Close global output if needed
    if($FILO ne STDOUT){ close($FILO);}


    # default return status
    $rtsts = 0;

    if( $DEBUG){
	print "Debug mode. Stopping at this stage. $ERRSTR\n";

    } else {
	# LEAVE NOW. Either send Email warning to admin or
	# release the volume.
	if($sts != 0){
	    # Send Emails to the list of managers if the compilation
	    # failed. We do NOT want to be influcenced by anything else
	    &SendMessage("AutoBuild: Action failed",
			 "Last error is $ERRSTR\nFor more information,".
			 "check ".IUHtmlRef()."/$FLNM.html");
	    $rtsts = 1;
	} else {
	    # Change in 2008 / create the marker file in a sub-tree, only
	    # release code if checkout / update was requested or -U was asked
	    
	    if ( $RELCODE ){
		push(@REPORT,"<BLOCKQUOTE>");
		push(@REPORT,"%%REF%%<B>AFS release</B>\n");
		push(@REPORT," <UL>\n");
		# Release the volume now
		$subd = $tmp = IUReleaseFile();
		$subd =~ m/(.*\/)(.*)/; $subd = $1;
		if ( -e $subd ){
		    ABUnlink($tmp);
		    if ( open(FO,">$tmp") ){
			print FO "Ready to release on ".localtime()."\n";
			close(FO);
			push(@REPORT," <LI>Release via $tmp done ".localtime()."\n");
		    } else {
			push(@REPORT," <LI>Failed to create $tmp on ".localtime()."\n");
		    }
		} else {
		    push(@REPORT," <LI>No dir for creating $tmp\n");
		} 

		# this would not always work because the CHVER would be in a COMPILC
		# spawn. We need to recover the version
		#if ( defined($ENV{STAR_HOST_SYS}) ){
		my($STAR_level)=undef;
		if ( -e $RECSTARLVL){
		    open(RECV,$RECSTARLVL);
		    chomp($STAR_level = <RECV>);
		    close(RECV);
		    unlink($RECSTARLVL);
		}
		if ( defined($STAR_level) ){
		    my($subf,$subd);
		    
		    $subd = $subf =".".$STAR_level."/".$tmp;
		    $subd =~ m/(.*\/)(.*)/; $subd = $1;
		    if ( -e $subd ){
			ABUnlink($subf);
			if ( open(FO,">$subf") ){
			    print FO "Ready to release on ".localtime()."\n";
			    close(FO);
			    push(@REPORT," <LI>Release via $subf done ".localtime()."\n");
			} else {
			    push(@REPORT," <LI>Failed to create $subf on ".localtime()."\n");
			}
		    } else {
			push(@REPORT," <LI>No dir for creating $subf<br>\n");
		    }
		} else {
		    push(@REPORT,
			 " <LI>Release via subdir cannot be done - no STAR_HOST_SYS defined\n");
		}
		push(@REPORT," </UL>\n");
		push(@REPORT,"</BLOCKQUOTE>\n");
	    }
	    # And exit with normal status
	    $rtsts = 0;
	}
    }

    # Now andonly now dump report - change JL 200912
    # Output HTML
    &ReportToHtml($sts);


    exit($rtsts);
}


# Send Email to managers
sub SendMessage
{
    my($subject,$boddy)=@_;
    my(@email,$tmp);

    if ( $NOTIFY ){
	@email  = IUManagers();

	foreach $tmp (@email){
	    system("echo \"$boddy\" | Mail -s \"$subject\" $tmp");
	}
    }
}


# Output report to HTML file.
sub ReportToHtml
{
    my($sts)=@_;
    my($flnm);
    my($i,$k,$line,$tmp);
    my($fo);

    # depending on the mode we use, create file on expected
    # target directory or not.
    if($DEBUG){
	$flnm=$ENV{HOME}."/$FLNM";
	print "Output to $flnm.html\n";
    } else {
	$flnm="$TRGTDIR/$FLNM";
    }


    # move old file.
    $i = 0;
    if( -e "$flnm.html"){
	while( -e "$flnm$i.html"){ $i++;}
	if( $i == 100){
	    ABUnlink(glob("$flnm\[0-9\]*.html"));
	    $i = 0;
	}
	ABRename("$flnm.html","$flnm$i.html");
    }

    # this is the current generated page
    if( open(FO,">$flnm.html") ){
	$fo = FO;
    } else {
	&SendMessage("AutoBuild: Problem writting $flnm error=$? [$!].",
		     "Action is incomplete, results dumped to ".($FILO==STDOUT?"STDOUT":$FILO));
	$fo = STDOUT;
    }


    # Title and link to preceeding if any.
    # The beginning of the "prev" thread at 1 is NOT a coding
    # bug but a design i.e. Version 0 will NOT be referenced
    # and this was intended for debug purposes
    if($LIBRARY ne "adev"){
	print $fo IUhead("AutoBuild report using $LIBRARY");
    } else {
	print $fo IUhead("AutoBuild report");
    }
    if ($i != 0){
	print $fo
	    "<H5 ALIGN=\"center\">",
	    "<A HREF=\"$FLNM$i.html\">Prev</A></H5>\n";
    }

    # global success
    print $fo
	"<TABLE BORDER=\"0\">\n",
	"<TR><TD><B>Global Status</B></TD><TD>".
	    uc(&STRsts($sts))."</TD></TR>\n",
	"<TR><TD><B>Skipped DIRS </B></TD><TD><TT>".
	    join(" ",@SKIP)."</TT></TD></TR>\n";
    if(IUCompDir($LIBRARY) ne $COMPDIR){
	print $fo
	"<TR><TD><B>Working directory</B></TD>".
	    "<TD><TT>$COMPDIR</TT></TD></TR>\n";
    }
    print $fo "</TABLE>\n";

    # sort/create references
    $k = 0;
    for($i = 0 ; $i <= $#REPORT ; $i++){
	$line = $tmp = $REPORT[$i];
	if($line =~ m/(.*)(%%REF%%)(.*)/){
	    if( ! defined($1) ){ $1 = "";}
	    $line = $1."<A NAME=\"Ref$k\"></A>".$3;
	    $tmp  = $3;
	    $tmp =~ s/</<!-- /g;
	    $tmp =~ s/>/ -->/g;
	    push(@REFS,"<A HREF=\"#Ref$k\">$tmp</A>");

	    $REPORT[$i] = $line;
	    $k++;
	    
	    # TODO: those should be made extendable patterns
	} elsif ($line =~ m/errors constructing/ ||
		 $line =~ m/error constructing/  ||
		 $line =~ m/cons: don\'t know how to construct/ ){
	    push(@REFS,"<A HREF=\"#Ref$k\">$line</A>");
	    $REPORT[$i] = "<A NAME=\"Ref$k\"></A>$line";
	    $ERRSTR = $line;         # over-write stupid default
	    $k++;
	}
    }
    print $fo "<UL>\n";
    foreach $line (@REFS){
	print $fo "<LI>$line\n";
    }
    print $fo "</UL>\n<P>\n";

    # dump all lines with fixed references
    foreach $line (@REPORT){
	print $fo "$line\n";
    }

    # write trailer, close file
    print $fo IUtrail();
    if($fo ne STDOUT){ close(FO);}

    # We can do this part only now because the HTML
    # parsing also determine the GUILTY maker ...
    # This will be used for later processing by some
    # statistics CGI .
    if( open(FO,">>$flnm.dat") ){
	if( $ERRSTR =~ m/(StRoot\/)(.*\/)(.*)/){
	    $GUILTY = $2;
	} else {
	    $GUILTY = $ERRSTR;
	    $GUILTY =~ s/\s+//g;
	}
	print FO time()." ".($i+1)." $sts $GUILTY ".join(" ",@STATUS)."\n";
	close(FO);
    }
}





# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



#
# Execute command, trap stderr only and push the output
# in the REPORT array. Here, the return status of this
# routine is the reverse than expected for a TRUE/FALSE
# answer ; it follows the $rc code ...
#
sub Execute
{
    my($cmd,$mode)=@_;
    my($rc,$line,$k);
    my(%track_recover);

    if( ! defined($mode) ){ $mode = 0;}
    IULockWrite("Executing [$cmd]");

    ABUnlink("$FLNMSG.err");
    open(SAVEERR,">&STDERR");
    open(STDERR,">$FLNMSG.err");
    select(STDERR); $| = 1;
    if($mode == 1){
	print $FILO " - Executing [$cmd]. Please wait ...\n";
	ABUnlink("$FLNMSG.out");
	open(SAVOUT,">&STDOUT");
	open(STDOUT,">$FLNMSG.out");
	select(STDOUT); $| = 1;
    }


    $rc    = 0xffff & system($cmd);
    $ERRSTR= "Error str = $!";         # temptative

    close(STDERR);
    open(STDERR, ">&SAVEERR");
    close(SAVEERR);
    if($mode == 1){
	close(STDOUT);
	open(STDOUT,">&SAVOUT");
	close(SAVOUT);
    }

    # may be more complex later on. There are some known well
    # known/established returned code.
    if($rc != 0 && $rc != IUconsOK() ){
	print $FILO " * [$cmd] FAILED. Return Code =$rc\n";
	push(@REPORT,&STRsts(1)."Return Code = $rc");
	# RE-assign errstr
	$ERRSTR = "ReturnCode $rc";
    } else {
	push(@REPORT,&STRsts(0));
    }


    # Glue the errors with it
    if( -e "$FLNMSG.err"){
	open(FI,"$FLNMSG.err");
	push(@REPORT,"<TABLE BORDER=\"0\">");

	$k   = -1;
	$ppat= "";

	while ( defined($line = <FI>) ){
	    chomp($line);

	    # Color scheme change between blocks based on warning word ?
	    # This purely Experimental and should be moved to a routine
	    # later. It manipulates / adds lines to @REPORTS
	    if($line ne ""){
		if( ($line =~ m/warning/i      ||
		     $line =~ m/In\sfunction/i ||
		     $line =~ m/In\smethod/i     ) &&
		    $line !~ m/warning\:\s*by/     &&
		    $line !~ m/included\sfrom/      ){
		    $cpat = (split(":",$line))[0];
		    if($ppat ne $cpat){
			$ppat = $cpat;
			if($k != -1){ push(@REPORT,"</PRE></TR></TD>");}
			push(@REPORT,"<TR BGCOLOR=\"$COLORS[$k]\"><TD><PRE>");
			$k++;  if($k > $#COLORS){ $k = 0;}
		    }
		} elsif ($k == -1){
		    push(@REPORT,"<TR BGCOLOR=\"$COLORS[$k]\"><TD><PRE>");
		    $k++;  if($k > $#COLORS){ $k = 0;}
		}
	    }

	    # Treat recovery here
	    foreach $re (keys %RECOVER){
		# Is this a recoverable error ??
		if ($line =~ m/$re/i){
		    # It matches, yes it is ...
		    #print "DEBUG [$re] is a recoverrable error\n"         if ($DEBUG);
		    # An initial logic error was that a log may contain multiple
		    # times the same error message before the compiler/cons/whatever
		    # gives up. We need to ensure the count is right so ...
		    # If do not need the FERCOVER count, we can remove the extra
		    # associative array.
		    if ( ! defined($track_recover{$re}) ){
			$track_recover{$re} = $CRECOVER{$re};
			$FRECOVER++;
			$CRECOVER{$re}++;
		    }
		    last;
		}
	    }

	    # If this is a real error, push it in pre-formatted by external
	    # global routine (in our case, replace special character by html
	    # escapes. See ABUtils.pm for more information).
	    if( IUError($line) ){
		push(@REPORT,IUl2pre($line,"%%REF%%"));
	    }
	}
	if($k != -1){ push(@REPORT,"</PRE></TR></TD>");}
	push(@REPORT,"</TABLE>");
	close(FI);
	ABUnlink("$FLNMSG.err");
    }


    #
    # Results from output requested - but we need to filter
    # in real errors detected by patter & exclude what is known
    # to be OK.
    #
    if($mode == 1 && -e "$FLNMSG.out"){
	my(@BuffRESULTS);
	my($pat,$skip);

	open(FI,"$FLNMSG.out");
	while (defined($line = <FI>) ){
	    chomp($line);

	    # treat patterns to skip in addition of IUError
	    # IUIfAnyMatchExclude is a "if found NOT OK" func so, need 
	    # negate as if found, OK to skip.
	    next if ( ! IUIfAnyMatchExclude($line,@IGNUNKNOWN) );

	    # check if really an error
	    if ( IUError($line) ){
		push(@BuffRESULTS,IUl2pre($line,"%%REF%%"));
	    }
	}
	close(FI);
	ABUnlink("$FLNMSG.out");

	# prepare block afterward - now allows filtering
	if ( $#BuffRESULTS != -1){
	    push(@REPORT,"<BR><U>STDOUT relevant results</U>\n<PRE>");
	    push(@REPORT,@BuffRESULTS);
	    push(@REPORT,"</PRE>");
	}

    }
    return $rc;
}


sub lhelp
{

    my($excl)=join(" ",@SKIP);
    print $FILO qq~
 This utility compiles and test the code automatically. It heavily relies on
 the ABUtils module so any modification to this module would affect this
 utility. Apart from that, command line arguments may be used to modify the
 default values. They are :

 -i           Do not checkout/update from cvs.
 -c           Performs a cvs checkout automatically 
 -u           Performs a cvs update automatically

 -t           Tag any resulting output summary file with OS name i.e.
              $FLNM-$^O .
 -T Tag       Tag any resulting output summary file with arbitrary tag
              $FLNM-\$Tag . '-t' and '-T Tag' may be combined (order
              will make precedence)

 -v Lib       Change default library version from $LIBRARY to 'Lib' where
              'Lib' stands for new, old, SL01i etc ...
 -p Path      Change the default $COMPDIR working directory
              to 'Path'. 

 -b Cmd       Executes 'Cmd' before star library version change (can be used
              for executing a setup)
 -B           Same as -b but will take Cmd from the environment variable
              AutoBuild_setup_cmd (allowing multiple arguments)
 -a Cmd       Executes 'Cmd' after star library version change (can be used
              for modifying the setup)

 -A Cmd       Append/Add 'Cmd' as an additional compilation environment setup. 
              With this option, AutoBuild may perform more than two passes. 
              This is useful if you want to syncrhonize other compilation 
              passes with the same code update (but beware that a failure 
              of that pass will make the entire compilation fail)

 -s           Silent i.e. do not send Email to managers on failures
 -d           Debug. DO NOT perform post compilation tasks and perform
              a HTML output in $ENV{HOME} instead of
              $TRGTDIR
 -R           Release main volume (code) in post compilation tasks.

 -f           Flush the default list of directories to skip
 -x ABC       Exclude ABC from list in addition to default exclusion
              list $excl
 -1           First compilation pass only
 -k           Keep going as far as possible after errors (cons option)

 -pst         Preserve sticky tag - default is to destroy it upon CVS
              actions and take the code from the head.

 -h or --help Display this help.

 Without -{i|c|u} the procedure will ask you for an answer in interractive
 mode. Possible answers are y=checkout (-c),u=update (-u) i=ignore (-i) or
 n=none (abort).
		  ~;
    print $FILO "\n";

}

sub STRsts
{
    my($sts,$mess)=@_;
    my($xtra);

    if ( defined($mess) ){
	$xtra = "<!-- ".IUl2pre($mess)." -->";
    } else {
	$xtra = "";
    }
    if($sts==0){
	return $xtra."<FONT COLOR=\"#0000FF\"><B>Success </B></FONT><BR>";
    } else {
	return $xtra."<FONT COLOR=\"#FF0000\"><B>FAILURE </B></FONT><BR>";
    }
}



