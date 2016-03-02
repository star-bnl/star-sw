
#
# This simple module was added to regroup a bunch of utility
# routines for the Insure++ build.
# Put there WebStyles, routines or anything which would be
# common to several scripts (so we don't have to re-write
# it and we would have everything centralized)
#

package ABUtils;
require 5.000;
use Exporter;
use Digest::MD5;

@ISA = (Exporter);
@EXPORT=qw(IUbody IUcmt IUhead IUtrail IUGetRef IUl2pre IUresource
	   IUFuncMatch IUExcluded IUIfAnyMatchExclude IUSourceDirs IUError IUconsOK
	   IULoad JPLoad VLGLoad
	   IURTFormat JPRFFormat VLGFormat
	   JPRExec
	   IUCanonicalName
	   IUTests IUTestDir IUListRoutine IUErrorURL IUCVSRef
	   IUQueue IUSubmit
	   IUCheckFile IUMoveFile
	   IUReleaseFile IUManagers IUCompDir
	   IUHtmlDir IUHtmlRef IUHtmlPub
	   IULockPrepare IULockCheck IULockWrite IULockDelete
	   IUGetLogin
	   ABUnlink ABRename
	   );

# ------------------------------------------
# ---- Compilation section (insure and cons)
# ------------------------------------------
# Directory where we compile
$INSU::COMPDIR="/afs/.rhic/star/replicas/DEV";
$INSU::STARAFS="/afs/rhic.bnl.gov/star/packages";

# list of dirs affected by a cvs co
@INSU::DIRS=("StRoot","StarDb","StDb","StarVMC","pams","asps","mgr","QtRoot","OnlTools");

# List of excluded modules in StRoot compilation. This is a default
# and does not preclude the SKIP_DIRS usage.
@INSU::SKIP=("StEbyePool");
$INSU::SKIPNM="SKIP_DIRS";


# This array declares a list of errors or messages
# to ignore.
# Pattern only ; case insensitive.
@INSU::MSGISERR=("Do not generate Streamer",
		 ".consign",
		 "install_headerfiles.*Error 1",
		 ".flc",
		 "cvs update: Updating",
		 "Run Conscript-standard in");

# OK return status from cons is
$INSU::CONSOK=0;

# ------------------------------------------
# Afs volume release file. Relative path
# Mailing list in case of failure.
# ------------------------------------------
$INSU::RELFLNM=".log/afs.release";
@INSU::MANAGERS=(#"jlauret\@bnl.gov"
                 "jlauret\@bnl.gov",
		 "didenko\@bnl.gov",
		 "gene\@bnl.gov"
		 );


# ------------------------------------------
# --- Tests and running section
# ------------------------------------------
$INSU::QUEUE   ="star_cas_prod";

# Command to issue to load the Insure environment
$INSU::INSLOAD = "setenv INSURE yes ; unsetenv NODEBUG ; staradev";
$INSU::JPRLOAD = "staradev";
$INSU::VLGLOAD = "staradev";

# directory which will contain the test result files
$INSU::TDIR    ="/gpfs01/star/rcf/test/dev/Insure";

# Misc values
$INSU::FLNMLCK = "";
$INSU::LCK     =  0;
$INSU::md5seed = "";

# LSF submit command
$INSU::BSUB = (defined($ENV{LSF_BINDIR})?$ENV{LSF_BINDIR}."/bsub":"/usr/local/lsf/bin/bsub");


# All tests may be declared here in this array.
# - The first element is the chain
#   A ";" separator may use indicating a string to use for filename
#   generation (if empty, will concatenate chain options)
# - the second is a list of files to work on.
#
# Note that the array will be later sorted so no need to try to put the chain
# in a different order, hoping it will do something different.
#
%INSU::TESTS=(
	      "p00h",
	      "/gpfs01/star/daq/2000/09/st_physics_1248022_raw_0001.daq",

	      "trs srs fss rrs C2001 GeantOut big evout fzin",
	      "/gpfs01/star/rcf/simu/cocktail/hadronic/default/highdensity/year2001/hadronic_on/Gstardata/hc_highdensity.16_evts.fz",

	      "trs sss fss rrs C2001 GeantOut big evout fzin",
	      "/gpfs01/star/rcf/simu/cocktail/hadronic/default/highdensity/year2001/hadronic_on/Gstardata/hc_highdensity.16_evts.fz",

	      "p2000",
	      "/gpfs01/star/daq/2000/08/st_physics_1229021_raw_0003.daq",

	      #"p2001 -Kalman fiedloff",
	      #"/gpfs01/star/daq/2001/192/st_physics_2192029_raw_0003.daq",

	      "p2001a",
	      "/gpfs01/star/daq/2001/251/st_physics_2251004_raw_0001.daq",

	      "p2001",
	      "/gpfs01/star/daq/2001/251/st_physics_2251004_raw_0001.daq",

	      "pp2001",
	      "/gpfs01/star/daq/2002/017/st_physics_3017028_raw_0006.daq",

	      "dau2003 est beamLine CMuDst",
	      "/gpfs01/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",

	      "pp2003 eemcD alltrigger trgd",
	      "/gpfs01/star/daq/2003/111/st_physics_4111036_raw_0030004.daq",

	      # Y4 chains
	      "P2004 EST svt_daq svtD eemcD pmdRaw OShortR Xi2 V02 CMuDst",
	      "/gpfs01/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq",

	      "trs,srs,y2004,tpc,l0,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,evout,est,-xi,-v0,xi2,XiSvt,svtdEdx,SvtMatchVtx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin ; Full Y4 sim chain",
	      "/gpfs01/star/rcf/simu/rcf1207_01_225evts.fzd",

	      # ITTF chains
	      "ry2004 in tpc_daq tpc -tcl -tpt -PreVtx fcf Physics svtDb ITTF dst event ".
	      "genvtx Sti Tree evout trgd debug1".
	      "; TPC based chain",
	      "/gpfs01/star/daq/2004/061a/st_physics_5061059_raw_3030004.daq",

	      "ry2004 in tpc_daq tpc -tcl -tpt -PreVtx fcf Physics svtDb ITTF dst ".
	      "event genvtx Sti analysis EventQA tags Tree evout  l3onl tofDat emcDY2 ".
	      "fpd Corr2 ftpc trgd OSpaceZ OShortR Kink2 V02 Xi2 debug1".
	      "; ITTF full chain",
	      "/gpfs01/star/daq/2004/061a/st_physics_5061059_raw_3030004.daq",

	      "trs y2004 globT tcl TpcHitMover sim_T svt_T ftpcT ctf_T l0 SvtCL svtDb ITTF ".
	      "genvtx Sti DstOut gen_T l3_T dst dEdxY2 EventQA geant tags bbcSim tofsim EvOut ".
	      "analysis emcY2 -Match EEfs GeantOut big V02 Xi2 Kink2 CMuDst fzin MiniMcMk ".
	      "; ITTF nightly test",
	      "/gpfs01/star/rcf/simu/rcf1207_01_225evts.fzd"


	      );

# Default formatting script for run-time
$INSU::RTFORMAT ="/afs/rhic.bnl.gov/star/packages/scripts/insrtm.pl";
$INSU::JPFORMAT ="/afs/rhic.bnl.gov/star/packages/scripts/jprtm.pl";
$INSU::VLGFORMAT="/afs/rhic.bnl.gov/star/packages/scripts/valgrtm.pl";
$INSU::JPEXEC   ="/afs/rhic.bnl.gov/star/packages/dev/.\@sys/BIN/jprof";

# routine exclusion in listing-by-routine
@INSU::SKIPFUNC=("_Cint",
		 "_TableCint",
		 "_Table.");


# ------------------------------------------
# ---- HTML section
# ------------------------------------------
# directory where the HTML file will be stored (final target)
$INSU::HTMLREPD="/afs/rhic.bnl.gov/star/doc/www/comp/prod/Sanity";   # final direcory
$INSU::WSERVR = "http://www.star.bnl.gov";                           # Web server root
$INSU::HTMLREF="/public/comp/prod/Sanity";                           # URL
$INSU::HTMLPUBD="/afs/rhic.bnl.gov/star/doc/www/html/tmp/pub/Sanity";# public dir (temp)
$INSU::CVSWEB="/cgi-bin/protected/cvsweb.cgi";                       # CVS web utility


# variables subject to export. So far, we will be using
# functions to return their value
# HTML body attributes
$INSU::BODYATTR="bgcolor=cornsilk text=black link=navy vlink=maroon alink=tomato";
# HTML font declaration
$INSU::FONT="<basefont face=\"verdana,arial,helvetica,sans-serif\">";
$INSURL="/public/comp/sofi/Insure/manuals/";
%INSERRORS=(
	    # the above list was generated dumping and parsing the main
	    # insure++ index.htm page. Script used is named format_insure.pl
        "ALLOC_CONFLICT","err_allo.htm#69",
        "BAD_CAST","err_badc.htm#4301",
        "BAD_DECL","err_badd.htm#175",
        "BAD_FORMAT","err_badf.htm#1558",
        "BAD_INTERFACE","err_badi.htm#117",
        "BAD_PARM","err_badp.htm#234",
        "COPY_BAD_RANGE","err_copy.htm#48",
        "COPY_DANGLING","err_copa.htm#51",
        "COPY_UNINIT_PTR","err_copb.htm#1240",
        "COPY_WILD","err_copc.htm#33",
        "DEAD_CODE","err_dead.htm#69",
        "DELETE_MISMATCH","err_delm.htm#25",
        "EXPR_BAD_RANGE","err_expr.htm#104",
        "EXPR_DANGLING","err_expa.htm#113",
        "EXPR_NULL","err_expb.htm#122",
        "EXPR_UNINIT_PTR","err_expc.htm#89",
        "EXPR_UNRELATED_ PTRCMP","err_expd.htm#134",
        "EXPR_UNRELATED_ PTRDIFF","err_expe.htm#847",
        "EXPR_WILD","err_expf.htm#65",
        "FREE_BODY","err_free.htm#132",
        "FREE_DANGLING","err_frea.htm#140",
        "FREE_GLOBAL","err_freb.htm#152",
        "FREE_LOCAL","err_frec.htm#165",
        "FREE_UNINIT_PTR","err_fred.htm#192",
        "FREE_WILD","err_fref.htm#61",
        "FUNC_BAD","err_func.htm#62",
        "FUNC_NULL","err_funa.htm#70",
        "FUNC_UNINIT_PTR","err_funb.htm#80",
        "INSURE_ERROR","err_insu.htm#41",
        "INSURE_WARNING","err_insa.htm#67",
        "LEAK_ASSIGN","err_leak.htm#80",
        "LEAK_FREE","err_leaa.htm#92",
        "LEAK_RETURN","err_leab.htm#108",
        "LEAK_SCOPE","err_leac.htm#119",
        "PARM_BAD_RANGE","err_parm.htm#73",
        "PARM_DANGLING","err_para.htm#95",
        "PARM_NULL","err_parb.htm#109",
        "PARM_UNINIT_PTR","err_parc.htm#137",
        "PARM_WILD","err_pard.htm#47",
        "READ_BAD_INDEX","err_read.htm#198",
        "READ_DANGLING","err_reaa.htm#207",
        "READ_NULL","err_reab.htm#212",
        "READ_OVERFLOW","err_reac.htm#224",
        "READ_UNINIT_MEM","err_reae.htm#98",
        "READ_UNINIT_PTR","err_reaf.htm#233",
        "READ_WILD","err_reag.htm#71",
        "RETURN_DANGLING","err_retu.htm#94",
        "RETURN_FAILURE","err_reta.htm#140",
        "RETURN_INCONSISTENT","err_retb.htm#104",
        "UNUSED_VAR","err_unus.htm#58",
        "USER_ERROR","err_user.htm#151",
        "VIRTUAL_BAD","err_virt.htm#60",
        "WRITE_BAD_INDEX","err_writ.htm#112",
        "WRITE_DANGLING","err_wria.htm#123",
        "WRITE_NULL","err_wrib.htm#131",
        "WRITE_OVERFLOW","err_wric.htm#140",
        "WRITE_UNINIT_PTR","err_wrid.htm#150",
        "WRITE_WILD","err_wrie.htm#66"
	    );



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ---- Internal variables only
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
$PRGM="ABUtils ::";
$AUTHOR="Jerome LAURET";
$EMAIL="jlauret\@bnl.gov";
$VERSION="V01-005";


#
# Returns a list of directories to exclude from compilation.
# This list DOES not preclude the SKIP_DIRS usage.
#
sub IUExcluded
{
    if( defined($ENV{$INSU::SKIPNM}) ){
	@INSU::SKIP = split(" ",$ENV{$INSU::SKIPNM});
    }
    return @INSU::SKIP;
}

# Although I am forseing only the use if INSUhead, flexibility
# of 2 extra routines added.
sub IUbody {
    "<BODY $INSU::BODYATTR>\n$INSU::FONT\n";
}
sub IUcmt {
    my($tmp);
    $tmp  = "<!-- Listing auto-generated on ".localtime()." -->\n";
    $tmp .= "<!-- $PRGM version $VERSION -->\n";
    $tmp .= "<!-- Script written by $AUTHOR $EMAIL  -->\n";
}
# Both head and trail may later include templates
# or style sheets etc ...
sub IUhead
{
    my($title)=@_;
    my($tmp);
    $tmp  = "<HTML>\n<HEAD><TITLE>$title</TITLE></HEAD>\n";
    $tmp .= &IUbody().&IUcmt();
    $tmp .= "<H1 ALIGN=\"center\">$title</H1>\n";
    $tmp .= "<H4 ALIGN=\"center\">Formatted on ".localtime()."</H4>\n";
    $tmp;
}
sub IUtrail
{
    "</BODY>\n</HTML>\n";
}


# Format line to something suitable to a pre tag.
# Some character needs HTML escaping.
sub IUl2pre
{
    my($line,$flag)=@_;
    if( defined($line) ){
	$line =~ s/&/&amp;/g;
	$line =~ s/</&lt;/g;
	$line =~ s/>/&gt;/g;
	if($line =~ m/\serror\s/i){
	    if( defined($flag) ){
		#"$flag<b>$line</b>";
		"$flag $line ";
	    } else {
		#"<b>$line</b>";
		"$line";
	    }
	} else {
	    $line;
	}
    } else {
	"";
    }
}

# This routine converts a line into a string suitable
# for NAME and HREF relative document reference
sub IUGetRef
{
    my($line)=@_;

    if( ! defined($line) ){ &IUdie("$PRGM IUGetRef requires an argument\n");}
    if( $line ne ""){
	$line =~ s/[\.\[\]:\(\)]/_/g;
	$line =~ s/\s//g;
    }
    $line;
}


# Creates an Insure++ .psrc file in the current directory
# Argument is the file to redirect the output to.
# Arg2 : message to display.
sub IUresource
{
    my($ofile,$mess)=@_;

    if(! -e ".psrc"){
	open(FO,">.psrc") ||
	    &IUdie("$PRGM Could not open any file in W mode in the current tree\n");
	print FO qq~
insure++.summarize bugs
insure++.checking_uninit on
insure++.demangle full_types
insure++.symbol_table off
insure++.stack_internal on
insure++.leak_combine none
insure++.leak_search on
insure++.leak_sort size
insure++.leak_sweep on
insure++.leak_trace off
insure++.summarize leaks outstanding
insure++.uninit_flow 300
insure++.report_file $ofile
insure++.GusCacheDir /tmp
    ~;
#insure++.unsuppress all
	close(FO);
	print "$mess\n";
    }
}


sub IUCanonicalName
{
    my($func)=@_;
    $func =~ s/.*OBJ\///;
    $func =~ s/.*obj\///;
    $func =~ s/\/afs\/.*ROOT\//ROOT\//;
    $func;
}

#
# If argument match anything from what we were supposed
# to compile ...
#
sub IUFuncMatch
{
    my($func)=@_;
    my($dir,$ret);

    return "" if ( ! defined($func) );
    foreach $dir (@INSU::DIRS){
	if ($func =~ m/(.*)($dir\/.*)/){
	    return $2;
	}
    }
    return "";
}

# Return TRUE or FALSE if a routine has to be displayed
# in the Insure report or not. Based on pattern exclusion
# of course ...
sub IUListRoutine
{
    my($func) = @_;

    return &IUIfAnyMatchExclude($func,@INSU::SKIPFUNC);
}


# Return true if this error can be taken, false otherwise.
# False is based on patterns in @INSU::MSGISERR.
sub IUError
{
    my($line) = @_;
    return &IUIfAnyMatchExclude($line,@INSU::MSGISERR);
}

# Global internal routine managing both
# INSListRoutine and IUError. Based on pattern
# exclusion (if found, not OK) mechanism.
sub IUIfAnyMatchExclude
{
    my($line,@PATS)=@_;
    my($pat,$sts);

    $sts = 1==1; # i.e. take it by default
    foreach $pat (@PATS){
	if($line =~ m/$pat/i){
	    $sts = 0==1; # found a pattern, not OK then
	    last;
	}
    }
    $sts;
}

# Returns a URL reference for the error message
sub IUErrorURL
{
    my($err,$mode)=@_;
    my($perr);

    if ( ! defined($mode) ){  $mode = 0;}

    if ( $err =~ m/(\w+)(\(.*\))/){
	# some errors will come here like BAD_FORMAT(other)
	# and we need the reference to BAD_FORMAT only
	$perr = $1;
    } else {
	$perr = $err;
    }

    if( defined($INSERRORS{$perr}) ){
	($mode == 1?" ":$err).
	    " <FONT SIZE=\"-1\">(<A HREF=\"$INSURL/$INSERRORS{$perr}\">learn more</A>)</FONT>";
    } else {
	($mode == 1?" ":$err);
    }
}

# Returns a formatted reference / string relating to CVS web
sub IUCVSRef
{
    my($file)=@_;
    "<A HREF=\"$INSU::CVSWEB/$file\">$file</A>";
}



# Returns the compilation directory
sub IUCompDir
{
    my($lib)=@_;
    my($retv);

    $retv = $INSU::COMPDIR;

    if( defined($lib) ){
	if ($lib !~ /adev/){
	    if( -d "$INSU::STARAFS/$lib"){
		$retv = "$INSU::STARAFS/$lib";
	    }
	}
    }
    $retv;
}

# Submits a job to "a" queue system
# Currently used, LSF ..
#
# Arg1 : job to submit
# Arg2 : flag
#
sub IUSubmit
{
    my($job,$flag)=@_;
    my($log,$cmd,$tmpfile);

    $log = $job.".log";

    if ( ! defined($flag) ){ $flag = 0;}

    if ( -e $log ){
	print "Deleting preceding log\n";
	&ABUnlink($log);
    }

    $cmd = "$INSU::BSUB -q $INSU::QUEUE -o $log $job";
    if ( ! $flag ){
	print "$cmd\n";
    } else {
	$tmpfile = "/tmp/$$-$<.ABUtils";
	if (open(FO,">$tmpfile") ){
	    print FO
		"#!/bin/csh\n",
		"$cmd\n";
	    close(FO);
	    chmod(0700,$tmpfile);
	    print "Executing : $cmd\n";
	    system($tmpfile);
	    &ABUnlink($tmpfile);
	} else {
	    print "Could not open $tmpfile\n";
	}
    }
}






#
# This routines only checks for existence of an output file
# and prepares for a new one to appear. Arguments are
#   mode     0 returns a csh instruction
#            1 do it in perl on the fly
#   file     the filename
#
# This routine does not create an empty file.
#
sub IUCheckFile
{
    my($mode,$file)=@_;
    my($dir,$sts);

    $sts = 0;
    $file =~ m/(.*\/)(.*)/;
    ($dir,$file) = ($1,$2);
    chop($dir);

    if ($mode == 0){
	# csh mode
	$sts = "";
	$sts = "if ( -e $dir/$file-old) /bin/rm -f $dir/$file-old\n";
	$sts.= "if ( -e $dir/$file)     /bin/mv -f $dir/$file $dir/$file-old\n";

    } elsif ($mode == 1){
	# perl mode
	                            &ABUnlink("$dir/$file-old");
	if ( -e "$dir/$file")    {  &ABRename("$dir/$file","$dir/$file-old");}


    } elsif ($mode == 3){
	# csh mode
	$sts = "";
	$sts = "if ( ! -e $dir/$file) echo \"This test has never run\" >$dir/$file\n";

    } else {
	# unknown mode
	print "ABUtil:: IUCheckFile : Unknown mode $mode\n";
    }
    $sts;
}


#
# This routine move a file infile to outfile using target
# outfile versioning with filename-version.ext
# The argument $mode functions as above.
#
sub IUMoveFile
{
    my($mode,$infile,$outfile,$limit)=@_;
    my($sts,$file,$odir,$oflnm,$oext);
    my($cnt,$line);

    $cnt = $sts   = 0;

    $outfile      =~ m/(.*\/)(.*)/;
    ($odir,$oflnm)= ($1,$2);
    chop($odir);
    $oflnm        =~ m/(.*)(\..*)/;
    ($oflnm,$oext)= ($1,$2);

    while ( -e "$odir/$oflnm-$cnt$oext" && $cnt < $limit){ $cnt++;}
    if ($cnt == $limit) {
	$cnt = 0;
	&ABUnlink(glob("$odir/$oflnm-*$oext"));
    }


    if ($mode == 0){
	# csh mode ; we are cheating for the version
	$sts = "";
	$sts.= "if ( -e $odir/$oflnm$oext ) /bin/mv -f $odir/$oflnm$oext $odir/$oflnm-$cnt$oext\n";
	$sts.= "if ( -e $infile) /bin/cp -f $infile $odir/$oflnm$oext\n";

    } elsif ($mode == 1){
	# perl mode
	if ( -e "$odir/$oflnm$oext"){ &ABRename("$odir/$oflnm$oext","$odir/$oflnm-$cnt$oext");}
	if ( -e "$infile"){
	    $sts = open(FO,">$odir/$oflnm$oext") && open(FI,"$infile");
	    if ($sts){
		while ( defined($line = <FI>) ){
		    chomp($line);
		    print FO "$line\n";
		}
	    }
	    close(FI);
	    close(FO);
	}
    } else {
	# unknown mode
	print "ABUtil:: IUMoveFile : Unknown mode $mode\n";
    }
    $sts;
}

#+
# Routines to handle "a" lock file
#-

# Create a lock file. First argument is the file
# principle name and the second is a string to encrypt
# and use for uniqueness (MD5 is used). Returns the file
# name it will use. Does not create the file ...
sub IULockPrepare
{
    my($mdir,$md5seed)=@_;
    $INSU::md5seed = $md5seed;
    $INSU::FLNMLCK = $mdir."/.AutoBuild.".
	(Digest::MD5->new->add($INSU::md5seed)->hexdigest()).".lock";
    $INSU::FLNMLCK;
}

# Check and delete the lock file if older than
# unique argument 1 (a time in secondes). Returns
# 1 if it is OK to create, 0 otherwise.
sub IULockCheck
{
    my($delta)=@_;
    my($date);

    if ($INSU::FLNMLCK eq ""){  return 1;}  # nothong to check (it is OK)
    if ($INSU::LCK != 0){       return 0;}  # cannot check an opened file (logic error)

    if (-e $INSU::FLNMLCK){
	# a file is found, check date
	$date = time() - (stat($INSU::FLNMLCK))[9];
	if ( $date >  $delta ){
	    print
		"ABUtil:: LockCheck: ",
		"$INSU::FLNMLCK has a date greater than $delta. Deleting.\n";
	    &ABUnlink($INSU::FLNMLCK);
	} else {
	    print
		"ABUtil:: LockCheck: ",
		"Found a $INSU::FLNMLCK file (another process is running).\n";
	}
	return 0;
    } else {
	# nothing found, can proceed
	1;
    }
}

# Write (and create if necessary) the file. Argument(s)
# are as many info as necessary (will be chomped and \n)
sub IULockWrite
{
    my(@info)=@_;
    my($sts,$el,$ii);

    if ( $INSU::FLNMLCK eq ""){  return 0;}
    if ( -e $INSU::FLNMLCK){
	# append
	$sts = open($INSU::LCK,">>$INSU::FLNMLCK");
    } else {
	# first time
	if ($sts = open($INSU::LCK,">$INSU::FLNMLCK")){
	    print $INSU::LCK
		`uname -a`,
		"This file was created with seed [$INSU::md5seed]\n",
		"by ".&IUGetLogin()." on ".localtime()."\n";
	}
    }
    if ($sts){
	for ($ii = 0 ; $ii <= $#info ; $ii++){
	    chomp($el = $info[$ii]);
	    if ($ii == 0){
		printf $INSU::LCK "%25s %s\n","".localtime(),$el;
	    } else {
		printf $INSU::LCK "%25s %s\n","",$el;
	    }
	}
	close($INSU::LCK);
	$INSU::LCK = 0;
    }
    $sts;
}

# no arguments needed. Just delete it. Return success status.
sub IULockDelete
{
    if ( $INSU::FLNMLCK ne ""){
	if ( -e $INSU::FLNMLCK){
	    if ( ! &ABUnlink($INSU::FLNMLCK) ){
		print "ABUtil:: LockDelete: could not remove $INSU::FLNMLCK\n";
		return 0;
	    } else {
		$INSU::FLNMLCK = "";
		$INSU::LCK     = 0;
		return 1;
	    }
	}
    } else {
	return 1;
    }
}


# returns the login name (several methods)
sub IUGetLogin
{
    if( ! defined($INSU::USER) ){
	my($user) = getpwuid($<);
	if ( ! defined($user) ){
	    chomp($user = `id`);
	    $user =~ m/(uid=\d+\()(.*)\)(\s+gid=)/;
	    $user = $2;
	}
	$INSU::USER = $user;
    }
    if ($INSU::USER eq ""){ $INSU::USER = "?unknown?";}
    $INSU::USER;
}

sub IUdie {
    my($msgs)=@_;
    &IULockDelete();
    die $msgs;
}


#
# Trap for unlink - will retry a few times (5)
#
sub ABUnlink
{
    my(@files)=@_;
    my($file,$i,$cnt);
    
    $cnt = 0;
    foreach $file (@files){
	$i=0;
	UNLINKTRIAL:
	if ( -e $file ){
	    if ( ! unlink($file) ){
		if ( $i++ < 5){
		    sleep(1);
		    goto UNLINKTRIAL;
		}
	    } else {
		$cnt++;
	    }
	} else {
	    # a subtle issue, we will count as OK
	    # if the file is not present
	    $cnt++;
	}
    }
    $cnt;
}

# Trap for rename - will sync() the FS
sub ABRename
{
    my($i,$f)=@_;
    my($sts);

    $sts = rename($i,$f);
    system("/bin/sync") if ( -x  "/bin/sync");

    $sts;
}




# Returns the directories to update using cvs
sub IUSourceDirs { return @INSU::DIRS;}

# OK status from cons
sub IUconsOK { return $INSU::CONSOK;}

# Return command to load Insure environment
sub IULoad { return $INSU::INSLOAD;}

# Return command to load JProf environment.
# Currently, only 'adev' is loaded
sub JPLoad { return $INSU::JPRLOAD;}

# REturn what is needed to load valgrind (nothing)
sub VLGLoad { return $INSU::VLGLOAD;}

# Return the test list
sub IUTests { return %INSU::TESTS;}

# Returns the default run-time formatting script
sub IURTFormat { return $INSU::RTFORMAT;}

# Returns the jprof formatting program
sub JPRFFormat { return $INSU::JPFORMAT;}

# REturn the valgrind formatting program
sub VLGFormat {  return $INSU::VLGFORMAT;}

# Returns the jprof program location
sub JPRExec {    return $INSU::JPEXEC;}

# Returns the Test working directory
sub IUTestDir { return $INSU::TDIR;}

# Returns the release file name
sub IUReleaseFile { return $INSU::RELFLNM;}

# Return the mailing-list /admin accounts
sub IUManagers { return @INSU::MANAGERS;}

# Returns the HTML (or other) report directory (token required)
sub IUHtmlDir  { return $INSU::HTMLREPD;}

# Returns the HTML URL reference
sub IUHtmlRef  { return $INSU::WSERVR.$INSU::HTMLREF;}

# Returns "a" HTML output public directory (volatile ; no token)
sub IUHtmlPub  { return $INSU::HTMLPUBD;}

# Returns the queue name
sub IUQueue    { return $INSU::QUEUE;}
