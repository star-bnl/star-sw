#
# A few utility routine for Spidering and constants
# so multiple scripts could share the logic.
#
# May be a bit messy for now as I moved common routines
# from scripts to a module.
#
# J. Lauret 2009
#

package SpiderUtil;
require 5.008;

require Exporter;
@ISA    = qw(Exporter);
@EXPORT = qw(GetWDir GetLogDir GetHPSSDir
	     GetScanner GetSpider GetCleaner GetTags GetCmd GetOptions
	     GetFileName
	     GetGrep GetZcat
	     CheckTime SetTimeThreshold SetTimeout
	     SetCacheLimit SetCacheOffset SetCachePrefix ToFromCache
	     ShowPerms
	     );


use vars qw($SPDR::VERSION);
$SPDR::VERSION = "V01.000";
$SPDR::NAME    = "SpiderUtil";

# Now global variables and default values
$SPDR::WDIR      = "/afs/rhic.bnl.gov/star/doc/www/html/tmp/pub/Spider";
$SPDR::LOGDIR    = "/gpfs01/star/rcf/prodlog";
$SPDR::HPSSD     = "/home/starreco";
$SPDR::SITE      = "BNL";
$SPDR::THRESHOLD = 0.9;
$SPDR::CACHELM   = 10;
$SPDR::TIMEOUT   = 7200;
$SPDR::XSELF     = undef;
$SPDR::SCAND     = undef;

# scripts should probably be concentrated as well
$SPDR::SCANNER   = "/star/u/jeromel/work/ddb/util/DBUpdate.pl";
$SPDR::SPIDER    = "/star/u/starreco/bin/DBUpdateProd.pl";
$SPDR::CLEANER   = "/star/u/jeromel/work/ddb/fC_cleanup.pl";

# disk pattern info in case a number is used
$SPDR::DISKP     = "/star/data%2.2d";




#+
# Handles caching
#-
sub SetCacheLimit
{ 
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 
    my($val)=@_;
    if ( ! defined($val) ){ $val = -1;}
    if ( $val > 0){         $SPDR::CACHELM = $val;}
    return $SPDR::CACHELM;
}
sub SetCacheOffset
{ 
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 
    my($val)=@_;
    if ( ! defined($val) ){ $val = -1;}
    if ( $val > 0)        { $SPDR::CACHOFF = $val;}
    return $SPDR::CACHOFF;
}
sub SetCacheName
{   
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 
    my($val)=@_;
    if ( ! defined($val) ){ $val = "";}
    if ( $val ne ""){   $SPDR::SCAND = $val;}
    return $SPDR::SCAND;
}



#
# -2  => Return when cache will be flushed
# -1  => delete cache
#  0  => from cache
#  1  => to cache
#
sub ToFromCache
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 

    my($mode,@ALL)=@_;
    my(@TEMP,$kk);
    my(%RECORDS,$file);
    my(@count)=(0,0,0);


    # Check values we need
    if ( ! defined($SPDR::SELF) ){
	$SPDR::SELF  = $0; 
	$SPDR::SELF =~ s/.*\///; 
	$SPDR::SELF =~ s/\..*//;
    }
    if ( ! defined($SPDR::XSELF) ){
	$SPDR::XSELF  = $SPDR::SELF;
	$SPDR::XSELF .= $SPDR::SCAND if ( $SPDR::XSELF !~ m/$SPDR::SCAND/);
	$SPDR::XSELF  =~ s/[+\/\*]/_/g; 
    }
    if ( ! defined($SPDR::CACHELM) ){  $SPDR::CACHELM = 10;} # cache limit
    if ( ! defined($SPDR::CACHOFF) ){  $SPDR::CACHOFF =  0;} # cache offset


    # get cache count for this pass
    if ( ! defined($SPDR::CACHECNT) ){
	$kk=0;
	$SPDR::CACHEDIR = $ENV{HOME}."/.cache";
	if ( ! -e $SPDR::CACHEDIR ){ mkdir($SPDR::CACHEDIR);}
	while ( -e "$SPDR::CACHEDIR/$SPDR::XSELF"."_$kk.lis"){  $kk++;}
	if ( $kk > ($SPDR::CACHELM+$SPDR::CACHOFF) ){
	    # we have exhausted all numbers
	    print "Cache is being reset (reached limit)\n";
	    unlink(glob("$SPDR::CACHEDIR/$SPDR::XSELF"."_*.lis"));
	    $SPDR::CACHECNT = 0;
	    return @ALL;
	}
	    
	# else we have something
	$SPDR::CACHECNT = $kk;

	print "Cache file will be $SPDR::CACHEDIR/$SPDR::XSELF"."_$SPDR::CACHECNT.lis\n";
    }


    #+
    # Treat the cases now
    #-
    if ($mode == -1){
	# delete all cache pretty much now
	print "Deleting cache\n";
	unlink(glob("$SPDR::CACHEDIR/$SPDR::XSELF"."_*.lis"));

    } elsif ($mode == -2){
	# return when cache will be deleted
	return (($SPDR::CACHELM+$SPDR::CACHOFF)-$SPDR::CACHECNT);

    } elsif ( $mode == 0 || $mode == 1){
	# From cache, return an array of values, second argument
	# must be present

	# To cache - We want to flush all to cache
	# It is then only  a question of adding previous to current

	# In both cases, we start the same way by reading the
	# previous cache
	if ($SPDR::CACHECNT != 0){
	    # there is a previous $kk-1 file
	
	    # read cache, suck into %RECORDS
	    if ( open(OCACHE,"$SPDR::CACHEDIR/$SPDR::XSELF"."_".($SPDR::CACHECNT-1).".lis") ){
		while ( defined($file = <OCACHE>) ){  
		    chomp($file);
		    $RECORDS{$file}=1;
		    $count[0]++;
		}
		close(OCACHE);
	    }
	}
	
	
	if ( $mode == 1 ){
	    # ToCache - need to merge all files together
	    foreach $file (@ALL){
		chomp($file);
		$RECORDS{$file} = 1 if ( ! defined($RECORDS{$file}) );
	    }


	    # Open the real cache filer now not the index -1 and dump it all
	    if ( open(OCACHE,">$SPDR::CACHEDIR/$SPDR::XSELF"."_".($SPDR::CACHECNT-0).".lis") ){
		foreach $file ( keys %RECORDS ){
		    print OCACHE "$file\n";
		}
		close(OCACHE);
	    }

	} elsif ( $mode == 0){
	    # Now we either have previous records or not
	    # scan @ALL and rebuild the list excluding what is
	    # in the cache
	    foreach $file (@ALL){
		chomp($file);
		if ( ! defined($RECORDS{$file}) ){
		    $count[1]++;
		    push(@TEMP,$file);
		} else {
		    $count[2]++;
		}
	    }

	    # are done, @TEMP contains the files which are new
	    if ($count[1] != $count[2]){
		print "Previous pass had $count[0], found $count[2] old and selected $count[1]\n";
	    }
	    return @TEMP;
	}
    } else {
	print "Mode $mode not implemented\n";
    }

}


#+
# Some Unix utilities
#-
# This sub has been taken from fcheck software
# as-is (was too lazzy to get it done by myself)
sub ShowPerms
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 
    local ($mode) = @_;

    local (@perms) = ("---", "--x",  "-w-",  "-wx",  "r--",  "r-x",  "rw-",  "rwx");
    local (@ftype) = ("?", "p", "c", "?", "d", "?", "b", "?", "-", "?", "l", "?", "s", "?", "?", "?");
    local ($setids) = ($mode & 07000)>>9;
    local (@permstrs) = @perms[($mode & 0700) >> 6, ($mode & 0070) >> 3, ($mode & 0007) >> 0];
    local ($ftype) = $ftype[($mode & 0170000)>>12];
    if ($setids){
	# Sticky Bit?
	if ($setids & 01) { $permstrs[2] =~ s/([-x])$/$1 eq 'x' ? 't' : 'T'/e; }
	# Setuid Bit?
	if ($setids & 04) { $permstrs[0] =~ s/([-x])$/$1 eq 'x' ? 's' : 'S'/e; }
	# Setgid Bit?
	if ($setids & 02) { $permstrs[1] =~ s/([-x])$/$1 eq 'x' ? 's' : 'S'/e; }
    }
    return (join('', $ftype, @permstrs));
}



#+
#   Handles timer
#-
# return true false if we have timed-out
# Call with  0 to initialize the timer
# Call with a value > 0 to initialize timeout time
# Call with -1 to check timer
# Call with -3 to return laps time
sub CheckTime
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 

    my($w)=@_;

    #print "I am being passed $w\n";

    if ($w == 0){
	# Init timer
	$SPDR::STARTT = time();
    } elsif ($w > 0){
	# set timeout
	$SPDR::TIMEOUT = $w;
    } elsif ($w == -3){
	return (time()-$SPDR::STARTT);
    } else { # < 0
	return 0 if ( ! defined($SPDR::TIMEOUT) );
	return (time()-$SPDR::STARTT) > $SPDR::THRESHOLD*$SPDR::TIMEOUT; 
    }
}

sub SetTimeThreshold 
{ 
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 

    my($val)=@_;

    if ($val > 1){  $val /= 100;}            # assume %tage
    if ($val > 0){ $SPDR::THRESHOLD = $val};

    return  $SPDR::THRESHOLD;
}

sub SetTimeout
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 
    
    my($val)=@_;

    if ( ! defined($val) ){ $val = 0;}
    if ($val > 0){  $SPDR::TIMEOUT = $val;} 
    return $SPDR::TIMEOUT;
}


#+
# Get directories, commands,unified tags and options
#-
sub GetWDir    {  return $SPDR::WDIR;}
sub GetLogDir  {  return $SPDR::LOGDIR;}
sub GetHPSSDir {  return $SPDR::HPSSD;}

sub GetScanner {  return $SPDR::SCANNER;}
sub GetSpider  {  return $SPDR::SPIDER;}
sub GetCleaner {  return $SPDR::CLEANER;}

# Some Unix program location
sub GetGrep
{
    my($grep);

    if ( $^O eq "solaris" && -x "/usr/xpg4/bin/grep"){ 
	$grep = "/usr/xpg4/bin/grep";
    } else {
	$grep = "/bin/grep";
    }
    return $grep;
}

sub GetZcat
{
    my($zcat);

    if ( $^O eq "solaris" && -x "/star/starlib/opt/sun4x_59/bin/zcat"){   
	# <--- This is a hack due to AFS /opt/star
	$zcat = "/star/starlib/opt/sun4x_59/bin/zcat";} # collapse
    elsif (-x "/usr/local/bin/zcat"){ $zcat = "/usr/local/bin/zcat";}
    elsif (-x "/usr/bin/zcat"){       $zcat = "/usr/bin/zcat";}
    else {                            $zcat = "/bin/zcat";}
}


# Get commands for Scanner, Spider and Cleaner
sub GetCmd
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);}
    my($arg)=@_;

    if ($arg eq "Scanner" || $arg eq "Scan"){
	return &GetScanner();

    } elsif ($arg eq "Spider"){
	return &GetSpider();

    } elsif ($arg eq "Cleaner" || $arg eq "Clean" || $arg eq "Check"){
	return &GetCleaner();

    } else {
	return undef;
    }
}

sub GetOptions
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);}  

    my($arg)=@_;

    if ($arg eq "Scanner" || $arg eq "Scan"){
	return ("-cache","-z","-t",$SPDR::TIMEOUT,"-o","%%OUTPUT%%","-of",
		"%%MARKER%%","%%DISK%%");

    } elsif ($arg eq "Spider"){
	return ("-cache","-z","-t",$SPDR::TIMEOUT,"-o","%%OUTPUT%%","%%DISK%%");

    } elsif ($arg eq "Cleaner" || $arg eq "Clean" || $arg eq "Check"){
	return ("-check","-doit","-t",$SPDR::TIMEOUT,"-o","%%OUTPUT%%",
		"-cond","storage=NFS,site=$SPDR::SITE,path~%%DISK%%")

    } else {
	return undef;
    }
}

# Initial stupidity simplified 
# Returns a prefix for the log and lock and a marker file (enable/disable)
# Extension will differ.
sub GetTags
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 
    my($arg)=@_;

    if ($arg eq "Scanner" || $arg eq "Scan"){
	return ("FC","SPDR");

    } elsif ($arg eq "Spider"){
	return ("SD","SPDR")

    } elsif ($arg eq "Cleaner" || $arg eq "Clean" || $arg eq "Check"){
	return ("CK","CHKR");

    } else {
	return undef;
    }

}

# Composite command to make it even simpler
#   Arg0 0 -> use tag / 1 -> use marker
#   Arg1 One of Spider, Scanner, Cleaner
#   Arg2 Disk number or name
#
sub GetFileName
{
    if ($_[0] =~ m/$SPDR::NAME/) {   shift(@_);} 

    my($m,$arg,$disk)=@_;
    my($tag,$mrk)= &GetTags($SPDR::NAME,$arg);
    my($prefix);

    die "Argument 2=$arg is not valid" if ( !defined($tag) || ! defined($mrk) );
    $prefix = ($m==0?$tag:$mrk);
    
    # wildcard will be allowed for globing
    if ( ! defined($disk) ){  $disk = "*";} 
    elsif ($disk*1 == 0){     $disk =~ s/\//_/g;}
    else {                    $disk = sprintf($SPDR::DISKP,$disk); $disk =~ s/\//_/g;}
	
    return &GetWDir()."/".$prefix.$disk;
}




# Standard constructor
sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self  = {};

  bless ($self , $class);
  # $self->_initialize();

  return $self;
}





 



1;

