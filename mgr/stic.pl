#!/usr/bin/env perl
use File::Basename;
use Cwd;
use File::Copy;

my $target = shift;
my @pars = ();
while (my $par = shift) {
  if ($par =~ /ROOT|ObjectSpace/) {next;} 
  push @pars, $par;
}
my $file = basename($target); 
my $dir  = dirname($target);   
my $Dir  = cwd();
my $idl  = pop @pars;

# check target path
if ($idl !~ /^\//) { $idl = $Dir . "/" . $idl;}
if (! -d $dir    && !mkdir ($dir,    0755)) {
    die "$0:Can't create directory $dir\n";
}

# From where we are, get the STIC command
my $STIC;
my $tmp;

chomp($tmp = `pwd`);
chomp($STIC=`which stic`);

if ($STIC eq ""){
    # try one more thing which is to find it locally
    # This would happen if we freshly created stic and
    # the path .$STAR_SYS/bin is not yet in the path 
    # (until a rehash)
    my $lexec=".".$ENV{STAR_SYS}."/bin/stic";
    if ( -x $lexec || -l $lexec ){
	# make it absolute 
	$STIC = $tmp."/".$lexec;
    } else {
	# Ho well ... we tried 2 method now
	die "$0 :: FATAL : Cannot find stic anywhere\n";
    }
} else {
    if ( substr($STIC,0,1) ne "/" ){
	# Not an absolute path
	$STIC = $tmp."/".$lexec;
    }
}


# create temp dir
my $tmpdir = "/tmp/" . $$;
if (! -d $tmpdir && !mkdir ($tmpdir, 0755)) {
    die "$0 :: ERROR : Can't create directory $tmpdir\n";
}

# exec command, in detach mode, environment variables may not
# be passed. We use a modified version of stic to pass an
# arbitrary string for the version.
my $STVER;
my $com;

if ( defined($STVER = $ENV{STAR_VERSION}) ){
    $com  = "cd $tmpdir && $STIC -version $STVER @pars $idl";
} else {
    $com  = "cd $tmpdir && $STIC @pars $idl";
}

my $exec = `$com`; if ($?) { exit 2; }

# copy file and delete temp directory
my $sfile = $tmpdir . "/" . $file;
if (-f $sfile) {copy ($sfile,$target);}
`rm -rf $tmpdir`; 
if ($?) { exit 2; }
exit 0;



