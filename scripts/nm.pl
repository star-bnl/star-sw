#!/usr/bin/perl

#
# Usage: nm.pl SymbolToLocate [LibraryPattern]
#
# (c) J.Lauret 2003-2010
#
# Add NMPL_DEBUG
#
use Digest::MD5;

$STAR       = $ENV{STAR};
$ROOTSYS    = $ENV{ROOTSYS};
$OS         = $ENV{STAR_HOST_SYS};
$CERN       = $ENV{CERN};
$CERN_LEVEL = $ENV{CERN_LEVEL};
$OPTSTAR    = $ENV{OPTSTAR};
$OSG        = $ENV{OSG};
$DEBUG      = $ENV{NMPL_DEBUG};

$SELF       = "nm.pl";


if ( !defined($ARGV[0]) ){
    die "Syntax: $0 SymbolToLocate [LibraryPattern]\n";
}

# try to locate nm
$NM = "";
foreach ( ("/usr/bin/nm","/bin/nm")){
    if ( -x $_ ){
	$NM = $_;
	last
    }
}
$NM ="nm" if ($NM eq "");


# else ...
$symb = $ARGV[0];

#
# ARG0 is the symbol but ARG1 may be a pattern
# for the library name to search.
#
if( ! defined($ARGV[1]) ) {
    $pat = "*";
} else {
    $pat = $ARGV[1];
}
if($pat !~ /\./){ $pat .= ".so";}

#
# Defines the directory to look-up.
# There is a STAR dependency here ...
#
$LIBL = "lib"; # default
if( ! defined($ARGV[2]) ){
    if ( defined($ENV{INSURE}) ) {
	$LIBL= "ILIB";
    } elsif ( defined($ENV{NODEBUG}) ){
	$LIBL = "LIB";
    } else {
	$LIBL = "lib";
    }
    $dir = "$STAR/.$OS/$LIBL";
} else {
    $dir = $ARGV[2];
}


#
# Resolve symbol 
#
if ($OS =~ m/icc/){
    chomp($CXXFILT = `echo $symb | iccfilt`);
} else {
    chomp($CXXFILT = `c++filt $symb`);
}

print 
  "Symbol demangler and search tool. Send suggestions to jlauret\@bnl.gov\n",
  "Syntax is % $SELF symbol {LibNamePattern}\n\n",
  "Searching for   $symb = $CXXFILT\n";


print "\tLooking into $dir/$pat\n";
push(@all,glob("$dir/$pat"));

# Match LIBL pattern if found, revert to "lib" otherwise
if ( -e  "$ROOTSYS/$LIBL"){
    $rs = "$ROOTSYS/$LIBL/$pat";
} else {
    $rs = "$ROOTSYS/lib/$pat";
}
print "\tAdding files from $rs\n";
push(@all,glob("$rs"));

# Also add user's directory
if ( -e  ".$OS/$LIBL"){
    print "\tAdding files from .$OS/$LIBL/$pat\n";
    push(@all,glob(".$OS/$LIBL/$pat"));
}



print "\tAdding files from $CERN/$CERN_LEVEL/lib/*.a\n";
push(@all,glob("$CERN/$CERN_LEVEL/lib/*.a"));


# also pushing OPTSTAR now but this will take time
if ( -e "$OPTSTAR/lib"){
    print "\tAdding files from $OPTSTAR/lib/$pat\n";
    push(@all,glob("$OPTSTAR/lib/$pat"));
}
    
    
    
if ( defined($OSG) ){
    print "\tAdding Globus libraries from $OSG/globus/lib/*.so\n";
    push(@all,glob("$OSG/globus/lib/*.so"));
}


# Make a common cache for this script 
umask(0000);
$CACHEDIR = "/tmp/cache_nm".(getpwuid($<))[3];
if ( ! -d $CACHEDIR ){  print "Creating $CACHEDIR\n"; 
			mkdir($CACHEDIR,0775);}


$md5 = Digest::MD5->new();
chomp($ldir = `pwd`);

foreach $file (@all){
    # in case the file dispapeared on us between search and now
    next if ( ! -e $file); 

    @stat = stat($file);
    $md5->reset();

    # better treatment of soft-links - 2010/04
    $name = "";
    if ( -l $file ){
	my($lnk)=readlink($file);
	if ( substr($lnk,0,1) eq "." ){
	    # This path is relative
	    # strip the name and replace with the relative path
	    $file =~ m/(.*\/)(.*)/;
	    my($p,$f)=($1,$2);
	    $f = $1.$lnk;
	    #print "*** $f\n" if ($file =~ m/StarRoot/);
	    $name = $f;
	} else {
	    # this path is absolute, use that name
	    $name = $lnk;
	}
    } else {
	$name = $file;
    }

    $md5->add($name);                                  # name
    $md5->add($stat[9]);                               # add mdate
    if ( substr($file,0,1) eq "."){ $md5->add($ldir);} # relative path requires additional pwd()
				    
    $digest = $md5->hexdigest();
    $FF     = "$CACHEDIR/$digest";

    # skip known files in this session
    if ( defined($SESSIONMD5{$digest}) ){
	print "$file = $SESSIONMD5{$digest}\n" if ($DEBUG);
	next;
    }
    $SESSIONMD5{$digest} = $file;

    # unlink($FF) if ( -e $FF);
    if ( ! -e "$FF" ){
	print "\tCreating cache $digest for $file\n" if ($DEBUG);
	`$NM $file >&$FF.tmp`;
	chmod(0660,"$FF.tmp");
	rename("$FF.tmp","$FF");
    }
    if ( -e $FF ){
	open(FI,"<$FF"); @SYM = <FI>; close(FI);
	chomp(@res = grep(/$symb/,@SYM));
    }

    

    if($#res != -1){
	print "$file >>\n";
	foreach $el (@res){
	    $l   = length($el);
	    $c   = 80-$l;
	    $pad = "";
	    for ( ; $c >= 0 ; $c--){  $pad .= " ";}

	    if ($el =~ /(\s+A )(.*)/){
		print "$el$pad [Absolute symbol]\n";		
	    } elsif ($el =~ /(\s+B )(.*)/){		
		print "$el$pad [bss global (unitialized data space)]\n";
	    } elsif ($el =~ /(\s+b )(.*)/){		
		print "$el$pad [bss local  (unitialized data space)]\n";
	    } elsif ($el =~ /(\s+C )(.*)/){		
		print "$el$pad [COMMON symbol]\n";
	    } elsif ($el =~ /(\s+D )(.*)/){		
		print "$el$pad [Data object symbol global]\n";
	    } elsif ($el =~ /(\s+d )(.*)/){		
		print "$el$pad [Data object symbol local ]\n";
	    } elsif ($el =~ /(\s+F )(.*)/){		
		print "$el$pad [File symbol]\n";
	    } elsif ($el =~ /(\s+G )(.*)/){		
		print "$el$pad [Small data object symbol (optimized access)]\n";
	    } elsif ($el =~ /(\s+I )(.*)/){		
		print "$el$pad [Indirect reference (GNU extension)]\n";		
	    } elsif ($el =~ /(\s+N )(.*)/){		
		print "$el$pad [Symbol has no type or is a debugging information]\n";
	    } elsif ($el =~ /(\s+L )(.*)/){		
		print "$el$pad [Thread-Local storage symbol]\n";
	    } elsif ($el =~ /(\s+R )(.*)/){		
		print "$el$pad [Read only data section]\n";
	    } elsif ($el =~ /(\s+S )(.*)/){		
		print "$el$pad [Section symbol for small objects]\n";
	    } elsif ($el =~ /(\s+T )(.*)/){		
		print "$el$pad [Text symbol or code section global (defined)]\n";
	    } elsif ($el =~ /(\s+t )(.*)/){		
		print "$el$pad [Text symbol or code section local  (defined)]\n";
	    } elsif ($el =~ /(\s+U )(.*)/){
		print "$el$pad [Undefined symbol (MUST be resolved))]\n";		
	    } elsif ($el =~ /(\s+V )(.*)/){
		print "$el$pad [Weak object symbol]\n";	
	    } elsif ($el =~ /(\s+W )(.*)/){
		print "$el$pad [Implicit weak object symbol (untagged)]\n";
	    } else {
		print "$el\n";
	    }
	}
    }
}

