#!/usr/bin/env perl

#
# Small interface to create the appropriate
# directory structure and support multiple 
# instance of root vs OS. Compilation should
# be done separatly because the option of
# the ROOT-provided configure script may
# change and it would be difficult to provide
# a transparent support.
#
# If an argument is specified, only one version
# of the tree will be created. Usually, this is
# the case on non-linux system only (usefull for
# cross Linux platform)
#
# Usage:
#   % MakeRootDir.pl 
#   % MakeRootDir.pl  {1|2}
#   % MakeRootDir.pl  Grootdeb 
#

chomp($PWD = `pwd`);
$SYS  = $ENV{STAR_HOST_SYS};
$SSYS = $ENV{STAR_SYS};
$PATH = $ENV{GROUP_DIR};
$LNDIR= "$PATH/lndir";

if( ! defined($SYS) ){ die "No value set for STAR__HOST_SYS\n";}
if( ! -e $LNDIR){      die "We rely on lndir ... Could not find it\n";}

if( ! -d "root"){      die "Could not find any ./root directory\n";}

if( ! -d ".$SYS"){     mkdir(".$SYS",0775);}
if( ! -d ".$SYS"){     die ".$SYS does not exists. Could not create it\n";}

if( $PWD =~ m|^/afs|){ 
    if ( $SSYS ne $SYS ){
	$ANFS = ".$SYS";
	$FLINK= 0;
    } else {
	print "AFS tree\n";
	$ANFS = ".\@sys";
	$FLINK= 1;
    }
} else {
    print "NFS tree\n";
    $ANFS = ".$SYS";
    $FLINK= 0;    
}


# Startup values for directory names
my $pass = 0;
my $root = "rootdeb"; 
my $lib  = "lib";
my $bin  = "bin";

my $COMPAT=0; # will remove soon


if ( defined($ARGV[0]) ){
    my($tmp)=$ARGV[0];

    if ( $tmp !~ m/\d+/ ){
	# if string, assume version
	$root = $ARGV[0];
	print "Would create .$SYS/$root - please confirm: ";
	chomp($ans = <STDIN>);
	exit if ($ans !~ m/y/i);
    } else {
	# Assume syntax $0 {1|2} for one version
	$pass = $tmp;
    }
}


# Loop over 2 possibilities
for ( my $iter=0; $iter < 2; $iter++ ) {
    
   if( $pass == 2 ){ 
       $pass--;
       print "Skippping iteration $iter for $root\n";
       goto NEXTPASS;
   }

   # First task, don't do it twice ...
   if(  -d ".$SYS/$root"){
       print "The directory structure already exists for $SYS\n";
   } else {
       mkdir(".$SYS/$root",0775);
   }
   if( ! -d ".$SYS/$root"){
       die ".$SYS/$root does not exists. Could not create it\n";
   }

   # Finally, we can go ahead
   print "Creating directory structure for .$SYS/$root\n";
   system("cd .$SYS/$root && $LNDIR $PWD/root ");

   if ($COMPAT){
       # Second task, fix up a bunch of required soft-links for
       # backward infrastructure compatible layout (AFS only).
       if( ! -l "$lib")   { symlink("$ANFS/$root/lib","$lib" );}
       if( ! -l "$bin")   { symlink("$ANFS/$root/bin","$bin" );}
       if( $iter == 0){
	   if( ! -l "etc")      { symlink("$ANFS/$root/etc","etc"  );}
	   if( ! -l "cint")     { symlink("$ANFS/$root/cint","cint");}
	   if( ! -l "icons")    { symlink("$ANFS/$root/icons","icons");}
	   if( ! -l "include")  { symlink("$ANFS/$root/include","include");}
	   if( ! -l "tutorials"){ symlink("$ANFS/$root/tutorials","tutorials");}
	   if( ! -l "fonts")    { symlink("$ANFS/$root/fonts","fonts");}
	   if( ! -l "test")     { symlink("$ANFS/$root/test","test");}
       }
       # Backward support for old scripts
       if( ! -l "$lib/libStar.so")   { symlink("libTable.so","$lib/libStar.so" );}
   }


   if( ! $FLINK ){
       print 
	   "$lib,$bin soft-linked to $ANFS/$root/lib,bin .\n",
	   "One platform/OS support\n";
   }


   # Beware of sysname change under linux. Fix those out.
   # order is oldfile= expected value for $STAR_HOST_SYS
   # 6.1 support
   #if( ! -l ".i386_linux22"){ symlink(".i386_redhat61",".i386_linux22");}
   # 7.x support
   #if( ! -l ".i386_redhat72"){ symlink(".i386_linux24",".i386_redhat72");}
   #if( ! -l ".rh73_gcc296"){   symlink(".i386_linux24",".rh73_gcc296");}
   # 8.0 support. Starting here, the sysname changed
   #if( ! -l ".i386_redhat80"){ symlink(".rh80_gcc32",".i386_redhat80");}


   # Little extra message for guidance
   if( $iter == 0){
       # non-Optimized
       print
	   "Non Optimized/Debug version will require extra configure ".
	       "script options. This is not mandatory. Tree is ".
		   ".$SYS/$root\n";
   } else {
       # Optimized
       print 
	   "You can now go in .$SYS/$root and run the ".
	       "provided configure command\n";
   }

   # Change name between 0 and 1 ($iter)
   if( $^O ne "linux" || $pass == 1 ){ last;}
 NEXTPASS:
   $root = "root"; 
   $lib  = "LIB";
   $bin  = "BIN";
}


