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

chomp($PWD = `pwd`);
$SYS=$ENV{STAR_SYS};
$PATH = $ENV{GROUP_DIR};
$LNDIR= "$PATH/lndir";

if( ! defined($SYS) ){ die "No value set for STAR_SYS\n";}
if( ! -e $LNDIR){      die "We rely on lndir ... Could not find it\n";}

if( ! -d "root"){      die "Could not find any ./root directory\n";}

if( ! -d ".$SYS"){     mkdir(".$SYS",0775);}
if( ! -d ".$SYS"){     die ".$SYS does not exists. Could not create it\n";}

if( $PWD =~ m|^/afs|){ 
    print "AFS tree\n";
    $ANFS = ".\@sys";
    $FLINK= 1;
} else {
    print "NFS tree\n";
    $ANFS = ".$SYS";
    $FLINK= 0;    
}



# Startup values for directory names
my $root = "rootdeb"; 
my $lib  = "lib";
my $bin  = "bin";


# Loop over 2 possibilities
for ( my $iter=0; $iter < 2; $iter++ ) {
   # First task, don't do it twice ...
   if( ! -d ".$SYS/$root"){
       mkdir(".$SYS/$root",0775);
       if( ! -d ".$SYS/$root"){
	   die ".$SYS/$root does not exists. Could not create it\n";
       }

       # Finally, we can go ahead
       print "Creating directory structure for .$SYS/$root\n";
       system("cd .$SYS/$root && $LNDIR $PWD/root ");
   } else {
       print "The directory structure already exists for $SYS\n";
   }


   # Second task, fix up a bunch of required soft-links for
   # backward infrastructure compatible layout (AFS only).
   if( ! -l "$lib")   { symlink("$ANFS/$root/lib","$lib" );}
   if( ! -l "$bin")   { symlink("$ANFS/$root/bin","$bin" );}
   if( $iter == 1){
       if( ! -l "etc")    { symlink("$ANFS/$root/etc","etc"  );}
       if( ! -l "cint")   { symlink("$ANFS/$root/cint","cint");}
       if( ! -l "icons")  { symlink("$ANFS/$root/icons","icons");}
       if( ! -l "include"){ symlink("$ANFS/$root/include","include");}
   }

   if( ! $FLINK ){
       print 
	   "$lib,$bin soft-linked to $ANFS/$root/lib,bin .\n",
	   "One platform/OS support\n";
   }


   # Beware of sysname change under linux. Fix those out.
   if( ! -l ".i386_linux22"){ symlink(".i386_redhat61",".i386_linux22");}
   if( ! -l ".i386_linux24"){ symlink(".i386_redhat72",".i386_linux24");}


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
   $root = "root"; 
   $lib  = "LIB";
   $bin  = "BIN";
}


