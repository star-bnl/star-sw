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
if( ! -s ".$SYS"){     die ".$SYS does not exists. Could not create it\n";}

my $root = "rootdeb"; 
my $lib  = "lib";
my $bin  = "bin";

for ( my $iter=0; $iter < 2; $iter++ ) {
   # First task, don't do it twice ...
   if( ! -d ".$SYS/$root"){
       mkdir(".$SYS/$root",0775);
       if( ! -s ".$SYS/$root"){die ".$SYS/$root does not exists. Could not create it\n";}

       # Finally, we can go ahead
       print "Creating directory structure for $SYS\n";
       system("cd .$SYS/$root && $LNDIR $PWD/root ");
   } else {
       print "The directory structure already exists for $SYS\n";
   }

   # Second task, fix up a bunch of required soft-links for
   # backward infrastructure compatible layout.
   print    "if( ! -l \"$lib\")   { symlink(\".\@sys/$root/lib\",\"$lib\" );}";
   if( ! -l "$lib")   { symlink(".\@sys/$root/lib","$lib" );}
   if( ! -l "$bin")   { symlink(".\@sys/$root/bin","$bin" );}
   if( ! -l "etc")    { symlink(".\@sys/$root/etc","etc"  );}
   if( ! -l "cint")   { symlink(".\@sys/$root/cint","cint");}
   if( ! -l "icons")  { symlink(".\@sys/$root/icons","icons");}
   if( ! -l "include"){ symlink(".\@sys/$root/include","include");}

   # Beware of sysname change under linux. Fix those out.
   if( ! -l ".i386_linux22"){ symlink(".i386_redhat61",".i386_linux22");}
   if( ! -l ".i386_linux24"){ symlink(".i386_redhat72",".i386_linux24");}

   print 
       "You can now go in .$SYS/$root and run the ".
       "provided configure command\n";

   $root = "root"; 
   $lib  = "LIB";
   $bin  = "BIN";
}


