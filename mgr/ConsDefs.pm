# $Id: ConsDefs.pm,v 1.145 2017/01/24 14:14:21 fisyak Exp $
{
 use File::Basename;
 use Sys::Hostname;
 use Cwd;
 use Env;
 use File::Find();
 use Sys::Hostname;
 #________________________________________
 *name     = *File::Find::name;
 *prune    = *File::Find::prune;
 *dir      = *File::Find::dir;
 *topdir   = *File::Find::topdir;
 *topdev   = *File::Find::topdev;
 *topino   = *File::Find::topino;
 *topmode  = *File::Find::topmode;
 *topnlink = *File::Find::topnlink;
 
 if ( !$ROOT_LEVEL )   { print "ROOT_LEVEL has to be defined\n"; exit 1;}
 if ( !$ROOTSYS )      { print "ROOT_SYS   has to be defined\n"; exit 1;}
 if ( !$STAR_HOST_SYS) { print "STAR_HOST_SYS   has to be defined\n"; exit 1;}
 if ( !$CERN_ROOT)     { print "CERN_ROOT   has to be defined\n"; exit 1;}
 if ( !$OPTSTAR ) { $OPTSTAR = "/opt/star"; } # print "OPTSTAR = $OPTSTAR\n"; die;
 if ( !$XOPTSTAR or $XOPTSTAR eq '/dev/null') { $XOPTSTAR = $OPTSTAR;} # print "OPTSTAR = $OPTSTAR\n"; die;
 my $host = hostname;
 $BUILD   = "#." . $STAR_HOST_SYS; print "build for $BUILD on host $host\n" unless ($param::quiet);
 $INCLUDE = $BUILD  . "/include";
 
 @search_files = ();
 my $ROOT_VERSION = `root-config --version`;
 chomp($ROOT_VERSION);
 my ($ROOT_MAIN,$ROOT_MINOR) = split ('/',$ROOT_VERSION); #print "ROOT_VERSION = $ROOT_VERSION => $ROOT_MAIN $ROOT_MINOR\n";
 $PLATFORM      = `root-config --platform`; chomp($PLATFORM);
 $ARCH          = `root-config --arch`; chomp($ARCH);
 $CC           = `root-config  --cc`; chomp($CC); # print "CC = $CC\n";
 if (! $CC) {die "ROOT has not been set !";}
 $CXX          = `root-config --cxx`; chomp($CXX);
 $SO           = $CXX;
 $FC           = `root-config --f77`; chomp($FC);
 $LD           = `root-config  --ld`; chomp($LD);
 $LDFLAGS      = `root-config  --ldflags`; chomp($LDFLAGS); 
 $LDFLAGS     .= " -rdynamic";
 $SOFLAGS      = $LDFLAGS;
 
 $SOFLAGS  .= " -shared -Wl,-Bdynamic";
 
 $XLIBS     = "-L/usr/X11R6/$LLIB -lXpm -lX11";
 
 $THREAD    = "-lpthread";
 $CRYPTLIBS = "-lcrypt";
 
 $SYSLIBS   = "-lm -ldl";
 $CLIBS    .= " -lm -ldl";
 $SYSLIBS   .= " -lrt -rdynamic";
 $CLIBS     .= " -lrt -rdynamic";
 $F77LD        = $LD;
 $CXXOPT        = "";
 
 $FCPATH        = "";
 
 
 $CPP           = $CC . " -E -P";
 $CPPPATH       = "";
 $CPPFLAGS      = "";
 $FPP           = $CPP;
 $AR            = "ar";
 $ARFLAGS       = "rvu";
 $LDEXPORT      = " -Wl,-export-dynamic -Wl,-noinhibit-exec,-Bdynamic";
 $LDALL         = " -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs";
 $LDNONE        = " -Wl,--no-whole-archive -Wl,-Bdynamic";
 $F77LD         = $LD;
 $F77LDFLAGS    = $LDFLAGS;
 $STIC          = "stic";
 $STICFLAGS     = "";
 $AGETOF        = "agetof";
 $AGETOFLAGS    = "-V 1";
 
 $KUIP          = $CERN_ROOT . "/bin/kuipc";
 $ROOTCINT      = $ROOTSYS . "/bin/rootcint";
 my $RLIBMAP    = $ROOTSYS . "/bin/rlibmap";
 if ($RLIBMAP and ! -e $RLIBMAP) {$RLIBMAP = "";}
 if ($RLIBMAP) {
   my ($M,$S,$V) = split('\.',$ROOT_LEVEL);
   if ($M <4 or $M == 4 and $S == 0) {$RLIBMAP = "";}
#   print "ROOT_LEVEL = $ROOT_LEVEL => M,S,V = $M,$S,$V => RLIBMAP = $RLIBMAP ==================================================\n"; 
 } 
 $CXX_VERSION  = `$CXX -dumpversion`;
 chomp($CXX_VERSION);
 ($CXX_MAJOR,$CXX_MINOR) = split '\.', $CXX_VERSION;
 my $cxx_version = $CXX_MAJOR . ".". $CXX_MINOR;
 my $CXXFLAGS     = `root-config --auxcflags`; chomp($CXXFLAGS); #$CXXFLAGS  =~ s/-I.*//; 
# print "CXXFLAGS = $CXXFLAGS\n";
 $CXXFLAGS    .= " -fPIC"; #$LDFLAGS;
# $CXXFLAGS    .= " -Wall -Wextra -Wno-long-long  -Wabi"; # garfield
 $CFLAGS       = $CXXFLAGS;
 my @words     = split(' ',$CFLAGS);# print "words = @words\n";
 my $cflags    = "";
 foreach my $w (@words) {
   next if ($w =~ /c++/);
   if ($cflags) {$cflags .= " ";}
   $cflags   .= $w;
 }
 $CFLAGS = $cflags;
 $FFLAGS       = $CFLAGS;
 $LIBSTDC       = `$CC $CFLAGS -print-file-name=libstdc++.a | awk '{ if (\$1 != "libstdc++.a") print \$1}'`;
 chomp($LIBSTDC);
 $LIBG2C        = "";
 
 $LIBS          = "";
 $Libraries     = "";
 $CLIBS         = "";
 $FLIBS         = "";
 $XLIBS         = "";
 $THREAD        = "";
 $CERNLIBS      = "";
 $CRYPTLIBS     = "";
 $SYSLIBS       = "";
 $OSFID         = "";
 $OSFCFID       = "";
 $date   = `date +\%d\%b-%T`;
 $EXTRA_CPPFLAGS = "";
 $EXTRA_CPPPATH = "";
 $EXTRA_CXXFLAGS= "";
 $EXTRA_FCPATH  = "";
 $EXTRA_FPPFLAGS= "";
 $EXTRA_LDFLAGS = "";
 # MACOSX
 $FINK_CXXFLAGS= "";
 $FINK_CFLAGS= "";
 $FINK_LDFLAGS = "";
 $CERNLIB_FPPFLAGS = "-DCERNLIB_TYPE -DCERNLIB_UNIX -DCERNLIB_BSLASH -DCERNLIB_DZDOC -DCERNLIB_SHL -DCERNLIB_NONEWL -DCERNLIB_HIGZ -DCERNLIB_CG -DCERNLIB_HADRON -DCERNLIB_COMIS -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";# -DCERNLIB_GCALOR;
 my $LLIB = "lib";
 if (! $USE_64BITS and $STAR_HOST_SYS =~ /darwin/) { $USE_64BITS = "yes";}
 if (  $USE_64BITS) { $LLIB = "lib64";}
 if ($USE_64BITS){
#    $CXXFLAGS .= " -m64";
#    $CFLAGS   .= " -m64";
#    $FFLAGS   .= " -m64";
   $CERNLIB_FPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
 } else {
#    $CXXFLAGS .= " -m32";
#    $CFLAGS   .= " -m32";
#   $FFLAGS   .= " -m32";
 }
 # Vc: gcc :  -Wno-unused-function  -Wno-parentheses  -Wno-strict-aliasing -Wno-uninitialized 
 if ($CXX eq "g++" or $CXX eq "gcc") {
    $CXXFLAGS .= " -Wall -Woverloaded-virtual -Wcast-align -fno-threadsafe-statics";
    $CFLAGS   .= " -Wall -Wcast-align";
    $FFLAGS   .= " -fPIC -pipe";
    $FFLAGS   .= " -std=legacy -fno-second-underscore -fno-automatic -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wsurprising -Wcast-align"; # -Wline-truncation  -W
   $FEXTEND = "-ffixed-line-length-132";
   $CERNLIB_FPPFLAGS .= " -DCERNLIB_GCC" . $CXX_MAJOR;
   
 } else {
   if ($CXX eq "icc" or $CXX eq "icpc") {
     $SOFLAGS      .= " -shared -u*";
     if ($USE_64BITS){
       $LDFLAGS  .= " -m64";
       $SOFLAGS  .= " -m64";
       $CXXFLAGS .= " -m64";
       $CFLAGS   .= " -m64";
       $FFLAGS   .= " -m64";
     } else {
       $LDFLAGS  .= " -m32";
       $SOFLAGS  .= " -m32";
       $CXXFLAGS .= " -m32";
       $CFLAGS   .= " -m32";
       $FFLAGS   .= " -m32";
     }
     if ($FC eq 'ifort') {# To use cernlib done with gfortran
       $FEXTEND = "-132";
     }
   }
   $LIBG2C  = `$CC $CFLAGS  -print-file-name=libg2c.a | awk '{ if (\$1 != "libg2c.a") print \$1}'`;
   chomp($LIBG2C);
 }
 if    ($FC eq 'gfortran') {
   $FLIBS .= " -lgfortran";
 } elsif ( $FC eq 'ifort') {
   my $lib = `which $FC`; chomp($lib);
   $FLIBS      .= " -lifcore -lifport";
   $GFLAGS    = $FFLAGS;
   $GFLAGS   =~ s/-axAVX//;
   #   print "GFLAGS = $GFLAGS, FLIBS = $FLIBS ==========================\n";
   my @flibs = qw(libgfortran.a libquadmath.a libgfortranbegin.a);
   foreach my $flib (@flibs) {
     my $f = `gfortran $GFLAGS -print-file-name=$flib`; chomp($f);
     if ($f != $flib) {$FLIBS      .= " " . $f;}
   }
   #   print "FLIBS = $FLIBS ================================================================================\n";
 }
 if ($STAR_HOST_SYS !~ /darwin/ ) {
   $CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX";
 } else {
   $CERNLIB_FPPFLAGS .= " -Dunix=unix -D__DARWIN__";
 }
 $CERNLIB_CPPFLAGS = $FPPFLAGS;
 if ( $FC eq "gfortran" ){
   # TODO: Possible cleanup to do between GFORTRAN and CERNLIB_LINUX
   $CERNLIB_FPPFLAGS .= " -DCERNLIB_GFORTRAN";
 }
 

 #     print "CXX = $CXX, CXXFLAGS = $CXXFLAGS, CC = $CC, CFLAGS = $CFLAGS\n";
 # print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
 # print "CC = $CC CXX = $CXX FC = $FC LD = $LD\n";
 $DEBUG        = "-g";
 $FDEBUG       = $DEBUG;
 $NOOPT        = "";
 if ( defined( $ARG{NODEBUG} ) || $NODEBUG ) {
   $DEBUG = $ENV{DEBUG_OPTIONS}||"-O2 -g";
   $FDEBUG= $DEBUG;
   print "Base DEBUG options = $DEBUG\n" unless ($param::quiet);
 }
 if ( (defined( $ARG{NODEBUG} ) or $NODEBUG) && !defined($ENV{DEBUG_OPTIONS}) ) {
   #	$CXXFLAGS    .= " -Wno-long-long";
   #       print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
   if ($CXX_VERSION < 3){
     $optflags = "-malign-loops=2 -malign-jumps=2 -malign-functions=2";
   } elsif ( $CXX_VERSION < 4.5 ){
     # this naiming convention starts at gcc 3.2 which happens to
     # have a change in the options.
     # Valid and used up to 4.4.7
     $optflags = "-falign-loops=2 -falign-jumps=2 -falign-functions=2";
   }
   # JL patch for gcc 4.1 -> 4.3.x (report that it is broken in 4.4 as well)
   if ( $STAR_HOST_SYS =~ m/(_gcc4)(\d+)/ ){
     print "Notice: Enabling gcc patch for V4.x series\n";
     if ( $2 <= 49 ){
       # Note: all inlining failed with gcc 4.4.7 with no indication
       # of a resolve up to 4.4.9 . Symbols would be removed and
       # linking would fail.
       $DEBUG .= " -fno-inline";
     }
     
     $DEBUG .= " ".$optflags;
     $FDEBUG = $DEBUG;
     print "set DEBUG = $DEBUG\n" unless ($param::quiet);
   }
 }
 
 my $rootlibs = `root-config --nonew --libs`; chomp($rootlibs);
 my @List = ();
 my $threadlib = "";
 foreach my $f (split ' ', $rootlibs) {
   next if $f =~ /^-L/;
   if ($f =~ /thread/) {$threadlib = " " . $f; next;}
   next if $f !~ /^-l/ or $f eq '-lm' or $f eq '-ldl';
   push @List, $f;
 }
 my $ROOTLIBS = join ' ', @List;
 # print "ROOTLIBS = $ROOTLIBS   threadlib = $threadlib\n";

 chomp($HOST = `/bin/hostname`);

 $CINTSYSDIR    = $ROOTSYS . "/cint";
 $INCLUDE_PATH = $INCLUDE;
 $Salt = undef;
 $NoKeep = undef;
 $_ = $STAR_HOST_SYS;
 print "System: ", $_, "\n" unless ($param::quiet);
 # ============================================================
 # Platform support should be concentrated here
 # Above was a generic gcc support
 # ============================================================
 my($packl,$cernl,$kernl,$strip);
 
 $strip = "";
 # now check
 if ( -e "$CERN_ROOT/lib/libpacklib_noshift.a" &&
      -e "$CERN_ROOT/lib/libkernlib_noshift.a") {
   $packl = "packlib_noshift";
   $kernl = "kernlib_noshift";
   if (-e "$CERN_ROOT/bin/cernlib_noshift"){
     $cernl = "$CERN_ROOT/bin/cernlib_noshift";
   } else {
     $cernl = "cernlib";
   }
 } elsif ( -e "$CERN_ROOT/lib/libpacklib-noshift.a" &&
	   -e "$CERN_ROOT/lib/libkernlib-noshift.a") {
   $packl = "packlib-noshift";
   $kernl = "kernlib-noshift";
   if (-e "$CERN_ROOT/bin/cernlib-noshift"){
     $cernl = "$CERN_ROOT/bin/cernlib-noshift";
   } else {
     $cernl = "cernlib";
   }
   
 } else {
   print "WARNING: using cernlib from the default path\n"
     unless ($param::quiet);
   #	    $cernl = "cernlib -s";
   $cernl = "cernlib";
   $packl = "packlib";
   $kernl = "kernlib";
 }
 $CERNLIBS .= " " . `$cernl pawlib packlib graflib/X11 packlib mathlib kernlib`;
 $CERNLIBS =~ s/packlib\./$packl\./g;
 $CERNLIBS =~ s/kernlib\./$kernl\./g;
 $CERNLIBS =~ s/$strip//g     if ($strip ne "");
 #	$CERNLIBS =~ s/lib /lib64 /g if ($USE_64BITS);
 chop($CERNLIBS);
 if ( $STAR_HOST_SYS !~ /darwin/ ) {
   $CERNLIBS =~ s#lX11#L/usr/X11R6/$LLIB -lX11#;
 }
 print "CERNLIB = $CERNLIBS\n" unless ($param::quiet);
 if ( $STAR_HOST_SYS =~ /darwin/) {
   #      print "===================== $STAR_HOST_SYS ===============\n";
   $CXX_VERSION  = `$CXX -dumpversion`;
   chomp($CXX_VERSION);
   ($CXX_MAJOR,$CXX_MINOR) = split '\.', $CXX_VERSION;  
   #      print "CXX_VERSION : $CXX_VERSION MAJOR = $CXX_MAJOR MINOR = $CXX_MINOR\n";
   # OS version
   $MACOSX_MINOR = `sw_vers | sed -n 's/ProductVersion://p' | cut -d . -f 2`; chomp($MACOSX_MINOR);
   $MACOSX_CPU   = `uname -p`; chomp($MACOSX_CPU);
   $FINK_DIR     = `which fink | sed -ne "s/\\/bin\\/fink//p"`; chomp($FINK_DIR);
   # Extras
   if (-d $FINK_DIR) {
     if (-d "$FINK_DIR/include") {
       $FINK_CXXFLAGS = "-I$FINK_DIR/include"; $CXXFLAGS .= " -fno-stack-protector";
       $FINK_CFLAGS = "-I$FINK_DIR/include";   $CFLAGS   .= " -fno-stack-protector";
     }
     if (-d "$FINK_DIR/lib") {$FINK_LDFLAGS = "-L$FINK_DIR/lib";}
   }
   if ($CXX eq 'g++') {
     if ($CXX_MAJOR > 4 or $CXX_MAJOR == 4 and $CXX_MINOR >= 6) {$CXXFLAGS .= " -fpermissive";}
   } else {# clang
     $CXXFLAGS    .= " -std=c++11";
   }
   #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
   if (! $EXTRA_CPPPATH) {$EXTRA_CPPPATH  =                         $FINK_CXXFLAGS;}
   else                  {$EXTRA_CPPPATH .= $main::PATH_SEPARATOR . $FINK_CXXFLAGS;}
   $CINTCXXFLAGS  = $CXXFLAGS .
     " -fsigned-char -fno-common $FINK_CXXFLAGS " .
       "-DG__REGEXP -DG__UNIX -DG__SHAREDLIB " .
	 "-DG__ROOT -DG__REDIRECTIO -DG__OSFDLL " .
	   "-DG__STD_EXCEPTION";
   $CINTCFLAGS    = $CFLAGS .
     " $FINK_CFLAGS -DG__REGEXP -DG__UNIX -DG__SHAREDLIB " .
       "-DG__ROOT -DG__REDIRECTIO -DG__OSFDLL -DG__STD_EXCEPTION";
   
   # Linker:
   $LDEXPORT      = "";
   $LDALL         = "-Wl,-force_load"; #"-Wl,-all_load";
   $LDNONE        = "";#"-Wl,-noall_load";
   $LDFLAGS       .= " -mmacosx-version-min=10.$MACOSX_MINOR";
   $SOFLAGS       = "-dynamiclib -single_module -undefined dynamic_lookup";#  suppress";# -install_name $(LIBDIR)/
   $CXXFLAGS     .= " -Wno-long-double";
   #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
   $CFLAGS       .= " -Wno-long-double";
   $CINTCXXFLAGS .= " -Wno-long-double";
   $CINTCFLAGS   .= " -Wno-long-double";
   $SOFLAGS  .= " -install_name \@rpath/%>";
  # System libraries:
   $OSTHREADLIBDIR = "";
   $OSTHREADLIB = "";
   $SYSLIBS       = "-lm $FINK_LDFLAGS $OSTHREADLIBDIR $OSTHREADLIB -ldl";
   $FFLAGS      .= " -funroll-loops -fomit-frame-pointer -ftree-vectorize";
#   $FLIBS      .= " " . `$FC $FFLAGS  -print-file-name=libgfortran.dylib`; chomp($FLIBS);
#   $FLIBS      .= " " . `$FC $FFLAGS -print-file-name=libgfortranbegin.a`; chomp($FLIBS);
   $FPP = $FC . " -E -P"; 
 }

 $SYSLIBS   .= $threadlib;
 $CLIBS     .= $threadlib;
 
 # remove duplicates options coming from ROOTCFLAGS - used block to be sure
 # vars are gone when we go out of scope
 {
   my(@ARGS)  =($CFLAGS ,$CXXFLAGS,$FFLAGS,$CERNLIB_FPPFLAGS,$CERNLIB_CPPFLAGS);    # to clean for duplicates / restore below in the same order
   my(@LABELS)=("CFLAGS","CXXFLAGS","FFLAGS","CERNLIB_FPPFLAGS","CERNLIB_CPPFLAGS");   # labels should also relate to @ARGS order - for printing
   for ($i =0 ; $i <= $#ARGS ; $i++){
     my $Arg =  $ARGS[$i]; 
     my(@CmpArgs)=split(" ",$Arg);
     my(%CmpOpts)=undef; 
     $ARGS[$i] = "";
     
     foreach $tmp (@CmpArgs){
       if ( ! defined($CmpOpts{$tmp}) ){
	 $ARGS[$i] .= $tmp." ";
	 $CmpOpts{$tmp} = 1;
	 #print "DEBUG found $tmp\n";
       } else {
	 print "\tRemoving duplicate $tmp from $LABELS[$i]\n" unless ($param::quiet);
       }
     }
   }
   # restore  
   ($CFLAGS,$CXXFLAGS,$FFLAGS,$CERNLIB_FPPFLAGS,$CERNLIB_CPPFLAGS) = @ARGS;        # <--- same order than above
 }
 $OSFID    .= " __ROOT__";
 $CPPFLAGS .= " -D" . join ( " -D", split ( " ", $OSFID ) );
 
 if ($OSFCFID) {
   $FPPFLAGS  .= " -D" . join ( " -D", split ( " ", $OSFCFID ) );
 }
 
 $ROOTSRC = $ROOTSYS . "/include";
 
 $CERNINO = "#asps/Simulation/geant321/include" . $main::PATH_SEPARATOR . $CERN_ROOT . "/include";
 if ($CPPPATH) {$CPPPATH .= $main::PATH_SEPARATOR;}
 $CPPPATH .= "#".  $main::PATH_SEPARATOR . "#StRoot" .  $main::PATH_SEPARATOR . $INCLUDE;# . $main::PATH_SEPARATOR . $ROOTSRC;# . $main::PATH_SEPARATOR . "#";
 $CPPPATH .= $main::PATH_SEPARATOR . $XOPTSTAR . "/include";
 if ($XOPTSTAR ne $OPTSTAR) {
   $CPPPATH .= $main::PATH_SEPARATOR . $OPTSTAR . "/include";
 }
 $CPPPATH .= $main::PATH_SEPARATOR . $ROOTSRC;
 #    $CPPPATH .= $main::PATH_SEPARATOR ."#";# . $CERNINO;
 
 my $pwd = cwd();
 my $path2bin = $pwd . "/." . $STAR_HOST_SYS . "/bin";
 if ($PATH !~ /$STAR_BIN/) {$PATH = $STAR_BIN . ":" . $PATH;}
 $PATH = $path2bin .":". $PATH;  # print "PATH = $PATH\n";
 $FCPATH = $INCLUDE . $main::PATH_SEPARATOR . $CERNINO . $main::PATH_SEPARATOR . "#";
 
 
 # ------- packages -------
 # MySQL
 # my $os_name = `uname`;
 # chomp($os_name);
 #
 # *** Standard package first, then XOPTSTAR ***
 #	
 my ($MYSQLINCDIR,$mysqlheader);
 if ( defined($ENV{USE_LOCAL_MYSQL}) ){
   ($MYSQLINCDIR,$mysqlheader) =
     script::find_lib( $XOPTSTAR . "/include " .  $XOPTSTAR . "/include/mysql ".
		       $MYSQL . " " .
		       "/sw/include/mysql ".
		       "/include /usr/include ".
		       "/usr/include/mysql  ".
		       "/usr/mysql/include  ".
		       "/usr/mysql  ",
		       "mysql.h");
 } else { 
   ($MYSQLINCDIR,$mysqlheader) =
     script::find_lib( $MYSQL . " " .
		       "/sw/include/mysql ".
		       "/include /usr/include ".
		       "/usr/include/mysql  ".
		       "/usr/mysql/include  ".
		       "/usr/mysql  ".
		       $XOPTSTAR . "/include " .  $XOPTSTAR . "/include/mysql " ,
		       "mysql.h");
 }
 
 if (! $MYSQLINCDIR) {
   die "Can't find mysql.h in standard path and $XOPTSTAR/include  $XOPTSTAR/include/mysql\n";
 }
 
 # search for the config    
 my ($MYSQLCONFIG,$mysqlconf);
 # if ( defined($ENV{USE_LOCAL_MYSQL}) ){
 ($MYSQLCONFIG,$mysqlconf) =
 script::find_lib($XOPTSTAR . "/bin " .  $XOPTSTAR . "/bin/mysql /sw/bin ".
		  $MYSQL . " ".
		  "/usr/$LLIB/mysql /usr/bin/mysql /usr/bin ",
		  "mysql_config");
 # Associate the proper lib with where the inc was found
 my ($mysqllibdir)=$MYSQLINCDIR;
 $mysqllibdir =~ s/include/$LLIB/;# print "mysqllibdir => $mysqllibdir =========\n";
 if ( $mysqlconf ){
   $mysqlconf = "$MYSQLCONFIG/$mysqlconf";# print "mysqlconf = $mysqlconf\n";
   # if ( 1==1 ){
   # Do not guess, just take it - this leads to a cons error though TBC
   chomp($MYSQLLIB = `$mysqlconf  --libs`);# print "MYSQLLIB = $MYSQLLIB\n";
   # but remove -L which are treated separately by cons
   my(@libs) = split(" ", $MYSQLLIB);
   foreach my $test (@libs) {
     if ($test !~ /^-L/) {next;}
     #	 print "test = $test\n";
     $MYSQLLIBDIR = $test; $MYSQLLIBDIR =~ s/-L//;# print "MYSQLLIBDIR = $MYSQLLIBDIR\n";
     $MYSQLLIBDIR =~ s/lib64\//$LLIB\//;# print "MYSQLLIBDIR = $MYSQLLIBDIR\n";
     $MYSQLLIBDIR =~ s/lib\//$LLIB\//;# print "MYSQLLIBDIR = $MYSQLLIBDIR\n";
     $MYSQLLIB = "";
     foreach my $el (@libs){
       $MYSQLLIB  .= " ".$el if ($el !~ m/-L/);
     }
     last;
   }
   # here is a check for libmysqlclient
 } else {
   die "No mysql_config found\n";
 }
 print "Using $mysqlconf\n\tMYSQLINCDIR = $MYSQLINCDIR MYSQLLIBDIR = $MYSQLLIBDIR  \tMYSQLLIB = $MYSQLLIB\n"
 if ! $param::quiet;
 # QT
 if ( defined($QTDIR) && -d $QTDIR) {
   $QT_VERSION = 3;
   
   if (-e $QTDIR . "/bin/moc") {
     $QTLIBDIR = $QTDIR . "/lib";
     $QTBINDIR = $QTDIR . "/bin";
   }
   if ($QTBINDIR) {
     $QTINCDIR = $QTDIR . "/include";
     if (! -r $QTINCDIR) {$QTINCDIR = $QTDIR . "/../../include";}
     $QTFLAGS  = "-DR__QT";#-DQT_THREAD_SUPPORT";
     if ( -d $QTINCDIR . "/QtCore" ) { $QT_VERSION = 4;}
     if ($QT_VERSION==4) {
       my $QtIncBase = $QTINCDIR;
       if( opendir(QT4INCLUDE,$QtIncBase)) {
	 my $Qt4Header;
	 while($Qt4Header = readdir(QT4INCLUDE))
	   {
	     if ($Qt4Header =~ /^Qt\D*/ && -d $QtIncBase . "/" . $Qt4Header ) {
	       $QTINCDIR .= $main::PATH_SEPARATOR . "$QtIncBase/" . $Qt4Header;
	     }
	   }
	 $QTFLAGS .=  " -DQT_QT3SUPPORT_LIB -DQT3_SUPPORT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED ";
	 $QTFLAGS .=  " -DNoQtWebkit";
	 closedir(QT4INCLUDE);
	 
	 if( opendir(QT4LIBS,$QTLIBDIR)) {
	   my $Qt4Lib;
	   while($Qt4Lib = readdir(QT4LIBS)) {
	     if ($Qt4Lib =~ /^libQt.+\.so$/ && -f $QTLIBDIR  . "/" . $Qt4Lib ) {
	       $Qt4Lib=~ s/^lib//;  $Qt4Lib=~ s/\.so$//;
	       $QTLIBS .= " -l" . $Qt4Lib;
	     }
	   }
	   closedir(QT4LIBS);
	 }
       }
     } else {
       $QTLIBS   = "-lqt-mt";
       if ($main::_WIN32) {
	 $QTLIBS  .= " " . $QTDIR . "/lib/qt-mt*.lib " .
	   $ROOTSYS . "/lib/libGraf.lib " .
	     $ROOTSYS . "/lib/libGpad.lib shell32.lib Ws2_32.lib Imm32.lib Winmm.lib";
       }
     }
     print "Use QTLIBDIR = $QTLIBDIR \tQTINCDIR = $QTINCDIR \tQTFLAGS = $QTFLAGS \tQTLIBS = $QTLIBS\n"
       if $QTLIBDIR && ! $param::quiet;
   }
   # Coin3D - WHAT??! JL 2009
   if ( ! defined($IVROOT) ){  $IVROOT = $XOPTSTAR;}
   if ( defined($IVROOT) &&  -d $IVROOT) {
     # This is an initial logic relying on IVROOT to be defined
     if (-e $IVROOT . "/bin/coin-config") {
       $COIN3DIR     = $IVROOT;
       $COIN3DBINDIR = $COIN3DIR . "/lib";
       $COIN3DLIBDIR = $COIN3DIR . "/bin";
     }
     if ($COIN3DIR) {
       $COIN3DINCDIR = $COIN3DIR . "/include";
       $COIN3DFLAGS  = ""; # "-DR__QT";#-DQT_THREAD_SUPPORT";
       $COIN3DLIBS   = "-lCoin -lSmallChange -lSoQt -lsimage";
     }
   } else {
     # try finding it in $XOPTSTAR
     my($coin);
     if ( -e "$XOPTSTAR/bin/coin-config"){
       $coin = "$XOPTSTAR/bin/coin-config";
     }
     if ( defined($coin) ){
       chomp($COIN3DINCDIR = `$coin --includedir`);
       chomp($COIN3DFLAGS  = "");
       chomp($COIN3DLIBDIR = `$coin --prefix`); $COIN3DLIBDIR .= "/lib";
       chomp($COIN3DLIBS   = `$coin --libs`);
     }
   }
   if ( ! $param::quiet){
     if (defined($COIN3DLIBDIR)){
       print "Use COIN3DLIBDIR = $COIN3DLIBDIR \tCOIN3DINCDIR = $COIN3DINCDIR \tCOIN3DFLAGS = $COIN3DFLAGS \tCOIN3DLIBS = $COIN3DLIBS\n";
     } else {
       print "No Coin defined\n";
     }
   }
 }
 # Logger
 $LoggerDir = $XOPTSTAR . "/include/log4cxx";
 if (-d $LoggerDir) {
   $LoggerINCDIR = $XOPTSTAR . "/include";
   $LoggerLIBDIR = $XOPTSTAR . "/lib";
   $LoggerLIBS   = "-llog4cxx";
   if ($STAR_HOST_SYS =~ /darwin/) {$LoggerLIBS .= " -lstdc++";}
   print
     "Use Logger  ",
       "LIBDIR = $LoggerLIBDIR \tLoggerINCDIR = $LoggerINCDIR \tLoggerLIBS = $LoggerLIBS\n"
	 if $LoggerLIBDIR && ! $param::quiet;
 }
 # xml2
 my  ($XMLINCDIR,$XMLLIBDIR,$XMLLIBS) = ("","","");
 my ($xml) =  script::find_lib($XOPTSTAR . "/bin /usr/bin",
			       "xml2-config");
 if ($xml) {
   $xml .= "/xml2-config";
   $XMLINCDIR = `$xml --cflags`;
   chomp($XMLINCDIR);
   $XMLINCDIR =~ s/-I//;
   my $XML  = `$xml --libs`; # die "$XML\n";
   my(@libs)= split(" ", $XML);
   $XMLLIBDIR = shift(@libs);
   if ($XMLLIBDIR =~ /-L/){
     $XMLLIBDIR =~ s/-L//;
     $XMLLIBS   = join(" ",@libs);
   } else {
     # no -L, assume all were LIBS
     $XMLLIBS   = $XMLLIBDIR ." ".join(" ",@libs);
     # and fix -L / should work for both 32 and 64
     $XMLLIBDIR = "/usr/$LLIB";
   }
   
   my $XMLVersion = `$xml --version`;            # print "XMLVersion = $XMLVersion\n";
   my ($major,$minor) = split '\.', $XMLVersion; # print "major = $major,minor = $minor\n";
   $XMLCPPFlag = "";#-DXmlTreeReader";
   if ($major < 2 or $major == 2 and $minor < 5) {
     $XMLCPPFlag = "-DNoXmlTreeReader";
   }
   if ( ! $param::quiet ){
     if ( $XMLLIBDIR ){
       print "Use xml $xml XMLLIBDIR = $XMLLIBDIR \tXMLINCDIR = $XMLINCDIR \tXMLLIBS = $XMLLIBS XMLCPPFlag =$XMLCPPFlag\n";
     } else {
       print "Use xml -> WARNING ** Could not define XMLLIBDIR, XMLINCDIR, XMLLIBS\n";
     }
   }
 } else {
   print "Could not find xml libs\n" if (! $param::quiet);
 }
 $CXXCOM = 
 "%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %CXXinp%< %Cout%>";
 # print "CXXCOM : $CXXCOM\n";
 $CCCOM =  
 "%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %Cinp%< %Cout%>";
 $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
 $LINKCOM =
 "%LD %DEBUG %LDFLAGS %EXTRA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 my $FCCOM = 
 "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>;";
 my $FCCOM_noExt = 
 "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>;";
 my $FCCOM_noExt = 
 "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>;";
 my $F90COM = 
 "cd %<:d; %FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND %_IFLAGS %EXTRA_FCPATH -c %<:f %Fout%>:f;";
  my $FCviaAGETOFCOM
  = " test -f %>:b.g && rm %>:b.g; %FPP %FPPFLAGS %EXTRA_FPPFLAGS %_IFLAGS %EXTRA_FCPATH %<:b.F -o %>:b.g;"
  . "cd %>:d; test -f %>:F.for && rm %>:F.for; %AGETOF %AGETOFLAGS -V f %<:F.g -o %>:F.for; cd -;";
  $FCviaAGETOFCOM .= " if [ -f %>:b.for ]; then %FC %FFLAGS %EXTRA_FPPFLAGS %FDEBUG -c %>:b.for %Fout%>;";
  $FCviaAGETOFCOM .= " else ". $FCCOM . " fi";
 
  my $AGETOFCOM  = "cd %>:d; test -f %>:F.F && /bin/rm %>:F.F;";
  $AGETOFCOM .= "%AGETOF %AGETOFLAGS %<:f -o %>:F.F && cd - &&";
  $AGETOFCOM .= "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %_IFLAGS %EXTRA_FCPATH -c";
  $AGETOFCOM .= " %>:b.F %Fout%>";
 
 my @params = (
	       'Package'        => 'None',
	       'CPP'            => $CPP,
	       'FPP'            => $FPP,
	       'CPPPATH'        => $CPPPATH,
	       'EXTRA_CPPPATH'  => $EXTRA_CPPPATH,
	       'CERNLIB_FPPFLAGS'  => $CERNLIB_FPPFLAGS,
	       'CERNLIB_CPPFLAGS'  => $CERNLIB_CPPFLAGS,
	       'CPPFLAGS'       => $CPPFLAGS,
	       'EXTRA_CPPFLAGS' => $EXTRA_CPPFLAGS,
	       'ROOTLIBS'       => $ROOTLIBS,
	       'DEBUG'          => $DEBUG,
	       'GPROF'          => $GPROF,
	       'FDEBUG'         => $FDEBUG,
	       'NOOPT'          => $NOOPT,
	       'G77'            => $G77,
	       'G77FLAGS'       => $G77FLAGS,
	       'G77EXTEND'      => $G77EXTEND,
	       'LIBG2C'         => $LIBG2C,
	       'LIBSTDC'        => $LIBSTDC,
	       'FC'             => $FC,
	       'FPPFLAGS'       => $FPPFLAGS,
	       'EXTRA_FPPFLAGS' => $EXTRA_FPPFLAGS,
	       'FEXTEND'        => $FEXTEND,
	       'FFLAGS'         => $FFLAGS,
	       'FCPATH'         => $FCPATH,
	       'EXTRA_FCPATH'   => $EXTRA_FCPATH,
	       'Fout'           => "-o ",
	       'CXXinp'         => "",
	       'Cinp'           => "",
	       'Cout'           => "-o ",
	       'Lout'           => "-o ",
	       'SoOUT'          => "-o ",
	       'FCCOM'          => $FCCOM,
	       'F90COM'         => $F90COM,
	       'AGETOF'         => $AGETOF,
	       'AGETOFLAGS'     => $AGETOFLAGS,
	       'AGETOFCOM'      => $AGETOFCOM,
	       'FCviaAGETOFCOM' => $FCviaAGETOFCOM,
	       'CC'             => $CC,
	       'CFLAGS'         => $CFLAGS,
	       'EXTRA_CFLAGS'   => $EXTRA_CFLAGS,
	       'KUIP'           => $KUIP,
	       'KUIPCOM'        => '%KUIP %< %<.f && %FC %FDEBUG %FFLAGS -c %<.f -o %>',
	       'CCCOM'          => $CCCOM,
	       'CXX'            => $CXX,
	       'CXX_VERSION'    => $CXX_VERSION,
	       'CXX_MAJOR'      => $CXX_MAJOR,
	       'CXX_MINOR'      => $CXX_MINOR,
	       'CXXFLAGS'       => $CXXFLAGS,
	       'EXTRA_CXXFLAGS' => $EXTRA_CXXFLAGS,
	       'CXXCOM'         => $CXXCOM,
	       'CLIBS'          => $CLIBS,
	       'FLIBS'          => $FLIBS,
	       'XLIBS'          => $XLIBS,
	       'THREAD'         => $THREAD,
	       'CRYPTLIBS'      => $CRYPTLIBS,
	       'SYSLIBS'        => $SYSLIBS,
	       'CERNLIBS'       => $CERNLIBS,
	       'Libraries'      => $Libraries,
	       'LIBS'           => $LIBS,
	       'LD'             => $LD,
	       'LDFLAGS'        => $LDFLAGS,
	      'LDEXPORT'       => $LDEXPORT,
	      'LDALL'	   => $LDALL,
	       'LDNONE'	   => $LDNONE,
	       'EXTRA_LDFLAGS'  => $EXTRA_LDFLAGS,
	       'F77LD'          => $F77LD,
	       'F77LDFLAGS'     => $F77LDFLAGS,
	       'EXEFLAGS'       => $EXEFLAGS,
	       'LINKCOM'        => $LINKCOM,
	       'SO'             => $SO,
	       'SOFLAGS'        => $SOFLAGS,
	       'EXTRA_SOFLAGS'  => $EXTRA_SOFLAGS,
	       'STIC'           => $STIC,
	       'LINKMODULECOM'  => $MAKELIB,
	       'AR'             => $AR,
	       'ARFLAGS'        => $ARFLAGS,
	       'RANLIB'         => 'ranlib',
	       'AS'             => 'as',
	       'ASFLAGS'        => '',
	       'ASCOM'          => '%AS %%DEBUG ASFLAGS %< -o %>',
	       'PREFLIB'        => 'lib',
	       'SUFLIB'         => 'a',
	       'SUFSOLIB'       => "so",
	       'SUFEXE'         => "",
	       'SUFLIBS'        => ".%SUFSOLIB" . $main::PATH_SEPARATOR . ".%SUFLIB",
	       'SUFMAP'         => {
				    '.g'   => 'build::command::agetof',
				    '.age' => 'build::command::agetof',
				    '.f'   => 'build::command::fc',
				    '.f90' => 'build::command::f90',
				    '.F'   => 'build::command::fc',
				    '.C'   => 'build::command::cxx',
				    '.s'   => 'build::command::cc',
				    '.S'   => 'build::command::cc',
				    '.c'   => 'build::command::cc',
				    '.cc'  => 'build::command::cxx',
				    '.cxx' => 'build::command::cxx',
				    '.cpp' => 'build::command::cxx',
				    '.cdf' => 'build::command::kuip',
				    '.o'   => 'build::command::user'
				   },
	       'SUFOBJ' => ".o",
	       'ENV'    => {
			    'PATH'            => $PATH,
			    'LM_LICENSE_FILE' => $LM_LICENSE_FILE,
			    'INCLUDE'         => $INCLUDE_PATH,
			    'ROOT'            => $ROOT,
			    'ROOT_LEVEL'      => $ROOT_LEVEL,
			    'ROOTSRC'         => $ROOTSRC,
			    'ROOTSYS'         => $ROOTSYS,
			    'CINTSYSDIR'      => $CINTSYSDIR,
			    'LD_LIBRARY_PATH' => $LD_LIBRARY_PATH,
			    'SHLIB_PATH'      => $SHLIB_PATH,
			    'LIB'             => $LIB,
			    'PGI'             => $PGI,
			    'PGILIB'          => $PGILIB,
			    'STAR'            => $STAR,
			    'STAR_LEVEL'      => $STAR_LEVEL,
			    'CERN_ROOT'       => $CERN_ROOT,
			    'STAF'            => $STAF,
			    'STAR_BIN'        => $STAR_BIN,
			    'STAR_SYS'        => $STAR_HOST_SYS,
			    'STAR_HOST_SYS'   => $STAR_HOST_SYS,
			    'STAR_OBJ'        => $STAR_OBJ,
			    'STAR_LIB'        => $STAR_LIB,
			    'STAR_VERSION'    => $STAR_VERSION,
			    'PERL5LIB'        => $PERL5LIB,
			    'OPTSTAR'         => $XOPTSTAR,
			    'QTDIR'           => $QTDIR,
			    'COIN3DIR'        => $COIN3DIR,
			    'IVROOT'          => $IVROOT,
			    'HOME'            => $HOME,
			    'GARFIELD_HOME'   => $GARFIELD_HOME,
			    'NODEBUG'         => $NODEBUG,
			    'CC'       => $CC, #         C compiler command
			    'CFLAGS'   => $CFLAGS, #     C compiler flags
			    'LDFLAGS'  => $LDFLAGS, #    linker flags, e.g. -L<lib dir> if you have libraries in a nonstandard directory <lib dir>
			    'CPPFLAGS' => $CPPFLAGS, #   C/C++ preprocessor flags, e.g. -I<include dir> if you have headers in a nonstandard directory <include dir>
			    'CPP'      => $CPP,#         C preprocessor
			    'CXX'      => $CXX,#         C++ compiler command
			    'CXXFLAGS' => $CXXFLAGS,#    C++ compiler flags
			    'CXXCPP'   => $CXXCPP,#      C++ preprocessor
			    #                      'F77'      => $F77,#         Fortran 77 compiler command
			    #                      'FFLAGS'   => $FFLAGS,#      Fortran 77 compiler flags
			    'USE_64BITS'     => $USE_64BITS
			   },
	       'Packages' => {
			      'ROOT' => {
					 'BINDIR'=> $ROOTSYS . "/bin",
					 'LIBDIR'=> $ROOTSYS . "/lib",
					 'INCDIR'=> $ROOTSYS . "/include",
					 'LIBS'  => $ROOTLIBS,
					 'RLIBMAP'  => $RLIBMAP,
					 'ROOTCINT' => $ROOTCINT
					},
			      'CERNLIB' => {
					    'BINDIR' => $CERN . "/bin",
					    'LIBDIR' => $CERN . "/" . $LLIB,
					    'INCDIR' => $CERNINO,
					    'FPPFLAGS' => $CERNLIB_FPPFLAGS,
					    'CPPFLAGS' => $CERNLIB_CPPFLAGS,
					    'CERNLIBS' => $CERNLIBS
					   },
			      'MYSQL' => {
					  'LIBDIR'=> $MYSQLLIBDIR,
					  'INCDIR'=> $MYSQLINCDIR,
					  'LIBS'  => $MYSQLLIB
					 },
			      'QT' => {
				       'QT_VERSION' => $QT_VERSION,
				       'DIR'        => $QTDIR,
				       'INCDIR'     => $QTINCDIR,
				       'BINDIR'     => $QTBINDIR,
				       'FLAGS'      => $QTFLAGS,
				       'LIBDIR'     => $QTLIBDIR,
				       'LIBS'       => $QTLIBS
				      },
			      'COIN3D' => {
					   'DIR'   => $COIN3DIR,
					   'INCDIR'=> $COIN3DINCDIR,
					   'BINDIR'=> $COIN3DBINDIR,
					   'FLAGS' => $COIN3DFLAGS,
					   'LIBDIR'=> $COIN3DLIBDIR,
					   'LIBS'  => $COIN3DLIBS,
					  },
			      'XML' => {
					'LIBDIR'=> $XMLLIBDIR,
					'INCDIR'=> $XMLINCDIR,
					'LIBS'  => $XMLLIBS,
					'CPP'   => $XMLCPPFlag
				       },
			      'Logger' => {
					   'INCDIR'=> $LoggerINCDIR,
					   'LIBDIR'=> $LoggerLIBDIR,
					   'LIBS'  => $LoggerLIBS
					  },
			     }
	      );
 
 push ( @param::defaults, @params );
}


#________________________________________________________________________________
sub find_lib {
  my @libsdirs = split ' ',shift;# print "libsdirs: @libsdirs\n";
  my @libs     = split ' ',shift;# print "libs: @libs\n";
  my @libexts  = ("",".so", ".sl", ".a", ".lib");
  foreach my $i (@libsdirs) {
    next if ! -d $i;
    foreach my $j (@libs) {
      foreach my $ext (@libexts) {
	my $l = $i . "/" . $j . $ext;;# print "l = $l\n";
	next if ! -r $l;
	my $k = $j;
	$k =~ s/^lib/-l/;
	$k =~ s/\.(so|lib|a)$//;
	return ($i,$k);
      }
    }
  }
  print "Can't find @libs in @libsdirs\n";
}
1;
