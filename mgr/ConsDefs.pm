# $Id: ConsDefs.pm,v 1.148 2020/06/11 19:20:37 genevb Exp $
{
    use File::Basename;
    use Sys::Hostname;
    use Cwd;
    use Env;
    use File::Find();

    #________________________________________
    *name     = *File::Find::name;
    *prune    = *File::Find::prune;
    *dir      = *File::Find::dir;
    *topdir   = *File::Find::topdir;
    *topdev   = *File::Find::topdev;
    *topino   = *File::Find::topino;
    *topmode  = *File::Find::topmode;
    *topnlink = *File::Find::topnlink;

    #use strict;
    if ( !$STAR_SYS ) {
        $STAR_SYS = `sys`;
        chop($STAR_SYS);
        $STAR_HOST_SYS = $STAR_SYS;
    }
    if ( !$OPTSTAR )  { $OPTSTAR  = "/opt/star"; } # print "OPTSTAR = $OPTSTAR\n"; die;
    if ( !$XOPTSTAR ) { $XOPTSTAR = $OPTSTAR;}     # print "OPTSTAR = $OPTSTAR\n"; die;
    my $MYSTAR;
    if ( -e "$OPTSTAR/lib"){        $MYSTAR = $OPTSTAR;
    } elsif ( -e "$XOPTSTAR/lib"){  $MYSTAR = $XOPTSTAR;
    } else {   die "Neitehr OPTSTAR nor XOPTSTAR have the proper structure\n";}


    $BUILD   = "#." . $STAR_HOST_SYS; print "build for $BUILD\n" unless ($param::quiet);
    $INCLUDE = $BUILD  . "/include";

    @search_files = ();
    $DEBUG        = "-g";
    $FDEBUG       = $DEBUG;
    $NOOPT        = "";

    $O      = "o";
    $A      = "a";
    $Cxx    = "cxx";
    $SOEXT  = "so";
    $EXESUF = "";

    $SoOUT    = "-o ";
    $Cout     = "-o ";
    $Fout     = "-o ";
    $Lout     = "-o ";
    $Cinp     = "";
    $CXXinp   = "";

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
 # MACOSX
    $FINK_CXXFLAGS= "";
    $FINK_CFLAGS= "";
    $FINK_LDFLAGS = "";

    $AFSFLAGS = "";
    $AFSDIR   = "/usr/afsws";
    $AFSLIBS  = "-L" . $AFSDIR . "/lib -L" . $AFSDIR . "/lib/afs";
    $AFSLIBS .= " -lkauth -lprot -lubik -lauth -lrxkad -lsys -ldes -lrx -llwp";
    $AFSLIBS .= " -lcmd -lcom_err -laudit " . $AFSDIR . "/lib/afs/util.a";

    $ROOT_VERSION = `root-config --version`;
    $ROOT_VERSION =~ s/^\s+|\s+$//; # trim
    $ROOT_VERSION =~ s/\//./;       # dot notation
    ($ROOT_VERSION_MAJOR, $ROOT_VERSION_MINOR, $ROOT_VERSION_PATCH) = split('\.', $ROOT_VERSION);

    if ( !$ROOT )       { $ROOT       = $AFS_RHIC."/star/ROOT"; }
    if ( !$ROOT_LEVEL ) { $ROOT_LEVEL = $ROOT_VERSION; }
    if ( !$ROOTSYS )    { $ROOTSYS    = $ROOT . "/" . $ROOT_LEVEL; }
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

    my $rootcflags = `root-config --cflags`; chomp($rootcflags);
    my $ROOTCFLAGS = "";
    @List = ();
    foreach my $f (split ' ', $rootcflags) {
      next if $f =~ /^-I/;
      push @List, $f;
    }
    if ($#List >= 0) {
      $ROOTCFLAGS = " " . join ' ', @List;
    }

    # print "ROOTCFLAGS = $ROOTCFLAGS\n";
    # die;
    $SRPDIR   = $ROOTSYS . "/lib";
    $SRPFLAGS = "";                  # -DR__SRP -I" . $SRPDIR . "/include";
    $SRPLIBS  = "";                  # -L" . $SRPDIR . "/lib -lsrp -lgmp";

    chomp($HOST = `/bin/hostname`);
    $CPPPATH       = "";
    $CPPFLAGS      = "";
    $EXTRA_CPPPATH = "";

    # simulation
    $CERNLIB_FPPFLAGS = "";
    $CERNLIB_CPPFLAGS = "";


    # We make the assumption that STAR_HOST_SYS will contain the string 64_
    # 64 bits test does not suffice. It comes in two flavors with lib and lib64
    # depending of backward 32 support or not. We can extend here later
    #
    # ATTENTION: Scheme change 2009/10/20 (but before full transition so could
    # be cleaned later). USE_64BITS defined externally.
    #
    # $USE_64BITS     = ($STAR_HOST_SYS =~ m/64_/ && -e "/usr/lib64" );
    #
    $XMACHOPT = "";
    if ($USE_64BITS){
	if ($STAR_HOST_SYS =~ /(gcc)(\d)/){ if ( $2 >= 4 ){   $XMACHOPT = "-m64";}}
	$LLIB = "lib64";
    } else {
	if ($STAR_HOST_SYS =~ /(gcc)(\d)/){ if ( $2 >= 4 ){   $XMACHOPT = "-m32";}}
	$LLIB = "lib";
    }

    if ( ( -x "/usr/bin/gfortran" || -x "/sw/bin/gfortran" ) && !defined($ENV{USE_G77}) ){
	# JL 200908 - give preference to gfortran for now 
	# JL 201004 - added possibility to revertto g77 by defining USE_G77 but 
	#             this is at your own risk
	$G77       = "gfortran";
	$FC        = $G77;
	if ( defined( $ARG{NODEBUG} ) || $NODEBUG )  {
	    $G77FLAGS  = "$XMACHOPT -fd-lines-as-comments ";
	} else {
	    $G77FLAGS  = "$XMACHOPT -fd-lines-as-code ";
	}

	$G77FLAGS .= " -std=legacy -fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";
	
	$FFLAGS    = $G77FLAGS;     # will be overwritten below, ignore
	$FLIBS     = "-lgfortran";

    } else {
	$G77       = "g77";
	$G77FLAGS  = "$XMACHOPT -fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";
    }

    if ($STAR_HOST_SYS =~ /gcc3/) {  $G77FLAGS    = "-pipe " . $G77FLAGS;}
    $G77EXTEND     = "-ffixed-line-length-132";

    $CXX           = "g++";
    $LDFLAGS       = "";
    $SOFLAGS       = "";
    # $XMACHOPT would switch to -m32 or -m64
    $CXXFLAGS = $CFLAGS = $LDFLAGS = $SOFLAGS = "$XMACHOPT";

    $CXXFLAGS      .= " -fPIC -w";
    $EXTRA_CXXFLAGS= "";
    $CXXOPT        = "";

    $CC            = "gcc";

    $CFLAGS       .= " -fPIC -w";
    $EXTRA_CFLAGS  = "";

    $FCPATH        = "";
    $EXTRA_FCPATH  = "";

    $FFLAGS        = $G77FLAGS;
    $FEXTEND       = $G77EXTEND;

    $CPPCERN       = " -DCERNLIB_TYPE -DCERNLIB_DOUBLE -DCERNLIB_NOQUAD -DCERNLIB_LINUX ";
    $FPPFLAGS      = $CPPCERN;
    $EXTRA_FPPFLAGS= "";
    $CXXFLAGS     .= $CPPCERN;

    $CPP           = $CC . " -E -P";
    $FPP           = $CPP;
    $AR            = "ar";
    $ARFLAGS       = "rvu";
    $LD            = $CXX;

    if ( $STAR_HOST_SYS !~ /^x86_darwin/ ) {
	$LDEXPORT      = " -Wl,-export-dynamic -Wl,-noinhibit-exec,-Bdynamic";
	$LDALL         = " -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs";
	$LDNONE        = " -Wl,--no-whole-archive -Wl,-Bdynamic";
    } 
    $EXTRA_LDFLAGS = "";
    $F77LD         = $LD;
    $F77LDFLAGS    = $LDFLAGS;
    $SO            = $CXX;
    $STIC          = "stic";
    $STICFLAGS     = "";
    $AGETOF        = "agetof";
    $AGETOFLAGS    = "-V 1";
    $LIBSTDC       = `$CC $CFLAGS -print-file-name=libstdc++.a | awk '{ if (\$1 != "libstdc++.a") print \$1}'`;
    chomp($LIBSTDC);

    if ( $G77 =~ m/gfortran/ ){
	# JL: For gfortran version <  4.3, -lg2c may still be needed for ensuring 
	#   symbol resolve but this is a messy hack and should likely be avoided.
	#   The below line was tried and would not work in those cases.
        #$LIBG2C  = `$FC $FFLAGS -print-file-name=libgfortran.a | awk '{ if (\$1 != "libgfortran.a") print \$1}'`;
	#print "$FC $FFLAGS -print-file-name=libgfortran.a ========> LIBG2C = $LIBG2C\n";
    } else {
	$LIBG2C  = `$CC $CFLAGS  -print-file-name=libg2c.a | awk '{ if (\$1 != "libg2c.a") print \$1}'`;
    }
    chomp($LIBG2C);
# print "================= LIBG2C ; $LIBG2C\n";

    $KUIP          = $CERN_ROOT . "/bin/kuipc";
    if ( !$ROOT )       { print "ROOT_LEVEL has to be defined\n"; exit 1;}
    if ( !$ROOT_LEVEL ) { print "ROOT_LEVEL has to be defined\n"; exit 1;}
    if ( !$ROOTSYS )    { print "ROOT_SYS   has to be defined\n"; exit 1;}

    $ROOTCINT      = $ROOTSYS . "/bin/rootcint";
    my $RLIBMAP    = "";#$ROOTSYS . "/bin/rlibmap";
    # if ($RLIBMAP and ! -e $RLIBMAP) {$RLIBMAP = "";}
    if ($RLIBMAP) {
	my ($M,$S,$V) = split('.',$ROOT_LEVEL);
	if ($M <4 or $M == 4 and $S == 0) {$RLIBMAP = "";}
    }


    #+
    # now treat NODEBUG and GPROF - kind of assume generic gcc
    # and flag will need to be reset otherwise
    #-

    # historical had DEBUG_OPTIONS used to set optimizer options (confusing)
    # Kept it as support but added {OPTIM_OPTIONS, a more natural naming
    my($OPTIM_OPTS) = $ENV{OPTIM_OPTIONS}||$ENV{DEBUG_OPTIONS};

    if ( defined( $ARG{GPROF} ) or defined($ENV{GPROF}) ){
	print "Using GPROF\n" unless ($param::quiet);
	$EXTRA_CXXFLAGS= " -pg ";
	$EXTRA_CFLAGS  = " -pg ";
	$EXTRA_FPPFLAGS= " -pg ";
	$EXTRA_LDFLAGS = " -pg ";
	$EXTRA_SOFLAGS = " -pg ";
	$GPROF         = "yes";
    } else {
	# GPROF will imply that we do not allow optimized
	# ATTENTION - the debug options below are global for any compiler (not
	# gcc specific). The rest is treated later
	$GPROF= undef;
	if ( defined( $ARG{NODEBUG} ) || $NODEBUG ) {
	    $DEBUG = $OPTIM_OPTS||"-O2 -g";
	    $FDEBUG= $DEBUG;
	}
    }
    unless ($param::quiet){
	print 
	    "Base ".(defined($NODEBUG)?"OPTIM":"DEBUG")." options = $DEBUG\n",
	    defined($NODEBUG)?"\tOPTIM is enabled (the ENV NODEBUG is enabled)\n":
	                      "\tDEBUG is enabled (enable optimize by setting the ENV NODEBUG)\n",
	    defined($OPTIM_OPTS)?"":
	                         "\tThe ENV variable OPTIM_OPTIONS may override the default optimize options\n";
    }




    $CINTSYSDIR    = $ROOTSYS . "/cint";
    $ARCOM  = "%AR %ARFLAGS %> %< ; %RANLIB %>"; # "%AR %ARFLAGS %> %<;%RANLIB %>",
    my $gccfilter = "";
    if (-e "$STAR/mgr/gccfilter") { 
      $gccfilter = "$STAR/mgr/gccfilter -c -w -a ";
      my $flag = system($gccfilter);
#      print "$gccfilter   ===========> $flag\n";
      if ($flag) { $gccfilter = "";}
#      print "gccfilter = $gccfilter ==============\n";
#      die;
    } 
    $CXXCOM = $gccfilter .
 "%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %CXXinp%< %Cout%>";
    $CCCOM =  $gccfilter .
 "%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %Cinp%< %Cout%>";
    $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
    $LINKCOM =
      "%LD %DEBUG %LDFLAGS %EXTRA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 my $FCCOM = 
 "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>;";
 my $FCviaAGETOFCOM
 = " test -f %>:b.g && rm %>:b.g; %FPP %FPPFLAGS %EXTRA_FPPFLAGS %_IFLAGS %EXTRA_FCPATH %<:b.F -o %>:b.g;"
 . " test -f %>:b.for && rm %>:b.for; %AGETOF %AGETOFLAGS -V f %<:b.g -o %>:b.for;";
 $FCviaAGETOFCOM .= " if [ -f %>:b.for ]; then %FC %FFLAGS %EXTRA_FPPFLAGS %FDEBUG -c %>:b.for %Fout%>;";
 $FCviaAGETOFCOM .= " else ". $FCCOM . " fi";

 my $AGETOFCOM  = "test -f %>:b.F && /bin/rm %>:b.F;";
    $AGETOFCOM .= "%AGETOF %AGETOFLAGS %< -o %>:b.F &&";
    $AGETOFCOM .= "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %_IFLAGS %EXTRA_FCPATH -c";
    $AGETOFCOM .= " %>:b.F %Fout%>";

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

    # A small sanity check in case user redefines improperly CERN_ROOT
    if ( $CERN_ROOT eq "") {
        print "WARNING: CERN_ROOT is not defined (you may define it as \$CERN/\$CERN_LEVEL)\n"
        unless ($param::quiet);
    } elsif ( ! -d "$CERN_ROOT/lib"){
        print "WARNING: $CERN_ROOT/lib does not exists (may have CERN_ROOT ill-defined)\n"
        unless ($param::quiet);
    }

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
#        $cernl = "cernlib -s";
        $cernl = "cernlib";
        $packl = "packlib";
        $kernl = "kernlib";
    }

    $CERNLIBS .= " " . `$cernl pawlib packlib graflib/X11 packlib mathlib kernlib`;
    $CERNLIBS =~ s/packlib\./$packl\./g;
    $CERNLIBS =~ s/kernlib\./$kernl\./g;
    $CERNLIBS =~ s/$strip//g     if ($strip ne "");
    $CERNLIBS =~ s/lib /lib64 /g if ($USE_64BITS);

    chop($CERNLIBS);

    if ( $STAR_HOST_SYS !~ /^x86_darwin/ ) {
      $CERNLIBS =~ s#lX11#L/usr/X11R6/lib -lX11#;
    }
    print "CERNLIB = $CERNLIBS\n" unless ($param::quiet);

    $PLATFORM      = `root-config --platform`; chomp($PLATFORM);
    $ARCH          = `root-config --arch`; chomp($ARCH);
    #  ============================================================
    # Platform support should be concentrated here
    #  ============================================================
#   print "DEBUG >>> We will select architecture/compiler based on $STAR_HOST_SYS\n" if ($param::debug);


    if ( $STAR_HOST_SYS =~ /_icc/ && ($STAR_HOST_SYS =~ m/^rh/ || $STAR_HOST_SYS =~ m/^sl/ )) {
	#
	# Intel ICC
	#

	#print "DEBUG >>> Switching ot ICC compiler\n" if ($param::debug);
#	$PLATFORM      = "linux";
#	$ARCH          = "linuxicc";
	$PGI           = "";
	$PGILIB        = "";
	$CC            = "icc";
	$CXX           = "icc";
	$CPP           = $CC . " -EP";
	$CXXFLAGS      = "-w -ansi $XMACHOPT -fPIC -wd1476"; #-fpstkchk";
	$CFLAGS        = "-restrict -w $XMACHOPT -fPIC";     # -fpstkchk";# -restrict";# -Wall
	$ICC_MAJOR     = `$CXX -V -dryrun  >& /tmp/icc_version; awk '{ if (NR==1) print \$8 }' /tmp/icc_version| cut -d'.' -f1; /bin/rm  /tmp/icc_version;`;
        $ICC_MINOR     = `$CXX -V -dryrun  >& /tmp/icc_version; awk '{ if (NR==1) print \$8 }' /tmp/icc_version| cut -d'.' -f2; /bin/rm  /tmp/icc_version;`;
	chomp($ICC_MAJOR); chomp($ICC_MINOR);
	$LIBFRTBEGIN   = `gcc -print-file-name=libfrtbegin.a | awk '{ if (\$1 != "libfrtbegin.a") print \$1}'`;
	chomp($LIBFRTBEGIN);
	$NOOPT         = "-O0";
#	$LIBFRTBEGIN = `gcc -print-file-name=libfrtbegin.a | sed 's|/|-L/|`;
#	$CXXFLAGS      .= " -wd1476";
	if ($ICC_MAJOR eq 8 and $ICC_MINOR ne 0 or $ICC_MAJOR eq 9) {
	  $CXXFLAGS   .= " -wd1572";
	  $CFLAGS     .= " -wd1572";
	}
	if ($ICC_MAJOR ge '8') {
	  $FC          = "ifort";
	  $LIBIFCPATH  = `which ifort | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $FLIBS     =  $LIBFRTBEGIN ." -L". $LIBIFCPATH ." -lifcore ";# . $LIBG2C;
	}
	else                   {
	  $FC          = "ifc";
	  $LIBIFCPATH  = `which ifc | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $FLIBS     = $LIBG2C ." -L". $LIBIFCPATH ." -lF90 -lCEPCF90 -lintrins";
	}
	$FLIBS      .= " -lg2c";
	$FFLAGS        = "-save";
	$FEXTEND       = "-132";
	$XLIBS         = "-L" . $ROOTSYS . "/lib -lXpm  -lX11";
	$SYSLIBS       = "-lm -ldl -lrt";# -rdynamic";
	$CLIBS         = "-lm -ldl -lrt";# -rdynamic";
	$CRYPTLIBS     = "-lcrypt";
	$LD            = "icpc";
	$LDFLAGS       = "$XMACHOPT ";#--no-warn-mismatch";
	$F77LD         = $LD;
	$SO            = $CXX;
	$SOFLAGS       = "-shared -u*";
        $CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
        $CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";

	$EXTRA_CXXFLAGS= "";
	$EXTRA_CFLAGS  = "";
	$EXTRA_CPPFLAGS= "";
	$EXTRA_LDFLAGS = "";
	$EXTRA_SOFLAGS = "";

    } elsif ($STAR_HOST_SYS =~ /^i386_/ ||
	     $STAR_HOST_SYS =~ /^rh/    ||
	     $STAR_HOST_SYS =~ /^sl/    ||
	     $STAR_HOST_SYS =~ /gcc/ && $STAR_HOST_SYS !~ /^x86_darwin/ ) {
        #
        # Case linux
        #
	$CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
	$CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
      
	if ( $G77 =~ m/gfortran/ ){
	  # TODO: Possible cleanup to do between GFORTRAN and CERNLIB_LINUX
	  $CERNLIB_FPPFLAGS .= " -DCERNLIB_GFORTRAN";
	}
	if ($USE_64BITS){
	  $CERNLIB_FPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
	  $CERNLIB_CPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
	}

        #print "CERNLIB_FPPFLAGS = $CERNLIB_FPPFLAGS\n";
        $CXX_VERSION  = `$CXX -dumpversion`;
        chomp($CXX_VERSION);
	($CXX_MAJOR,$CXX_MINOR,$CXX_LOWER) = split '\.', $CXX_VERSION;
	$CERNLIB_FPPFLAGS .= " -DCERNLIB_GCC" . $CXX_MAJOR;
	$CERNLIB_CPPFLAGS .= " -DCERNLIB_GCC" . $CXX_MAJOR;
        print "CXX_VERSION : $CXX_VERSION MAJOR = $CXX_MAJOR MINOR = $CXX_MINOR\n";

        $CXXFLAGS    = "$XMACHOPT -fPIC -pipe -Wall -Woverloaded-virtual";

	# some fortran initial options
        if ($PGI) {
	  # under SL5 where PGI is installed, this test make PGI used
	    # but eventually fail at link-time - TBC [TODO: JL 200908]
	    $FC      = "pgf77";
	    $FFLAGS  = "";
	    $FEXTEND = "-Mextend";
	} else {
	    $FC      = $G77;
	    $FFLAGS  = $G77FLAGS;
	    $FEXTEND = $G77EXTEND;
	}


	# ---- compiler version fixes and command line option adjustements
	# general and using standards
        if ($CXX_VERSION < 3) {
	    $OSFID .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES";
	} else {
	    # can do elsif () later but for now, enable if CXX11 is defined
	    # AND version is al least 4.4 
	    if ( $CXX_MAJOR <= 4 && $CXX_MINOR <= 4 ){   # || ! defined($ENV{CXX11}) ) {
		# ansi works only with gcc3.2 actually ... may be removed later ...
		print "\tCXX version implies using C++ ansi syntax standards\n";
		$CXXFLAGS    .= " -ansi";
	    } else {
		# Starting from 4.4, c++0x has been implemented - feature metric
		# is not even though - see https://gcc.gnu.org/gcc-4.4/cxx0x_status.html
		# and related documents
		#   SL5 we had  4.3.2 supporting a weak set of c++11
		#   SL6 had gcc 4.4.7 with more advanced c++11 implementations via std=c++0x
		#   SL6 with gcc 4.8.2 supports all c++11 features via std=c++0x - for backward compat, 
		$CXXFLAGS    .= " -std=c++0x"; # -fpermissive";
		print "\tCXX version implies using C++/C++11 c++0x standard\n";
	    }
	}

        # use of pendantic / permissive 
	if ($CXX_MAJOR == 3 && $CXX_MINOR < 4 ) {
	    $CXXFLAGS    .= " -pedantic"; 
	}


	$CXXFLAGS    .= " -Wno-long-long";

	#
	# Additional GCC optimization flags if NODEBUG
	#
	if ( (defined( $ARG{NODEBUG} ) or $NODEBUG) && !defined($ENV{DEBUG_OPTIONS}) ) {
	    # Optimization is requested
	    my $optflags = "";

	    if ($CXX_VERSION < 3){
		$optflags = "-malign-loops=2 -malign-jumps=2 -malign-functions=2";
	    } elsif ( $CXX_VERSION < 4.5 ){
		# this naming convention starts at gcc 3.2 which happens to
		# have a change in the options.
		# Valid and used up to 4.4.7
		$optflags = "-falign-loops=2 -falign-jumps=2 -falign-functions=2";
	    } else {
		# 4.8.2
		# leave the alignement to the compiler.
		#   2015 NB: does it make sense to align on 2 bytes nowadays?
		$optflags = "-falign-loops -falign-jumps -falign-functions";
	    }


	    # JL patch for gcc 4.1 -> 4.3.x (report that it is broken in 4.4 as well)
	    if ( $STAR_HOST_SYS =~ m/(_gcc4)(\d+)/ ){
		print "\tChecking if OPTIM options tweak are needed for gcc V4.x series\n";
		if ( $2 <= 49 ){
		    # Note: all inlining failed with gcc 4.4.7 with no indication
		    # of a resolve up to 4.4.9 . Symbols would be removed and
		    # linking would fail.
		    $DEBUG .= " -fno-inline";
		    $FDEBUG = $DEBUG;
		} elsif ( $2 <= 82){
		    # Note: 4.8.2 is picky, we may need to adjust options here
		    #$DEBUG  =  "-O1 -g -fno-merge-constants";
		    if ( defined($ENV{OPTIM_WITH_O}) ) {
			# -O2 initially did ot work due to code issues (i.e. bugs
			# or tricks used with array overbound, the compiler resshuffled)
			# In case this happens again, the minal set of optimizing options
			# below worked fine and could be re-used
			#
			# Other possible options part of O1
			#   -fmerge-all-constants (implies merge-contstants)
			#   -finline-functions-called-once
			#   -fcombine-stack-adjustments
			#   -fcompare-elim
			#   -fcprop-registers
			#   -fdce -fdse
			#   -ftree-dce -ftree-dse 
			#   -frerun-cse-after-loop
			#   -ftree-dominator-opts
			# With O2
			#   -fpartial-inlining
			#   -foptimize-sibling-calls
			#  With O3
			#   -finline-functions
			$FDEBUG = $DEBUG   =  "-g -fif-conversion -fif-conversion2 -fforward-propagate -fmerge-constants -finline-small-functions -findirect-inlining -fpartial-inlining -fdevirtualize -floop-interchange -ftree-ccp";
		    } else {
			# STAR default optimization options
			$DEBUG   = "-O2 -g";
                        # downgraded pon request 2016/04/26
			$FDEBUG  = "-g -fif-conversion -fif-conversion2 -fforward-propagate -fmerge-constants -finline-small-functions -findirect-inlining -fpartial-inlining -fdevirtualize -floop-interchange -ftree-ccp";
		    }
		}
	    }

	    $DEBUG  .= " ".$optflags;
	    $FDEBUG .= " ".$optflags; # $DEBUG;
	    print "Set final OPTIM/DEBUG options as [$DEBUG]\n" unless ($param::quiet);
	}

        $CFLAGS   .= " -pipe -fPIC -Wall -Wshadow";
        $SOFLAGS  .= " -shared -Wl,-Bdynamic";

	$XLIBS     = "-L/usr/X11R6/$LLIB -lXpm -lX11";

        $THREAD    = "-lpthread";
        $CRYPTLIBS = "-lcrypt";

	$SYSLIBS   = "-lm -ldl";
	$CLIBS    .= " -L/usr/X11R6/$LLIB -lXt -lXpm -lX11 -lm -ldl";
	$SYSLIBS   .= " -lrt -rdynamic";
	$CLIBS     .= " -lrt -rdynamic";
	# print "*** $CXX_VERSION $SYSLIBS\n";
	

	if ( $G77 =~ m/gfortran/ ){
	    # nothing to set - used to be -lgfortran
	} else {
	    if ($CXX_VERSION >= 4 && $STAR_HOST_SYS =~ m/^x86_darwin/ ){
		# Same comment, not sure if V4 or a Mac issue
		# 2009/08 -> not an issue with gcc4
		$FLIBS = " -lg2c";
	    } else {
		$FLIBS = " -lg2c -lnsl";
	    }
	}

    } elsif ( $STAR_HOST_SYS =~ /x86_darwin/) {
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
	  $FINK_CXXFLAGS = "-I$FINK_DIR/include";
	  $FINK_CFLAGS = "-I$FINK_DIR/include";
	}
	if (-d "$FINK_DIR/lib") {$FINK_LDFLAGS = "-L$FINK_DIR/lib";}
      }
      $CXX           = "g++";
      $CC            = "gcc";
      
      if ($USE_64BITS){
	$CXXFLAGS = "-m64";
	$CFLAG    = "-m64";
	$LDFLAGS  = "-m64";
      } else {
	$CXXFLAGS = "-m32";
	$CFLAG    = "-m32";
	$LDFLAGS  = "-m32";
      }
      $CXXFLAGS      .= " -pipe -Wshadow  -W -Wall -Woverloaded-virtual -fsigned-char -fno-common";
      if ($CXX_MAJOR > 4 or $CXX_MAJOR == 4 and $CXX_MINOR >= 6) {$CXXFLAGS .= " -fpermissive";}
      $CXXFLAGS      .= " -Wshadow -Wunused-parameter -Wwrite-strings";
      if (! $EXTRA_CPPPATH) {$EXTRA_CPPPATH  =                         $FINK_CXXFLAGS;}
      else                  {$EXTRA_CPPPATH .= $main::PATH_SEPARATOR . $FINK_CXXFLAGS;}
      $CFLAGS       .= " -pipe -W -Wall -fsigned-char -fno-common";# -Df2cFortran";
      $CFLAGS      .= " -Wshadow -Wunused-parameter -Wwrite-strings";
      
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
      $LDALL         = "";# -Wl,-all_load";
      $LDNONE        = "";# -Wl,-noall_load";
#    $LDEXPORT      = " -Wl,-export-dynamic -Wl,-noinhibit-exec,-Bdynamic";
#    $LDALL         = " -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs";
#    $LDNONE        = " -Wl,--no-whole-archive -Wl,-Bdynamic";
      $LD            = $CXX;
#      $LDFLAGS       = "-bind_at_load";
      $LDFLAGS       .= " -mmacosx-version-min=10.$MACOSX_MINOR";
#      $LDFLAGS      .= " -expect_unresolved \"*\"";
      #FORCELINK     = yes
      #NEEDFINK      = yes
      if ($MACOSX_MINOR == 5) {$MACOSX_MINOR  = 4;}
      if ($MACOSX_MINOR == 4) {
	$SOFLAGS       = "-dynamiclib -single_module -undefined dynamic_lookup";# -install_name $(LIBDIR)/
#	$SOFLAGS       = "-dynamiclib -single_module -Wl,-dead_strip_dylibs -install_name $LIB/";
	#FORCELINK     = no
	#NEEDFINK      = no
	$CXXFLAGS     .= " -fvisibility-inlines-hidden";
	$CINTCXXFLAGS .= " -fvisibility-inlines-hidden";
      }
      elsif ($MACOSX_MINOR == 3) {
	$SOFLAGS      = "-dynamiclib -single_module -undefined dynamic_lookup";# -install_name $(LIBDIR)/
	$CXXFLAGS     .= " -Wno-long-double";
	$CFLAGS       .= " -Wno-long-double";
	$CINTCXXFLAGS .= " -Wno-long-double";
	$CINTCFLAGS   .= " -Wno-long-double";
      }
      elsif ($MACOSX_MINOR == 6) {
	$SOFLAGS       = "-dynamiclib -single_module -undefined dynamic_lookup";# suppress";# -install_name $(LIBDIR)/
      }
      else {
	$SOFLAGS       = "-dynamiclib -single_module -undefined dynamic_lookup";#  suppress";# -install_name $(LIBDIR)/
	$CXXFLAGS     .= " -Wno-long-double";
	$CFLAGS       .= " -Wno-long-double";
	$CINTCXXFLAGS .= " -Wno-long-double";
	$CINTCFLAGS   .= " -Wno-long-double";
      }
        $SOFLAGS  .= " -install_name \@rpath/%>";
      #      $SOEXT         = "dylib";
      # System libraries:
      $OSTHREADLIBDIR = "";
      $OSTHREADLIB = "";
      $SYSLIBS       = "-lm $FINK_LDFLAGS $OSTHREADLIBDIR $OSTHREADLIB -ldl";
      #XLIBS         = "$(XPMLIBDIR) $(XPMLIB) $(X11LIBDIR) -lXext -lX11";
      #CILIBS        = -lm $(EXTRA_LDFLAGS) $(FINK_LDFLAGS) -ldl

      # Fortran:
      $F77           = "";
      if ($MACOSX_MINOR == 4 && $ROOTBUILD =~ /g95/) {
	$F77          = "g95";
	$LIBS         = `$F77 -print-search-dirs | awk '/^install:/{print \$2}'`; chomp($LIBS);
	$FLIBS      = "-L$LIBS";
	$FLIBS      .= " -lf95";
      } else {
	$F77          = "gfortran";
	$FPP          = $F77;
#	$CC           = "gcc-4";
#	$CXX          = "g++-4";
	$FFLAGS       = "-funroll-loops -fomit-frame-pointer -ftree-vectorize -fno-second-underscore";
	$FFLAGS      .= " -w -fno-automatic -fd-lines-as-comments -Wall -W -Wsurprising -fPIC";
	$FEXTEND      = $G77EXTEND;
        $FPPFLAGS    .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_MACOSX -DCERNLIB_GFORTRAN";
#	$FLIBS      = `$F77 -print-file-name=libgfortran.$SOEXT`; chomp($FLIBS);
	$FLIBS      = `$F77 -print-file-name=libgfortran.dylib`; chomp($FLIBS);
	$FLIBS      .= " " . `$F77 -print-file-name=libgfortranbegin.a`; chomp($FLIBS);
      }
      # We add libg2c only in case of ppc because then we probably have cernlib
      # compiled with g77. In case of Intel Mac it should be compiled wiFLAGS      = "-fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";th the
      # same fortran we use.
      if ($MACOSX_CPU eq "powerpc") {
	$FLIBS .= " -L$(FINK_DIR)/lib -lg2c";
      }
      $FC = $F77;
      $FPP = $FC . " -E -P"; 
#      $FCviaAGETOFCOM = 
#	" test -f %>:b.g && rm %>:b.g; %FPP %FPPFLAGS %EXTRA_FPPFLAGS %_IFLAGS %EXTRA_FCPATH %<:b.F -o %>:b.g;" .
#	  " test -f %>:b.f && rm %>:b.f; %AGETOF %AGETOFLAGS -V f %<:b.g -o %>:b.f;" .
#	    " if [ -f %>:b.f ]; then %FC %FFLAGS %EXTRA_FPPFLAGS %FDEBUG -c %>:b.f %Fout%>;" .
#	      " else ". $FCCOM . " fi";
      $CERNLIB_FPPFLAGS .= " -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC ". 
	"-Dunix=unix -D__DARWIN__";
      $CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC ".
	"-DCERNLIB_MACOSX -Dunix=unix -D__DARWIN__";
#      $OSFID .= " __linux";
    } else {
	die "Unsupported platform $STAR_HOST_SYS\n";
    }
    # ============================================================
    # End of platform specific reshape of options and tweaks
    # ============================================================
#    print "FLIBS  ------  $FLIBS\n"; 



    $SYSLIBS   .= $threadlib;
    $CLIBS     .= $threadlib;

    #+
    # Option clean-up - JL 2015
    #-
    # Cleanup conflicting or redundant -ansi -std
    if ( $ROOTCFLAGS =~ m/std=/ && $CXXFLAGS =~ m/std=/ || $CXXFLAGS =~ /ansi/){
	# trim so we do not duplicate
	# NOTE: chosing to remove from ROOTCFLAGS though this may be a problem too. 
	#       Decided this as we begun introducing C++11 with a forward compatible
	#       compiler and ROOTCFLAGS are added to both CXXFLAGS and CFLAGS
	#
	my $tmp=$ROOTCFLAGS;
	$tmp =~ s/-std=[\w+-]*\s//;
	#print "DEBUG >> APPEND $tmp to CFLAGS and CXXFLAGS\n";
	$CFLAGS    .= $tmp;
	$CXXFLAGS  .= $tmp;
    } else {
	#print "DEBUG >> APPEND $tmp to CFLAGS [$CFLAGS] and CXXFLAGS [$CXXFLAGS]\n";
	$CFLAGS    .= $ROOTCFLAGS;
	$CXXFLAGS  .= $ROOTCFLAGS;
    }
    # remove duplicates options coming from ROOTCFLAGS - used block to be sure
    # vars are gone when we go out of scope
    {
	my(@ARGS)  =($CFLAGS ,$CXXFLAGS);    # to clean for duplicates / restore below in the same order
	my(@LABELS)=("CFLAGS","CXXFLAGS");   # labels should also relate to @ARGS order - for printing
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
	($CFLAGS,$CXXFLAGS) = @ARGS;        # <--- same order than above
    }



#    $OSFID .= " " . $STAR_SYS; $OSFCFID .= " " . $STAR_SYS;
#    if ( $STAR_SYS ne $STAR_HOST_SYS ) { $OSFID .= " " . $STAR_HOST_SYS; $OSFCFID .= " " . $STAR_HOST_SYS;}
    $OSFID .= " " . $STAR_HOST_SYS; $OSFCFID .= " " . $STAR_HOST_SYS;

    $OSFID    .= " __ROOT__";
    $CPPFLAGS .= " -D" . join ( " -D", split ( " ", $OSFID ) );
    $CFLAGS   .= " -D" . join ( " -D", split ( " ", $OSFID ) );

    if ($OSFCFID) {
	$FPPFLAGS  .= " -D" . join ( " -D", split ( " ", $OSFCFID ) );
    }

    $ROOTSRC = $ROOTSYS . "/include";

    $CERNINO = "#asps/Simulation/geant321/include" . $main::PATH_SEPARATOR . $CERN_ROOT . "/include";

    $CPPPATH = "#".  $main::PATH_SEPARATOR . "#StRoot" .  $main::PATH_SEPARATOR . $INCLUDE . $main::PATH_SEPARATOR . $ROOTSRC;# . $main::PATH_SEPARATOR . "#";
#    $CPPPATH .= $main::PATH_SEPARATOR ."#";# . $CERNINO;

    my $pwd = cwd();
    my $path2bin = $pwd . "/." . $STAR_HOST_SYS . "/bin";
    if ($PATH !~ /$STAR_BIN/) {$PATH = $STAR_BIN . ":" . $PATH;}
    $PATH = $path2bin .":". $PATH;  #print "PATH = $PATH\n";
    $FCPATH = $INCLUDE . $main::PATH_SEPARATOR . $CERNINO . $main::PATH_SEPARATOR . "#";


    # ------- packages -------
    # MySQL
    # my $os_name = `uname`;
    # chomp($os_name);
    #
    # *** Standard package first, then MYSTAR ***
    #	
    my ($MYSQLINCDIR,$mysqlheader);
    if ( defined($ENV{USE_LOCAL_MYSQL}) ){
	($MYSQLINCDIR,$mysqlheader) =
	    script::find_lib( $MYSTAR . "/include " .  $MYSTAR . "/include/mysql ".
			      $MYSQL . " " .
			      $MYSQL . "/include " .
			      "/sw/include/mysql ".
			      "/include /usr/include ".
			      "/usr/include/mysql  ".
			      "/usr/mysql/include  ".
			      "/usr/mysql  ",
			      "mysql.h");
    } else { 
	($MYSQLINCDIR,$mysqlheader) =
	    script::find_lib( $MYSQL . " " .
			      $MYSQL . "/include " .
			      "/sw/include/mysql ".
			      "/include /usr/include ".
			      "/usr/include/mysql  ".
			      "/usr/mysql/include  ".
			      "/usr/mysql  ".
			      $MYSTAR . "/include " .  $MYSTAR . "/include/mysql " ,
			      "mysql.h");
    }

    if (! $MYSQLINCDIR) {
	die "Can't find mysql.h in standard path and $MYSTAR/include  $MYSTAR/include/mysql\n";
    }

    # search for the config    
    my ($MYSQLCONFIG,$mysqlconf);
    # if ( defined($ENV{USE_LOCAL_MYSQL}) ){
	($MYSQLCONFIG,$mysqlconf) =
	    script::find_lib($MYSTAR . "/bin " .  $MYSTAR . "/bin/mysql ".
			     $MYSQL . " ".
			     $MYSQL . "/bin ".
			     "/usr/$LLIB/mysql /usr/bin/mysql /usr/bin ",
			     "mysql_config");
    # } else {
    #	($MYSQLCONFIG,$mysqlconf) =
    #	    script::find_lib($MYSQL . " ".
    #			     "/usr/$LLIB/mysql /usr/bin/mysql /usr/bin ".
    #			     $MYSTAR . "/bin " .  $MYSTAR . "/bin/mysql ",
    #			     "mysql_config");
    # } 


    # Associate the proper lib with where the inc was found
    my ($mysqllibdir)=$MYSQLINCDIR;
    $mysqllibdir =~ s/include/$LLIB/;

    # print "DEBUG :: $mysqllibdir\n";
    # Note - there is a trick here - the first element uses mysqllibdir
    #        which is dreived from where the INC is found hence subject to 
    #        USE_LOCAL_MYSQL switch. This may not have been obvious.
    # my ($MYSQLLIBDIR,$MYSQLLIB) =
    #	script::find_lib($mysqllibdir . " /usr/$LLIB/mysql ".
    #			 $MYSTAR . "/lib " .  $MYSTAR . "/lib/mysql ",
    #			 "libmysqlclient");
    #			 # "libmysqlclient_r libmysqlclient");
    # # die "*** $MYSQLLIBDIR,$MYSQLLIB\n";

    # if ($STAR_HOST_SYS =~ /^rh/ or $STAR_HOST_SYS =~ /^sl/) {
    if ( $mysqlconf ){
	$mysqlconf = "$MYSQLCONFIG/$mysqlconf";
	# if ( 1==1 ){
	# Do not guess, just take it - this leads to a cons error though TBC
	chomp($MYSQLLIB = `$mysqlconf  --libs`);
	# but remove -L which are treated separately by cons
	my(@libs) = split(" ", $MYSQLLIB);
	my($test) = shift(@libs);
	if ( $test =~ /-L/){
	    $MYSQLLIBDIR = $test; $MYSQLLIBDIR =~ s/-L//;
	    $MYSQLLIB = "";
	    foreach my $el (@libs){
		$MYSQLLIB  .= " ".$el if ($el !~ m/-L/);
	    }
	}
	
	# here is a check for libmysqlclient
	
	
	# die "DEBUG got $MYSQLLIBDIR $MYSQLLIB\n";
	
	# mysqlconf returns (on SL5, 64 bits)
	#  -L/usr/lib64/mysql -lmysqlclient -lz -lcrypt -lnsl -lm -L/usr/lib64 -lssl -lcrypto
	# } else {
	#    $MYSQLLIB .= " -L/usr/$LLIB";
	#    if (-r "/usr/$LLIB/libmystrings.a") {$MYSQLLIB .= " -lmystrings";}
	#    if (-r "/usr/$LLIB/libssl.a"      ) {$MYSQLLIB .= " -lssl";}
	#    if (-r "/usr/$LLIB/libcrypto.a"   ) {$MYSQLLIB .= " -lcrypto";}
	#    if ( $MYSQLLIB =~ m/client_r/     ) {$MYSQLLIB .= " -lpthread";}
	#    # if (-r "/usr/$LLIB/libk5crypto.a" ) {$MYSQLLIB .= " -lcrypto";}
	#    $MYSQLLIB .= " -lz";
	#    # $MYSQLLIB .= " -lz -lcrypt -lnsl";
	# }
    } else {
	die "No mysql_config found\n";
    }
    print "Using $mysqlconf\n\tMYSQLINCDIR = $MYSQLINCDIR MYSQLLIBDIR = $MYSQLLIBDIR  \tMYSQLLIB = $MYSQLLIB\n"
          if ! $param::quiet;

    # die "\n";



    # QT
    if ( defined($QTDIR) && -d $QTDIR) {
	$QT_VERSION = 3;

	if (-e $QTDIR . "/bin/moc") {
	    $QTLIBDIR = $QTDIR . "/lib";
	    $QTBINDIR = $QTDIR . "/bin";
	}
	if ($QTBINDIR) {
	    $QTINCDIR = $QTDIR . "/include";
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
	# if ( !defined($IVROOT)) {
	#    if ($QT_VERSION==4) {
	#	$IVROOT   = $ROOT . "/5.99.99/Coin2Qt4/$STAR_HOST_SYS/coin3d"; # the temporary place with the coin package
	#    } else {
	#	$IVROOT   = $ROOT . "/5.99.99/Coin2/.$STAR_HOST_SYS"; # the temporary place with the coin package
	#    }
	#    print "*** ATTENTION *** IVROOT $IVROOT\n";
	# }
	if ( ! defined($IVROOT) ){  $IVROOT = $MYSTAR;}
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
	    # try finding it in $MYSTAR
	    my($coin);
	    if ( -e "$MYSTAR/bin/coin-config"){
		$coin = "$MYSTAR/bin/coin-config";
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
    $LoggerDir = $MYSTAR . "/include/log4cxx";

    if (-d $LoggerDir) {
	$LoggerINCDIR = $MYSTAR . "/include";
	$LoggerLIBDIR = $MYSTAR . "/lib";
	$LoggerLIBS   = "-llog4cxx";
	print
	    "Use Logger  ",
	    "LIBDIR = $LoggerLIBDIR \tLoggerINCDIR = $LoggerINCDIR \tLoggerLIBS = $LoggerLIBS\n"
	    if $LoggerLIBDIR && ! $param::quiet;
    }
    # xml2
    my  ($XMLINCDIR,$XMLLIBDIR,$XMLLIBS) = ("","","");
    my ($xml) =  script::find_lib($MYSTAR . "/bin /usr/bin " . $LIBXML2_DIR . "/bin",
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


	# ($XMLLIBDIR,$XMLLIBS) = split(' ', $XML);
	# if ($XMLLIBDIR =~ /-L/){
	#    $XMLLIBDIR =~ s/-L//;
	# } else {
	#    # may not have any -L
	#    if ($XMLLIBS
	# }

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
 #Vc check SSE support
 my $cmd = "touch temp_gccflags.c; $CXX -E -dM -o - temp_gccflags.c | grep -q SSE";
 my $VcCPPFLAGS = " -DVC_IMPL=SSE";
 if ($STAR_HOST_SYS =~ 'gcc432$' || system($cmd)) {# No SSE
   $VcCPPFLAGS = " -DVC_IMPL=Scalar";
   if (-e "temp_gccflags.c") {`rm temp_gccflags.c`;}
 }
    my @params = (
		  'Package'        => 'None',
		  'CPP'            => $CPP,
		  'FPP'            => $FPP,
		  'CPPPATH'        => $CPPPATH,
		  'EXTRA_CPPPATH'  => $EXTRA_CPPPATH,
		  'CERNLIB_FPPFLAGS'  => $CERNLIB_FPPFLAGS,
		  'CPPFLAGS'       => $CPPFLAGS,
		  'EXTRA_CPPFLAGS' => $EXTRA_CPPFLAGS,
		  'CERNLIB_CPPFLAGS'  => $CERNLIB_CPPFLAGS,
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
		  'Fout'           => $Fout,
		  'CXXinp'         => $CXXinp,
		  'Cinp'           => $Cinp,
		  'Cout'           => $Cout,
		  'Lout'           => $Lout,
		  'SoOUT'          => $SoOUT,
		  'FCCOM'          => $FCCOM,
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
		  'LIBPATH'        => $LIBPATH,
		  'LINKCOM'        => $LINKCOM,
		  'SO'             => $SO,
		  'SOFLAGS'        => $SOFLAGS,
		  'EXTRA_SOFLAGS'  => $EXTRA_SOFLAGS,
		  'SoOUT'          => $SoOUT,
		  'STIC'           => $STIC,
		  'LINKMODULECOM'  => $MAKELIB,
		  'AR'             => $AR,
		  'ARFLAGS'        => $ARFLAGS,
		  'ARCOM'          => $ARCOM,
		  'RANLIB'         => 'ranlib',
		  'AS'             => 'as',
		  'ASFLAGS'        => '',
		  'ASCOM'          => '%AS %%DEBUG ASFLAGS %< -o %>',
		  'PREFLIB'        => 'lib',
		  'SUFLIB'         => $A,
		  'SUFLIBS'        => "." . $SOEXT . $main::PATH_SEPARATOR . "." . $A,
		  'SUFSOLIB'       => $SOEXT,
		  'SUFEXE'         => $EXESUF,
		  'SUFMAP'         => {
		      '.g'   => 'build::command::agetof',
		      '.age' => 'build::command::agetof',
		      '.f'   => 'build::command::fc',
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
		  'SUFOBJ' => "." . $O,
		  'ENV'    => {
		      'CPATH'           => $CPATH,
		      'PATH'            => $PATH,
		      'LM_LICENSE_FILE' => $LM_LICENSE_FILE,
		      'INCLUDE'         => $INCLUDE_PATH,
		      'ROOT'            => $ROOT,
		      'ROOT_LEVEL'      => $ROOT_LEVEL,
		      'ROOT_VERSION_MAJOR' => $ROOT_VERSION_MAJOR,
		      'ROOTSRC'         => $ROOTSRC,
		      'ROOTSYS'         => $ROOTSYS,
		      'CINTSYSDIR'      => $CINTSYSDIR,
		      'LD_LIBRARY_PATH' => $LD_LIBRARY_PATH,
		      'SHLIB_PATH'      => $SHLIB_PATH,
		      'LIB'             => $LIB,
		      'PGI'             => $PGI,
		      'PGILIB'          => $PGILIB,
		      'STAR'            => $STAR,
		      'CERN_ROOT'       => $CERN_ROOT,
		      'STAF'            => $STAF,
		      'STAR_BIN'        => $STAR_BIN,
		      'STAR_SYS'        => $STAR_HOST_SYS,
		      'STAR_HOST_SYS'   => $STAR_HOST_SYS,
		      'STAR_VERSION'    => $STAR_VERSION,
		      'PERL5LIB'        => $PERL5LIB,
		      'OPTSTAR'         => $MYSTAR,
		      'QTDIR'           => $QTDIR,
		      'COIN3DIR'        => $COIN3DIR,
		      'IVROOT'          => $IVROOT,
		      'HOME'            => $HOME
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
			    'LIBDIR' => $CERN . "/lib",
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
		       'Vc' => {
			   'CPP'   => $VcCPPFLAGS
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
