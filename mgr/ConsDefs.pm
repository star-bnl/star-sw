# $Id: ConsDefs.pm,v 1.105 2009/02/20 01:47:01 jeromel Exp $
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
    if ( !$OPTSTAR ) { $OPTSTAR = "/opt/star"; } # print "OPTSTAR = $OPTSTAR\n"; die;
    if ( !$STAR_SYS ) {
        $STAR_SYS = `sys`;
        chop($STAR_SYS);
        $STAR_HOST_SYS = $STAR_SYS;
    }
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

    $EXTRA_CPPFLAGS = "";

    $AFSFLAGS = "";
    $AFSDIR   = "/usr/afsws";
    $AFSLIBS  = "-L" . $AFSDIR . "/lib -L" . $AFSDIR . "/lib/afs";
    $AFSLIBS .= " -lkauth -lprot -lubik -lauth -lrxkad -lsys -ldes -lrx -llwp";
    $AFSLIBS .= " -lcmd -lcom_err -laudit " . $AFSDIR . "/lib/afs/util.a";
    if ( !$ROOT )       { $ROOT       = $AFS_RHIC."/star/ROOT"; }
    if ( !$ROOT_LEVEL ) { $ROOT_LEVEL = "2.25.01"; }
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
    $IS_64BITS     = ($STAR_HOST_SYS =~ m/64_/ && -e "/usr/lib64" );
    if ($IS_64BITS){
	$LLIB = "lib64";
    } else {
	$LLIB = "lib";
    }


    $G77           = "g77";
    $G77FLAGS      = "-fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";
    if ($STAR_HOST_SYS =~ /gcc3/) {
      $G77FLAGS    = "-pipe " . $G77FLAGS;
    }
    $G77EXTEND     = "-ffixed-line-length-132";

    $CXX           = "g++";
    $CXXFLAGS      = "-fpic -w";
    $EXTRA_CXXFLAGS= "";
    $CXXOPT        = "";

    $CC            = "gcc";
    $CFLAGS        = "-fpic -w";
    $EXTRA_CFLAGS  = "";

    $FC            = $G77;
    $FCPATH        = "";
    $EXTRA_FCPATH  = "";
    $FFLAGS        = $G77FLAGS;
    $FEXTEND       = $G77EXTEND;
    $CPPCERN       = " -DCERNLIB_TYPE -DCERNLIB_DOUBLE -DCERNLIB_NOQUAD -DCERNLIB_LINUX ";
    $FPPFLAGS      = $CPPCERN;
    $EXTRA_FPPFLAGS= "";
    $CXXFLAGS     .= $CPPCERN;

    $CPP           = $CC . " -E -P";

    $AR            = "ar";
    $ARFLAGS       = "rvu";
    $LD            = $CXX;
    $LDFLAGS       = "";#--no-warn-mismatch";#$CXXFLAGS;
    $LDEXPORT      = " -Wl,-export-dynamic -Wl,-noinhibit-exec,-Bdynamic";
    $LDALL         = " -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs";
    $LDNONE        = " -Wl,--no-whole-archive -Wl,-Bdynamic";
    $EXTRA_LDFLAGS = "";
    $F77LD         = $LD;
    $F77LDFLAGS    = "";#$LDFLAGS;
    $SO            = $CXX;
    $SOFLAGS       = "";
    $STIC          = "stic";
    $STICFLAGS     = "";
    $AGETOF        = "agetof";
    $AGETOFLAGS    = "-V 1";
    $LIBG2C        = `gcc -print-file-name=libg2c.a | awk '{ if (\$1 != "libg2c.a") print \$1}'`;
    chomp($LIBG2C);
    $LIBSTDC       = `gcc -print-file-name=libstdc++.a | awk '{ if (\$1 != "libstdc++.a") print \$1}'`;
    chomp($LIBSTDC);
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
    if ( defined( $ARG{GPROF} ) or defined($ENV{GPROF}) ){
	print "Using GPROF\n" unless ($param::quiet);
	$EXTRA_CXXFLAGS= " -pg ";
	$EXTRA_CFLAGS  = " -pg ";
	$EXTRA_FPPFLAGS= " -pg ";
	$EXTRA_LDFLAGS = " -pg ";
	$GPROF         = "yes";
    } else {
	# GPROF will imply that we do not allow optimized
	$GPROF= undef;
	if ( defined( $ARG{NODEBUG} ) || $NODEBUG ) {
	    $DEBUG = "-O -g";
	    $FDEBUG= $DEBUG;
	    print "set DEBUG = $DEBUG\n" unless ($param::quiet);
	}
    }




    $CINTSYSDIR    = $ROOTSYS . "/cint";
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
    $ARCOM  = "%AR %ARFLAGS %> %< ; %RANLIB %>"; # "%AR %ARFLAGS %> %<;%RANLIB %>",

    $CXXCOM =
 "%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %CXXinp%< %Cout%>";
    $CCCOM = "%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %Cinp%< %Cout%>";
    $MAKELIB .= "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
    $LINKCOM =
      "%LD %DEBUG %LDFLAGS %EXTRA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 my $FCCOM = "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>;";
 my $FCviaAGETOFCOM
 = " test -f %>:b.g && /bin/rm %>:b.g; %CPP %FPPFLAGS %EXTRA_FPPFLAGS %_IFLAGS %EXTRA_FCPATH %<:b.F -o %>:b.g;"
 . " test -f %>:b.f && /bin/rm %>:b.f; %AGETOF %AGETOFLAGS -V f %<:b.g -o %>:b.f;";

    $FCviaAGETOFCOM .= " if [ -f %>:b.f ]; then %FC %FFLAGS %EXTRA_FPPFLAGS %FDEBUG -c %>:b.f %Fout%>;";
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

    # cernlib if not intel WNT
    if ( $STAR_HOST_SYS !~ /^intel_wnt/ ) {
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
	  }
	  else {
	    $cernl = "cernlib";
	  }
	} else {
	    print "WARNING: using cernlib from the default path\n"
		unless ($param::quiet); 
	    $cernl = "cernlib -s";
	    $packl = "packlib";
	    $kernl = "kernlib";
	}

	$CERNLIBS .= " " . `$cernl pawlib packlib graflib/X11 packlib mathlib kernlib`;
	$CERNLIBS =~ s/packlib\./$packl\./g;
	$CERNLIBS =~ s/kernlib\./$kernl\./g;
	$CERNLIBS =~ s/$strip//g     if ($strip ne "");
	$CERNLIBS =~ s/lib /lib64 /g if ($IS_64BITS);


	chop($CERNLIBS);
	print "CERNLIB = $CERNLIBS\n" unless ($param::quiet);
    }

    print "DEBUG >>> We will select architecture/compiler based on $STAR_HOST_SYS\n" if ($param::debug);

    
    if ( ($STAR_HOST_SYS =~ m/^rh/ && $STAR_HOST_SYS =~ /_icc/) ||
	 ($STAR_HOST_SYS =~ m/^sl/ && $STAR_HOST_SYS =~ /_icc/) ) {
	#
	# Intel ICC
	#

	#print "DEBUG >>> Switching ot ICC compiler\n" if ($param::debug);
	$PLATFORM      = "linux";
	$ARCH          = "linuxicc";
	$PGI           = "";
	$PGILIB        = "";
	$CC            = "icc";
	$CXX           = "icc";
	$CPP           = $CC . " -EP";
	$CXXFLAGS      = "-w -ansi -fPIC -wd1476"; #-fpstkchk";
	$CFLAGS        = "-restrict -w -fPIC";# -fpstkchk";# -restrict";# -Wall
	$ICC_MAJOR     = `$CXX -V -dryrun  >& /tmp/icc_version; awk '{ if (NR==1) print \$8 }' /tmp/icc_version| cut -d'.' -f1; /bin/rm  /tmp/icc_version;`;
        $ICC_MINOR     = `$CXX -V -dryrun  >& /tmp/icc_version; awk '{ if (NR==1) print \$8 }' /tmp/icc_version| cut -d'.' -f2; /bin/rm  /tmp/icc_version;`;
	chomp($ICC_MAJOR); chomp($ICC_MINOR);
	$LIBFRTBEGIN   = `gcc -print-file-name=libfrtbegin.a | awk '{ if (\$1 != "libfrtbegin.a") print \$1}'`;
	chomp($LIBFRTBEGIN);
	$NOOPT         = "-O0";
	$CXXFLAGS     .= " -wd1476";
	if ($ICC_MAJOR eq 8 and $ICC_MINOR ne 0 or $ICC_MAJOR eq 9) {
	  $CXXFLAGS   .= " -wd1572";
	  $CFLAGS     .= " -wd1572";
	}
	if ($ICC_MAJOR ge '8') {
	  $FC          = "ifort";
	  $LIBIFCPATH  = `which ifort | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $F77LIBS     =  $LIBFRTBEGIN ." -L". $LIBIFCPATH ." -lifcore ";# . $LIBG2C;
	}
	else                   {
	  $FC          = "ifc";
	  $LIBIFCPATH  = `which ifc | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $F77LIBS     = $LIBG2C ." -L". $LIBIFCPATH ." -lF90 -lCEPCF90 -lintrins";
	}
	$F77LIBS      .= " -lg2c";
	$FLIBS         = $F77LIBS;
	$FFLAGS        = "-save";
	$FEXTEND       = "-132";
	$XLIBS         = "-L" . $ROOTSYS . "/lib -lXpm  -lX11";
	$SYSLIBS       = "-lm -ldl";# -rdynamic";
	$CLIBS         = "-lm -ldl";# -rdynamic";
	$CRYPTLIBS     = "-lcrypt";
	$LD            = "icpc";
	$LDFLAGS       = "";#--no-warn-mismatch";
	$F77LD         = $LD;
	$SO            = $CXX;
	$SOFLAGS       = "-shared -u*";
        $CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
        $CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";

	$EXTRA_CXXFLAGS= "";
	$EXTRA_CFLAGS  = "";
	$EXTRA_CPPFLAGS= "";
	$EXTRA_LDFLAGS = "";

    } elsif (/^alpha_dux/) {
	#
	# Trying True64
	#
        $ARCOM  = "%AR %ARFLAGS %>  -input %< ; %RANLIB %>";
	$PLATFORM      = "alpha";
	$ARCH          = "alphaxxx6";
	$CC            = "cc";
	$CXX           = "cxx";
	$CPP           = $CC . " -EP";
	$CXXFLAGS      = "tlocal";
	$CFLAGS        = "";
	$FC            = "f77";
	$F77LIBS       = "/usr/shlib/libFutil.so /usr/shlib/libUfor.so /usr/shlib/libfor.so /usr/shlib/libots.so";
	$FLIBS         = $F77LIBS;
	$FFLAGS        = "-old_f77";
	$FEXTEND       = "-extend_source -shared -warn argument_checking -warn nouninitialize";
	$NOOPT         = "-O0";
	$XLIBS         = "-L" . $ROOTSYS . "/lib -lXpm  -lX11";
	$SYSLIBS       = "-lm";
	$CLIBS         = "-lm -ltermcap";
	$LD            = $CXX;
	$LDFLAGS       = "";
	$LDEXPORT      = " -Wl,-call_shared -Wl,-expect_unresolved -Wl,\"*\""; #-B symbolic
        $LDALL         = " -Wl,-all";
        $LDNONE        = " -Wl,-none";
	$F77LD         = $LD;
	$SO            = $CXX;
	$SOFLAGS       = "-shared -nocxxstd -Wl,-expect_unresolved,*,-msym,-soname,";
	$OSFID        .= " ST_NO_NAMESPACES";

	$EXTRA_CXXFLAGS= "-Iinclude -long_double_size 64";
	$EXTRA_CFLAGS  = "";
	$EXTRA_CPPFLAGS= "";
	$EXTRA_LDFLAGS = "";

    } elsif (/^sun4x_/) {
	#
	# Solaris
	#
        $PLATFORM = "solaris";
        $ARCH     = "solarisCC5";
        if (/^sun4x_56/) {$OSFCFID    = "__SunOS_5_6";}
	if (/^sun4x_58/) {$OSFCFID    = "__SunOS_5_8";}
        $OSFCFID .= " CERNLIB_SOLARIS CERNLIB_SUN CERNLIB_UNIX DS_ADVANCED SOLARIS";
        if ($STAR) {
            $OSFID .= " ST_NO_MEMBER_TEMPLATES";
        }
        $OSFCFID .= " SUN Solaris sun sun4os5 " . $STAR_SYS;
        $EXTRA_CPPPATH = $main::PATH_SEPARATOR . "/usr/openwin/include";
	$SUNWS = $ENV{'SUNWS'};
	$SUNOPT= $ENV{'SUNOPT'};
	if( ! defined($SUNWS) ){ $SUNWS = "WS5.0";}
	if( ! defined($SUNOPT)){ $SUNOPT= "/opt";}
        $CC     = "$SUNOPT/$SUNWS/bin/cc";
        $CXX    = "$SUNOPT/$SUNWS/bin/CC";
	$CPP           = $CC . " -EP";
        $CXXCOM =
"%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS -ptr%ObjDir %_IFLAGS -c %CXXinp%< %Cout%>";
        $FC             = "$SUNOPT/$SUNWS/bin/f77";
        $CXXFLAGS       = "-KPIC";
        $CLIBS        =
          "-lm -ltermcap -ldl -lnsl -lsocket -lgen $SUNOPT/$SUNWS/lib/libCrun.so -L. -lCstd -lmalloc";
          # Brute force required for CC WS6.0 (?). Links all others but that one
	  # (libCrun however isa softlink unlike the others).
          # -L" . $OPTSTAR  . "/lib -lCstd -liostream -lCrun";
        $FLIBS = "-L$SUNOPT/$SUNWS/lib -lM77 -lF77 -lsunmath";
        $XLIBS = "-L" . $ROOTSYS . "/lib -lXpm -L/usr/openwin/lib -lX11";

        #   $XLIBS     = "-L/usr/local/lib -lXpm -L/usr/openwin/lib -lX11";
        $SYSLIBS    = "-lmalloc -lm -ldl -lnsl -lsocket";
        $FFLAGS     = "-KPIC -w";
        $FEXTEND    = "-e";
        $CFLAGS     = "-KPIC";
        $LD         = $CXX;
        $LDFLAGS    = " -Bdynamic";
	$F77LD         = $LD;
        $SO         = $CXX;
        $SOFLAGS    = "-G -ptr%ObjDir";

        $EXTRA_CXXFLAGS = " -D__CC5__";
        $EXTRA_CFLAGS   = " -D__CC5__";
	$EXTRA_CPPFLAGS = "";
	$EXTRA_LDFLAGS  = "";


	# ATTENTION
	# - Below is a generic gcc support tweaks
	# - Any platform specific support needs to appear prior to this
    } elsif ($STAR_HOST_SYS =~ /^i386_/ ||
	     $STAR_HOST_SYS =~ /^rh/    ||
	     $STAR_HOST_SYS =~ /^sl/    ||
	     $STAR_HOST_SYS =~ /gcc/    ) {
        #
        # Case linux
        #
	$CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
	$CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX  -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";

        #print "CERNLIB_FPPFLAGS = $CERNLIB_FPPFLAGS\n";
        $CXX_VERSION  = `$CXX -dumpversion`;
        chomp($CXX_VERSION);
	($CXX_MAJOR,$CXX_MINOR) = split '\.', $CXX_VERSION;

        # print "CXX_VERSION : $CXX_VERSION MAJOR = $CXX_MAJOR MINOR = $CXX_MINOR\n";
        $CXXFLAGS     = "-pipe -fPIC -Wall -Woverloaded-virtual";
	my $optflags = "";

        if ($CXX_VERSION < 3) {
	    $OSFID .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES";
	} else {
            # ansi works only with gcc3.2 actually ... may be removed later ...
	    $CXXFLAGS    .= " -ansi";
	}

	if ($CXX_MAJOR == 3 and $CXX_MINOR < 4) {$CXXFLAGS    .= " -pedantic"; } # -fpermissive ?
	#	  else {
	#	  print "CXXFLAGS = $CXXFLAGS\n"; die;
	#	}

	$CXXFLAGS    .= " -Wno-long-long";

	# Additional optimization flags if NODEBUG
	if ( defined( $ARG{NODEBUG} ) or $NODEBUG ) {
	    if ($CXX_VERSION < 3){
		$optflags = "-malign-loops=2 -malign-jumps=2 -malign-functions=2";
	    } else {
		# this na1ming convention starts at gcc 3.2 which happens to
		# have a change in the options
		$optflags = "-falign-loops=2 -falign-jumps=2 -falign-functions=2";
	    }
	    print "set DEBUG = $DEBUG\n" unless ($param::quiet);
	}

	if ($optflags) {
	    $CFLAGS   .= " " . $optflags;
	    $CXXFLAGS .= " " . $optflags;
	    $G77FLAGS .= " " . $optflags;
	}
        $CFLAGS    = "-pipe -fPIC -Wall -Wshadow";
        $SOFLAGS   = "-shared -Wl,-Bdynamic";

	$XLIBS     = "-L/usr/X11R6/$LLIB -lXpm -lX11";

        $THREAD    = "-lpthread";
        $CRYPTLIBS = "-lcrypt";

	if ($CXX_VERSION >= 4){
	    # Either a version 4 issue but made thisfor Mac
	    $SYSLIBS   = "-lm -ldl -dynamiclib -single_module ";
	    $CLIBS    .= " -L/usr/X11R6/$LLIB -lXt -lXpm -lX11 -lm -ldl  -dynamiclib -single_module ";
	} else {
	    $SYSLIBS   = "-lm -ldl -rdynamic";
	    $CLIBS    .= " -L/usr/X11R6/$LLIB -lXt -lXpm -lX11 -lm -ldl  -rdynamic ";
	}
	# print "*** $CXX_VERSION $SYSLIBS\n";

        if ($PGI) {
	    $FC    = "pgf77";
	    $FFLAGS = "";
	    $FEXTEND = "-Mextend";
	} else {
	    $FC      = $G77;
	    $FFLAGS  = $G77FLAGS;
	    $FEXTEND = $G77EXTEND;
	}
	if ($CXX_VERSION >= 4){
	    # Same coment, not sure if V4 or a Mac issue
	    $F77LIBS = " -lg2c";
	} else {
	    $F77LIBS = " -lg2c -lnsl";
	}
        $FLIBS  .= $F77LIBS;


    } else {
	die "Unsupported platform $STAR_HOST_SYS\n";
    }
    # ============================================================
    # End of platform specific reshape of options and tweaks
    # ============================================================



    $SYSLIBS   .= $threadlib;
    $CLIBS     .= $threadlib;
    $CFLAGS    .= $ROOTCFLAGS;
    $CXXFLAGS  .= $ROOTCFLAGS;
    if ( $STAR_SYS ne $STAR_HOST_SYS ) { $OSFID .= " " . $STAR_HOST_SYS; $OSFCFID .= " " . $STAR_HOST_SYS;}
    $OSFID    .= " __ROOT__";
    $CPPFLAGS .= " -D" . join ( " -D", split ( " ", $OSFID ) );
    $CFLAGS   .= " -D" . join ( " -D", split ( " ", $OSFID ) );

    if ($OSFCFID) {$FPPFLAGS  .= " -D" . join ( " -D", split ( " ", $OSFCFID ) );}

    $ROOTSRC = $ROOTSYS . "/include";

    $CERNINO = $CERN_ROOT . "/include";

    $CPPPATH = "#StRoot" .  $main::PATH_SEPARATOR . $INCLUDE . $main::PATH_SEPARATOR . $ROOTSRC;# . $main::PATH_SEPARATOR . "#";
#    $CPPPATH .= $main::PATH_SEPARATOR ."#";# . $CERNINO;

    my $pwd = cwd();
    my $path2bin = $pwd . "/." . $STAR_HOST_SYS . "/bin";
    if ($PATH !~ /$STAR_BIN/) {$PATH = $STAR_BIN . ":" . $PATH;}
    $PATH = $path2bin .":". $PATH;  #print "PATH = $PATH\n";
    $FCPATH = $INCLUDE . $main::PATH_SEPARATOR . $CERNINO;


    # ------- packages -------
    # MySQL
    #my $os_name = `uname`;
    #chomp($os_name);
    my ($MYSQLINCDIR,$mysqlheader) = 
	script::find_lib( $MYSQL . " " .
			 "/include /usr/include ".
			 "/usr/include/mysql  ".
			 "/usr/mysql/include  ".
			 "/usr/mysql  ".
			 $OPTSTAR . "/include " .  $OPTSTAR . "/include/mysql " ,
			 "mysql.h");
    if (! $MYSQLINCDIR) {
      die "Can't find mysql.h in $OPTSTAR/include  $OPTSTAR/mysql/include ";
    }

    my ($mysqllibdir)=$MYSQLINCDIR;

    $mysqllibdir =~ s/include/$LLIB/;
#    print "DEBUG :: $mysqllibdir\n";
    my ($MYSQLLIBDIR,$MYSQLLIB) =
	script::find_lib($mysqllibdir . " /usr/$LLIB/mysql ".
			 $OPTSTAR . "/lib " .  $OPTSTAR . "/lib/mysql ",
			 "libmysqlclient");
    if ($STAR_HOST_SYS =~ /^rh/ or $STAR_HOST_SYS =~ /^sl/) {
	$MYSQLLIB .= " -L/usr/$LLIB";
	if (-r "/usr/$LLIB/libmystrings.a") {$MYSQLLIB .= " -lmystrings";}
	if (-r "/usr/$LLIB/libssl.a"      ) {$MYSQLLIB .= " -lssl";}
	if (-r "/usr/$LLIB/libcrypto.a"   ) {$MYSQLLIB .= " -lcrypto";}
	$MYSQLLIB .= " -lz";
    }
    print "Use MYSQLINCDIR = $MYSQLINCDIR MYSQLLIBDIR = $MYSQLLIBDIR  \tMYSQLLIB = $MYSQLLIB\n"
	if $MYSQLLIBDIR && ! $param::quiet;

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
	if ( !defined($IVROOT)) {
	    if ($QT_VERSION==4) {
		$IVROOT   = $ROOT . "/5.99.99/Coin2Qt4/$STAR_HOST_SYS/coin3d"; # the temporary place with the coin package 
	    } else {
		$IVROOT   = $ROOT . "/5.99.99/Coin2/.$STAR_HOST_SYS"; # the temporary place with the coin package
	    }
	    print "*** ATTENTION *** IVROOT $IVROOT\n";
	}
	if ( defined($IVROOT) &&  -d $IVROOT) {
	    if (-e $IVROOT . "/bin/coin-config") {
		$COIN3DIR     = $IVROOT;
		$COIN3DBINDIR = $COIN3DIR . "/lib";
		$COIN3DLIBDIR = $COIN3DIR . "/bin";
	    }
	    if ($COIN3DBINDIR) {
		$COIN3DINCDIR = $COIN3DIR . "/include";
		$COIN3DFLAGS  = ""; # "-DR__QT";#-DQT_THREAD_SUPPORT";
		$COIN3DLIBS   = "-lCoin -lSmallChange -lSoQt -lsimage";
		
		print "Use COIN3DLIBDIR = $COIN3DLIBDIR \tCOIN3DINCDIR = $COIN3DINCDIR \tCOIN3DFLAGS = $COIN3DFLAGS \tCOIN3DLIBS = $COIN3DLIBS\n"
		    if $COIN3DLIBDIR ;//&& ! $param::quiet;
	    }
	}
    }

    # Logger
    $LoggerDir = $OPTSTAR . "/include/log4cxx";
    if (-d $LoggerDir) {
	$LoggerINCDIR = $OPTSTAR . "/include";
	$LoggerLIBDIR = $OPTSTAR . "/lib";
	$LoggerLIBS   = "-llog4cxx";
	print 
	    "Use Logger  ",
	    "LIBDIR = $LoggerLIBDIR \tLoggerINCDIR = $LoggerINCDIR \tLoggerLIBS = $LoggerLIBS\n"
	    if $LoggerLIBDIR && ! $param::quiet;
    }
 # xml2
    my  ($XMLINCDIR,$XMLLIBDIR,$XMLLIBS) = ("","","");
    my ($xml) =  script::find_lib($OPTSTAR . "/bin /usr/bin",
				  "xml2-config");
    if ($xml) {
      $xml .= "/xml2-config";
      $XMLINCDIR = `$xml --cflags`;
      chomp($XMLINCDIR);
      $XMLINCDIR =~ s/-I//;
      my $XML = `$xml --libs`;# print "$XML\n";
      ($XMLLIBDIR,$XMLLIBS) = split ' ', $XML;
      $XMLLIBDIR =~ s/-L//;
      my $XMLVersion = `$xml --version`;#      print "XMLVersion = $XMLVersion\n";
      my ($major,$minor) = split '\.', $XMLVersion;# print "major = $major,minor = $minor\n";
      $XMLCPPFlag = "";#-DXmlTreeReader";
      if ($major < 2 or $major == 2 and $minor < 5) {$XMLCPPFlag = "-DNoXmlTreeReader";}

      print "Use xml $xml XMLLIBDIR = $XMLLIBDIR \tXMLINCDIR = $XMLINCDIR \tXMLLIBS = $XMLLIBS XMLCPPFlag =$XMLCPPFlag\n" if $XMLLIBDIR && ! $param::quiet;
    }
    my @params = (
		  'Package'       => 'None',
		  'CPP'           => $CPP,
		  'CPPPATH'       => $CPPPATH,
		  'EXTRA_CPPPATH' => $EXTRA_CPPPATH,
		  'CERNLIB_FPPFLAGS' => $CERNLIB_FPPFLAGS,
		  'CPPFLAGS'      => $CPPFLAGS,
		  'EXTRA_CPPFLAGS'=> $EXTRA_CPPFLAGS,
		  'CERNLIB_CPPFLAGS' => $CERNLIB_CPPFLAGS,
		  'ROOTLIBS'      => $ROOTLIBS,
		  'DEBUG'         => $DEBUG,
		  'GPROF'         => $GPROF,
		  'FDEBUG'        => $FDEBUG,
		  'NOOPT'         => $NOOPT,
		  'G77'           => $G77,
		  'G77FLAGS'      => $G77FLAGS,
		  'G77EXTEND'     => $G77EXTEND,
		  'LIBG2C'        => $LIBG2C,
		  'LIBSTDC'       => $LIBSTDC,
		  'FC'            => $FC,
		  'FPPFLAGS'      => $FPPFLAGS,
		  'EXTRA_FPPFLAGS'=> $EXTRA_FPPFLAGS,
		  'FEXTEND'       => $FEXTEND,
		  'FFLAGS'        => $FFLAGS,
		  'FCPATH'        => $FCPATH,
		  'EXTRA_FCPATH'  => $EXTRA_FCPATH,
		  'Fout'          => $Fout,
		  'CXXinp'        => $CXXinp,
		  'Cinp'          => $Cinp,
		  'Cout'          => $Cout,
		  'Lout'          => $Lout,
		  'SoOUT'         => $SoOUT,
		  'FCCOM'         => $FCCOM,
		  'AGETOF'        => $AGETOF,
		  'AGETOFLAGS'    => $AGETOFLAGS,
		  'AGETOFCOM'     => $AGETOFCOM,
		  'FCviaAGETOFCOM'=> $FCviaAGETOFCOM,
		  'CC'            => $CC,
		  'CFLAGS'        => $CFLAGS,
		  'EXTRA_CFLAGS'  => $EXTRA_CFLAGS,
		  'KUIP'          => $KUIP,
		  'KUIPCOM'       => '%KUIP %< %<.f && %FC %FDEBUG %FFLAGS -c %<.f -o %>',
		  'CCCOM'         =>
		  '%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS -c %Cinp%< %Cout%>',
		  'CXX'            => $CXX,
	          'CXX_VERSION'    => $CXX_VERSION,
		  'CXXFLAGS'       => $CXXFLAGS,
		  'EXTRA_CXXFLAGS' => $EXTRA_CXXFLAGS,
		  'CXXCOM'         => $CXXCOM,
		  'CXX_VERSION'    => $CXX_VERSION,
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
		      'CERN_ROOT'       => $CERN_ROOT,
		      'STAF'            => $STAF,
		      'STAR_BIN'        => $STAR_BIN,
		      'STAR_SYS'        => $STAR_HOST_SYS,
		      'STAR_VERSION'    => $STAR_VERSION,
		      'PERL5LIB'        => $PERL5LIB,
		      'OPTSTAR'         => $OPTSTAR,
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
			     }
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
