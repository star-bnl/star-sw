# $Id: ConsDefs.pm,v 1.64 2004/01/15 01:01:48 fisyak Exp $
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
    $BUILD   = "#." . $STAR_HOST_SYS; print "build for $BUILD\n" unless ($param::quiet);
    $INCLUDE = $BUILD  . "/include"; 

    @search_files = ();
    $DEBUG        = "-g";
    $FDEBUG       = $DEBUG;
    if ( defined( $ARG{NODEBUG} ) or $NODEBUG ) {
      $DEBUG = "-O -g";
      $FDEBUG = $DEBUG;
      print "set DEBUG = $DEBUG\n" unless ($param::quiet);
    }
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
    #$EXTRA_CPPFLAGS = "-Iinclude" if( ! -d "include");                     
          #-I-";

    $AFSFLAGS = "";
    $AFSDIR   = "/usr/afsws";
    $AFSLIBS  = "-L" . $AFSDIR . "/lib -L" . $AFSDIR . "/lib/afs";
    $AFSLIBS .= " -lkauth -lprot -lubik -lauth -lrxkad -lsys -ldes -lrx -llwp";
    $AFSLIBS .= " -lcmd -lcom_err -laudit " . $AFSDIR . "/lib/afs/util.a";
    if ( !$ROOT )       { $ROOT       = $AFS_RHIC."/star/ROOT"; }
    if ( !$ROOT_LEVEL ) { $ROOT_LEVEL = "2.25.01"; }
    if ( !$ROOTSYS )    { $ROOTSYS    = $ROOT . "/" . $ROOT_LEVEL; }
    $SRPDIR   = $ROOTSYS . "/lib";
    $SRPFLAGS = "";                  # -DR__SRP -I" . $SRPDIR . "/include";
    $SRPLIBS  = "";                  # -L" . $SRPDIR . "/lib -lsrp -lgmp";

    chomp($HOST = `hostname`);
    $CPP           = "";
    $CPPPATH       = "";
    $CPPFLAGS      = "";
    $EXTRA_CPPPATH = "";

    $G77           = "g77";
    $G77FLAGS      = "-pipe -w %DEBUG -fno-second-underscore -fno-automatic -Wall -W -Wsurprising -fPIC";
    $G77EXTEND     = "-ffixed-line-length-132"; 

    $CXX           = "g++";
    $CXXFLAGS      = "-fpic -w";
    $EXTRA_CXXFLAGS = "";
    $CXXOPT         = "";

    $CC            = "gcc";
    $CFLAGS        = "-fpic -w";
    $EXTRA_CFLAGS  = "";

    $FC            = $G77;
    $FCPATH       = "";
    $EXTRA_FCPATH = "";
    $FFLAGS        = $G77FLAGS;
    $FEXTEND       = $G77EXTEND;
    $FPPFLAGS      = "-DCERNLIB_TYPE";
    $EXTRA_FFLAGS  = "";
    $AR            = "ar";
    $ARFLAGS       = "rvu";
    $LD            = $CXX;
    $LDFLAGS       = $CXXFLAGS;
    $EXTRA_LDFLAGS = "";
    $F77LD         = $LD;
    $F77LDFLAGS    = $LDFLAGS;
    $SO            = $CXX;
    $SOFLAGS       = "";
    $STIC          = "stic";
    $STICFLAGS     = "";
    $AGETOF        = "agetof";

    $KUIP          = $CERN_ROOT . "/bin/kuipc";
    if ( !$ROOT )       { print "ROOT_LEVEL has to be defined\n"; exit 1;}
    if ( !$ROOT_LEVEL ) { print "ROOT_LEVEL has to be defined\n"; exit 1;}
    if ( !$ROOTSYS )    { print "ROOT_SYS   has to be defined\n"; exit 1;}
    $ROOTCINT      = $ROOTSYS . "/bin/rootcint";
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
    $CXXCOM =
 "%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS -I%<:d %_IFLAGS %EXTRA_CPPPATH -c %CXXinp%< %Cout%>";
    $CCCOM = "%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS -I%<:d %_IFLAGS %EXTRA_CPPPATH -c %Cinp%< %Cout%>";
    $MAKELIB .= "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
    $LINKCOM =
      "%LD %DEBUG %LDFLAGS %EXTRA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 my $FCCOM = "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND -I%<:d %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>";
 my $AGETOFCOM  = "test -f %>:b.F && rm %>:b.F;";
    $AGETOFCOM .= "%AGETOF %< -o %>:b.F &&";
    $AGETOFCOM .= "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG -I%<:d %_IFLAGS %EXTRA_FCPATH -c";
    $AGETOFCOM .= " %>:b.F %Fout%>";
    $INCLUDE_PATH = $INCLUDE;
    $Salt = undef;
    $NoKeep = undef;
    if ( !$OPTSTAR ) { $OPTSTAR = "/opt/star"; }

    if ( !$STAR_SYS ) {
        $STAR_SYS = `sys`;
        chop($STAR_SYS);
        $STAR_HOST_SYS = $STAR_SYS;
    }
    $_ = $STAR_HOST_SYS;
    print "System: ", $_, "\n" unless ($param::quiet);


 if ( $STAR_HOST_SYS !~ /^intel_wnt/ ) {
   my($packl,$cernl,$kernl);
   
   if ( -e "$CERN_ROOT/bin/cernlib_noshift"      &&
	-e "$CERN_ROOT/lib/libpacklib_noshift.a" &&
	-e "$CERN_ROOT/lib/libkernlib_noshift.a" ){
     $cernl = "$CERN_ROOT/bin/cernlib_noshift";
     $packl = "packlib_noshift";
     $kernl = "kernlib_noshift";
   } else {
     if ( ! -e "$CERN_ROOT/bin/cernlib"){
       print 
	 "\n",
	 " ** WARNING  ** Could not find $CERN_ROOT/bin/cernlib\n",
	 "                Will attempt to proceed and assume it is in\n",
	 "                your path ...\n",
	 "\n";
       $cernl = "cernlib";
     } else {
       $cernl = "$CERN_ROOT/bin/cernlib";
     }
     print "WARNING :: Using default packlib (with possibly shift)\n";
     $packl = "packlib";
     $kernl = "kernlib";
   }
   
   $CERNLIBS .= " " . `$cernl geant321 pawlib packlib graflib/X11 packlib mathlib kernlib`;

   $CERNLIBS =~ s/packlib\./$packl\./g;
   $CERNLIBS =~ s/kernlib\./$kernl\./g;
   
   chop($CERNLIBS);
 }

    #  ============================================================
    # Platform support should be concentrated here
    #  ============================================================
 if ($STAR_HOST_SYS =~ /^rh80_icc/) {
	$PLATFORM      = "linux";
	$ARCH          = "linuxicc";
	$PGI           = "";
	$CC            = "icc";
	$CXX           = "icc";
	$CXXFLAGS      = "-w -ansi -fpic";
	$CFLAGS        = "-restrict -w -fpic";# -Wall
	$ICC_MAJOR     = `$CXX -V -dryrun  >& /tmp/icc_version; awk '{ if (NR==1) print \$8 }' /tmp/icc_version| cut -d'.' -f1; rm  /tmp/icc_version;`;
        $ICC_MINOR     = `$CXX -V -dryrun  >& /tmp/icc_version; awk '{ if (NR==1) print \$8 }' /tmp/icc_version| cut -d'.' -f2; rm  /tmp/icc_version;`;
	chomp($ICC_MAJOR); chomp($ICC_MINOR);
	$LIBG2C        = `gcc -print-file-name=libg2c.a | awk '{ if (\$1 != "libg2c.a") print \$1}'`;
	chomp($LIBG2C); 
#	$LIBFRTBEGIN   = `gcc -print-file-name=libfrtbegin.a | awk '{ if (\$1 != "libfrtbegin.a") print \$1}'`;
#	chomp($LIBFRTBEGIN);
	$LIBFRTBEGIN = "";
	if ($ICC_MAJOR eq '8') {
	  $FC            = "ifort";
	  $LIBIFCPATH    = `which ifort | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $F77LIBS       = $LIBFRTBEGIN ." ". $LIBG2C ." -L". $LIBIFCPATH ." -lifcore";
	}
	else                   {
	  $FC            = "ifc";
	  $LIBIFCPATH    = `which ifc | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $F77LIBS       = $LIBFRTBEGIN ." ". $LIBG2C ." -L". $LIBIFCPATH ." -lF90 -lCEPCF90 -lintrins";
	  $F77LIBS       = $LIBG2C ." -L". $LIBIFCPATH ." -lF90 -lCEPCF90 -lintrins";
	}
	
	$FLIBS         = $F77LIBS;
	$FFLAGS        = "";
	$FEXTEND       = "-132";
	
	$XLIBS         = "-L" . $ROOTSYS . "/lib -lXpm  -lX11";
	$SYSLIBS       = "-lm -ldl -rdynamic";
	$CLIBS         = "-lm -ldl -rdynamic";
	$CRYPTLIBS     = "-lcrypt";
	$LD            = $CXX;
	$LDFLAGS       = "";
	$SO            = $CXX;
	$SOFLAGS       = "-shared -u*";

 } elsif ($STAR_HOST_SYS =~ /^i386_/ || $STAR_HOST_SYS =~ /^rh/) {
        #
        # Case linux
        #
        if ($STAR && $STAR_HOST_SYS !~ /gcc3/ ) {
            $OSFID .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES";
        }
        $CXXFLAGS     = "-pipe -fPIC";
	$CXXFLAGS    .= " -Wall";
	$CXXFLAGS    .= " -Woverloaded-virtual";
	if ($STAR_HOST_SYS =~ /^rh/){
            # ansi works only with gcc3.2 actually ...
	    # may be removed later ...
	    $CXXFLAGS    .= " -ansi -pedantic -Wno-long-long";
	}
	if ( defined( $ARG{NODEBUG} ) or $NODEBUG ) {
	    if ($STAR_HOST_SYS =~ /gcc/){
		# this naming convention starts at gcc 3.2 which happens to
		# have a change in the options
		$DEBUG = "-O -g -march=pentium -mcpu=pentium -falign-loops=2";
		$DEBUG.= " -falign-jumps=2 -falign-functions=2";
		$FDEBUG = "-O -g";
	    } else {
		$DEBUG = "-O -g -march=pentium -mcpu=pentium -malign-loops=2";
		$DEBUG.= " -malign-jumps=2 -malign-functions=2";
		$FDEBUG = "-O -g";
	    }
	    print "set DEBUG = $DEBUG\n" unless ($param::quiet);
	}
        $CFLAGS    = "-pipe -fPIC -Wall";
	$CFLAGS   .= " -Wall";
	$CFLAGS   .= " -Wshadow";
        $SOFLAGS   = "-shared -Wl,-Bdynamic";
        $XLIBS     = "-L/usr/X11R6/lib -lXpm -lX11";
        $THREAD    = "-lpthread";
        $CRYPTLIBS = "-lcrypt";
        $SYSLIBS   = "-lm -ldl -rdynamic";
	$CLIBS    .= " -L/usr/X11R6/lib -lXt -lXpm -lX11 -lm -ldl  -rdynamic ";

        if ( defined($ARG{INSURE}) or defined($ENV{INSURE}) ) {
            print "Use INSURE++\n";
            $CC  = "insure -g -Zoi \"compiler_c gcc\"";
            $CPP = "insure -g -Zoi \"compiler_c gcc\"";
            $CXX = "insure -g -Zoi \"compiler_cpp g++\"";
            $LD  = $CXX;
            $SO  = $CXX;
        }

        if ($PGI) {
	  $FC    = "pgf77";
	  $FFLAGS = "-w -DCERNLIB_TYPE";
	  $FPPFLAGS  = "";
	  $FEXTEND = "-Mextend";
	}
        $F77LIBS = " -lg2c -lnsl";
        $FLIBS .= $F77LIBS;


    } elsif (/^alpha_dux/) {
	#
	# Trying True64
	#
	$PLATFORM      = "alpha";
	$ARCH          = "alphaxxx6";
	$CC            = "cc";
	$CXX           = "cxx";
	$CXXFLAGS      = "tlocal";
	$CFLAGS        = "";
	$EXTRA_CXXFLAGS= "-Iinclude -long_double_size 64";
	$FC            = "f77";
	$F77LIBS       = "/usr/shlib/libFutil.so /usr/shlib/libUfor.so /usr/shlib/libfor.so /usr/shlib/libots.so";
	$FLIBS         = $F77LIBS;
	$FFLAGS        = "-old_f77";
	$FEXTEND       = "-extend_source -shared -warn argument_checking -warn nouninitialize";
	
	$XLIBS         = "-L" . $ROOTSYS . "/lib -lXpm  -lX11";
	$SYSLIBS       = "-lm";
	$CLIBS         = "-lm -ltermcap";
	$LD            = $CXX;
	$LDFLAGS       = "";
	$SO            = $CXX;
	$SOFLAGS       = "-shared -nocxxstd -Wl,-expect_unresolved,*,-msym,-soname,";
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
        $CXXCOM =
"%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS -ptr%ObjDir -I%<:d %_IFLAGS -c %CXXinp%< %Cout%>";
        $FC             = "$SUNOPT/$SUNWS/bin/f77";
        $CXXFLAGS       = "-KPIC";
        $EXTRA_CXXFLAGS = " -D__CC5__";
        $EXTRA_CFLAGS   = " -D__CC5__";
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
        $SO         = $CXX;
        $SOFLAGS    = "-G -ptr%ObjDir";

        if ( defined( $ARG{INSURE} ) ) {
            print "***Use INSURE++***\n";
            $CC  = "insure -g -Zoi \"compiler_c cc\"";
            $CPP = "insure -g -Zoi \"compiler_c CC\"";
            $CXX = "insure -g -Zoi \"compiler_cpp CC\"";
            $LD  = $CXX;
            $SO  = $CXX;
        }
    }

    if ( $STAR_SYS ne $STAR_HOST_SYS ) { $OSFID .= " " . $STAR_HOST_SYS; $OSFCFID .= " " . $STAR_HOST_SYS;}
    $OSFCFID  .= " CERNLIB_TYPE";
    $OSFID    .= " __ROOT__";
    $CPPFLAGS .= " -D" . join ( " -D", split ( " ", $OSFID ) );
    $CFLAGS   .= " -D" . join ( " -D", split ( " ", $OSFID ) );

    $FPPFLAGS  .= " -D" . join ( " -D", split ( " ", $OSFCFID ) );

    $ROOTSRC = $ROOTSYS . "/include";
    $CPPPATH = "#StRoot" .  $main::PATH_SEPARATOR . $INCLUDE . $main::PATH_SEPARATOR . $ROOTSRC . $main::PATH_SEPARATOR . "#";
    my $pwd = cwd(); 
    my $path2bin = $pwd . "/." . $STAR_HOST_SYS . "/bin";   
    if ($PATH !~ /$STAR_BIN/) {$PATH = $STAR_BIN . ":" . $PATH;}
    $PATH = $path2bin .":". $PATH;  #print "PATH = $PATH\n";
    $FCPATH = $INCLUDE . $main::PATH_SEPARATOR . $CERN_ROOT . "/include";
# packages
# MySQL
 my $os_name = `uname`;
 chomp($os_name);
 my ($MYSQLINCDIR,$mysqlheader) = 
 script::find_lib($MYSQL . " " .
		  "/include /usr/include ".
		  "/usr/include/mysql  ".
		  "/usr/mysql/include  ".
		  "/usr/mysql  ".
		  $OPTSTAR . "/include " .  $OPTSTAR . "/include/mysql " ,
		  "mysql.h");
 if ($MYSQLINCDIR) {
   print "Use MYSQLINCDIR = \t$MYSQLINCDIR \n" if $MYSQLINCDIR;
 }
 else {die "Can't find mysql.h in $OPTSTAR/include  $OPTSTAR/mysql/include ";}
 (my $mysqllibdir = $MYSQLINCDIR) =~ s/include$/lib/;
 my ($MYSQLLIBDIR,$MYSQLLIB) = 
 script::find_lib($mysqllibdir . " /usr/lib/mysql".
		  $OPTSTAR . "/lib " .  $OPTSTAR . "/lib/mysql " ,
		  "libmysqlclient");
 if ($STAR_HOST_SYS =~ /^rh/) { 
   $MYSQLLIB .= " -L/usr/lib";
   if (-r "/usr/lib/libmystrings.a") {$MYSQLLIB .= " -lmystrings";}
   if (-r "/usr/lib/libssl.a"      ) {$MYSQLLIB .= " -lssl";}
   if (-r "/usr/lib/libcrypto.a"   ) {$MYSQLLIB .= " -lcrypto";}
   $MYSQLLIB .= " -lz";
 }
 print "Use MYSQLLIBDIR = \t$MYSQLLIBDIR and MYSQLLIB = \t$MYSQLLIB\n" if $MYSQLLIBDIR; 
# QT
 if (defined($QTDIR) && -d $QTDIR) {
   if (-e $QTDIR . "/bin/moc") {
     $QTLIBDIR = $QTDIR . "/lib";
     $QTBINDIR = $QTDIR . "/bin";
   }
   if ($QTBINDIR) {
     $QTINCDIR = $QTDIR . "/include";
     $QTFLAGS  = "-DR__QT";#-DQT_THREAD_SUPPORT";
     $QTLIBS   = "-lqt-mt";
     if ($main::_WIN32) {
       $QTLIBS  .= " " . $QTDIR . "/lib/qt-mt*.lib " . 
	 $ROOTSYS . "/lib/libGraf.lib " . 
	   $ROOTSYS . "/lib/libGpad.lib shell32.lib Ws2_32.lib Imm32.lib Winmm.lib";}
     print "Use QTLIBDIR = \t$QTLIBDIR \tQTINCDIR = \t$QTINCDIR \tQTFLAGS = \t$QTFLAGS \tQTLIBS = \t$QTLIBS\n" if $QTLIBDIR;
   }
 }

    
    my @params = (
      'CPPPATH'       => $CPPPATH,
      'EXTRA_CPPPATH' => $EXTRA_CPPPATH,
      'CPPFLAGS'      => $CPPFLAGS,
      'EXTRA_CPPFLAGS'=> $EXTRA_CPPFLAGS,
      'DEBUG'         => $DEBUG,
      'FDEBUG'        => $FDEBUG,
      'G77'           => $G77, 
      'G77FLAGS'      => $G77FLAGS,
      'G77EXTEND'     => $G77EXTEND,
      'FC'            => $FC,
      'FPPFLAGS'      => $FPPFLAGS,
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
      'AGETOFCOM'     => $AGETOFCOM,
      'CC'            => $CC,
      'CFLAGS'        => $CFLAGS,
      'EXTRA_CFLAGS'  => $EXTRA_CFLAGS,
      'KUIP'          => $KUIP,
      'KUIPCOM'       => '%KUIP %< %<.f && %FC %FFLAGS -c %<.f -o %>',
      'CCCOM'         =>
      '%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS -c %Cinp%< %Cout%>',
      'CXX'            => $CXX,
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
      'EXTRA_LDFLAGS'  => $EXTRA_LDFLAGS,
      'F77LD'          => $F77LD,
      'F77LDFLAGS'     => $F77LDFLAGS,
      'EXEFLAGS'       => $EXEFLAGS,
      'LIBPATH'        => $LIBPATH,
      'LINKCOM'        => $LINKCOM,
      'SO'             => $SO,
      'SOFLAGS'        => $SOFLAGS,
      'SoOUT'          => $SoOUT,
      'LINKMODULECOM'  => $MAKELIB,
      'AR'             => $AR,
      'ARFLAGS'        => $ARFLAGS,
      'ARCOM'          => "%AR %ARFLAGS %> %<;%RANLIB %>",
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
          'ROOTSRC'         => $ROOTSRC,
          'ROOTSYS'         => $ROOTSYS,
          'CINTSYSDIR'      => $CINTSYSDIR,
          'LD_LIBRARY_PATH' => $LD_LIBRARY_PATH,
          'SHLIB_PATH'      => $SHLIB_PATH,
          'LIB'             => $LIB,
          'PGI'             => $PGI,
          'STAR'            => $STAR,
          'CERN_ROOT'       => $CERN_ROOT,
          'STAF'            => $STAF,
          'STAR_BIN'        => $STAR_BIN,
          'TEMP'            => $TEMP,
          'TMP'             => $TMP,
          'STAR_SYS'        => $STAR_HOST_SYS,
	  'STAR_VERSION'    => $STAR_VERSION,
          'PERL5LIB'        => $PERL5LIB,
          'OPTSTAR'         => $OPTSTAR
      },
	  'Packages' => {		  
			 'MYSQL' => {
				     'LIBDIR'=> $MYSQLLIBDIR,
				     'INCDIR'=> $MYSQLINCDIR,
				     'LIBS'  => $MYSQLLIB
				    },
			 'QT' => {
				  'DIR'   => $QTDIR,
				  'INCDIR'=> $QTINCDIR,
				  'BINDIR'=> $QTBINDIR,
				  'FLAGS' => $QTFLAGS,
				  'LIBDIR'=> $QTLIBDIR,
				  'LIBS'  => $QTLIBS 
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
