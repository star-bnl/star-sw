# $Id: ConsDefs.pm,v 1.5 2014/10/06 14:15:07 fisyak Exp $
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
    if ( !$OPTSTAR ) { $OPTSTAR = "/opt/star"; } # print "OPTSTAR = $OPTSTAR\n"; die;
    if ( !$XOPTSTAR or $XOPTSTAR eq '/dev/null') { $XOPTSTAR = $OPTSTAR;} # print "OPTSTAR = $OPTSTAR\n"; die;
    $BUILD   = "#." . $STAR_HOST_SYS; print "build for $BUILD\n" unless ($param::quiet);
    $INCLUDE = $BUILD  . "/include";

    @search_files = ();
    $CC           = `root-config  --cc`; chomp($CC);
    $CXX          = `root-config --cxx`; chomp($CXX);
    $SO           = $CXX;
    $FC           = `root-config --f77`; chomp($FC);
    $LD           = `root-config  --ld`; chomp($LD);
    $F77LD        = $LD;
    $CXX_VERSION  = `$CXX -dumpversion`;
    chomp($CXX_VERSION);
    ($CXX_MAJOR,$CXX_MINOR) = split '\.', $CXX_VERSION;
    my $cxx_version = $CXX_MAJOR . ".". $CXX_MINOR;
    my $cxxflags     = `root-config --cflags`; chomp($cxxflags); $CXXFLAGS  =~ s/-I.*//;
    my @words     = split(' ',$cxxflags);
    $CXXFLAGS     = "";
    $CFLAGS       = "";
     foreach my $w (@words) {
       if ($w =~ /^-I/) {next;}
       if ($CXXFLAGS) {$CXXFLAGS .= " ";}
       $CXXFLAGS .= $w;
       if ($w =~ /\+\+/) {next;}
       if ($CFLAGS) {$CFLAGS .= " ";}
       $CFLAGS .= $w;
     }
#     print "CXX = $CXX, CXXFLAGS = $CXXFLAGS, CC = $CC, CFLAGS = $CFLAGS\n";
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
    $LDFLAGS      = `root-config  --ldflags`; chomp($LDFLAGS); 
    $SOFLAGS      = $LDFLAGS;
# print "CC = $CC CXX = $CXX FC = $FC LD = $LD\n";
    $FFLAGS      = $LDFLAGS;
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

    # die;
    $SRPDIR   = $ROOTSYS . "/lib";
    $SRPFLAGS = "";                  # -DR__SRP -I" . $SRPDIR . "/include";
    $SRPLIBS  = "";                  # -L" . $SRPDIR . "/lib -lsrp -lgmp";

    chomp($HOST = `/bin/hostname`);
    $CPPPATH       = "";
    $CPPFLAGS      = "";
    $EXTRA_CPPPATH = "";
    # simulation
#   $CERNLIB_FPPFLAGS = "-DCERNLIB_TYPE -DCERNLIB_DOUBLE -DCERNLIB_NOQUAD";
    $CERNLIB_FPPFLAGS = "-DCERNLIB_TYPE -DCERNLIB_UNIX -DCERNLIB_BSLASH -DCERNLIB_DZDOC ".
 "-DCERNLIB_SHL -DCERNLIB_NONEWL -DCERNLIB_HIGZ -DCERNLIB_CG  -DCERNLIB_HADRON -DCERNLIB_COMIS";# -DCERNLIB_GCALOR";
    $CERNLIB_CPPFLAGS = $FPPFLAGS;


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
  
    my $LLIB = "lib";
    if (! $USE_64BITS and $STAR_HOST_SYS =~ /darwin/) { $USE_64BITS = "yes";}
    if (  $USE_64BITS and $STAR_HOST_SYS !~ /darwin/) { $LLIB = "lib64";}
    # default is gfortran
#    if ( ( -x "/usr/bin/gfortran" or -x "/sw/bin/gfortran" ) && !defined($ENV{USE_G77}) ){
    if ( !defined($ENV{USE_G77}) ) {
	# JL 200908 - give preference to gfortran for now 
	# JL 201004 - added possibility to revertto g77 by defining USE_G77 but 
	#             this is at your own risk
	$G77       = "gfortran";
#	$FC        = $G77;
	if ( defined( $ARG{NODEBUG} ) || $NODEBUG )  {
	    $G77FLAGS  = "$XMACHOPT -fd-lines-as-comments ";
	} else {
	    $G77FLAGS  = "$XMACHOPT -fd-lines-as-code ";
	}

	$G77FLAGS .= " -std=legacy -fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";
	$FFLAGS   .= $G77FLAGS;
	if ($STAR_HOST_SYS =~ m/darwin/) {	$FLIBS .= " -L/sw/lib -lf2c";}
	else {$FLIBS    .= " -lgfortran";}
#	$FLIBS      = `$FC $FFLAGS -print-file-name=libgfortran.$SOEXT`; chomp($FLIBS);
#	if ($FLIBS eq "libgfortran.$SOEXT") {
#	  $FLIBS    = `$FC $FFLAGS -print-file-name=libgfortran.a`; chomp($FLIBS);
#	}
    } else {
	$G77       = "g77";
	$G77FLAGS  = "$XMACHOPT -fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";
    }

    $G77EXTEND     = "-ffixed-line-length-132";

#    $CXX           = "g++";
    # $XMACHOPT would switch to -m32 or -m64
#    $LDFLAGS = $SOFLAGS = "$XMACHOPT";

    $CXXFLAGS      .= " -fPIC -w";
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
    $EXTRA_CXXFLAGS= "";
    $CXXOPT        = "";

#    $CC            = "gcc";

    $CFLAGS       .= " -fPIC -w";
    $EXTRA_CFLAGS  = "";

    $FCPATH        = "";
    $EXTRA_FCPATH  = "";
    $FFLAGS       .= $G77FLAGS;
    $FEXTEND       = $G77EXTEND;
#    $CPPCERN       = " -DCERNLIB_TYPE -DCERNLIB_DOUBLE -DCERNLIB_NOQUAD -DCERNLIB_LINUX ";
#    $FPPFLAGS      = $CPPCERN;
    $EXTRA_FPPFLAGS= "";
#    $CXXFLAGS     .= $CPPCERN;
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";

    $CPP           = $CC . " -E -P";
    $FPP           = $CPP;
    $AR            = "ar";
    $ARFLAGS       = "rvu";
#    $LD            = $CXX;
#    $LDFLAGS       = "$XMACHOPT ";#--no-warn-mismatch";#$CXXFLAGS;
    $LDEXPORT      = " -Wl,-export-dynamic -Wl,-noinhibit-exec,-Bdynamic";
    $LDALL         = " -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs";
    $LDNONE        = " -Wl,--no-whole-archive -Wl,-Bdynamic";
    $EXTRA_LDFLAGS = "";
    $F77LD         = $LD;
    $F77LDFLAGS    = $LDFLAGS;
#    $F77LDFLAGS    = "$XMACHOPT ";#$LDFLAGS;
#    $SO            = $CXX;
#    $SOFLAGS       = "$XMACHOPT";
    $STIC          = "stic";
    $STICFLAGS     = "";
    $AGETOF        = "agetof";
    $AGETOFLAGS    = "-V 1";
    $LIBSTDC       = `$CC $CFLAGS -print-file-name=libstdc++.a | awk '{ if (\$1 != "libstdc++.a") print \$1}'`;
    chomp($LIBSTDC);

    if ( $G77 eq "gfortran" ){
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
	$GPROF= undef;
	if ( defined( $ARG{NODEBUG} ) || $NODEBUG ) {
	    $DEBUG = "-O -g";
	    # JL patch for gcc 4.1 -> 4.3.x (report that it is broken in 4.4 as well)
	    if ( $STAR_HOST_SYS =~ m/(_gcc4)(\d+)/ ){
		print "Notice: Enabling gcc patch for V4.x series\n";
		if ( $2 <= 49 ){
		    $DEBUG .= " -fno-inline";
		}
	    }
	    $FDEBUG= $DEBUG;
	    print "set DEBUG = $DEBUG\n" unless ($param::quiet);
	}
    }




    $CINTSYSDIR    = $ROOTSYS . "/cint";
    $ARCOM  = "%AR %ARFLAGS %> %< ; %RANLIB %>"; # "%AR %ARFLAGS %> %<;%RANLIB %>",

    my $gccfilter = "";
    if ($CC eq 'clang') {
#      $CFLAGS .= " -fcolor-diagnostics"; $CXXFLAGS .= " -fcolor-diagnostics";
      #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
    } elsif ($cxx_version >= 4.9) {
#      $CFLAGS .= " -fdiagnostics-color=always"; $CXXFLAGS .= " -fdiagnostics-color=always";
    }  elsif (-e "$STAR/mgr/gccfilter") { 
      $gccfilter = "$STAR/mgr/gccfilter -c -w -a ";
      my $flag = system($gccfilter);
#      print "$gccfilter   ===========> $flag\n";
      if ($flag) { $gccfilter = "";}
#      print "gccfilter = $gccfilter ==============\n";
#      die;
    } 
#    if ($cxx_version >= 4.7) {
#      print "C++11 activated.\n";
#      $CXXFLAGS    .= " -std=c++11"; #gnu++11
#    } elsif (defined($ENV{CXX11}) or $cxx_version .= 4.3) {
#      print "C++0x activated. If you get any errors update to a compiler which fully supports C++11\n";
#      $CXXFLAGS    .= " -std=c++0x"; # gnu++0x
#    } else {
#      print "C++11 needed. Therefore a gcc compiler with a version higher than 4.3 is needed.\n";
#      $CXXFLAGS    .= " -ansi"; # == -std=c89
#    }

    $CXXCOM = $gccfilter .
 "%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %CXXinp%< %Cout%>";
# print "CXXCOM : $CXXCOM\n";
    $CCCOM =  $gccfilter .
 "%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %EXTRA_CPPFLAGS %_IFLAGS %EXTRA_CPPPATH -c %Cinp%< %Cout%>";
    $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
    $LINKCOM =
      "%LD %DEBUG %LDFLAGS %EXTRA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 my $FCCOM = 
 "%FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND %_IFLAGS %EXTRA_FCPATH -c %< %Fout%>;";
 my $F90COM = 
 "cd %<:d; %FC %FPPFLAGS %FFLAGS %EXTRA_FPPFLAGS %FDEBUG %FEXTEND %_IFLAGS %EXTRA_FCPATH -c %<:f %Fout%>:f;";
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
    }
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
#	$CC            = "icc";
#	$CXX           = "icc";
	$CPP           = $CC . " -EP";
	$CXXFLAGS      = "-w -ansi -fPIC -wd1476"; #-fpstkchk"; $XMACHOPT 
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
	$CFLAGS        = "-restrict -w -fPIC";     # -fpstkchk";# -restrict";# -Wall $XMACHOPT 
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
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
	  $CFLAGS     .= " -wd1572";
	}
	if ($ICC_MAJOR ge '8') {
#	  $FC          = "ifort";
	  $LIBIFCPATH  = `which ifort | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $FLIBS     .=  " " . $LIBFRTBEGIN ." -L". $LIBIFCPATH ." -lifcore ";# . $LIBG2C;
	}
	else                   {
#	  $FC          = "ifc";
	  $LIBIFCPATH  = `which ifc | sed -e 's|bin/ifort|lib|'`; chomp($LIBIFCPATH);
	  $FLIBS     .= " " . $LIBG2C ." -L". $LIBIFCPATH ." -lF90 -lCEPCF90 -lintrins";
	}
	$FLIBS      .= " -lg2c";
	$FFLAGS       .= "-save";
#	$F77LIBS      .= " -lg2c";
#	$FLIBS         = $F77LIBS;
#	$FFLAGS        = "$XMACHOPT -save";
	$FEXTEND       = "-132";
	$XLIBS         = "-L" . $ROOTSYS . "/lib -lXpm  -lX11";
	$SYSLIBS       = "-lm -ldl -lrt";# -rdynamic";
	$CLIBS         = "-lm -ldl -lrt";# -rdynamic";
	$CRYPTLIBS     = "-lcrypt";
	$LD            = "icpc";
#	$LDFLAGS       = "$XMACHOPT ";#--no-warn-mismatch";
	$F77LD         = $LD;
#	$SO            = $CXX;
	$SOFLAGS       = "-shared -u*";
        $CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
        $CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";

	$EXTRA_CXXFLAGS= "";
	$EXTRA_CFLAGS  = "";
	$EXTRA_CPPFLAGS= "";
	$EXTRA_LDFLAGS = "";
	$EXTRA_SOFLAGS = "";

     } elsif ($STAR_HOST_SYS =~ /^i386_/ ||
	     $STAR_HOST_SYS =~ /^rh/    ||
	     $STAR_HOST_SYS =~ /^sl/    ||
	     $STAR_HOST_SYS =~ /gcc/ && $STAR_HOST_SYS !~ /darwin/ ) {
        #
        # Case linux
        #
	$CERNLIB_FPPFLAGS .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
	$CERNLIB_CPPFLAGS .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_BLDLIB -DCERNLIB_CZ -DCERNLIB_QMGLIBC";
      
	if ( $G77 eq "gfortran" ){
	  # TODO: Possible cleanup to do between GFORTRAN and CERNLIB_LINUX
	  $CERNLIB_FPPFLAGS .= " -DCERNLIB_GFORTRAN";
	}
	if ($USE_64BITS){
	  $CERNLIB_FPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
	  $CERNLIB_CPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
	}

        #print "CERNLIB_FPPFLAGS = $CERNLIB_FPPFLAGS\n";
	$CERNLIB_FPPFLAGS .= " -DCERNLIB_GCC" . $CXX_MAJOR;
	$CERNLIB_CPPFLAGS .= " -DCERNLIB_GCC" . $CXX_MAJOR;
        #print "CXX_VERSION : $CXX_VERSION MAJOR = $CXX_MAJOR MINOR = $CXX_MINOR => $cxx_version\n";
        $CXXFLAGS    .= " -fPIC -pipe -Wall -Woverloaded-virtual";# $XMACHOPT 
 #print "CXXFLAGS = $CXXFLAGS -1-------------------------------------------------------------------------------\n";
	my $optflags = "";
 #print "CXXFLAGS = $CXXFLAGS -2-------------------------------------------------------------------------------\n";
	if ($cxx_version < 3) {
	  $OSFID .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES";
	}

        # -fpermissive ?
	if ($CXX_MAJOR == 3 and $CXX_MINOR < 4) {$CXXFLAGS    .= " -pedantic"; }
	#	  else {
	#	  print "CXXFLAGS = $CXXFLAGS\n"; die;
	#	}

	$CXXFLAGS    .= " -Wno-long-long";
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";

	# Additional optimization flags if NODEBUG
	if ( defined( $ARG{NODEBUG} ) or $NODEBUG ) {
	    if ($CXX_VERSION < 3){
		$optflags = "-malign-loops=2 -malign-jumps=2 -malign-functions=2";
	    } else {
		# this naming convention starts at gcc 3.2 which happens to
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
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
        $CFLAGS   .= " -pipe -fPIC -Wall -Wshadow";
        $SOFLAGS  .= " -shared -Wl,-Bdynamic";

	$XLIBS     = "-L/usr/X11R6/$LLIB -lXpm -lX11";

        $THREAD    = "-lpthread";
        $CRYPTLIBS = "-lcrypt";

	$SYSLIBS   = "-lm -ldl";
	$CLIBS    .= " -lm -ldl";
	$SYSLIBS   .= " -lrt -rdynamic";
	$CLIBS     .= " -lrt -rdynamic";
	# print "*** $CXX_VERSION $SYSLIBS\n";
	
#        if ($PGI) {
#	  # under SL5 where PGI is installed, this test make PGI used
#	    # but eventually fail at link-time - TBC [TODO: JL 200908]
#	  $FC    = "pgf77";
#	  $FFLAGS = "";
#	  $FEXTEND = "-Mextend";
#	} else {
##	    $FC      = $G77;
##	    $FFLAGS  = $G77FLAGS;
##	    $FEXTEND = $G77EXTEND;
#	}

	if ( $G77 eq "gfortran"){
#	  $LIBIFCPATH  = `$FC -print-file-name=libgfortranbegin.a`; chomp($LIBIFCPATH);
#	  $FLIBS     =  $LIBFRTBEGIN;
#	  $FLIBS    .= " -lgfortran";
	} else {
	    if ($CXX_VERSION >= 4 && $STAR_HOST_SYS =~ m/darwin/ ){
		# Same comment, not sure if V4 or a Mac issue
		# 2009/08 -> not an issue with gcc4
		$FLIBS = " -lg2c";
	    } else {
		$FLIBS .= " -lg2c -lnsl";
	    }
	}
	my $ROOT_VERSION = `root-config --version`;
	chomp($ROOT_VERSION);
	my ($ROOT_MAIN,$ROOT_MINOR) = split ('/',$ROOT_VERSION); #print "ROOT_VERSION = $ROOT_VERSION => $ROOT_MAIN $ROOT_MINOR\n";
	if ($ROOT_MAIN > 5.34 || $ROOT_MAIN == 5.34 && $ROOT_MINOR >= 18) {#VERSION =~ '5.34/18'
	  $CXXFLAGS .= " -msse -mfpmath=sse";
	  $CFLAGS   .= " -msse -mfpmath=sse";
	  $FFLAGS   .= " -msse -mfpmath=sse";
	 # print "CXXFLAGS = $CXXFLAGS ; CFLAGS = $CFLAGS ; FFLAGS = $FFLAGS\n";
	}
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
    } elsif ( $STAR_HOST_SYS =~ /darwin/) {
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
#      $CXX           = "g++";
#      $CC            = "gcc";
      if ($CXX eq 'g++') {
	$CXXFLAGS      .= " -pipe -Wshadow  -W -Wall -Woverloaded-virtual -fsigned-char -fno-common";
	if ($CXX_MAJOR > 4 or $CXX_MAJOR == 4 and $CXX_MINOR >= 6) {$CXXFLAGS .= " -fpermissive";}
	$CXXFLAGS      .= " -Wshadow -Wunused-parameter -Wwrite-strings";
      } else {# clang
	$CXXFLAGS    .= " -std=c++11";
      }
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
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
      $LDALL         = "-Wl,-force_load"; #"-Wl,-all_load";
      $LDNONE        = "";#"-Wl,-noall_load";
#    $LDEXPORT      = " -Wl,-export-dynamic -Wl,-noinhibit-exec,-Bdynamic";
#    $LDALL         = " -Wl,--whole-archive -Wl,-Bstatic -Wl,-z -Wl,muldefs";
#    $LDNONE        = " -Wl,--no-whole-archive -Wl,-Bdynamic";
#      $LDALL        = " -Wl,-force_load"
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
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
	$CINTCXXFLAGS .= " -fvisibility-inlines-hidden";
      }
      elsif ($MACOSX_MINOR == 3) {
	$SOFLAGS      = "-dynamiclib -single_module -undefined dynamic_lookup";# -install_name $(LIBDIR)/
	$CXXFLAGS     .= " -Wno-long-double";
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
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
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
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
#	$F77          = "gfortran";
#	$FPP          = $F77;
	$FPP          = $FC;
#	$CC           = "gcc-4";
#	$CXX          = "g++-4";
	$FFLAGS      .= " -funroll-loops -fomit-frame-pointer -ftree-vectorize";
#	$FFLAGS      .= "-std=legacy";
#	$FFLAGS      .= " -fd-lines-as-comments"; # -fd-lines-as-code
#	$FFLAGS      .= " -ff2c";
	$FEXTEND      = $G77EXTEND;
#        $CERNLIB_FPPFLAGS    .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_MACOSX -DCERNLIB_GFORTRAN";
#        $CERNLIB_CPPFLAGS    .= " -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_MACOSX -DCERNLIB_GFORTRAN";
      if ($USE_64BITS){
	$CERNLIB_FPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
	$CERNLIB_CPPFLAGS .= " -DCERNLIB_QMLXIA64 -DCERNLIB_LXIA64";
      }
#	print "============== FPPFLAGS = $FPPFLAGS; CERNLIB_FPPFLAGS = $CERNLIB_FPPFLAGS\n";
#	$FLIBS      = `$FC $XMACHOPT -print-file-name=libgfortran.$SOEXT`; chomp($FLIBS);
      $FLIBS      .= " " . `$FC $FFLAGS  -print-file-name=libgfortran.dylib`; chomp($FLIBS);
	$FLIBS      .= " " . `$FC $FFLAGS -print-file-name=libgfortranbegin.a`; chomp($FLIBS);
      # We add libg2c only in case of ppc because then we probably have cernlib
      # compiled with g77. In case of Intel Mac it should be compiled wiFLAGS      = "-fno-second-underscore -w -fno-automatic -Wall -W -Wsurprising -fPIC";th the
      # same fortran we use.
      if ($MACOSX_CPU eq "powerpc") {
	$FLIBS .= " -L$(FINK_DIR)/lib -lg2c";
      }
#      $FC = $F77;
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
    $CFLAGS     =~ s/-stdlib=libc\+\+//;
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
    if ($CPPPATH) {$CPPPATH .= $main::PATH_SEPARATOR;}
    $CPPPATH .= "#".  $main::PATH_SEPARATOR . "#StRoot" .  $main::PATH_SEPARATOR . $INCLUDE . $main::PATH_SEPARATOR . $ROOTSRC;# . $main::PATH_SEPARATOR . "#";
    $CPPPATH .= $main::PATH_SEPARATOR . $XOPTSTAR . "/include";
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
    # } else {
    #	($MYSQLCONFIG,$mysqlconf) =
    #	    script::find_lib($MYSQL . " ".
    #			     "/usr/$LLIB/mysql /usr/bin/mysql /usr/bin ".
    #			     $XOPTSTAR . "/bin " .  $XOPTSTAR . "/bin/mysql ",
    #			     "mysql_config");
    # } 


    # Associate the proper lib with where the inc was found
    my ($mysqllibdir)=$MYSQLINCDIR;
    $mysqllibdir =~ s/include/$LLIB/;# print "mysqllibdir => $mysqllibdir =========\n";

    # print "DEBUG :: $mysqllibdir\n";
    # Note - there is a trick here - the first element uses mysqllibdir
    #        which is dreived from where the INC is found hence subject to 
    #        USE_LOCAL_MYSQL switch. This may not have been obvious.
     my ($MYSQLLIBDIR,$MYSQLLIB) =
                  script::find_lib($mysqllibdir . " /usr/$LLIB/mysql ".
		  $XOPTSTAR . "/lib " .  $XOPTSTAR . "/lib/mysql ",
		  "libmysqlclient");
    #			 # "libmysqlclient_r libmysqlclient");
    # # die "*** $MYSQLLIBDIR,$MYSQLLIB\n";
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
	# if ( !defined($IVROOT)) {
	#    if ($QT_VERSION==4) {
	#	$IVROOT   = $ROOT . "/5.99.99/Coin2Qt4/$STAR_HOST_SYS/coin3d"; # the temporary place with the coin package
	#    } else {
	#	$IVROOT   = $ROOT . "/5.99.99/Coin2/.$STAR_HOST_SYS"; # the temporary place with the coin package
	#    }
	#    print "*** ATTENTION *** IVROOT $IVROOT\n";
	# }
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
#    die;

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
 my $cmd = "touch /tmp/temp_gccflags.c; $CXX -E -dM -o - /tmp/temp_gccflags.c | grep -q SSE";
 my $VcCPPFLAGS = " -DVC_IMPL=SSE";
#$VcCPPFLAGS .= " -msse -mfpmath=sse";
 if ($STAR_HOST_SYS =~ 'gcc432$' || system($cmd)) {# No SSE
   $VcCPPFLAGS = " -DVC_IMPL=Scalar";
 } else {# check Vc from ROOT
#   my $configFile = $ROOTSYS . "/config/Makefile.config";
#   if (! -r $configFile) {
#     $configFile = $ROOTSYS . "/Build/config/Makefile.config";
#   }
#   if (-r $configFile) {
#      open (In, $configFile) or die "Can't open $configFile";
#      $VcCPPFLAGS = "";
#      while (my $line = <In>) {
#	chop($line);
#	print "line: $line\n";
#	if ($line =~ /SIMDCXXFLAGS/) {
#	  $line =~ s/^.*:=//; 
#	  $VcCPPFLAGS .= " " . $line; print "VcCPPFLAGS = $VcCPPFLAGS\n";
#	} elsif ($line =~ /AVXCXXFLAG/) {
##	  $line =~ s/^.*:=//; 
##	  $VcCPPFLAGS .= " " . $line; print "VcCPPFLAGS = $VcCPPFLAGS\n";
#	} elsif ($line =~ /VCFLAGS/) {
#	  $line =~ s/^.*:=//; 
#	  $VcCPPFLAGS .= " " . $line; print "VcCPPFLAGS = $VcCPPFLAGS\n";
#	}
#      }
#      close(In);
#   } else {
#     print "configFile : $configFile is not found\n";
#   }
 }
 if (-r "/tmp/temp_gccflags.c") {`rm /tmp/temp_gccflags.c`;}
 #print "CXXFLAGS = $CXXFLAGS --------------------------------------------------------------------------------\n";
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
