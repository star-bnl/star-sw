# $Id: ConsDefs.pm,v 1.6 2000/04/03 13:38:11 fisyak Exp $
{
 if (defined($AFS)) {$File::Find::dont_use_nlink;}
 use File::Basename;
 use Sys::Hostname;
 use Cwd;
 use File::Find ();
 #________________________________________
 *name           = *File::Find::name;
 *prune          = *File::Find::prune;
 *dir            = *File::Find::dir;
 *topdir         = *File::Find::topdir;
 *topdev         = *File::Find::topdev;
 *topino         = *File::Find::topino;
 *topmode        = *File::Find::topmode;
 *topnlink       = *File::Find::topnlink;
 #use strict;
 my $pwd = cwd();
 if ($pwd =~ '^/afs/') {$File::Find::dont_use_nlink;}
 
 my $DEBUG = "-g";
 @search_files   = ();
 my $O        = "o";
 my $A        = "a";
 my $Cxx      = "cxx";
 my $SOEXT    = "so";
 my $EXESUF   = "";
 
 my $SoOUT    = "-o ";
 my $Cout     = "-o ";
 my $Fout     = "-o ";
 my $Lout     = "-o ";
 my $Cinp     = "";
 my $CXXinp   = "";
 my $CPP      = "gcc";
 my $CPPFLAGS = "";#-I-";
 my $CPPPATH  = "";
 my $EXTRA_CPPPATH = "";
 my $CXX      = "g++";
 my $CXXFLAGS = "-fpic -w";
 my $EXTRA_CXXFLAGS = "";
 my $CXXOPT   = "";
 my $CINTCXXFLAGS = "";
 
 my $CC       = "gcc";
 my $CFLAGS   = "-fpic -w";
 my $EXTRA_CFLAGS = "";
 my $CINTCFLAGS = "";
 my $FC       = "f77";
 my $FFLAGS   = "-KPIC -w -DCERNLIB_TYPE";
 my $AR       = "ar";
 my $ARFLAGS  = "rvu";
 my $LD       = $CXX;
 my $LDFLAGS  = $CXXFLAGS;
 my $EXTRA_LDFLAGS = "";
 my $SO       = $CXX;
 my $SOFLAGS  = "";
 my $STIC     = "stic";
 my $STICFLAGS= "";
 my $GEANT3   = "geant3";
 my $KUIP     = $CERN_ROOT . "/bin/kuipc";
 my $ROOTCINT = $ROOTSYS . "/bin/rootcint";
 my $LIBS     = "";
 my $Libraries= "";
 my $CLIBS    = "";
 my $FLIBS    = "";
 my $XLIBS    = "";
 my $THREAD   = "";
 my $CERNLIBS = "";
 my $OSFID    = "";
 my $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
 my $LINKCOM  = "%LD %DEBUG %LDFLAGS %EXTA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 my $FCCOM    = "%FC %FFLAGS %CPPFLAGS %DEBUG %FEXTEND %_IFLAGS  %FCPPPATH -c %< %Fout%>";
 my $GEANT3COM= "test -f %>.F && rm %>.F;";
 $GEANT3COM.="%GEANT3 %< -o %>.F && %FC %FFLAGS %CPPFLAGS %DEBUG %_IFLAGS  %FCPPPATH -c %>.F -o %>";
 my $INCLUDE_PATH  = $INCLUDE;
 my $Salt = undef;
 $_ = $STAR_HOST_SYS;
 print "System: ", $_,"\n" unless ($param::quiet);
 if (/^i386_/) {
   #    case linux
   #  ====================
   $OSFID    = "f2cFortran";
   $OSFID   .= " lnx Linux linux LINUX";
   $OSFID   .= " CERNLIB_LINUX CERNLIB_UNIX CERNLIB_LNX CERNLIB_QMLNX NEW_ARRAY_ON GNU_GCC";
   $OSFID   .= " R__GLIBC";
   $OSFID   .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES";
   my $CINTD  = "-DG__REGEXP -DG__UNIX -DG__SHAREDLIB -DG__OSFDLL -DG__ROOT -DG__REDIRECTIO";
   $CXXFLAGS = "-fPIC -Wall";
   $CINTCXXFLAGS = $CXXFLAGS . " " . $CINTD;
   #                                             -fpipe
   $CFLAGS   = "-fPIC -Wall";
   $CINTCFLAGS = $$CFLAGS . " " . $CINTD;
   $LDFLAGS  = $DEBUG . " " . $CXXFLAGS . " -Wl,-Bdynamic";
   $SOFLAGS  = "-shared -Wl,-Bdynamic";  
   $XLIBS    = "-L/usr/X11R6/lib -lX11 -lXpm";
   $THREAD   = "-lpthread";
   if (/egcs$/) {
     $CLIBS    = "-L/usr/X11R6/lib  -lXt -lXpm -lX11 -lm -ldl  -rdynamic";
     $FC       = "g77";
     $FLIBS   .= " -L/usr/local/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
     $FFLAGS   = "-w %DEBUG -fno-second-underscore -fno-automatic";
     $FCCOM    = "test -f %>.g && rm %>.g ; test -f %>.f && rm %>.f;";
     $FCCOM   .= "%FC -E -P %CPPFLAGS %DEBUG %_IFLAGS  %FCPPPATH -c %< %Fout%>.g &&"; 
     $FCCOM   .= "%GEANT3 -V 1 -V f -i %>.g %Fout%>.f;"; 
     $FCCOM   .= "if [ -f %>.f ]; then %FC %FFLAGS -c %>.f %Fout%> ;";
     $FCCOM   .= "else %FC %FFLAGS %CPPFLAGS %DEBUG %FEXTEND %_IFLAGS  %FCPPPATH -c %< %Fout%>; fi";
     my $GEANT3COM= $FCCOM;
     $FEXTEND  = "-ffixed-line-length-132"; 
   }
   elsif (/kcc$/) {
     $OSFID    = "f2cFortran";
     $OSFID   .= " lnx Linux linux LINUX CERNLIB_LINUX CERNLIB_UNIX CERNLIB_LNX CERNLIB_QMLNX NEW_ARRAY_ON GNU_GCC";
     $OSFID   .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS";
     $DEBUG    = "-g";
     $CXXOPT   = "+K0 -O0 --no_exceptions";
     $CXX      = "KCC";
     $CXXFLAGS = $CXXOPT . " --signed_chars -D_EXTERN_INLINE=inline";
     $CXXFLAGS.= " --display_error_number --diag_suppress 191 -fPIC";
     $CLIBS    = "-L/usr/pgi/linux86/lib -L/usr/X11R6/lib  -lXt -lXpm -lX11  -lpgc -lm -ldl -Bdynamic";
     $FC       = "pgf77";
     $FLIBS    = "-L/usr/pgi/linux86/lib -lpgftnrtl -lpgc";
     $FLIBS   .= " -L/opt/star/lib -lpgf77S -lpgf77A";
     $FLIBS   .= " -L/usr/local/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
     $FFLAGS   = "-DPGI";  
     $FEXTEND  = "-Mextend";
     $LD       = $CXX;
     $LDFLAGS  = $CXXOPT;
     $SO       = $CXX;
     $SOFLAGS  = $CXXOPT;
   }
   else {
     $CLIBS    = "-L/usr/pgi/linux86/lib -L/usr/X11R6/lib  -lXt -lXpm -lX11  -lpgc -lm -ldl  -rdynamic";
     $FC       = "pgf77";
     $FLIBS    = "-L/opt/star/lib -lpgf77S -lpgf77A";
     $FLIBS   .= " -L/usr/pgi/linux86/lib -lpgftnrtl -lpgc";
     $FLIBS   .= " -L/usr/local/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
     $FFLAGS   = "-DPGI";  
     $FEXTEND  = "-Mextend";
   }
   if (/^i386_linux2/) {$FLIBS   .= " -lI77 -lF77";}
   if (defined($ARG{INSURE})){
     print "Use INSURE++\n";
     $CC       = "insure -g -Zoi \"compiler_c gcc\"";
     $CPP      = "insure -g -Zoi \"compiler_c gcc\"";
     $CXX      = "insure -g -Zoi \"compiler_cpp g++\"";
     $LD       = $CXX;
     $SO       = $CXX;
   }
 }
 elsif (/^hp_ux102/) {
   #    case "hp":
   #  ====================
   $OSFID = "HPUX CERNLIB_HPUX CERNLIB_UNIX ST_NO_NAMESPACES ST_NO_EXCEPTIONS";
   $OSFID   .= " NEW_ARRAY_ON";
   $CXX      = "aCC";
   $CC       = "cc";
   $LD       = $CXX;
   $SO       = $CXX;
   $CXXFLAGS = "+W70,495,740,749,823,829 -DR__ACC -z +Z -Dextname";
   $CINTCXXFLAGS = "-z +Z -DG__REGEXP -DG__UNIX -DG__HPUXCPPDLL -DG__SHAREDLIB -D_POSIX2_SOURCE -DG__ROOT -DG__REDIRECTIO";
   $CFLAGS   = " -Ae -z +Z -Dextname";  
   $CINTCFLAGS = $CFLAGS . " -DG__REGEXP -DG__UNIX -DG__HPUXCPPDLL -DG__SHAREDLIB";
   $CINTCFLAGS.= " -D_POSIX2_SOURCE -DG__ROOT -DG__REDIRECTIO -D__STDCPP__";
   $SOEXT    = "sl";
   $XLIBS    = $ROOTSYS . "../lib/libXpm.a -lX11";
   if (defined($SCL_OPTIMISE)){ # from Brian
     $CXXFLAGS .= " +Olibcalls +Onolimit";
   }
   else {$CXXFLAGS .= " +d";}
   $LDFLAGS   = "-z -Wl,+s -Wl,-E,+vnocompatwarnings";
   $SOFLAGS   = "-b -Wl,+vnocompatwarnings";#-b -z";  
   $CLIBS     = "/usr/lib/libM.sl";
   $FC        = "fort77";
   $FFLAGS    = "-WF,-P +ppu +Z +B -K";
   $FEXTEND   = "+es";
   print "SHLIB_PATH = $SHLIB_PATH\n";
 }
 elsif (/^sun4x_56_CC5$/) {
   $OSFID     = "__CC5__ __SunOS_5_6";
   $OSFID    .= " CERNLIB_SOLARIS CERNLIB_SUN CERNLIB_UNIX DS_ADVANCED QUIET_ASP SOLARIS";
   $OSFID    .= " ST_NO_MEMBER_TEMPLATES";
   $OSFID    .= " SUN Solaris sun sun4os5 sun4x_56";
   $EXTRA_CPPPATH = ":/usr/openwin/include";
   $CC        = "/opt/WS5.0/bin/cc";
   $CXX       = "/opt/WS5.0/bin/CC";
   $FC        = "/opt/WS5.0/bin/f77";
   $CXXFLAGS  = "-KPIC -D__SunOS_5_6 -library=iostream";
   my $CINTD  = "-DG__REGEXP1 -DG__UNIX -DG__OSFDLL -DG__SHAREDLIB -DG__ROOT -DG__REDIRECTIO";
   $CINTCXXFLAGS = $CXXFLAGS . " " . $CINTD;
   $CLIBS     = "-lm -ltermcap -ldl -lnsl -lsocket -lgen -L/opt/WS5.0/SC5.0/lib -lCstd -liostream -lCrun";
   $FLIBS     = "-L/opt/WS5.0/lib -lM77 -lF77 -lsunmath";
   $XLIBS     = $ROOTSYS . "/lib/libXpm.a -L/usr/openwin/lib -lX11";
   $FFLAGS    = "-KPIC -w";
   $FEXTEND   = "-e";
   $CFLAGS    = "-KPIC";
   $CINTCFLAGS= $CFLAGS . " " . $CINTD;
   $LD        = $CXX;
#   $EXEFLAGS  = "-library=iostream";
   $LDFLAGS   = "-library=iostream";
   $SO        = $CXX;
   $SOFLAGS   = "-G";#
   if (defined($ARG{INSURE})){
     print "***Use INSURE++***\n";
     $CC       = "insure -g -Zoi \"compiler_c cc\"";
     $CPP      = "insure -g -Zoi \"compiler_c CC\"";
     $CXX      = "insure -g -Zoi \"compiler_cpp CC\"";
     $LD       = $CXX;
     $SO       = $CXX;
   }
 }
 elsif (/^sun4x_5.$/) {
   $OSFID     = "__SunOS_5_6";
   $OSFID    .= "CERNLIB_SOLARIS CERNLIB_SUN CERNLIB_UNIX DS_ADVANCED QUIET_ASP SOLARIS ";
   $OSFID    .= "ST_NO_EXCEPTIONS ST_NO_MEMBER_TEMPLATES ST_NO_NAMESPACES ";
   $OSFID    .= "ST_NO_NUMERIC_LIMITS ST_NO_TEMPLATE_DEF_ARGS SUN Solaris sun sun4os5 sun4x_56";
   $EXTRA_CPPPATH = ":/usr/openwin/include";
   $CC        = "cc";
   $CXX       = "CC";
   $FC        = "f77";
   $CXXFLAGS  = "-KPIC +w -features=no_anachronisms -features=rtti";
   my $CINTD  = "-DG__REGEXP1 -DG__UNIX -DG__OSFDLL -DG__SHAREDLIB -DG__ROOT -DG__REDIRECTIO";
   $CINTCXXFLAGS = $CXXFLAGS . " " . $CINTD;
   $CLIBS     = "-lw -lgen -lsocket -lnsl -lintl";
   $FLIBS     = "-lM77 -lF77 -lsunmath";
   $XLIBS     = $ROOTSYS . "/lib/libXpm.a -L/usr/openwin/lib -lX11";
   $FFLAGS    = "-KPIC -w";
   $FEXTEND   = "-e";
   $CFLAGS    = "-KPIC";
   $CINTCFLAGS= $CFLAGS . " " . $CINTD;
   $LD        = $CXX;
   $LDFLAGS  = "-z muldefs -Bdynamic -t";
   $SO        = $CXX;
   $SOFLAGS   = "-G";
   my $STDHOME= "/afs/rhic/star/packages/ObjectSpace/2.0m";
   $EXTRA_CPPPATH.= " -I" . $STDHOME . " -I" . $STDHOME . "/ospace/std"; 
   $EXTRA_CPPPATH.= " -I" . $STDHOME . "/ospace/stl  -I" . $STDHOME . "/ospace"; 
   $EXTRA_CPPPATH.= " -I/usr/openwin/include -I/usr/local/include";
   $MAKELIB = "test -d %>:d/Templates.DB &&";
   $MAKELIB.= "%SO %DEBUG %SOFLAGS %SoOUT%> %< %>:d/Templates.DB/*.o %_LDIRS %LIBS ||";
   $MAKELIB.= "%SO %DEBUG %SOFLAGS %SoOUT%> %< %_LDIRS %LIBS ";
   $LINKCOM       = "test -d %>:d/Templates.DB &&";
   $LINKCOM      .= "%LD %DEBUG %LDFLAGS %EXTAR_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%> ";
   $LINKCOM      .= "%>:d/Templates.DB/*.o $ROOTSYS/obj/Templates.DB/*.o ||";  
   $LINKCOM      .= "%LD %DEBUG %EXEFLAGS %EXTAR_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
   if (defined($ARG{INSURE})){
     print "***Use INSURE++***\n";
     $CC       = "insure -g -Zoi \"compiler_c cc\"";
     $CPP      = "insure -g -Zoi \"compiler_c CC\"";
     $CXX      = "insure -g -Zoi \"compiler_cpp CC\"";
     $LD       = $CXX;
     $SO       = $CXX;
   }
 }
 elsif (/^intel_wnt/) {
   #  $DEBUG    = "-Zi";
   $DEBUG    = "";
   $EXESUF   = ".exe";
   $O        = "obj";
   $A        = "lib";
   $Cxx      = "cxx";
   $SOEXT    = "DLL";
   $AR       = "set LIB && lib"; print "NT Lib: lib = $lib / LIB = $LIB\n";
   $ARFLAGS  = "-nologo -MACHINE:IX86";
   $Cout     = "-Fo";
   $Fout     = "-Fo";
   $Lout     = "-out:";
   $SoOUT    = "-out:";
   $Cinp     = "-Tc";
   $CXXinp   = "-Tp";
   $Copt     = "-O2";
   $CXXOpt   = "-O2";
   
   $GEANT3   += $EXESUF;
   $ROOTCINT += $EXESUF;
   
   $OSFID     = "VISUAL_CPLUSPLUS CERNLIB_WINNT CERNLIB_MSSTDCALL WIN32 ";
   $OSFID    .= "ST_NO_NAMESPACES ";
   $CPP       = "cl";
   $CC        = "cl";
   #  $CXX       = "set inc && path && cl";
   $CXX       = "cl";
   $FC        = "fl32";
   $CLIBS     = "ws2_32.lib mswsock.lib user32.lib kernel32.lib msvcrt.lib oldnames.lib MSVCIRT.LIB";
   $FLIBS     = "DFORDLL.LIB";
   $FFLAGS    = " -MD -G5 -fpp -Oxp -nokeep -nologo";
   $FEXTEND   = "-extend_source";
   $CFLAGS    = " -MD -G5 -nologo -DASU_MALLOC_OFF";
   $CXXFLAGS  = $CFLAGS;
   $LD        = $CXX;
   #  $LDFLAGS   =  $DEBUG . $(conlflags); 
   $SO        =  "link";
   $SOFLAGS   = " -DEBUG -NODEFAULTLIB -INCREMENTAL:NO -NOLOGO -DLL ";
   $MAKELIB = 
     "BINDEXPLIB.exe %> %< > %>.def && %AR %ARFLAGS %Lout%>.lib  %< -def:%>.def && %SO %SOFLAGS %SoOUT%> %< %_LDIRS %LIBS %Libraries";
   $LIB .= ";" . $ROOTSYS . "\\lib";
   $Libraries = $ROOTSYS . "/lib/*.lib" . " ws2_32.lib mswsock.lib user32.lib kernel32.lib msvcrt.lib oldnames.lib MSVCIRT.LIB";
 }
 
 $MAIN    = cwd();
 $include = "include";
 $INCLUDE = "#" . $include;
 $EXPORT = "#.share";
 $FFLAGS .= " -DCERNLIB_TYPE";
 $OSFID .= " " . $STAR_SYS;
 if ($STAR_SYS ne $STAR_HOST_SYS) {$OSFID .= " " . $STAR_HOST_SYS;}
 my $FLAGS = $OSFID . " CERNLIB_TYPE" . " __ROOT__";
 $CPPFLAGS .= " -D" . join (" -D", split (" ",$FLAGS));
 if (defined($ARG{NODEBUG}))  {$DEBUG = "-O2"              ; print "set DEBUG = $DEBUG\n" unless ($param::quiet);}
 if (defined($ARG{DEBUG}))    {$DEBUG = $ARG{DEBUG}        ; print "set DEBUG = $DEBUG\n" unless ($param::quiet);}
 if (defined($ARG{CPPFLAGS})) {$CPPFLAGS = $ARG{CPPFLAGS}  ; print "set CPPFLAGS = $CPPFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{EXTRA_CPPFLAGS})) {$EXTRA_CPPFLAGS = $ARG{EXTRA_CPPFLAGS}  ; print "set EXTRA_CPPFLAGS = $EXTRA_CPPFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{CPPPATH})) {$CPPPATH = $ARG{CPPPATH}  ; print "set CPPPATH = $CPPPATH\n" unless ($param::quiet);}
 if (defined($ARG{EXTRA_CPPPATH})) {$EXTRA_CPPPATH = $ARG{EXTRA_CPPPATH}  ; print "set EXTRA_CPPPATH = $EXTRA_CPPPATH\n" unless ($param::quiet);}
 if (defined($ARG{CPP}))      {$CPP = $ARG{CPP}            ; print "set CPP = $CPP\n" unless ($param::quiet);}
 if (defined($ARG{CC}))       {$CC = $ARG{CC}              ; print "set CC = $CC\n" unless ($param::quiet);}
 if (defined($ARG{CFLAGS}))   {$CFLAGS = $ARG{CFLAGS}      ; print "set CFLAGS = $CFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{EXTRA_CFLAGS}))   {$EXTRA_CFLAGS = $ARG{EXTRA_CFLAGS}      ; print "set EXTRA_CFLAGS = $EXTRA_CFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{CLIBS}))    {$CLIBS = $ARG{CLIBS}        ; print "set CLIBS = $CLIBS\n" unless ($param::quiet);}
 if (defined($ARG{CXX}))      {$CXX = $ARG{CXX}            ; print "set CXX = $CXX\n" unless ($param::quiet);}
 if (defined($ARG{CXXFLAGS})) {$CXXFLAGS = $ARG{CXXFLAGS}  ; print "set CXXFLAGS = $CXXFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{FC}))       {$FC = $ARG{FC}              ; print "set FC = $FC\n" unless ($param::quiet);}
 if (defined($ARG{FFLAGS}))   {$FFLAGS = $ARG{FFLAGS}      ; print "set FFLAGS = $FFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{FLIBS}))    {$FLIBS = $ARG{FLIBS}        ; print "set FLIBS = $FLIBS\n" unless ($param::quiet);}
 if (defined($ARG{LD}))       {$LD = $ARG{LD}              ; print "set LD = $LD\n" unless ($param::quiet);}
 if (defined($ARG{LDFLAGS}))  {$LDFLAGS = $ARG{LDFLAGS}    ; print "set LDFLAGS = $LDFLAGS\n" unless ($param::quiet);}
# if (defined($ARG{EXEFLAGS})) {$EXEFLAGS = $ARG{EXEFLAGS}  ; print "set EXEFLAGS = $EXEFLAGS\n" unless ($param::quiet);}
 if (defined($ARG{Libraries})){$Libraries = $ARG{Libraries}; print "set Libraries = $Libraries\n" unless ($param::quiet);}
 if (defined($ARG{LIBS}))     {$LIBS = $ARG{LIBS}          ; print "set LIBS = $LIBS\n" unless ($param::quiet);}
 if (defined($ARG{LIBPATH}))  {$LIBPATH = $ARG{LIBPATH}    ; print "set LIBPATH = $LIBPATH\n" unless ($param::quiet);}
 if (defined($ARG{SO}))       {$SO = $ARG{SO}              ; print "set SO = $SO\n" unless ($param::quiet);}
 if (defined($ARG{SOFLAGS}))  {$SOFLAGS = $ARG{SOFLAGS}    ; print "set SOFLAGS = $SOFLAGS\n" unless ($param::quiet);}
 
 my $ROOTLIBS;
 my $RINTLIBS;
 my $ROOTGLIBS;
 $ROOTLIBS    = " -L$ROOTSYS/lib -lNew -lCore -lCint -lPhysics -lMatrix -lHist -lGpad -lGraf -lGraf3d -lTree -lGui";
 $RINTLIBS    = " -lRint ";
 my $ROOTSRC = "/afs/rhic/star/ROOT/" . $ROOT_LEVEL . "/ROOT/src";
 $CPPPATH  = $INCLUDE . ":#StRoot:" .  $ROOTSRC;
 $CPPPATH .= ":" . $CERN_ROOT . "/include:/opt/star/include" . $EXTRA_CPPPATH;
 $FCPPPATH ="";
 if (! $param::quiet) {
   print "STAR     = \t$STAR\n";
   print "DEBUG    = \t$DEBUG\n";
   print "CPP      = \t$CPP    \tCPPFLAGS = \t$CPPFLAGS \nCPPPATH  = \t$CPPPATH\n";
   print "CC       = \t$CC     \tCFLAGS   = \t$CFLAGS \tEXTRA_CFLAGS  = \t$EXTRA_CFLAGS     \tCLIBS    =\t$CLIBS\n"; 
   print "CXX      = \t$CXX    \tCXXFLAGS = \t$CXXFLAGS \tEXTRA_CXXFLAGS = \t$EXTRA_CXXFLAGS\n";
   print "FC       = \t$FC     \tFFLAGS   = \t$FFLAGS     \tFLIBS    =\t$FLIBS\n"; 
   print "LD       = \t$LD     \tLDFLAGS  = \t$LDFLAGS \tEXTRA_LDFLAGS  = \t$EXTRA_LDFLAGS\n";
                            #    \nEXEFLAGS =\t$EXEFLAGS\n";
   print "LIBPATH  = \t$LIBPATH\tLIBS     = \t$LIBS       \tLibraries=\t$Libraries\n";
   print "SO       = \t$SO     \tSOFLAGS  = \t$SOFLAGS\n";
 }
 (my $DirPath  = cwd()) =~ s/\/$//g;
 chdir($STAR); my $repository = cwd(); chdir($DirPath); print "repository $repository\n";
 Repository ($repository);# unless $param::noRepository;
 my @Repo = Repository_List; 
 print "Build directory: $DirPath;" unless ($param::quiet);
 if (defined($ARG{Salt})) {$Salt = $ARG{Salt} ; print "set Salt = $Salt\n" unless ($param::quiet);}
 if ($#Repo > -1) {
   print "\tRepositories:  \"@Repo\";" unless ($param::quiet);
   if (!$Salt) {$Salt = "user";}
 }
 if ($Salt) {Salt($Salt); print "\tSet Salt to \"$param::salt\"" unless ($param::quiet);}
 my $build  = "." . $STAR_HOST_SYS; print "\nbuild for $build\n" unless ($param::quiet);
 $BUILD  = "#" . $build;
 if ($DEBUG eq "-g" || $#Repo > -1) {
   $LIB    = $BUILD . "/lib";
   $BIN    = $BUILD . "/bin"; 
   $OBJ    = $BUILD . "/obj";
 }
 else {
   $LIB    = $BUILD . "/LIB";
   $BIN    = $BUILD . "/BIN"; 
   $OBJ    = $BUILD . "/OBJ";
 }
 $LIBPATH = $LIB;
 Link $OBJ => $DirPath; print "Link $BUILD => $DirPath\n" unless ($param::quiet);
 if ($STAR_HOST_SYS !~ /^intel_wnt/) {
   $CERNLIBS .= " " . `cernlib geant321 pawlib graflib/X11 packlib mathlib kernlib `;
   chop ($CERNLIBS);
   $Libraries = $ROOTLIBS . $ROOTGLIBS . $RINTLIBS . $CERNLIBS ." ";
   $Libraries .=  " ". $FLIBS . " " . $CLIBS . " ";
   chop ($Libraries);
   print "Libraries     = \t$Libraries\n" unless ($param::quiet);
 }
 else {
   $CPPPATH  = "";
   $FCPPPATH = "";
   foreach my $dir ($DirPath, @Repo) {
     $INCLUDE_PATH .= ";" . $dir . "/include";
     $INCLUDE_PATH .= ";" . $dir . "/StRoot";
     $INCLUDE_PATH .= ";" . $dir . "/pams";
     $INCLUDE_PATH .= ";" . $dir . "/include/tables";
   }
   my $ROOTSRC = $AFS_RHIC . "\\star\\ROOT\\" . $ROOT_LEVEL . "\\ROOT\\src";
   $INCLUDE_PATH .= ";" . $ROOTSRC;
   $INCLUDE_PATH .= ";" . $CERN_ROOT . "/include";
 }
 $env = new cons(
		 'CPPPATH'      => $CPPPATH,
		 'CPPFLAGS'     => $CPPFLAGS,
		 'DEBUG'        => $DEBUG,
		 'FC'	        => $FC,
		 'FFLAGS'       => $FFLAGS,
		 'FEXTEND'      => $FEXTEND,
		 'FCPPPATH'     => $FCPPPATH,
		 'Fout'         => $Fout,
		 'CXXinp'       => $CXXinp,
		 'Cinp'         => $Cinp,
		 'Cout'         => $Cout,
		 'Lout'         => $Lout,
		 'SoOUT'        => $SoOUT,
		 'FCCOM'        => $FCCOM,
		 'GEANT3'       => $GEANT3,
		 'GEANT3COM'    => $GEANT3COM,
		 'CC'	        => $CC,
		 'CFLAGS'       => $CFLAGS,
		 'EXTRA_CFLAGS' => $EXTRA_CFLAGS,
		 'KUIP'         => $KUIP,
		 'KUIPCOM'      => '%KUIP %< %<.f && %FC %FFLAGS -c %<.f -o %>',
		 'CCCOM'        => '%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %_IFLAGS  -c %Cinp%< %Cout%>',
		 'CXX'          => $CXX,
		 'CXXFLAGS'     => $CXXFLAGS,
		 'EXTRA_CXXFLAGS'     => $EXTRA_CXXFLAGS,
		 'CXXCOM'       => '%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %_IFLAGS -c %CXXinp%< %Cout%>',
		 'CLIBS'        => $CLIBS,
		 'FLIBS'        => $FLIBS,
		 'XLIBS'        => $XLIBS,
		 'THREAD'       => $THREAD,
		 'CERNLIBS'     => $CERNLIBS,
		 'ROOTLIBS'     => $ROOTLIBS,
		 'ROOTGLIBS'    => $ROOTGLIBS,
		 'Libraries'    => $Libraries,
		 'LIBS'         => $LIBS,
		 'LD' 	        => $LD,
		 'LDFLAGS'      => $LDFLAGS,
		 'EXEFLAGS'     => $EXEFLAGS,
		 'LIBPATH'      => $LIBPATH,
		 'LINKCOM'      => $LINKCOM,
		 'SO'           => $SO,
		 'SOFLAGS'      => $SOFLAGS, 
		 'SoOUT'        => $SoOUT,
		 'LINKMODULECOM'=> $MAKELIB,
		 'AR'	        => $AR,
		 'ARFLAGS'      => $ARFLAGS,
		 'ARCOM'        => "%AR %ARFLAGS %> %<;%RANLIB %>",
		 'RANLIB'       => 'ranlib',
		 'AS'	        => 'as',
		 'ASFLAGS'      => '',
		 'ASCOM'        => '%AS %%DEBUG ASFLAGS %< -o %>',
		 'LD'	        => $LD,
		 'LDFLAGS'      => $LDFLAGS,
		 'PREFLIB'      => 'lib',
		 'SUFLIB'       => $A,
		 'SUFLIBS'      => "." . $SOEXT . ":." . $A,
		 'SUFSOLIB'     => $SOEXT,
		 'SUFEXE'       => $EXESUF,
		 'SUFMAP'       => {
				    '.g'  => 'build::command::geant3',
				    '.f'  => 'build::command::fc',
				    '.F'  => 'build::command::fc',
				    '.C'  => 'build::command::cxx',
				    '.s'  => 'build::command::cc',
				    '.S'  => 'build::command::cc',
				    '.c'  => 'build::command::cc',
				    '.cc' => 'build::command::cxx',
				    '.cxx'=> 'build::command::cxx',
				    '.cpp'=> 'build::command::cxx',
				    '.cdf'=> 'build::command::kuip',
				    '.o'  => 'build::command::user'
				   },
		 'SUFOBJ'	=> "." . $O,
		 'ENV'	        => { 'PATH' => $PATH,
				     'LM_LICENSE_FILE' => $LM_LICENSE_FILE,
				     'INCLUDE' => $INCLUDE_PATH,
				     'ROOTSYS' => $ROOTSYS,
				     'ROOTSRC' => $ROOTSRC,
				     'LD_LIBRARY_PATH' => $LD_LIBRARY_PATH,
				     'SHLIB_PATH' => $SHLIB_PATH,
				     'LIB'   => $LIB,
				     'PGI' => $PGI,
				     'STAR' => $STAR,
				     'CERN_ROOT' => $CERN_ROOT,
				     'STAF' => $STAF,
				     'STAR_BIN' => $STAR_BIN,
				     'TEMP' => $TEMP,
				     'TMP'  => $TMP,
				     'STAR_SYS' => $STAR_HOST_SYS
				   },
		);
 if (defined($ARG{LinksCheck})) {
   print "Clean up links. It takes a time.\n";
   &File::Find::find (\&wanted_dead,$include) if -d $include;
   &File::Find::find (\&wanted_dead,$build)   if -d $build;
 }
#________________________________________________________________________________
sub script::wanted_dead {
  -l and not -e and print "bigus link: $File::Find::name \t--- remove it\n" and `rm $_`;
}
}
1;
