# $Id: ConsDefs.pm,v 1.18 2000/06/27 20:38:45 fisyak Exp $
{
 use File::Basename;
 use Sys::Hostname;
 use Cwd;
 use File::Find ();
 if (defined($AFS)) {$File::Find::dont_use_nlink;}
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
 
 @search_files   = ();
 $DEBUG = "-g";
 $O        = "o";
 $A        = "a";
 $Cxx      = "cxx";
 $SOEXT    = "so";
 $EXESUF   = "";
 
 $SoOUT    = "-o ";
 $Cout     = "-o ";
 $Fout     = "-o ";
 $Lout     = "-o ";
 $Cinp     = "";
 $CXXinp   = "";
 $CPP      = "gcc";
 $CPPFLAGS = "";#-I-";
 $AFSDIR   = "/usr/afsws";
 $AFSLIBS  = "-L" . $AFSDIR . "/lib -L" . $AFSDIR . "/lib/afs";
 $AFSLIBS .= " -lkauth -lprot -lubik -lauth -lrxkad -lsys -ldes -lrx -llwp";
 $AFSLIBS .= " -lcmd -lcom_err -laudit ". $AFSDIR . "/lib/afs/util.a";
 $SRPDIR   = $ROOTSYS . "/lib";
 $SRPFLAGS = "";# -DR__SRP -I" . $SRPDIR . "/include";
 $SRPLIBS  = "";# -L" . $SRPDIR . "/lib -lsrp -lgmp";
##### use shadow passwords for authentication #####
 $SHADOWFLAGS = ""; #-DR__SHADOWPW
 $SHADOWLIBS  = "";
 $AUTHFLAGS   = $SHADOWFLAGS . " " . $AFSFLAGS . " " . $SRPFLAGS;
 $AUTHLIBS    = $SHADOWLIBS . " " .  $AFSLIBS . " " . $SRPLIBS;
 $R_CPPFLAGS = " -DR__AFS -DHAVE_CONFIG ";
 $CPPPATH  = "";
 $EXTRA_CPPPATH = "";
 $CXX      = "g++";
 $CXXFLAGS = "-fpic -w";
 $EXTRA_CXXFLAGS = "";
 $CXXOPT   = "";
 $CINTCXXFLAGS = "";
 $PLATFORM = "linux"; 
 $ARCH     = "linuxegcs";
 $CC       = "gcc";
 $CFLAGS   = "-fpic -w";
 $EXTRA_CFLAGS = "";
 $CINTCFLAGS = "";
 $FC       = "f77";
 $FFLAGS   = "-KPIC -w -DCERNLIB_TYPE";
 $AR       = "ar";
 $ARFLAGS  = "rvu";
 $LD       = $CXX;
 $LDFLAGS  = $CXXFLAGS;
 $F77LD    = $LD;
 $F77LDFLAGS = $LDFLAGS;
 $EXTRA_LDFLAGS = "";
 $SO       = $CXX;
 $SOFLAGS  = "";
 $STIC     = "stic";
 $STICFLAGS= "";
 $GEANT3   = "geant3";
 $KUIP     = $CERN_ROOT . "/bin/kuipc";
 $ROOTCINT = $ROOTSYS . "/bin/rootcint";
 $CINTSYSDIR = $ROOTSYS . "/cint";
 $LIBS     = "";
 $Libraries= "";
 $CLIBS    = "";
 $FLIBS    = "";
 $XLIBS    = "";
 $THREAD   = "";
 $CERNLIBS = "";
 $CRYPTLIBS= "";
 $SYSLIBS  = "";
 $OSFID    = "";
# $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
 $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< %_LDIRS %LIBS";
# $MAKELIB = "%SO %DEBUG %SOFLAGS %EXTRA_SOFLAGS %SoOUT%> %< ";
 $LINKCOM  = "%LD %DEBUG %LDFLAGS %EXTA_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%>";
 $FCCOM    = "%FC %FFLAGS %CPPFLAGS %DEBUG %FEXTEND %_IFLAGS  %FCPPPATH -c %< %Fout%>";
 $GEANT3COM= "test -f %>.F && rm %>.F;";
 $GEANT3COM.="%GEANT3 %< -o %>.F && %FC %FFLAGS %CPPFLAGS %DEBUG %_IFLAGS  %FCPPPATH -c %>.F -o %>";
 $INCLUDE_PATH  = $INCLUDE;
 $Salt = undef;
 $_ = $STAR_HOST_SYS;
 print "System: ", $_,"\n" unless ($param::quiet);
 if (/^i386_/) {
   #    case linux
   #  ====================
   $PLATFORM = "linux"; 
   $ARCH     = "linuxegcs";
   $OSFID    = "f2cFortran";
   $OSFID   .= " lnx Linux linux LINUX";
   $OSFID   .= " CERNLIB_LINUX CERNLIB_UNIX CERNLIB_LNX NEW_ARRAY_ON GNU_GCC";
   $OSFID   .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES";
   $R_CPPFLAGS  .= " -DGNU_CC -DR__GLIBC -DG__REGEXP -DG__UNIX -DG__SHAREDLIB -DG__OSFDLL -DG__ROOT -DG__REDIRECTIO";
   $CXXFLAGS = "-fPIC -Wall";# -march=pentiumpro";
   $CINTCXXFLAGS = $CXXFLAGS . " " . $R_CPPFLAGS;
   #                                             -fpipe
   $CFLAGS   = "-fPIC -Wall";# -march=pentiumpro";
   $CINTCFLAGS = $CFLAGS . " " . $R_CPPFLAGS;
   $LDFLAGS  = "";#$DEBUG . " " . $CXXFLAGS . " -Wl,-Bdynamic";
   $F77FLAGS      = "";
   $SOFLAGS  = "-shared -Wl,-Bdynamic";  
   $XLIBS    = "-L/usr/X11R6/lib -lXpm -lX11";
   $THREAD   = "-lpthread";
   $CRYPTLIBS= "-lcrypt";
   $SYSLIBS  = "-lm -ldl -rdynamic";
   if (/egcs$/) {
     $CLIBS    = "-L/usr/X11R6/lib  -lXt -lXpm -lX11 -lm -ldl  -rdynamic";
     $FC       = "g77";
#    $FLIBS   .= " -L/usr/local/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
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
     $OSFID   .= " lnx Linux linux LINUX CERNLIB_LINUX CERNLIB_UNIX CERNLIB_LNX NEW_ARRAY_ON GNU_GCC";
     $OSFID   .= " ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS";
     $DEBUG    = "-g";
     $CXXOPT   = "+K0 -O0 --no_exceptions";
     $CXX      = "KCC";
     $CXXFLAGS = $CXXOPT . " --signed_chars -D_EXTERN_INLINE=inline";
     $CXXFLAGS.= " --display_error_number --diag_suppress 191 -fPIC";
     $CLIBS    = "-L/usr/X11R6/lib  -lXt -lXpm -lX11 -lm -ldl -Bdynamic";
     $SYSLIBS  = "-lm -ldl";
     $FC       = "pgf77";
     $FLIBS    = "-L/usr/pgi/linux86/lib -lpgftnrtl -lpgc";
     $FLIBS   .= " -L/opt/star/lib -lpgf77S -lpgf77A";
#     $FLIBS   .= " -L/usr/local/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
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
#     $FLIBS   .= " -L/usr/local/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
     $FFLAGS   = "-DPGI";  
     $FEXTEND  = "-Mextend";
     if ($HOST =~ /pcstar/) {# From Janet for MPI /usr/pgi -> /usr/local/pgi
       $CLIBS    = "-L/usr/local/pgi/linux86/lib -L/usr/X11R6/lib  -lXt -lXpm -lX11 -lpgc -lm -ldl  -rdynamic";
       $FLIBS    = "-L/usr/local/pgi/linux86/lib -lpgftnrtl -lpgc";
       $FLIBS   .= " -L/opt/star/lib -lpgf77S -lpgf77A";
#       $FLIBS   .= " -L/usr/local/egcs/lib/gcc-lib/i686-pc-linux-gnu/egcs-2.91.66 -lg2c";
     }
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
   $F77LIBS = " -lg2c -lnsl";
   $FLIBS .= $F77LIBS;
 }
 elsif (/^hp_ux102/) {
   #    case "hp":
   #  ====================
   $PLATFORM = "hpux"; 
   $ARCH     = "hpuxacc";
   $OSFID = "HPUX CERNLIB_HPUX CERNLIB_UNIX ST_NO_NAMESPACES ST_NO_EXCEPTIONS";
   $OSFID   .= " NEW_ARRAY_ON";
   $CXX      = "aCC";
   $CC       = "cc";
   $LD       = $CXX;
   $SO       = $CXX;
   $CXXFLAGS = "+W70,495,740,749,823,829 -z +Z -Dextname";
   $R_CPPFLAGS .= " -DR__ACC -DG__REGEXP -DG__UNIX -DG__HPUXCPPDLL -DG__SHAREDLIB  -DG__ROOT -DG__REDIRECTIO -D__STDCPP__";
   $CINTCXXFLAGS = "-z +Z " . $R_CPPFLAGS . " -D_POSIX2_SOURCE"; 
   $CFLAGS   = " -Ae -z +Z -Dextname";  
   $CINTCFLAGS = $CFLAGS .  $R_CPPFLAGS . " -D_POSIX2_SOURCE";
   $SOEXT    = "sl";
#   $XLIBS    = $ROOTSYS . "../lib/libXpm.a -lX11";
   $XLIBS    = $ROOTSYS . "/lib/libXpm.a -L/usr/lib/X11R5 -lX11";
   if (defined($SCL_OPTIMISE)){ # from Brian
     $CXXFLAGS .= " +Olibcalls +Onolimit";
   }
   else {$CXXFLAGS .= " +d";}
   $LDFLAGS   = "-z -Wl,+s -Wl,-E,+vnocompatwarnings";
   $SOFLAGS   = "-b -Wl,+vnocompatwarnings";#-b -z";  
   $CLIBS     = "/usr/lib/libM.sl";
   $SYSLIBS  = "-lm -ldld";
   $FC        = "fort77";
   $FFLAGS    = "-WF,-P +ppu +Z +B -K";
   $FEXTEND   = "+es";
   $F77LD     = $FC;
   $F77LDFLAGS= "-K +ppu";
   print "SHLIB_PATH = $SHLIB_PATH\n";
 }
 elsif (/^sun4x_56_CC5$/) {
   $PLATFORM = "solaris"; 
   $ARCH     = "solarisCC5";
   $OSFID     = "__CC5__ __SunOS_5_6";
   $OSFID    .= " CERNLIB_SOLARIS CERNLIB_SUN CERNLIB_UNIX DS_ADVANCED QUIET_ASP SOLARIS";
   $OSFID    .= " ST_NO_MEMBER_TEMPLATES";
   $OSFID    .= " SUN Solaris sun sun4os5 sun4x_56";
   $EXTRA_CPPPATH = ":/usr/openwin/include";
   $CC        = "/opt/WS5.0/bin/cc";
   $CXX       = "/opt/WS5.0/bin/CC";
   $FC        = "/opt/WS5.0/bin/f77";
   $CXXFLAGS  = "-KPIC -D__SunOS_5_6 -library=iostream,no_Cstd";
   $R_CPPFLAGS  = "-DG__REGEXP1 -DG__UNIX -DG__OSFDLL -DG__SHAREDLIB -DG__ROOT -DG__REDIRECTIO";
   $CINTCXXFLAGS = $CXXFLAGS . " " . $R_CPPFLAGS;
#   $CLIBS     = "-lm -ltermcap -ldl -lnsl -lsocket -lgen -L/opt/WS5.0/SC5.0/lib -lCstd -liostream -lCrun";
   $CLIBS     = "-lm -ltermcap -ldl -lnsl -lsocket -lgen -L/opt/star/lib -lCstd -liostream -lCrun";
#   $CLIBS     = "-lm -ltermcap -ldl -lnsl -lsocket -lgen -L/opt/star/lib -lCstd -liostream -lCrun";
   $FLIBS     = "-L/opt/WS5.0/lib -lM77 -lF77 -lsunmath";
   $XLIBS     = $ROOTSYS . "/lib/libXpm.a -L/usr/openwin/lib -lX11";
   $SYSLIBS   = "-lm -ldl -lnsl -lsocket -L/opt/star/lib -lCstd -liostream -lCrun";
   $FFLAGS    = "-KPIC -w";
   $FEXTEND   = "-e";
   $CFLAGS    = "-KPIC";
   $CINTCFLAGS= $CFLAGS . " " . $R_CPPFLAGS;
   $LD        = $CXX;
#   $EXEFLAGS  = "-library=iostream";
   $LDFLAGS   = "-library=iostream -Bdynamic";
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
#   else {
#     $MAKELIB = "%SO %DEBUG %SOFLAGS %SoOUT%> %< %_LDIRS %LIBS ;"; 
#     $MAKELIB.= "test -d %>:d/SunWS_cache && rm -rf %>:d/SunWS_cache ; echo $status";
#     $LINKCOM = "%LD %DEBUG %LDFLAGS %EXTAR_LDFLAGS %< %_LDIRS %LIBS %Libraries %Lout%> ;";
#     $LINKCOM.= "test -d %>:d/SunWS_cache && rm -rf %>:d/SunWS_cache; echo $status";
#   }
 }
 elsif (/^sun4x_5.$/) {
   $PLATFORM = "solaris"; 
   $ARCH     = "solaris";
   $OSFID     = "__SunOS_5_6";
   $OSFID    .= "CERNLIB_SOLARIS CERNLIB_SUN CERNLIB_UNIX DS_ADVANCED QUIET_ASP SOLARIS ";
   $OSFID    .= "ST_NO_EXCEPTIONS ST_NO_MEMBER_TEMPLATES ST_NO_NAMESPACES ";
   $OSFID    .= "ST_NO_NUMERIC_LIMITS ST_NO_TEMPLATE_DEF_ARGS SUN Solaris sun sun4os5 sun4x_56";
   $EXTRA_CPPPATH = ":/usr/openwin/include";
   $CC        = "cc";
   $CXX       = "CC";
   $FC        = "f77";
   $CXXFLAGS  = "-KPIC +w -features=no_anachronisms -features=rtti";
   $R_CPPFLAGS  = "-DG__REGEXP1 -DG__UNIX -DG__OSFDLL -DG__SHAREDLIB -DG__ROOT -DG__REDIRECTIO";
   $CINTCXXFLAGS = $CXXFLAGS . " " . $R_CPPFLAGS;
   $CLIBS     = "-lw -lgen -lsocket -lnsl -lintl";
   $SYSLIBS   = "-lm -ldl -L/usr/include/sys -lsocket -lnsl";
   $FLIBS     = "-lM77 -lF77 -lsunmath";
   $XLIBS     = $ROOTSYS . "/lib/libXpm.a -L/usr/openwin/lib -lX11";
   $FFLAGS    = "-KPIC -w";
   $FEXTEND   = "-e";
   $CFLAGS    = "-KPIC";
   $CINTCFLAGS= $CFLAGS . " " . $R_CPPFLAGS;
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
   $PLATFORM = "win32"; 
   $ARCH     = "win32";
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
 
 if ($STAR_SYS ne $STAR_HOST_SYS) {$OSFID .= " " . $STAR_HOST_SYS;}
 my $FLAGS = $OSFID . " CERNLIB_TYPE" . " __ROOT__";
 $CPPFLAGS .= " -D" . join (" -D", split (" ",$FLAGS));
 if (defined($ARG{NODEBUG}) or $NODEBUG)  {$DEBUG = "-O1 -g"              ; print "set DEBUG = $DEBUG\n" unless ($param::quiet);}
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
 $ROOTSRC = "/afs/rhic/star/ROOT/" . $ROOT_LEVEL . "/include";
 my @params = (	'PLATFORM'     => $PLATFORM,
		'ARCH'         => $ARCH,
		'AFSDIR'       => $AFSDIR,
		'AFSLIB'       => $AFSLIB,
		'AFSFLAGS'     => $AFSFLAGS,
		'SRPDIR'       => $SRPDIR,
		'SRPFLAGS'     => $SRPFLAGS,
		'SRPLIBS'      => $SRPLIBS,
		'SHADOWFLAGS'  => $SHADOWFLAGS,
		'SHADOWLIBS'   => $SHADOWLIBS,
		'AUTHFLAGS'    => $AUTHFLAGS,
		'AUTHLIBS'     => $AUTHLIBS,
	        'CPPPATH'      => $CPPPATH,
		'EXTRA_CPPPATH'=> $EXTRA_CPPPATH,
		'CPPFLAGS'     => $CPPFLAGS,
		'R_CPPFLAGS'   => $R_CPPFLAGS,
		'DEBUG'        => $DEBUG,
		'FC'	       => $FC,
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
		'CC'	       => $CC,
		'CFLAGS'       => $CFLAGS,
		'CINTCFLAGS'   => $CINTCFLAGS,
		'EXTRA_CFLAGS' => $EXTRA_CFLAGS,
		'KUIP'         => $KUIP,
		'KUIPCOM'      => '%KUIP %< %<.f && %FC %FFLAGS -c %<.f -o %>',
		'CCCOM'        => '%CC %CFLAGS %EXTRA_CFLAGS %DEBUG %CPPFLAGS %_IFLAGS  -c %Cinp%< %Cout%>',
		'CXX'          => $CXX,
		'CXXFLAGS'     => $CXXFLAGS,
		'CINTCXXFLAGS' => $CINTCXXFLAGS,
		'EXTRA_CXXFLAGS'     => $EXTRA_CXXFLAGS,
		'CXXCOM'       => '%CXX %CXXFLAGS %EXTRA_CXXFLAGS %DEBUG %CPPFLAGS %_IFLAGS -c %CXXinp%< %Cout%>',
		'CLIBS'        => $CLIBS,
		'FLIBS'        => $FLIBS,
		'XLIBS'        => $XLIBS,
		'THREAD'       => $THREAD,
		'CRYPTLIBS'    => $CRYPTLIBS,
		'SYSLIBS'      => $SYSLIBS,
		'CERNLIBS'     => $CERNLIBS,
		'Libraries'    => $Libraries,
		'LIBS'         => $LIBS,
		'LD' 	       => $LD,
		'LDFLAGS'      => $LDFLAGS,
		'F77LD'        => $F77LD,
		'F77LDFLAGS'   => $F77LDFLAGS,
		'EXEFLAGS'     => $EXEFLAGS,
		'LIBPATH'      => $LIBPATH,
		'LINKCOM'      => $LINKCOM,
		'SO'           => $SO,
		'SOFLAGS'      => $SOFLAGS, 
		'SoOUT'        => $SoOUT,
		'LINKMODULECOM'=> $MAKELIB,
		'AR'	       => $AR,
		'ARFLAGS'      => $ARFLAGS,
		'ARCOM'        => "%AR %ARFLAGS %> %<;%RANLIB %>",
		'RANLIB'       => 'ranlib',
		'AS'	       => 'as',
		'ASFLAGS'      => '',
		'ASCOM'        => '%AS %%DEBUG ASFLAGS %< -o %>',
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
				     'ROOTSRC' => $ROOTSRC,
				     'ROOTSYS' => $ROOTSYS,
				     'CINTSYSDIR' => $CINTSYSDIR,
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
				   }
);
 push(@param::defaults, @params);

}
1;
