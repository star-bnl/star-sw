#!/opt/star/bin/perl

use lib "/star/u2d/wenaus/datadb";
use File::Basename;
require "dbheader.pl";
require "dbsetup.pl";

$curTime = time();

&cgiSetup();
$q->param('ver','dev') if ( $q->param('ver') eq '');
$q->param('detail','1') if ( $q->param('detail') eq '');

$dynamic = $q->param('dynamic');
if ( $dynamic ne "yes" && $q->param('pkg') eq '' && $q->param('find') eq '') {
    # just display the pre-prepared page
    $fpath = "/usr/local/apache/htdocs/code";
    $fname = $fpath."/swguide-".$q->param('ver')."-".$q->param('detail').".html";
    if ( -e $fname ) {
        open(FILE,"< $fname");
        while (<FILE>) { print; }
        exit;
    } else {
        print "$fname not found<br>\n";
    }
}

%okExtensions = (
                 ".hh" => "C++",
                 ".hpp" => "C++", # yes, someone's actually using this
                 ".h" => "C++",
                 ".cc" => "C++",
                 ".cxx" => "C++",
                 ".C" => "C++",
                 ".c" => "C",
                 ".inc" => "FORTRAN",
                 ".F" => "FORTRAN",
                 ".f" => "FORTRAN",
                 ".idl" => "IDL",
                 ".ddl" => "DDL",
                 ".sh" => "script",
                 ".pl" => "script",
                 ".csh" => "script",
                 ".batch" => "script",
                 ".bat" => "script",
                 ".scr" => "script",
                 ".kumac" => "KUMAC",
                 ".g" => "MORTRAN",
                 ".mk" => "Makefile"
                 );

foreach $typ (sort keys %okExtensions) {
    $typeCounts{$okExtensions{$typ}} = 0;
}

&printMainHeader("STAR Offline Software Guide",1);

print <<END;
The purpose of this page is to gather together information and
documentation on all offline software components: source code,
macros, and scripts.
<p>
Pointers and comments...
<ul>
    <li> The basic package list provides links to the more detailed
    package listing, to README file and documentation area (if
    existing, and they are supposed to exist), and to CVS and
    cross-referenced source code browsers for the package.
    <li> The package list with details adds summary info about
    the package: currently owner (from the karma file),
                 file count, line count, date of most recent mod,
    days since most recent mod, associated PAM.
    <li> The full listing adds all files (the same level of detail
    as the individual package listings)
    <li> The file listings report the username of the most recent
    modifier, if it can be determined from the file, ie. if a 
    comment line containing \$Id\$ has been included in the file (such
    a line should be in every file).
    <li> Ball color indicates time since most recent mod:
    <img src="/images/redball.gif">=2days, <img src="/images/greenball.gif">=2weeks, <img src="/images/blueball.gif">=2months, <img src="/images/whiteball.gif">=older
</ul>
END

$debugOn = 0;

%ignoreStuff = (
                "." => 1,
                ".." => 1,
                "CVS" => 1,
                ".cvsignore" => 1,
                "D" => 1,
                "README" => 1,
                ".rootrc" => 1,
                "html" => 1
                );

if ( $debugOn ) {
    $getString = $ENV{'QUERY_STRING'};
    $getString =~ s/%(..)/sprintf("%c", hex($1))/ge;	# unquote %-quoted
    print "\nGET: \"".$getString."\"<br>\n" if $debugOn;
}
if ($qstring = $ENV{'QUERY_STRING'}) {
    foreach (split(/&/, $qstring)) {
        s/%(..)/sprintf("%c", hex($1))/ge;	# unquote %-quoted
        if (/([^=><!]+)=(.*)/) {
            if ($2 ne "") {
                $kyv = $2;
                if (exists($input{$1})) {
                    $input{$1} .= ",$kyv";
                } else {
                    $input{$1} = $kyv;
                }
                print "'$1' = '$input{$1}'<br>\n" if $debugOn;
            }
        } else {
            $input{$_}++;
        }
    }
}

$ver = $q->param('ver');
$ver = '.dev' if ( $ver eq ".dev" );

$showFlag = $q->param('detail');

$find = $q->param('find');

$pkg = $q->param('pkg');

#read in avail file of package owners
open(AVAIL,"< /afs/rhic/star/packages/repository/CVSROOT/avail")
    or die "Can't open avail file: $!";
@availFile=<AVAIL>;
close AVAIL;

#read in loginfo file of package mod notification email
open(LOGINFO,"< /afs/rhic/star/packages/repository/CVSROOT/loginfo")
    or die "Can't open loginfo file: $!";
@loginfoFile=<LOGINFO>;
close LOGINFO;

$STAR = "/afs/rhic/star/packages/$ver";
$root = $STAR;
$rel = readlink("/afs/rhic/star/packages/$ver");
($f, $d, $e) = fileparse($rel);
$rel = $f;

$ddevVer = (fileparse(readlink("/afs/rhic/star/packages/.dev")))[0];
$devVer = (fileparse(readlink("/afs/rhic/star/packages/dev")))[0];
$newVer = (fileparse(readlink("/afs/rhic/star/packages/new")))[0];
$proVer = (fileparse(readlink("/afs/rhic/star/packages/pro")))[0];
$oldVer = (fileparse(readlink("/afs/rhic/star/packages/old")))[0];

$verChecked{$ver} = "checked";
$detailChecked{$showFlag} = "checked";
print <<END;
<form method="GET" action="http://duvall.star.bnl.gov/cgi-bin/prod/swguide.pl">
<b>Version:</b>
    <input type="radio" $verChecked{".dev"} name="ver" value=".dev"> .dev
    ($ddevVer)
    <input type="radio" $verChecked{"dev"} name="ver" value="dev"> dev
    ($devVer)
    <input type="radio" $verChecked{"new"} name="ver" value="new"> new
    ($newVer)
    <input type="radio" $verChecked{"pro"} name="ver" value="pro"> pro
    ($proVer)
    <input type="radio" $verChecked{"old"} name="ver" value="old"> old
    ($oldVer)
<br>
<b>Detail:</b>
    <input type="radio" $detailChecked{"0"} name="detail" value="0"> Package list
    <input type="radio" $detailChecked{"1"} name="detail" value="1"> Detailed package list
    <input type="radio" $detailChecked{"2"} name="detail" value="2"> Full listing <br>
<b>Find package:</b> <input type="text" name="pkg" value="$pkg">
<b>or Find file:</b> <input type="text" name="find" value="$find"><br>
<input type="checkbox" name="dynamic" value="yes">
    Force regeneration of page. Slow; only for debugging or if displayed 
page is too old.
<br>
<input type="submit"> &nbsp; <input type="reset"><br>
</form>

<hr>
<p>
<h3>Version $ver = $rel</h3>
</h3>
<pre>
END

# Build list of pams
@pamList = `cd $root; find pams -maxdepth 2 -mindepth 2 -type d`;
foreach $pm (@pamList) {
    $pm =~ m/pams\/([a-zA-Z0-9]+)\/([a-zA-Z0-9]+)/;
    if ( $1 ne "CVS" && $1 ne "idl" && $1 ne "inc" && $1 ne "kumac" ) {
        $pams{$2} = "$1/$2";
        $pams{$1} = "$1";
    }
}
if ( $debugOn ) {
    foreach $p ( keys %pams ) {
        print $p." ".$pams{$p}."\n";
    }
}

$totlines = 0;
$totfiles = 0;
#### Loop through packages

@allDirs = (
            "StRoot",
            "StDb",
            "mgr",
            "pams/ctf",
            "pams/db",
            "pams/ebye",
            "pams/emc",
            "pams/ftpc",
            "pams/gen",
            "pams/geometry",
            "pams/global",
            "pams/l3",
            "pams/magnet",
            "pams/mwc",
            "pams/sim",
            "pams/strange",
            "pams/svt",
            "pams/tpc",
            "pams/trg",
            "pams/vpd"
            );

%oneLevel = (
             "mgr" => 1
             );

for ($idr=0; $idr<@allDirs; $idr++) {
    $dir = $allDirs[$idr];
    if (exists($oneLevel{$dir})) {
        if ( $pkg ne "" ) {
            if ( $dir eq $pkg ) {
                &showPackage($root,".",$dir);
                last;
            } else {
                next;
            }
        } else {
            &showPackage($root,".",$dir);
        }
        $totlines += $linecount;
        $totfiles += $filecount;
        if ( $find ne "" && $showFlag > 0 ) {
            # We're done
            last;
        }
        next;
    }
    print "Open $root/$dir<br>\n" if $debugOn;
    opendir(DIR, "$root/$dir");
    @files = 0;
    $if=0;
    while (defined ($file = readdir DIR)) {
        $files[$if] = $file;
        $if++;
    }
    @files = sort @files;
    foreach $file ( @files ) {
        next if (exists($ignoreStuff{$file}));
        if ( $pkg ne "" ) {
            if ( $file eq $pkg ||
                 $dir."/".$file eq $pkg ||
                 $dir."/".$file eq "pams/".$pkg ) {
                &showPackage($root,$dir,$file);
                last;
            } else {
                if ( $dir eq "pams/".$pkg ) { &showPackage($root,$dir,$file); }
                next;
            }
        } else {
            &showPackage($root,$dir,$file);
        }
        $totlines += $linecount;
        $totfiles += $filecount;
        if ( $find ne "" && $showFlag > 0 ) {
            # We're done
            last;
        }
    }
}

if ( $find eq "" && $pkg eq "" && $showFlag > 0 ) {
    print "\n<b>Total files $totfiles</b>";
    print "\n<b>Total lines $totlines</b>";
    print "\n  By type:\n";
    foreach $typ (sort keys %typeCounts) {
        if ( $typeCounts{$typ} > 0 ) {
            printf("    %-10s   %-d\n",$typ,$typeCounts{$typ});
        }
    }
}

print <<END;
</pre>
</body>
</html>
END

    exit;

#########################

sub showPackage {
    my ( $theRoot, $theDir, $thePkg ) = @_;
    $lastMod = 0;
    $linecount = 0;
    $filecount = 0;
    # if the package searched for is this one, display it
    if ( $find ne "" ) {
        if ( $thePkg eq $find ) {
            $showFlag = 2;
        } else {
            $showFlag = -1;
        }
    }
    print "flag $showFlag $theRoot $theDir $thePkg<br>\n" if $debugOn;
    $pkgLine = "";
    $nsl = 0;
    ### README file
    if ( -e "$theRoot/$theDir/$thePkg/README" ) {
        $readme = "<a href=\"http://duvall.star.bnl.gov/STARAFS/comp/pkg/$ver/$theDir/$thePkg/README\">README</a>";
    } else {
        $readme = "<font color=\"gray\">README</font>";
    }
    ### doc directory
    $docDir = "$theRoot/$theDir/$thePkg/doc";
    $doc = "<font color=\"gray\">doc</font>";
    if ( -d $docDir ) {
        opendir(DOC, $docDir);
        while (defined ($docf = readdir DOC)) {
            if ( $docf ne "." && $docf ne ".." && $docf ne "CVS" ) {
                # something seems to be there
                $doc = "<a href=\"http://duvall.star.bnl.gov/STARAFS/comp/pkg/$ver/$theDir/$thePkg/doc\">doc</a>";
                last;
            }
        }
        close DOC;
    }
    ### CVS link
    $cvs = "<a href=\"http://www.star.bnl.gov/cgi-bin/cvsweb.cgi/$theDir/$thePkg\">CVS</a>";
    ### Source browser
    $src = "<a href=\"http://duvall.star.bnl.gov/lxr/source/$theDir/$thePkg?v=$rel\">src</a>";
    ### Try to find the owner
    $pkgOwner = "";
    for ($ia=0; $ia<@availFile; $ia++) {
        if ( $theDir =~ m/(\S+)\/(\S+)/ ) {
            $dr = "$1\\\/$2";
        } else {
            $dr = $theDir;
        }
        if ( $availFile[$ia] =~ m/$dr\/$thePkg\/*\s*$/ ) {
            $own = $availFile[$ia];
            $own =~ m/(avail\s+\|)([a-z]+)/;
            $pkgOwner = $2;
        } elsif ( $availFile[$ia] =~ m/$dr\/*\s*$/ ) {
            if ( $pkgOwner eq "" ) {
                $own = $availFile[$ia];
                $own =~ m/^\s*([a-z]+\s*\|)([a-z]+)/;
                $pkgOwner = $2;
            }
        }
    }
    ### Find the people who get CVS commit email
    $eList = "";
    @eListH = 0;
    for ($ia=0; $ia<@loginfoFile; $ia++) {
        if ( $theDir =~ m/(\S+)\/(\S+)/ ) {
            $dr = "$1\\\/$2";
        } else {
            $dr = $theDir;
        }
        if ( $loginfoFile[$ia] =~ m/^ALL.*-m/ ) {
            print $loginfoFile[$ia]."\n" if $debugOn;
            $eList = &getList($loginfoFile[$ia]);
        } else {
            if ( $loginfoFile[$ia] =~ m/$dr\/$thePkg\/*\s+/ ) {
                print $loginfoFile[$ia]."\n" if $debugOn;
                $eList .= &getList($loginfoFile[$ia]);
            } elsif ( $loginfoFile[$ia] =~ m/$dr\/*\s+/ ) {
                print $loginfoFile[$ia]."\n" if $debugOn;
                $eList .= &getList($loginfoFile[$ia]);
            }
        }
    }

    ### subdirectories
#    if ( -d "$theRoot/$theDir/$thePkg/doc" ) {

    $nsub = 0;
    opendir(SUB, "$theRoot/$theDir/$thePkg");
    while (defined ($sub = readdir SUB)) {
        if (-d "$theRoot/$theDir/$thePkg/$sub") {
            if ( $sub ne "." && $sub ne ".." && $sub ne "CVS" 
                 && $sub ne "doc" ) {
                $subDirs[$nsub] = $sub;
                $subdetails[$nsub] = &showFiles ($theRoot, $theDir, $thePkg, $sub, 1);
                $nsub++;
            }
        }
    }
    close SUB;
    $level = 0;
    $theSubDir = "";
    $details = &showFiles ($theRoot, $theDir, $thePkg, $theSubDir, $level);

    ### Print results
    if ( $theDir =~ m/pams\/([a-z0-9A-Z]+)/ ) {
        # for pams, include domain in package name
        $pkgName = $1."/".$thePkg;
    } else {
        $pkgName = $thePkg;
    }
    $pkgUrl = "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/swguide.pl?ver=$ver&pkg=$pkgName&detail=2\">";
    if ( $showFlag > 0 ) { # print all pkg info

        ## associated pam?
        $thePamUrl = "";
        if ( $thePkg =~ m/St_([a-z0-9]+)_/ ) {
            if ( exists($pams{$1}) ) {
                $thePam = $pams{$1};
                $thePamUrl = "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/swguide.pl?ver=$ver&pkg=$thePam&detail=2\">$thePam</a>";
            }
        }
        ## time since last mod
        $sinceMod = $curTime - $lastMod;
        $sinceMod = $sinceMod/3600/24; # days
        if ( $sinceMod < 3 ) {
            $ball="red";
        } elsif ( $sinceMod < 14 ) {
            $ball="green";
        } elsif ( $sinceMod < 60 ) {
            $ball="blue";
        } else {
            $ball="white";
        }
        $ballUrl="<img src=\"/images/".$ball."ball.gif\">";
        ($dy, $mo, $yr) = (localtime($lastMod))[3,4,5];
        if ($yr == 69 ) {
            $dy = 0;
            $mo = -1;
            $yr = 0;
            $sinceMod = 999;
        }
        if ($linecount == 0) {
            $disp1="<font color=\"gray\">$ballUrl";
            $disp2="</font>";
        } else {
            $disp1="<b>$ballUrl";
            $disp2="</b>";
        }
        $pkgLine = sprintf("$disp1%s%-27s%s %s %s %s %s%9s%4d Files%5d Lines %02d/%02d/%02d %3d Days %s$disp2\n",
                           $pkgUrl,$theDir."/".$thePkg,"</a>",$readme,$doc,$cvs,$src,$pkgOwner,$filecount,$linecount,$mo+1,$dy,$yr,$sinceMod,$thePamUrl);
    } else {
        $pkgLine = sprintf("%s%-27s%s %s %s %s %s\n",
                           $pkgUrl,$theDir."/".$thePkg,"</a>",$readme,$doc,$cvs,$src);
    }
    if ( $showFlag >= 0 ) { print $pkgLine; }
    if ( $showFlag > 1 ) {
        print "<blockquote>\n";
        print $details;
        if ($nsub>0) {
            for ($ns=0; $ns<$nsub; $ns++) {
                print "<br><b>$subDirs[$ns]/</b>\n";
                print $subdetails[$ns];
            }
        }
        print "\n<b>CVS email recipients:</b>";
        print "    <a href=\"mailto:";
        foreach $e ( sort keys %eListH ) {
            print $e." ";
        }
        print "\">[Click to send email to recipients]</a>\n";
        $i=0;
        foreach $e ( sort keys %eListH ) {
            $i++;
            printf("%-25s",$e);
            if ($i%3 == 0) { print "\n"; }
        }
        print "</blockquote>\n";
    }
}

sub showFiles {
    my ($theRoot, $theDir, $thePkg, $theSubDir, $level) = @_;
    my $output;
    if ( $theSubDir ne "" ) { $thePkg .= "/".$theSubDir; }
    ### Files
    $lines = 0;
    $subdirs = "";
    open(ENTRIES, "<$theRoot/$theDir/$thePkg/CVS/Entries");
    while (<ENTRIES>) {
        $line = $_;
        chomp $line;
        print "Line $line\n" if $debugOn;
        @tokens = split(/\//,$line);
        if ( @tokens>2 ) {
            $fname = $tokens[1];
            # if the file searched for is in this package, display it
            if ( $find eq $fname ) { $showFlag = 2; }
            next if ( exists($ignoreStuff{$fname}) );
            next if ( $fname =~ m/^(\.)/ );
            next if ( $fname =~ m/\~$/ );
            $filecount++;
            $cver = $tokens[2];
            $cdate = $tokens[3];

            # stat
            $fullname = "$theRoot/$theDir/$thePkg/$fname";
            ($fmode, $uid, $gid, $filesize, 
             $readTime, $writeTime, $cTime) =
                 (stat($fullname))[2,4,5,7,8,9,10];
            $grnam = (getgrgid($gid))[0];
            $owner = (getpwuid($uid))[0];
            if ( $writeTime > $lastMod ) { $lastMod = $writeTime; }

            $count = 0;
            $showLines = 0;
            if ($showFlag > 0) {
                $ee = "";
                ($ff, $dd, $ee) = fileparse("$fullname",'\.[a-zA-z]*');
                print "\"$ff\" \"$dd\" \"$ee\"\n" if $debugOn;
                $isScript = 0;
                if ( $ee eq "" ) {
                    open(FL,"< $fullname") or next;
                    $line1=<FL>;
                    close FL;
                    if ( $line1 =~ m/^#!/ ) {
                         $isScript = 1;
                     }
                }
                $owner = "";
                if ( exists($okExtensions{$ee}) || ($ff =~ m/akefile/)
                     || $isScript ) {
                    $showLines = 1;
                    if ( $isScript ) {
                        $ftype = "script";
                    } elsif ( $ff =~ m/akefile/ ) {
                        $ftype = "Makefile";
                    } else {
                        $ftype = $okExtensions{$ee};
                    }
                    open(FILE,"< $theRoot/$theDir/$thePkg/$fname") or next;
                    @lines=<FILE>;
                    close FILE;
                    $inComment=0;
                    for ($ii=0; $ii<@lines; $ii++) {
                        ## Don't count comments. Imperfect but should get
                        ## most.
                        if ($ii<10) {
                            # Give a try at finding the owner
                            if ( $lines[$ii] =~ m/(Id:).*(,v).*(\d\d:\d\d:\d\d)\s([a-z]+)/ ) {
                                $owner = $4;
                            }
                        }
                        next if ( $lines[$ii] =~ m/^\s*$/ );
                        if ( $ftype eq "C++" || $ftype eq "C"
                             || $ftype eq "IDL" || $ftype eq "DDL" ) {
                            next if ( $lines[$ii] =~ m/^\s*\/\// );
                            if ( $lines[$ii] =~ m/^\s*\/\*/ ) { $cstart=1; }
                            if ( $lines[$ii] =~ m/\*\// ) { $cend=1; }
                            if ( $cstart==1 ) { $inComment=1; $cstart=0; }
                            # Bug: it includes the last line of a multi-line comment block
                            if ( $cend==1 ) { $inComment=0; $cend=0; }
                            next if ( $inComment );
                        } elsif ($ftype eq "FORTRAN" || $ftype eq "MORTRAN") {
                            next if ( $lines[$ii] =~ m/^[\*CDcd]{1}/ );
                        } elsif ($ftype eq "script"
                                  || $ftype eq "Makefile") {
                            next if ( $lines[$ii] =~ m/^\s*\#/ );
                        }
                        $count++;
                    }
                    if ( $ftype eq "" ) {
                        print "ERROR: no ftype for $theDir/$thePkg/$fname\n";
                    }
                    $typeCounts{$ftype} = $typeCounts{$ftype} +$count;
                }
                if ($showLines) {
                    $theLines = sprintf("%5dLines",$count);
                } else {
                    $theLines = "";
                }
                $date = substr($cdate,4,12)." ".substr($cdate,22,2);
                ## Get most recent modifier from ownership of 
                ## repository file. (Doesn't work; almost always Lidia)
                if ( 0 ) {
                    $repfile = "/afs/rhic/star/packages/repository/$theDir/$thePkg/$fname,v";
                    ($fmode, $uid, $gid, $filesize, 
                     $readTime, $writeTime, $cTime) =
                         (stat($repfile))[2,4,5,7,8,9,10];
                    $grnam = (getgrgid($gid))[0];
                    $owner = (getpwuid($uid))[0];
                }

                $sinceMod = $curTime - $writeTime;
                $sinceMod = $sinceMod/3600/24; # days
                if ( $sinceMod < 3 ) {
                    $ball="red";
                } elsif ( $sinceMod < 14 ) {
                    $ball="green";
                } elsif ( $sinceMod < 60 ) {
                    $ball="blue";
                } else {
                    $ball="white";
                }
                $ballUrl="<img src=\"/images/".$ball."ball.gif\">";

                $output .= sprintf("%s%-35s %s%-7s%s %s %s %s%9s %s\n",
                                   $ballUrl,$fname,
                                   "<a href=\"http://www.star.bnl.gov/cgi-bin/cvsweb.cgi/$theDir/$thePkg/$fname?rev=$cver&content-type=text/x-cvsweb-markup\">",$cver,"</a>",
                                   $date,
                                   "<a href=\"http://www.star.bnl.gov/cgi-bin/cvsweb.cgi/$theDir/$thePkg/$fname\">CVS</a>",
                                   "<a href=\"http://duvall.star.bnl.gov/lxr/source/$theDir/$thePkg/$fname?v=$rel\">src</a>",
                                   $owner,$theLines);
                print "$output" if $debugOn;
                $linecount += $count;
            }
        }
    }
    return $output;
}

######################
sub getList {
    my ( $line ) = @_;
    my @tks = split(/\s/,$line);
    my $i=0;
    my $list = "";
    foreach $t ( @tks ) {
        if ( $t eq "-m" ) {
            if ( $list ne "" ) { $list .= " "; }
            $list .= $tks[$i+1];
            $eListH{$tks[$i+1]}++;
        }            
        $i++;
    }
    return $list;
}
