#!/opt/star/bin/perl

use File::Basename;
require "dbheader.pl";
require "dbsetup.pl";

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
                 file count, line count
    <li> The full listing adds all files (the same level of detail
    as the individual package listings)
    <li> The file listings report the username of the most recent
    modifier, if it can be determined from the file, ie. if a 
    comment line containing \$Id\$ has been included in the file (such
    a line should be in every file).
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

if ( exists($input{"ver"}) ) {
    $ver = $input{"ver"};
} else {
    $ver = "dev";
}

if ( exists($input{"detail"}) ) {
    $showFlag = $input{"detail"};
} else {
    $showFlag = 0;
}

if ( exists($input{"find"}) ) {
    $find = $input{"find"};
} else {
    $find = "";
}

if ( exists($input{"pkg"}) ) {
    $pkg = $input{"pkg"};
} else {
    $pkg = "";
}

#read in avail file of package owners
open(AVAIL,"< /afs/rhic/star/packages/repository/CVSROOT/avail")
    or die "Can't open avail file: $!";
@availFile=<AVAIL>;
close AVAIL;

$STAR = "/afs/rhic/star/packages/$ver";
$root = $STAR;
$rel = readlink("/afs/rhic/star/packages/$ver");
($f, $d, $e) = fileparse($rel);
$rel = $f;

$devVer = (fileparse(readlink("/afs/rhic/star/packages/dev")))[0];
$newVer = (fileparse(readlink("/afs/rhic/star/packages/new")))[0];
$proVer = (fileparse(readlink("/afs/rhic/star/packages/pro")))[0];
$oldVer = (fileparse(readlink("/afs/rhic/star/packages/old")))[0];

$verChecked{$ver} = "checked";
print <<END;
<form method="GET" action="http://duvall.star.bnl.gov/cgi-bin/prod/swguide.pl">
<b>Version:</b>
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
    <input type="radio" checked name="detail" value="0"> Package list
    <input type="radio" name="detail" value="1"> Package list with details
    <input type="radio" name="detail" value="2"> Full listing <br>
<b>Find package:</b> <input type="text" name="pkg" value="$pkg">
<b>or Find file:</b> <input type="text" name="find" value="$find"><br>

<input type="submit"> &nbsp; <input type="reset"><br>
</form>

<hr>
<p>
<h3>Version $ver = $rel</h3>
</h3>
<pre>
END

$totlines = 0;
$totfiles = 0;
#### Loop through packages

@allDirs = (
            "StRoot",
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
            "pams/vpd",
            "StDb",
            "mgr"
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
    while (defined ($file = readdir DIR)) {
        next if (exists($ignoreStuff{$file}));
        if ( $pkg ne "" ) {
            if ( $file eq $pkg || $dir."/".$file eq $pkg ) {
                &showPackage($root,$dir,$file);
                last;
            } else {
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
        if ( $availFile[$ia] =~ m/$dr\/$thePkg/ ) {
            $own = $availFile[$ia];
            $own =~ m/(avail\s+\|)([a-z]+)/;
            $pkgOwner = $2;
        } elsif ( $availFile[$ia] =~ m/$dr/ ) {
            if ( $pkgOwner eq "" ) {
                $own = $availFile[$ia];
                $own =~ m/^\s*([a-z]+\s*\|)([a-z]+)/;
                $pkgOwner = $2;
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

    ### Find the newest file in the package
    ### Print results
    $pkgUrl = "<a href=\"http://duvall.star.bnl.gov/cgi-bin/prod/swguide.pl?ver=$ver&pkg=$thePkg&detail=2\">";
    if ( $showFlag > 0 ) { # print all pkg info
        if ($linecount == 0) {
            $disp1="<font color=\"gray\">";
            $disp2="</font>";
        } else {
            $disp1="<b>";
            $disp2="</b>";
        }
        $pkgLine = sprintf("$disp1%s%-30s%s %s %s %s %s %12s %3d files %6d lines$disp2\n",
                           $pkgUrl,$theDir."/".$thePkg,"</a>",$readme,$doc,$cvs,$src,$pkgOwner,$filecount,$linecount);
    } else {
        $pkgLine = sprintf("%s%-30s%s %s %s %s %s\n",
                           $pkgUrl,$theDir."/".$thePkg,"</a>",$readme,$doc,$cvs,$src);
    }
    if ( $showFlag >= 0 ) { print $pkgLine; }
    if ( $showFlag > 1 ) {
        print $details;
        if ($nsub>0) {
            for ($ns=0; $ns<$nsub; $ns++) {
                print "<br><b>&nbsp;&nbsp;$subDirs[$ns]/</b>\n";
                print $subdetails[$ns];
            }
        }
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
            $count = 0;
            $showLines = 0;
            if ($showFlag > 0) {
                $ee = "";
                ($ff, $dd, $ee) = fileparse(
                    "$theRoot/$theDir/$thePkg/$fname",'\.[a-zA-z]*');
                print "\"$ff\" \"$dd\" \"$ee\"\n" if $debugOn;
                $isScript = 0;
                if ( $ee eq "" ) {
                    open(FL,"< $theRoot/$theDir/$thePkg/$fname") or die "Can't open file $fname: $!";
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
                    open(FILE,"< $theRoot/$theDir/$thePkg/$fname") or die "Can't open file $fname: $!";
                    @lines=<FILE>;
                    close FILE;
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
                            if ( $lines[$ii] =~ m/\*\/\s*$/ ) { $cend=1; }
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
                    $theLines = sprintf("%5d Lines",$count);
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

                $output .= sprintf("    %-35s %s%-7s%s %s %s %s %12s %s\n",
                                   $fname,
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
