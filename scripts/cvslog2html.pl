#!/usr/bin/env perl
#
# cvslog2html.pl
#
# Parses the CVS commitlog file and builds an HTML history of CVS
# commits and a number of commit statistics.
# Modified J.Lauret Apr 2001
#  - secure rename instead of straight dump
#  - added ?sort=bydate so the directory is sorted by date
#    (more suitable)
#  - Found a strange case where CVS: appears. Removed from
#    final listing with regexp.
#
# Modified a few times from 2001-2006, J.Lauret
#
$commitlog   = "/afs/rhic.bnl.gov/star/packages/repository/CVSROOT/commitlog";
$cvsHtml     = "/afs/rhic.bnl.gov/star/doc_public/www/html/tmp/cvs";
$cvsUrl      = "/cgi-bin/protected/cvsweb.cgi";
$currentTime = localtime time;
$tmp         = "-tmp".getppid();


if( defined($ARGV[0]) ){ print "Starting on ".localtime()."\n";}
open (COMMITLOG,"< $commitlog") or die "Can't read file $commitlog: $!";

if ( ! -d "$cvsHtml"){      mkdir("$cvsHtml")      || exit;}
if ( ! -d "$cvsHtml/user"){ mkdir("$cvsHtml/user") || exit;}


# Prevent 2 from starting
chomp($host    = `/bin/hostname -s`);
$LockFile="$cvsHtml/$host.lock";
if( ! -e $LockFile){
    open(FO,">$LockFile") || exit;
    print FO "Started on ".localtime()."\n";
    close(FO);
} else {
    # do not continue. There is another
    # process running on the same machine or a too
    # old version of a lock file.
    $date = time()-(stat("$cvsHtml/$host.lock"))[9];
    if( $date > 7200){ unlink("$cvsHtml/$host.lock");}
    exit;
}

$iCommit = 0;
while (<COMMITLOG>) {
  if ( m/^--------------------------/ ) {
      undef(%codes);
      if ( $iCommit > 0) {
        $modA[$iCommit] = $module;
        $userA[$iCommit] = $uname;
        $timeA[$iCommit] = $commitTime;
      }
      $iCommit++;
      close USERFILE;
      $uname = <COMMITLOG>;
      chomp $uname;
      # if screwy username is found, skip it. A few records are messed up.
      if ( ! ( $uname =~ m/^[a-z0-9]+$/ ) ) {next}
      $commitTime = <COMMITLOG>;
      chomp $commitTime;
      $userDir = "$cvsHtml/user/$uname";
      $userFile = "$userDir/index.html";
      $moduleLine = <COMMITLOG>;
      @fields = split(/ /,$moduleLine);
      $module = $fields[2];
      $modulex= " ";
      if ( $module =~ /rhic\.bnl\.gov/ ){
	  $module =~ s/\/afs\/rhic\.bnl\.gov\/star\/packages\/repository\///;
      } else {
	  $module =~ s/\/afs\/rhic\/star\/packages\/repository\///;
      }
      # if still remains, user has done something he should not do ...
      if ( $module =~ m/(\/afs.*\/repository\/)(.*)/){
	  ($modulex,$module)= ($1,$2);
	  $modulex = "<font color=\"red\">$modulex</font>";
      }

      chomp $module;
      # Now get the log message
      if (exists($commitUsers{"$uname"})) {
          $commitUsers{"$uname"}++;
          open (USERFILE, ">> $userFile") or die "Can't append $userFile: $!";
      } else {
          $commitUsers{"$uname"} = 1;
          if ( ! -d $userDir ) {
              mkdir $userDir, 0777 or die "Can't create dir $userDir: $!";
          }
          open (USERFILE, "> $userFile") or die "Can't create $userFile: $!";
	  print USERFILE qq~
          <html>
            <head>
            <title>CVS commits by $uname</title>
            <meta http-equiv="expires" content="$currentTime">
          </head>
        <body bgcolor=cornsilk text=black link=navy vlink=maroon alink=tomato>
        <basefont face="verdana,arial,helvetica,sans-serif">
        <a name="top"></a>
        <table border=0   cellpadding=5 cellspacing=0 width="100%">
        <tr bgcolor=lightgrey>
          <td align=left>CVS commits by <b>$uname</b></td>
          <td align=right> <font size="-1">Last updated $currentTime
        </tr></table>
        <font size="-1"><a href="#bottom">Go to bottom</a></font><br>
	    ~;
    };
    print USERFILE qq~
      <br><b>Commit to <a href="$cvsUrl/$module/">$module</a> $modulex
      at $commitTime</b><br>&nbsp;&nbsp;
      ~;

    $latestCommit{"$uname"} = $commitTime;
    $latestModule{"$uname"} = $module;

  } elsif ( m/^=============================/ ) {
      $fnameLine = <COMMITLOG>;
      @fields    = split (/\s/,$fnameLine);
      $fname     = $fields[1];
      if ( ! defined($codes{$fname}) ){
	  $codes{$fname} = 1;
	  print USERFILE " $fname (<a href=\"$cvsUrl/$module/$fname\">+</a>)";
      }

  } elsif ( m/^Log Message:/ ) {
      $logMessage = <COMMITLOG>;
      chomp $logMessage;
      print USERFILE qq~
<font size="-1" color=darkgreen>$logMessage</font>
<br>&nbsp;&nbsp;
      ~;

  }
}


if( defined($ARGV[0]) ){ print "User commit ".localtime()."\n";}
$userCommits = "$cvsHtml/userCommits.html";
#if( -e "$userCommits$tmp"){  exit;}

open (USERCOMMITS, "> $userCommits$tmp") or die "Can't write file $userCommits: $!";
print USERCOMMITS qq~
<html>
<head>
<title>CVS commits by user</title>
<meta http-equiv="expires" content="$currentTime">
</head>
<body bgcolor=cornsilk text=black link=navy vlink=maroon alink=tomato>
<basefont face="verdana,arial,helvetica,sans-serif">
<table border=0   cellpadding=5 cellspacing=0 width="100%">
	<tr bgcolor=lightgrey>
<td align=left><font size="+2">CVS commits by user</font></td>
<td align=right> <font size="-1">Last updated $currentTime
</tr></table>
<p>
<table width="70%" align=center border=1>
<tr bgcolor=whitesmoke><td>
This material is based on the logfile \$CVSROOT/CVSROOT/commitlog.
Click on the username for a record of commits, and on the module
name to browse that module\'s CVS area.
</tr></td>
</table>
<p>
<table border=1 cellpadding=3 cellspacing=0>
<tr bgcolor=lightgrey>
  <td align=center><b> User </b></td>
  <td align=center><b> Commits </b></td>
  <td align=center><b> Latest Commit </b></td>
  <td align=center><b> Latest Package </b></tr>
    ~;


$firstTime = $timeA[1];
foreach $cuser (sort keys %commitUsers) {
    if($cuser =~ /CVS:/ || $cuser eq ""){ next;}

    print USERCOMMITS qq~
    <tr bgcolor=whitesmoke>
        <td>&nbsp; <a href="user/$cuser/index.html#bottom">$cuser</a> &nbsp;</td>
        <td align=center>$commitUsers{$cuser}</td>
        <td>&nbsp; $latestCommit{$cuser} &nbsp;</td>
        <td>&nbsp; <a href="$cvsUrl/$latestModule{$cuser}">$latestModule{$cuser}</a> &nbsp;</td>
    </tr>
	~;

    $userDir = "$cvsHtml/user/$cuser";
    $userFile = "$userDir/index.html";
    open (USERFILE, ">> $userFile") or die "Can't append $userFile: $!";
    print USERFILE qq~
        <p>
        <font size="-1"><a href="#top">Go to top</a></font><br>
        <table border=0   cellpadding=5 cellspacing=0 width="100%">
        <tr bgcolor=lightgrey>
          <td align=left>CVS commits by <b>$cuser</b></td>
          <td align=right> <font size="-1">Last updated $currentTime
        </tr></table>
        <a name="bottom"></a>
        <font size=-1>
        Commits since $firstTime
        </font>
        </body>
        </html>
	    ~;
    close USERFILE; 
}

#<a href="mailto:wenaus\@bnl.gov">Torre Wenaus</a>

print USERCOMMITS  qq~
</table>
<font size=-1>
Commits since $firstTime <br>
J&eacute;r&ocirc;me Lauret
</font>
</body></html>
    ~;
rename("$userCommits$tmp","$userCommits");



if( defined($ARGV[0]) ){ print "Commit histo ".localtime()."\n";}
$commitHistory = "$cvsHtml/commitHistory.html";
open (COMMITHISTORY, "> $commitHistory$tmp") or die "Can't write file $commitHistory: $!";

print COMMITHISTORY qq~
<html>
<head>
<title>Recent CVS commit history</title>
<meta http-equiv="Refresh" content="1200">
<meta http-equiv="expires" content="$currentTime">
</head>
<body bgcolor=cornsilk text=black link=navy vlink=maroon alink=tomato>
<basefont face="verdana,arial,helvetica,sans-serif">
<table border=0   cellpadding=5 cellspacing=0 width="100%">
	<tr bgcolor=lightgrey>
<td align=left><font size="+2">Recent CVS commit history</font></td>
<td align=right> <font size="-1">Last updated $currentTime
</tr></table>
<p>
<table width="70%" align=center border=1>
<tr bgcolor=whitesmoke><td>
This material is based on the logfile \$CVSROOT/CVSROOT/commitlog.
Click on the username for a record of commits, and on the module
name to browse that module\'s CVS area.
</tr></td> 
</table> 
<p>
<table border=1 cellpadding=3 cellspacing=0>
<tr bgcolor=lightgrey>
  <td align=center><b> User </b></td>
  <td align=center><b> Package </b></td>
  <td align=center><b> Date </b></td></tr>
    ~;

$modA[$iCommit]  = $module;
$userA[$iCommit] = $uname;
$timeA[$iCommit] = $commitTime;
$iFirstIndex = $iCommit - 500;
if ($iFirstIndex < 1) { $iFirstIndex = 1; }
for ( $i=$iCommit; $i>=$iFirstIndex; $i--) {
    if($userA[$i] =~ /CVS:/ || $userA[$i] eq ""){ next;}
    if ( ! defined($modA[$i]) || ! defined($timeA[$i]) ){   
	$x = "&nbsp;";
    } else {
	$x = "";
    }
    print COMMITHISTORY qq~
    <tr bgcolor=whitesmoke>
        <td>&nbsp; <a href="user/$userA[$i]/index.html#bottom">$userA[$i]</a></td>
        <td>&nbsp; <a href="$cvsUrl/$modA[$i]/?sortby=date">$modA[$i]</a> $x </td>
        <td align=center>$timeA[$i] $x</td>
    </tr>
	~;

}

#<font size=-1><a href="mailto:wenaus\@bnl.gov">Torre Wenaus</a></font>
print COMMITHISTORY qq~
</table>
<font size=-1>J&eacute;r&ocirc;me Lauret</a></font>
</body></html>
    ~;
close COMMITHISTORY;
rename("$commitHistory$tmp","$commitHistory");

unlink($LockFile);
