#!/usr/local/bin/perl
#
# cvslog2html.pl
#
# Parses the CVS commitlog file and builds an HTML history of CVS
# commits and a number of commit statistics.
#
$commitlog = "/afs/rhic/star/packages/repository/CVSROOT/commitlog";
$cvsHtml = "/star/starlib/doc/www/cvs";
$cvsUrl = "/cgi-bin/cvsweb.cgi";
$currentTime = localtime time;

open (COMMITLOG,"< $commitlog") or die "Can't read file $commitlog: $!";

$userCommits = "$cvsHtml/userCommits.html";
open (USERCOMMITS, "> $userCommits") or die "Can't write file $userCommits: $!";
$content = <<END_BLOCK;
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
name to browse that module's CVS area.
</tr></td>
</table>
<p>
<table border=1 cellpadding=3 cellspacing=0>
<tr bgcolor=lightgrey>
  <td align=center><b> User </b></td>
  <td align=center><b> Commits </b></td>
  <td align=center><b> Latest Commit </b></td>
  <td align=center><b> Latest Package </b></tr>
END_BLOCK
print USERCOMMITS $content;

$iCommit = 0;
while (<COMMITLOG>) {
  if ( m/^--------------------------/ ) {
      if ( $iCommit > 0) {
        @modA[$iCommit] = $module;
        @userA[$iCommit] = $uname;
        @timeA[$iCommit] = $commitTime;
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
      $module =~ s/\/afs\/rhic\/star\/packages\/repository\///;
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
          $content = <<END_BLOCK;
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
END_BLOCK
        print USERFILE $content;
    };
    $content =<<END_BLOCK;
      <br><b>Commit to <a href="$cvsUrl/$module/">$module</a> at $commitTime</b>
<br>&nbsp;&nbsp;
END_BLOCK
    print USERFILE $content;
    $latestCommit{"$uname"} = $commitTime;
    $latestModule{"$uname"} = $module;
  }

  if ( m/^=============================/ ) {
      $fnameLine = <COMMITLOG>;
      @fields = split (/\s/,$fnameLine);
      $fname = $fields[1];
      print USERFILE " $fname ";
  }
  if ( m/^Log Message:/ ) {
     $logMessage = <COMMITLOG>;
     chomp $logMessage;
    $content =<<END_BLOCK;
<font size="-1" color=darkgreen>$logMessage</font>
<br>&nbsp;&nbsp;
END_BLOCK
    print USERFILE $content;
  }
}

$firstTime = $timeA[1];
foreach $cuser (sort keys %commitUsers) {
#    print "$cuser $commitUsers{$cuser}\n";
    $content = <<END_BLOCK;
    <tr bgcolor=whitesmoke>
        <td>&nbsp; <a href="user/$cuser/index.html#bottom">$cuser</a> &nbsp;</td>
        <td align=center>$commitUsers{$cuser}</td>
        <td>&nbsp; $latestCommit{$cuser} &nbsp;</td>
        <td>&nbsp; <a href="$cvsUrl/$latestModule{$cuser}">$latestModule{$cuser}</a> &nbsp;</td>
    </tr>
END_BLOCK
    print USERCOMMITS $content;
    $userDir = "$cvsHtml/user/$cuser";
    $userFile = "$userDir/index.html";
    open (USERFILE, ">> $userFile") or die "Can't append $userFile: $!";
    $content = <<END_BLOCK;
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
END_BLOCK
    print USERFILE $content;
    close USERFILE; 
}
$content =<<END_BLOCK;
</table>
<font size=-1>
Commits since $firstTime <br>
<a href="mailto:wenaus\@bnl.gov">Torre Wenaus</a>
</font>
</body></html>
END_BLOCK
print USERCOMMITS $content;
close USERCOMMITS;

$commitHistory = "$cvsHtml/commitHistory.html";
open (COMMITHISTORY, "> $commitHistory") or die "Can't write file $commitHistory: $!";
$content = <<END_BLOCK;
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
name to browse that module's CVS area.
</tr></td>
</table>
<p>
<table border=1 cellpadding=3 cellspacing=0>
<tr bgcolor=lightgrey>
  <td align=center><b> User </b></td>
  <td align=center><b> Package </b></td>
  <td align=center><b> Date </b></td></tr>
END_BLOCK
print COMMITHISTORY $content;

@modA[$iCommit] = $module;
@userA[$iCommit] = $uname;
@timeA[$iCommit] = $commitTime;
$iFirstIndex = $iCommit - 500;
if ($iFirstIndex < 1) { $iFirstIndex = 1; }
for ( $i=$iCommit; $i>=$iFirstIndex; $i--) {
    $content = <<END_BLOCK;
    <tr bgcolor=whitesmoke>
        <td>&nbsp; <a href="user/$userA[$i]/index.html#bottom">$userA[$i]</a> &nbsp;</td>
        <td>&nbsp; <a href="$cvsUrl/$modA[$i]">$modA[$i]</a> &nbsp;</td>
        <td align=center>$timeA[$i]</td>
    </tr>
END_BLOCK
    print COMMITHISTORY $content;
}

$content =<<END_BLOCK;
</table>
<font size=-1><a href="mailto:wenaus\@bnl.gov">Torre Wenaus</a></font>
</body></html>
END_BLOCK
print COMMITHISTORY $content;
close COMMITHISTORY;
