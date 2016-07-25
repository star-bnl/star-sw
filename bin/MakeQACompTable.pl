#!/usr/bin/env perl
use File::Basename;

my $glob = "*/*.png";
my @Files = glob $glob;  print "found $#Files png-files\n";
my @dirs = ();
my @hist = ();
my %dirs = {};
my %hist = {};
my %dirs_hist = {};
my $nFiles = $#Files;
#foreach my $file (@Files) {
for (my $i = $nFiles; $i>=0; $i--) {
  my $file = $Files[$i];
  my $dir  = File::Basename::dirname($file);
  my $name = File::Basename::basename($file);
  $name =~ s/\.png//;
  $name =~ s/FailN//;
  $name =~ s/FailS//;
  $name =~ s/ //g;
  if (! $dirs{$dir} ) { $dirs{$dir} = $dir; push @dirs, $dir;}
  if (! $hist{$name} ) { $hist{$name} = $name; push @hist, $name;}
  my $tag = $dir . ":" . $name;
  if (! $dirs_hist{$tag} ) {
    $dirs_hist{$tag} = $file;
#    print "$file => $tag => $dirs_hist{$tag}\n";
  }
  else {
    die "dirs_hist has been defined for tag = $tag as $dirs_hist{$tag}. No redifinition with $file allowed\n";
  }
}
my $html_file = "compare_DEV2_SatDev.html";
open (HTML,">$html_file") or die "Can't open $html_file";
print HTML 
'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>Compare QA plots from DEV2 and Saturday DEV</title>
  </head>

  <body>
    <h1>Compare QA plots from DEV2 and Saturday DEV</h1>
<font size=-4> 
<table><COL width="30">
';
print HTML '<tr><td>Histogram/Chain</td>';
foreach  my $dir (@dirs) {
  my $d = $dir;
  $d =~ s/_/ /g;
  $d =~ s/\./ /g;
  $d =~ s/year//g;
  $d =~ s/MinBias/ mb/g;
  $d =~ s/minbias/ mb/g;
  $d =~ s/Prod/Prod /g;
  $d =~ s/prod/prod /g;
  $d =~ s/Low/ Low/g;
  $d =~ s/pp/ pp /g;
  $d =~ s/dAu/ dAu /g;
  $d =~ s/AuAu/ AuAu /g;
  $d =~ s/auau/ auau /g;
  $d =~ s/CuCu/ CuCu /g;
  $d =~ s/cucu/ cucu /g;
  $d =~ s/Tower/ Tow /g;
  $d =~ s/sl302//;
  $d =~ s/central/ cent /g;
  print HTML '<td>',$d,'</td>';
}
print HTML "</tr>\n";
print HTML "<hr>\n";
my $line;
foreach my $name (@hist) {
  $line = '<tr><td>' . $name . '</td>';
  my $nok = 0;
  foreach my $dir (@dirs) {
    my $tag = $dir . ":" . $name;
    my $file = $dirs_hist{$tag};
    if (! $file ) {$line .= '<td>N/A</td>';}
    else {
      $nok++;
      if ($file !~ /Fail/) {
	if ($file =~ /year_2005/) {$line .=  '<td><a href="' . $file . '"> OK.</a></td>'; $nok += 4;}
	else {$line .=  '<td>OK.</td>';}
      }
      else                 {$line .=  '<td><a href="' . $file . '"> Fail</a></td>'; $nok += 4;}
    }
  }
  if ($nok > 4) {
    $line .= "</tr>";
    print HTML "$line\n";
  }
}
print HTML
'</table>
';

print HTML
'    <hr>
    <address><a href="mailto:fisyak@rbnl.gov">Yuri Fisyak</a></address>
<!-- Created: Tue Jan  4 11:49:29 EST 2005 -->
<!-- hhmts start -->
Last modified: Thu Jan 20 14:55:15 EST 2005
<!-- hhmts end -->
  </body>
</html>
';
close(HTML);
