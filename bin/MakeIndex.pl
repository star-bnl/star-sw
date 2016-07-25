#!/usr/bin/env perl
use Env;
my $index = "indexMc.html";
open (Out, ">$index") or die "Can't open $index";
#________________________________________________________________________________
sub prefix($) {
  if ($_[0] eq 'none') {return "";}
  if ($_[0] ne 'pion') {return  $_[0];}
  else                 {return "pi";}
}
#________________________________________________________________________________
# Chi2 -> All   
#________________________________________________________________________________
sub BeginTable($$) {
  #  my $Name = shift @_;
  #  my $Title = shift @_;
  #  my $line = '<h2><a name="' . $Name . 'Chi2"' . $Title . '</a></h2>';
  my $line = '<h2><a name="' . $_[0] . '">' . $_[1] . '</a></h2>';
  print Out "$line";
  print Out '
    <table width="90%" border="1" cellspacing="2" cellpadding="0">
	<tr>
	  <td>Sti</td>
	  <td>StiCA</td>
	  <td>Stv</td>
	  <td>StvCA</td>
	</tr>
';
}
#________________________________________________________________________________
sub EndTable() {
  print Out ' 
   </table>
';
}
#________________________________________________________________________________
sub OneRow($$) {
  print Out "<tr>Fig.$_[1],\n"; 
  foreach my $v (qw(Sti StiCA Stv StvCA)) { 
    my $vv = "";
    if ($v eq 'Sti') {$vv .= " <a name=\"Fig.$_[1]\">Fig.$_[1]</a>";}
    my $line =  "<td>" . $vv . "<img src=\"" . $v . "/" . $_[0] . "\" alt=\"\" width=\"400\" border=\"0\"></td>"; 
    print Out "$line\n"; 
  } 
  print Out "</tr>\n";  
}
#________________________________________________________________________________
print Out '
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>Reconstruction versus Simulation</title>
  </head>

  <body>
    <h1>Reconstruction versus Simulation</h1>
    <h2>Legend</h2>
<p>The plots are designed to answer on the following questions:
  <ul>
      <li> What tracks (globals and primaries) can be considered as "good" ones depending on total no. of fit points and no. of bad hits ? </li>
      <li> What is the track parameters and their errors dependence on the track kinematics (&phi;, &eta;, 1/pT) ? </li>
      <li> What is the track paramerer pulls dependence on above kinematics ? </li>
      <li> What is the track reconstruction effeciencies for : </li>
      <ul>
           <li> Geometrical acceptance (MC only), </li>
           <li> Reconstruction effiency for track with only match between MC and RC </li>
           <li> Clones, for multiple (>1) match between single MC track to RC one, </li>
           <li> Lost tracks, MC tracks which have no RC partner. </li>
           <li> Ghost tracks, RC tracks which have no MC partner. </li>
      </ul> 
      <li> Color scheme: <font color=black>&nbsp;&bull; Positive</font> and <font color=red>&nbsp;&bull; Negative</font> Tracks. </li>
      <li> Results of Gauss fit for slices are presented as &bull; for &mu; and as  box for &sigma;.  </li>
  </ul>
  <ul>

'
;
my $FigNo = 1;
foreach my $tv qw(Tracks){# Vertices) {
  foreach my $x qw(NoHits EtapT) {
    foreach my $particle qw(All Pion) {
      foreach my $gp qw(Global Primary) {
	foreach my $type qw(Hit Rec Clone Lost Ghost) { # Mc
	  my $openedtable = 0;
	  foreach my $var qw(ChiSqXY ChiSqZ dDcaXY dDcaZ dPsi dPti dPtiR dTanL deta pDcaXY pDcaZ pPsi pPti pPtiR pTanL peta Phi) {
	    my @postfix = ("");
	    if ($var eq "Phi") {@postfix = qw(GeomA EffA EffG CloneA CloneG GhostA GhostG);}
	    foreach my $postfix (@postfix) {
	      foreach my $projection qw(x y yx yx_pfx yx_pfy  zx zx_pfx zx_pfy zy zy_pfx zy_pfy) {
		my $file = $tv . "_" . $gp . "_" . $type . "_" . $particle . "_" . $x . "_" . $postfix . $var . "_" . $projection . ".png";
		print "file = $file\n";
#		die;
		if (! -r $file) {next;}
		my $title = $file;
		$title =~ s/_/ /g;
		$title =~ s/\.png//;
		if (! $openedtable) {$openedtable++; BeginTable($title,$title);}
		OneRow($file,$FigNo++);
	      }
	    }
	  }
	  if ($openedtable) {EndTable();}
	}
      }
    }
  }
}
# _____ end of body _________
print Out '
    <p></p>
    <hr style="width: 100%; height: 2px;">
    

    <hr>
    <address><a href="mailto:fisyak@bnl.gov">Yuri Fisyak</a></address>
<!-- Created: Wed Mar  2 18:20:09 EST 2005 -->
<!-- hhmts start -->
Last modified: Mon Oct 10 17:01:26 EDT 2011
<!-- hhmts end -->
  </body>
</html>
';
close (Out);
