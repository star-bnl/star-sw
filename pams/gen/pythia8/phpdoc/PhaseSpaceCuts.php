<html>
<head>
<title>Phase Space Cuts</title>
<link rel="stylesheet" type="text/css" href="pythia.css"/>
<link rel="shortcut icon" href="pythia32.gif"/>
</head>
<body>

<script language=javascript type=text/javascript>
function stopRKey(evt) {
var evt = (evt) ? evt : ((event) ? event : null);
var node = (evt.target) ? evt.target :((evt.srcElement) ? evt.srcElement : null);
if ((evt.keyCode == 13) && (node.type=="text"))
{return false;}
}

document.onkeypress = stopRKey;
</script>
<?php
if($_POST['saved'] == 1) {
if($_POST['filepath'] != "files/") {
echo "<font color='red'>SETTINGS SAVED TO FILE</font><br/><br/>"; }
else {
echo "<font color='red'>NO FILE SELECTED YET.. PLEASE DO SO </font><a href='SaveSettings.php'>HERE</a><br/><br/>"; }
}
?>

<form method='post' action='PhaseSpaceCuts.php'>

<h2>Phase Space Cuts</h2>

<code>PhaseSpace</code> is base class for all hard-process phase-space 
generators, either generic <i>2 -> 1</i> or <i>2 -> 2</i> ones, 
or specialized ones like for elastic and diffractive scattering.

<p/>
In it, it is possible to constrain the kinematics of most processes.
(Exceptions are "soft physics", i.e. minimum bias, elastic and 
diffractive processes. The Coulomb singularity for elastic scatterings,
if simulated, is <?php $filepath = $_GET["filepath"];
echo "<a href='TotalCrossSections.php?filepath=".$filepath."' target='page'>";?>handled separately</a>.) 
These constraints apply in the rest frame of the hard subprocess, and 
topologies normally would be changed e.g. by subsequent showering 
activity. The cross section of a process is adjusted to only 
correspond to the allowed phase space.

<p/>
The more particles in the final state, the more cuts could be applied.
Here we have tried to remain with the useful minimum, however. More
generic possibilities could be handled by the 
<?php $filepath = $_GET["filepath"];
echo "<a href='UserHooks.php?filepath=".$filepath."' target='page'>";?>user hooks</a> facility. 

<h3>Cuts in all processes</h3>

<br/><br/><table><tr><td><strong>PhaseSpace:mHatMin </td><td></td><td> <input type="text" name="1" value="4." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>4.</strong></code>; <code>minimum = 0.</code>)</td></tr></table>
The minimum invariant mass.
  

<br/><br/><table><tr><td><strong>PhaseSpace:mHatMax </td><td></td><td> <input type="text" name="2" value="-1." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>-1.</strong></code>)</td></tr></table>
The maximum invariant mass.
A value below <code>mHatMin</code> means there is no upper limit.
  

<h3>Cuts in <i>2 -> 1</i> processes</h3>

When a resonance <code>id</code> is produced, the 
<?php $filepath = $_GET["filepath"];
echo "<a href='ParticleDataScheme.php?filepath=".$filepath."' target='page'>";?><code>mMin(id)</code>and 
<code>mMax(id)</code></a> methods restrict the allowed mass range
of this resonance. Therefore the allowed range is chosen to be the 
overlap of this range and the <code>mHatMin</code> to 
<code>mHatMax</code> range above. Most resonances by default have no 
upper mass limit, so effects mainly concern the lower limit. 
Should there be no overlap between the two ranges then the process 
will be switched off.

<h3>Cuts in <i>2 -> 2</i> processes</h3>

<br/><br/><table><tr><td><strong>PhaseSpace:pTHatMin </td><td></td><td> <input type="text" name="3" value="0." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.</strong></code>; <code>minimum = 0.</code>)</td></tr></table>
The minimum invariant <i>pT</i>.
  

<br/><br/><table><tr><td><strong>PhaseSpace:pTHatMax </td><td></td><td> <input type="text" name="4" value="-1." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>-1.</strong></code>)</td></tr></table>
The maximum invariant <i>pT</i>.
A value below <code>pTHatMin</code> means there is no upper limit.
  

<br/><br/><table><tr><td><strong>PhaseSpace:pTHatMinDiverge </td><td></td><td> <input type="text" name="5" value="1." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1.</strong></code>; <code>minimum = 0.5</code>)</td></tr></table>
Extra <i>pT</i> cut to avoid the divergences of some processes 
in the limit <i>pT -> 0</i>. Specifically, if either or both
produced particles have a mass below <code>pTHatMinDiverge</code> 
then <i>pT</i> is limited from below by the larger of 
<code>pTHatMin</code> and <code>pTHatMinDiverge</code>.
  

<br/><br/><strong>PhaseSpace:useBreitWigners</strong>  <input type="radio" name="6" value="on" checked="checked"><strong>On</strong>
<input type="radio" name="6" value="off"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>on</strong></code>)<br/>
Allows masses to be selected according to Breit-Wigner shapes in 
<i>2 -> 2</i> processes, whenever particles have been declared 
with a nonvanishing width above the threshold below. In those cases 
also the limits below will be used for the mass selection. For 
<i>2 -> 1</i> processes the Breit-Wigner shape is part of the 
cross section itself, and therefore always included.
  

<br/><br/><table><tr><td><strong>PhaseSpace:minWidthBreitWigners </td><td></td><td> <input type="text" name="7" value="0.01" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.01</strong></code>; <code>minimum = 1e-6</code>)</td></tr></table>
The minimum width a resonance must have for the mass to be dynamically
selected according to a Breit-Wigner shape, within the limits set below.
Only applies when <code>useBreitWigners</code> is on; else the nominal
mass value is always used.
  

<p/>
For a particle with a Breit-Wigner shape selected, according to the 
rules above and to the rules of the particle species itself, the 
<?php $filepath = $_GET["filepath"];
echo "<a href='ParticleDataScheme.php?filepath=".$filepath."' target='page'>";?><code>mMin(id)</code>
and <code>mMax(id)</code></a> methods restrict the allowed mass range
of the particle, just like for the <i>2 -> 1 </i> processes.   

<h3>Cuts in <i>2 -> 3</i> processes</h3>

Currently no further cuts have been introduced for <i>2 -> 3</i>
processes than those already available for <i>2 -> 2</i> ones.
A <i>2 -> 3</i> process in principle would allow to define a
separate <i>pT</i> range for each of the three particles (with some
correlations), but for now all three are restricted exactly the
same way by <code>pTHatMin</code> and <code>pTHatMax</code>.  
As above, Breit-Wigner mass ranges can be restricted.

<h3>Documentation</h3>

<br/><br/><strong>PhaseSpace:showSearch</strong>  <input type="radio" name="8" value="on"><strong>On</strong>
<input type="radio" name="8" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Possibility to print information on the search for phase-space 
coefficients that (in a multichannel approach) provides an analytical 
upper envelope of the differential cross section, and the 
corresponding upper estimate of the cross section. Of interest 
for crosschecks by expert users only. 
  

<br/><br/><strong>PhaseSpace:showViolation</strong>  <input type="radio" name="9" value="on"><strong>On</strong>
<input type="radio" name="9" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Possibility to print information whenever the assumed maximum 
differential cross section of a process is violated, i.e. when 
the initial maximization procedure did not find the true maximum.
Also, should negative cross sections occur, print whenever a more
negative value is encountered.
  

<input type="hidden" name="saved" value="1"/>

<?php
echo "<input type='hidden' name='filepath' value='".$_GET["filepath"]."'/>"?>

<table width="100%"><tr><td align="right"><input type="submit" value="Save Settings" /></td></tr></table>
</form>

<?php

if($_POST["saved"] == 1)
{
$filepath = $_POST["filepath"];
$handle = fopen($filepath, 'a');

if($_POST["1"] != "4.")
{
$data = "PhaseSpace:mHatMin = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "-1.")
{
$data = "PhaseSpace:mHatMax = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "0.")
{
$data = "PhaseSpace:pTHatMin = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "-1.")
{
$data = "PhaseSpace:pTHatMax = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "1.")
{
$data = "PhaseSpace:pTHatMinDiverge = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "on")
{
$data = "PhaseSpace:useBreitWigners = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "0.01")
{
$data = "PhaseSpace:minWidthBreitWigners = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
if($_POST["8"] != "off")
{
$data = "PhaseSpace:showSearch = ".$_POST["8"]."\n";
fwrite($handle,$data);
}
if($_POST["9"] != "off")
{
$data = "PhaseSpace:showViolation = ".$_POST["9"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
