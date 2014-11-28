<html>
<head>
<title>QCD Processes</title>
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

<form method='post' action='QCDProcesses.php'>

<h2>QCD Processes</h2>

This section is subdivided into soft and hard QCD processes, with
open charm and bottom production set aside as a special part of the
latter. 

<h3>Soft QCD processes</h3>

As a rule, the processes in this class should not be mixed with
the simulation of other processes. All by themselves, they are
intended to represent the total cross section of hadron collisions,
with the exception of the "rare processes" that one wishes to study
separately. In particular, jet physics at all scales occurs as part 
of the minimum-bias description. 

<p/>
We here use the "minimum bias" expression as a shorthand for 
inelastic, nondiffractive events. Strictly speaking, "minimum bias" 
represents an experimental procedure of accepting "everything", with 
some non-universal cuts to exclude elastic and diffractive topologies. 
In practice, the experimental mimimum-bias sample may then contain
some contamination of what is in PYTHIA classified as diffractive,
especially (high-mass) double diffractive. 

<p/>
Some options to modify these cross sections, and especially to include
Coulomb corrections to the elastic cross section, are found on the
<?php $filepath = $_GET["filepath"];
echo "<a href='TotalCrossSections.php?filepath=".$filepath."' target='page'>";?>Total Cross Sections</a> page.  

<br/><br/><strong>SoftQCD:all</strong>  <input type="radio" name="1" value="on"><strong>On</strong>
<input type="radio" name="1" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Common switch for the group of all soft QCD processes, 
as listed separately in the following.
  

<br/><br/><strong>SoftQCD:minBias</strong>  <input type="radio" name="2" value="on"><strong>On</strong>
<input type="radio" name="2" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Minimum-bias events, based on an eikonalized description of all the 
hard QCD processes, so includes them in combinationation with 
low-<i>pT</i> events. Code 101.<br/>
Since the current description is handled by the multiple-interactions 
machinery as part of the parton-level processing, no hard process at 
all is defined at the process-level part of the event generation.
Fortunately, in this case a special
<?php $filepath = $_GET["filepath"];
echo "<a href='EventInformation.php?filepath=".$filepath."' target='page'>";?><code>codeSub()</code></a> 
method provides information on the first, i.e. hardest, subprocess 
selected by the multiple-interactions machinery.

  

<br/><br/><strong>SoftQCD:elastic</strong>  <input type="radio" name="3" value="on"><strong>On</strong>
<input type="radio" name="3" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Elastic scattering <i>A B -> A B</i>.
Code 102.
  

<br/><br/><strong>SoftQCD:singleDiffractive</strong>  <input type="radio" name="4" value="on"><strong>On</strong>
<input type="radio" name="4" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Single diffractive scattering <i>A B -> X B</i> and 
<i>A B -> A X</i>. 
Codes 103 and 104.
  

<br/><br/><strong>SoftQCD:doubleDiffractive</strong>  <input type="radio" name="5" value="on"><strong>On</strong>
<input type="radio" name="5" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Double diffractive scattering <i>A B -> X_1 X_2</i>.
Code 105.
  

<h3>Hard QCD processes</h3>

This group contains the processes for QCD jet production above
some minimum <i>pT</i> threshold. The <i>pT_min</i> cut cannot be put 
too low, or else unreasonably large jet cross sections will be obtained.
An eikonalized description, intended to be valid at all <i>pT</i>,
is included as part of the multiple-interactions framework, e.g. in 
<code>SoftQCD:minBias</code> above.
  
<br/><br/><strong>HardQCD:all</strong>  <input type="radio" name="6" value="on"><strong>On</strong>
<input type="radio" name="6" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Common switch for the group of all hard QCD processes, 
as listed separately in the following.
  

<br/><br/><strong>HardQCD:gg2gg</strong>  <input type="radio" name="7" value="on"><strong>On</strong>
<input type="radio" name="7" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>g g -> g g</i>.
Code 111.
  

<br/><br/><strong>HardQCD:gg2qqbar</strong>  <input type="radio" name="8" value="on"><strong>On</strong>
<input type="radio" name="8" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>g g -> q qbar</i>, where <i>q</i> by default
is a light quark (<i>u, d, s</i>) (see below).
Code 112.
  

<br/><br/><strong>HardQCD:qg2qg</strong>  <input type="radio" name="9" value="on"><strong>On</strong>
<input type="radio" name="9" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q g -> q g</i> and <i>qbar g -> qbar g</i>.
Code 113.
  

<br/><br/><strong>HardQCD:qq2qq</strong>  <input type="radio" name="10" value="on"><strong>On</strong>
<input type="radio" name="10" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q q' -> q q'</i>, <i>q qbar' -> q qbar'</i>, 
<i>qbar qbar' -> qbar qbar'</i>, where <i>q'</i> and <i>q</i> 
may agree, but the outgoing flavours equals the incoming ones 
Code 114.
  

<br/><br/><strong>HardQCD:qqbar2gg</strong>  <input type="radio" name="11" value="on"><strong>On</strong>
<input type="radio" name="11" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q qbar -> g g</i>. 
Code 115.
  

<br/><br/><strong>HardQCD:qqbar2qqbarNew</strong>  <input type="radio" name="12" value="on"><strong>On</strong>
<input type="radio" name="12" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q qbar -> q' qbar'</i>, where <i>q'</i> 
by default is a light quark (<i>u, d, s</i>) (see below). 
Code 116.
  

<br/><br/><table><tr><td><strong>HardQCD:nQuarkNew  </td><td></td><td> <input type="text" name="13" value="3" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>3</strong></code>; <code>minimum = 0</code>; <code>maximum = 5</code>)</td></tr></table>
Number of allowed outgoing new quark flavours in the above 
<i>g g -> q qbar</i> and <i>q qbar -> q' qbar'</i> processes, 
where quarks are treated as massless in the matrix-element expressions 
(but correctly in the phase space). It is thus assumed that <i>c cbar</i> 
and <i>b bbar</i> are added separately with masses taken into account,
using the processes below. A change to 4 would also include <i>c cbar</i> 
in the massless approximation, etc. In order to avoid doublecounting
the processes below should then not be used simultaneously.
  

<h3>Hard QCD processes: heavy-flavour subset</h3>

These processes form a natural part of the above class, but can
also be generated separately. Formally the heavy-quark mass makes
these matrix elements finite in the <i>pT -> 0</i> limit, but at
high energies one may still question the validity of the expressions
at low <i>pT</i> values, like for the other hard-QCD processes.
Also as above, an eikonalized description, intended to be valid at all 
<i>pT</i>, is included as part of the multiple-interactions framework. 
<br/>Note that the processes below only represent the "tip of the iceberg"
of charm and bottom production at high energies, where flavour excitation
and shower branchings provide major additional sources. All these sources
come together in the descriptions offered by <code>SoftQCD:minBias</code>
and <code>HardQCD:all</code>.

<br/><br/><strong>HardQCD:gg2ccbar</strong>  <input type="radio" name="14" value="on"><strong>On</strong>
<input type="radio" name="14" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>g g -> c cbar</i>. 
Code 121.
  

<br/><br/><strong>HardQCD:qqbar2ccbar</strong>  <input type="radio" name="15" value="on"><strong>On</strong>
<input type="radio" name="15" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q qbar -> c cbar</i>. 
Code 122.
  

<br/><br/><strong>HardQCD:gg2bbbar</strong>  <input type="radio" name="16" value="on"><strong>On</strong>
<input type="radio" name="16" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>g g -> b bbar</i>. 
Code 123.
  

<br/><br/><strong>HardQCD:qqbar2bbbar</strong>  <input type="radio" name="17" value="on"><strong>On</strong>
<input type="radio" name="17" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q qbar -> b bbar</i>. 
Code 124.
  

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

if($_POST["1"] != "off")
{
$data = "SoftQCD:all = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "off")
{
$data = "SoftQCD:minBias = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "off")
{
$data = "SoftQCD:elastic = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "off")
{
$data = "SoftQCD:singleDiffractive = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "off")
{
$data = "SoftQCD:doubleDiffractive = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "off")
{
$data = "HardQCD:all = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "off")
{
$data = "HardQCD:gg2gg = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
if($_POST["8"] != "off")
{
$data = "HardQCD:gg2qqbar = ".$_POST["8"]."\n";
fwrite($handle,$data);
}
if($_POST["9"] != "off")
{
$data = "HardQCD:qg2qg = ".$_POST["9"]."\n";
fwrite($handle,$data);
}
if($_POST["10"] != "off")
{
$data = "HardQCD:qq2qq = ".$_POST["10"]."\n";
fwrite($handle,$data);
}
if($_POST["11"] != "off")
{
$data = "HardQCD:qqbar2gg = ".$_POST["11"]."\n";
fwrite($handle,$data);
}
if($_POST["12"] != "off")
{
$data = "HardQCD:qqbar2qqbarNew = ".$_POST["12"]."\n";
fwrite($handle,$data);
}
if($_POST["13"] != "3")
{
$data = "HardQCD:nQuarkNew = ".$_POST["13"]."\n";
fwrite($handle,$data);
}
if($_POST["14"] != "off")
{
$data = "HardQCD:gg2ccbar = ".$_POST["14"]."\n";
fwrite($handle,$data);
}
if($_POST["15"] != "off")
{
$data = "HardQCD:qqbar2ccbar = ".$_POST["15"]."\n";
fwrite($handle,$data);
}
if($_POST["16"] != "off")
{
$data = "HardQCD:gg2bbbar = ".$_POST["16"]."\n";
fwrite($handle,$data);
}
if($_POST["17"] != "off")
{
$data = "HardQCD:qqbar2bbbar = ".$_POST["17"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->

