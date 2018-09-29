<html>
<head>
<title>Dark Matter Processes</title>
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

<form method='post' action='DarkMatterProcesses.php'>
 
<h2>Dark Matter Processes</h2> 
 
This page contains the production of Dirac fermion Dark Matter via new 
<i>s</i>-channel mediators. An example how these processes can be run 
is found in <code>main75.cc</code>. 
 
<h3>Scalar Mediator </h3> 
 
<br/><br/><strong>DM:gg2S2XX</strong>  <input type="radio" name="1" value="on"><strong>On</strong>
<input type="radio" name="1" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scattering <i>g g &rarr; S &rarr; X Xbar</i>. 
Code 6001. S is assumed to be on-shell. 
   
 
<br/><br/><strong>DM:gg2S2XXj</strong>  <input type="radio" name="2" value="on"><strong>On</strong>
<input type="radio" name="2" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scattering <i>g g &rarr;S &rarr; X Xbar j</i>. 
Code 6002. S is assumed to be on-shell. (Not validated) 
   
 
<p> 
Fermion couplings to scalar S are assumed to be proportional to mass 
of the fermion and couplings are the factor multiplying SM Higgs 
coupling (i.e. sin(mixing) in case of portal models). 
</p> 
 
<br/><br/><table><tr><td><strong>Sdm:vf </td><td></td><td> <input type="text" name="3" value="0.1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.1</strong></code>)</td></tr></table>
Scalar coupling of SM fermions. 
   
<br/><br/><table><tr><td><strong>Sdm:af </td><td></td><td> <input type="text" name="4" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Pseudo-scalar coupling of SM fermions. 
   
<br/><br/><table><tr><td><strong>Sdm:vX </td><td></td><td> <input type="text" name="5" value="1.0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1.0</strong></code>)</td></tr></table>
Scalar coupling of DM fermion. 
   
<br/><br/><table><tr><td><strong>Sdm:aX </td><td></td><td> <input type="text" name="6" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Pseudo-scalar coupling of DM fermion. 
   
 
<h3>Vector Mediator <i>Z'</i></h3> 
 
The Vector mediator model assumes a simplified U(1) model with couplings 
to fermionic Dark Matter.  Both vector and axial couplings are possible. 
 
<br/><br/><strong>DM:ffbar2Zp2XX</strong>  <input type="radio" name="7" value="on"><strong>On</strong>
<input type="radio" name="7" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scattering <i>f fbar &rarr;Z'^0 &rarr; X Xbar</i>. 
Code 6001. <i>Z'</i> is assumed to be on-shell. 
   
 
<br/><br/><strong>DM:ffbar2ZpH</strong>  <input type="radio" name="8" value="on"><strong>On</strong>
<input type="radio" name="8" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scattering <i>f fbar &rarr;Z'^0 H </i>. 
Code 6004. <i>Z'</i> is assumed to be on-shell. The coupling of the 
<i>Z'</i> to the SM Higgs is given by the parameter <code>Zp:coupH</code>. 
Interference with <i>gamma/Z</i> currently not implemented, therefore 
this is only suitable when <i>Z' &rarr; X Xbar</i>.  This can be 
ensured using <br/> 
55:onMode = off <br/> 
55:onIfAny = 52 
   
 
<br/><br/><strong>DM:ffbar2Zp2XXj</strong>  <input type="radio" name="9" value="on"><strong>On</strong>
<input type="radio" name="9" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scattering <i>f fbar &rarr;Z'^0 &rarr; X Xbar</i>. 
Code 6002. <i>Z'</i> is assumed to be on-shell. (Not validated) 
   
 
<br/><br/><strong>DM:qg2Zp2XXj</strong>  <input type="radio" name="10" value="on"><strong>On</strong>
<input type="radio" name="10" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scattering <i>f fbar &rarr;Z'^0 &rarr; X Xbar</i>. 
Code 6003. <i>Z'</i> is assumed to be on-shell. (Not validated) 
   
 
<p> 
The couplings of the <i>Z'^0</i> to quarks and leptons are be 
assumed universal, i.e. generation-independent.  The choice of fixed 
axial and vector couplings implies a resonance width that increases 
linearly with the <i>Z'</i> mass. 
</p> 
 
<p> 
Here are the couplings: 
</p> 
 
<br/><br/><table><tr><td><strong>Zp:gZp </td><td></td><td> <input type="text" name="11" value="0.1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.1</strong></code>)</td></tr></table>
Gauge coupling of new U(1). 
   
 
<br/><br/><table><tr><td><strong>Zp:coupH </td><td></td><td> <input type="text" name="12" value="0.1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.1</strong></code>)</td></tr></table>
Coupling to SM higgs. 
   
 
 
<br/><br/><table><tr><td><strong>Zp:vu </td><td></td><td> <input type="text" name="13" value="1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>)</td></tr></table>
Vector coupling of up-type quarks. 
   
<br/><br/><table><tr><td><strong>Zp:au </td><td></td><td> <input type="text" name="14" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Axial coupling of up-type quarks. 
   
 
<br/><br/><table><tr><td><strong>Zp:vd </td><td></td><td> <input type="text" name="15" value="1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>)</td></tr></table>
Vector coupling of down-type quarks. 
   
<br/><br/><table><tr><td><strong>Zp:ad </td><td></td><td> <input type="text" name="16" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Axial coupling of down-type quarks. 
   
 
<br/><br/><table><tr><td><strong>Zp:vl </td><td></td><td> <input type="text" name="17" value="1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>)</td></tr></table>
Vector coupling of charged leptons. 
   
<br/><br/><table><tr><td><strong>Zp:al </td><td></td><td> <input type="text" name="18" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Axial coupling of charged leptons. 
   
 
<br/><br/><table><tr><td><strong>Zp:vv </td><td></td><td> <input type="text" name="19" value="1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>)</td></tr></table>
Vector coupling of neutrinos. 
   
<br/><br/><table><tr><td><strong>Zp:av </td><td></td><td> <input type="text" name="20" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Axial coupling of neutrinos. 
   
 
<br/><br/><table><tr><td><strong>Zp:vX </td><td></td><td> <input type="text" name="21" value="1" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>)</td></tr></table>
Vector coupling of DM fermion. 
   
<br/><br/><table><tr><td><strong>Zp:aX </td><td></td><td> <input type="text" name="22" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Axial coupling of DM fermion. 
   
 
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
$data = "DM:gg2S2XX = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "off")
{
$data = "DM:gg2S2XXj = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "0.1")
{
$data = "Sdm:vf = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "0")
{
$data = "Sdm:af = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "1.0")
{
$data = "Sdm:vX = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "0")
{
$data = "Sdm:aX = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "off")
{
$data = "DM:ffbar2Zp2XX = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
if($_POST["8"] != "off")
{
$data = "DM:ffbar2ZpH = ".$_POST["8"]."\n";
fwrite($handle,$data);
}
if($_POST["9"] != "off")
{
$data = "DM:ffbar2Zp2XXj = ".$_POST["9"]."\n";
fwrite($handle,$data);
}
if($_POST["10"] != "off")
{
$data = "DM:qg2Zp2XXj = ".$_POST["10"]."\n";
fwrite($handle,$data);
}
if($_POST["11"] != "0.1")
{
$data = "Zp:gZp = ".$_POST["11"]."\n";
fwrite($handle,$data);
}
if($_POST["12"] != "0.1")
{
$data = "Zp:coupH = ".$_POST["12"]."\n";
fwrite($handle,$data);
}
if($_POST["13"] != "1")
{
$data = "Zp:vu = ".$_POST["13"]."\n";
fwrite($handle,$data);
}
if($_POST["14"] != "0")
{
$data = "Zp:au = ".$_POST["14"]."\n";
fwrite($handle,$data);
}
if($_POST["15"] != "1")
{
$data = "Zp:vd = ".$_POST["15"]."\n";
fwrite($handle,$data);
}
if($_POST["16"] != "0")
{
$data = "Zp:ad = ".$_POST["16"]."\n";
fwrite($handle,$data);
}
if($_POST["17"] != "1")
{
$data = "Zp:vl = ".$_POST["17"]."\n";
fwrite($handle,$data);
}
if($_POST["18"] != "0")
{
$data = "Zp:al = ".$_POST["18"]."\n";
fwrite($handle,$data);
}
if($_POST["19"] != "1")
{
$data = "Zp:vv = ".$_POST["19"]."\n";
fwrite($handle,$data);
}
if($_POST["20"] != "0")
{
$data = "Zp:av = ".$_POST["20"]."\n";
fwrite($handle,$data);
}
if($_POST["21"] != "1")
{
$data = "Zp:vX = ".$_POST["21"]."\n";
fwrite($handle,$data);
}
if($_POST["22"] != "0")
{
$data = "Zp:aX = ".$_POST["22"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>
 
<!-- Copyright (C) 2018 Torbjorn Sjostrand --> 
