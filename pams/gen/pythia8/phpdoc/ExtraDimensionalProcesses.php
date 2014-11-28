<html>
<head>
<title>Extra-Dimensional Processes</title>
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

<form method='post' action='ExtraDimensionalProcesses.php'>

<h2>Extra-Dimensional Processes</h2>

Scenarios with extra dimensions allow a multitude of processes.
Currently the only ones implemented involve the production of an 
excited graviton state <i>G^*</i> within a Randall-Sundrum scenario.
This state is assigned PDG code 5000039. Decays into fermion, gluon
and photon pairs are handled with the correct angular distributions,
while other decay channels currently are handled isotropically.

<h3>Production processes</h3>

There are two lowest-order processes that together normally should be 
sufficient for a simulation of <i>G^*</i> production. 

<br/><br/><strong>ExtraDimensionsG*:all</strong>  <input type="radio" name="1" value="on"><strong>On</strong>
<input type="radio" name="1" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Common switch for the group of lowest-order <i>G^*</i> production
processes, i.e. the two ones below.
  

<br/><br/><strong>ExtraDimensionsG*:gg2G*</strong>  <input type="radio" name="2" value="on"><strong>On</strong>
<input type="radio" name="2" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>g g -> G^*</i>. 
Code 5001.
  

<br/><br/><strong>ExtraDimensionsG*:ffbar2G*</strong>  <input type="radio" name="3" value="on"><strong>On</strong>
<input type="radio" name="3" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>f fbar -> G^*</i>. 
Code 5002.
  

<p/>
In addition there are three first-order processes included. These are of
less interest, but can be used for dedicated studies of the high-<i>pT</i> 
tail of <i>G^*</i> producton. As usual, it would be doublecounting to
include the lowest-order and first-order processes simultaneously.
Therefore the latter ones are not included with the 
<code>ExtraDimensionsG*:all = on</code> option. In this set of processes
all decay angles are assumed istropic.

<br/><br/><strong>ExtraDimensionsG*:gg2G*g</strong>  <input type="radio" name="4" value="on"><strong>On</strong>
<input type="radio" name="4" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>g g -> G^* g</i>. 
Code 5003.
  

<br/><br/><strong>ExtraDimensionsG*:qg2G*q</strong>  <input type="radio" name="5" value="on"><strong>On</strong>
<input type="radio" name="5" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q g -> G^* q</i>. 
Code 5004.
  

<br/><br/><strong>ExtraDimensionsG*:qqbar2G*g</strong>  <input type="radio" name="6" value="on"><strong>On</strong>
<input type="radio" name="6" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Scatterings <i>q qbar -> G^* g</i>. 
Code 5005.
  


<h3>Parameters</h3>

In the above scenario the main free parameter is the <i>G^*</i> mass,
which is set as usual. In addition there is one further parameter.

<br/><br/><table><tr><td><strong>ExtraDimensionsG*:kappaMG </td><td></td><td> <input type="text" name="7" value="0.054" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.054</strong></code>; <code>minimum = 0.0</code>)</td></tr></table>
dimensionless coupling, which enters quadratically in all partial
widths of the <i>G^*</i>. Is 
<i>kappa m_G* = sqrt(2) x_1 k / Mbar_Pl</i>,
where <i>x_1 = 3.83</i> is the first zero of the <i>J_1</i> Bessel 
function and <i>Mbar_Pl</i> is the modified Planck mass.
  

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
$data = "ExtraDimensionsG*:all = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "off")
{
$data = "ExtraDimensionsG*:gg2G* = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "off")
{
$data = "ExtraDimensionsG*:ffbar2G* = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "off")
{
$data = "ExtraDimensionsG*:gg2G*g = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "off")
{
$data = "ExtraDimensionsG*:qg2G*q = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "off")
{
$data = "ExtraDimensionsG*:qqbar2G*g = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "0.054")
{
$data = "ExtraDimensionsG*:kappaMG = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->

