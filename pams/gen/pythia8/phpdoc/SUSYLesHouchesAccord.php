<html>
<head>
<title>SUSY Les Houches Accord</title>
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

<form method='post' action='SUSYLesHouchesAccord.php'>

<h2>SUSY Les Houches Accord</h2>

The PYTHIA 8 program does not contain an internal spectrum calculator
(a.k.a. RGE package) to provide supersymmetric couplings, mixing angles,
masses and branching ratios. Thus the SUSY Les Houches Accord (SLHA)
[<a href="Bibliography.php" target="page">Ska04</a>] is the only way of inputting SUSY models, and
SUSY processes cannot be run unless such an input has taken place. 

<br/><br/><strong>SUSY</strong>  <input type="radio" name="1" value="on"><strong>On</strong>
<input type="radio" name="1" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Global switch for SUSY on or off. When on, the initialization step
(<code>pythia.init</code>) will read in the file below and set up
the information required for simulation of supersymmetric processes.   
  

<br/><br/><table><tr><td><strong>SUSY:SusyLesHouchesFile  </td><td></td><td> <input type="text" name="2" value="softsusy.spc" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>softsusy.spc</strong></code>)</td></tr></table>
Name of a SUSY Les Houches Accord (SLHA) spectrum file containing the
SUSY model definition, masses, and other parameters pertaining to 
the desired SUSY model. Note that SLHA files can still be used
even if no supersymmetric processes are switched on, as an 
alternative way of inputting particle masses, decay tables, etc. 
  

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
$data = "SUSY = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "softsusy.spc")
{
$data = "SUSY:SusyLesHouchesFile = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->


