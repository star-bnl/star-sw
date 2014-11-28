<html>
<head>
<title>Main-Program Settings</title>
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

<form method='post' action='MainProgramSettings.php'>

<h2>Main-Program Settings</h2>

<h3>Introduction</h3>

The main program is up to the user to write. However, 
<?php $filepath = $_GET["filepath"];
echo "<a href='SampleMainPrograms.php?filepath=".$filepath."' target='page'>";?>sample main programs</a> 
are provided. In one such class of programs, key settings of the run 
are read in from a "cards file". These commands may be of two types<br/>
(a) instructions directly to <code>Pythia</code>, like which 
processes to generate, and<br/>
(b) instructions to the main program for what it should do, 
like how many events to generate, i.e. how many times 
<code>pythia.next()</code> should be called.<br/>
In principle these two kinds could be kept completely separate. 
However, to make life simpler, a number of useful main-program 
settings are defined on this page, so that they are recognized by 
the <code>Settings</code> machinery. They can thus be put among 
the other cards without distinction. It is up to you to decide which 
ones, if any, you actually want to use when you write your main program.
For convenience, some in the second section below can also be interpreted 
directly by <code>Pythia</code>, while the subsequent ones really have 
to be used in your main program. 

<p/>
Once you have used the <code>pythia.readFile(fileName)</code> method to
read in the cards file, you can interrogate the <code>Settings</code>
database to make the values available in your main program. A slight
complication is that you need to use a different  <code>Settings</code>
method for each of the four possible return types that you want to 
extract. To save some typing the same method names are found directly 
in the <code>Pythia</code> class, and just send on to the
<code>Settings</code> ones to do the job, e.g.
<pre>
  bool   showCS = pythia.flag("Main:showChangedSettings");
  int    nEvent = pythia.mode("Main:numberOfEvents");
  double spare1 = pythia.parm("Main:spareParm1");
  string file   = pythia.word("Main:allSettingsFile"); 
</pre>

<h3>Run settings</h3>

Here settings related to how many events to generate and whether
to print some information on data used in run. These variables 
can be set in an input "cards" file, and thereafter read out an used 
in the user-written main program. Usage is purely optional, but may help
you reduce the need to recompile your main program. 

<br/><br/><table><tr><td><strong>Main:numberOfEvents  </td><td></td><td> <input type="text" name="1" value="1000" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1000</strong></code>; <code>minimum = 0</code>)</td></tr></table>
The number of events to be generated.
  

<br/><br/><table><tr><td><strong>Main:numberToList  </td><td></td><td> <input type="text" name="2" value="2" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>2</strong></code>; <code>minimum = 0</code>)</td></tr></table>
The number of events to list.
  

<br/><br/><table><tr><td><strong>Main:timesToShow  </td><td></td><td> <input type="text" name="3" value="50" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>50</strong></code>; <code>minimum = 0</code>)</td></tr></table>
Print the number of events generated so far, this many times, 
i.e. once every <code>numberOfEvents/numberToShow</code> events.
  

<br/><br/><table><tr><td><strong>Main:timesAllowErrors  </td><td></td><td> <input type="text" name="4" value="10" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>10</strong></code>)</td></tr></table>
Allow this many times that <code>pythia.next()</code> returns false, 
i.e. that an event is flawed, before aborting the run.
  

<br/><br/><strong>Main:showChangedSettings</strong>  <input type="radio" name="5" value="on" checked="checked"><strong>On</strong>
<input type="radio" name="5" value="off"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>on</strong></code>)<br/>
Print a list of the changed flag/mode/parameter/word settings.
  

<br/><br/><strong>Main:showAllSettings</strong>  <input type="radio" name="6" value="on"><strong>On</strong>
<input type="radio" name="6" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Print a list of all flag/mode/parameter/word settings.
Warning: this will be a long list.
  

<br/><br/><table><tr><td><strong>Main:showOneParticleData  </td><td></td><td> <input type="text" name="7" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>; <code>minimum = 0</code>)</td></tr></table>
Print particle and decay data for the particle with this particular 
identity code. Default means that no particle is printed.
  

<br/><br/><strong>Main:showChangedParticleData</strong>  <input type="radio" name="8" value="on"><strong>On</strong>
<input type="radio" name="8" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Print a list of particle and decay data for those particles 
that were changed (one way or another).
  

<br/><br/><strong>Main:showChangedResonanceData</strong>  <input type="radio" name="9" value="on"><strong>On</strong>
<input type="radio" name="9" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
In the previous listing also include the resonances that are 
initialized at the beginning of a run and thus get new particle
data, even if these may well agree with the default ones. 
Warning: this will be a rather long list.
  

<br/><br/><strong>Main:showAllParticleData</strong>  <input type="radio" name="10" value="on"><strong>On</strong>
<input type="radio" name="10" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Print a list of all particle and decay data.
Warning: this will be a long list.
  

<br/><br/><strong>Main:writeChangedSettings</strong>  <input type="radio" name="11" value="on"><strong>On</strong>
<input type="radio" name="11" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Write a file with the changed flag/mode/parameter/word settings, in
a format appropriate to be read in at the beginning of a new  
run, using the <code>pythia.readFile(fileName)</code> method. 
  

<br/><br/><table><tr><td><strong>Main:changedSettingsFile  </td><td></td><td> <input type="text" name="12" value="currentSettings.cmnd" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>currentSettings.cmnd</strong></code>)</td></tr></table>
The name of the file to which the changed flag/mode/parameter/word
settings are written if <code>Main:writeChangedSettings</code>
is on. 
  

<br/><br/><strong>Main:writeAllSettings</strong>  <input type="radio" name="13" value="on"><strong>On</strong>
<input type="radio" name="13" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Write a file with all flag/mode/parameter/word settings, in
a format appropriate to be read in at the beginning of a new  
run, using the <code>pythia.readFile(fileName)</code> method. 
  

<br/><br/><table><tr><td><strong>Main:allSettingsFile  </td><td></td><td> <input type="text" name="14" value="allSettings.cmnd" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>allSettings.cmnd</strong></code>)</td></tr></table>
The name of the file to which a flag/mode/parameter/word 
settings are written if <code>Main:writeAllSettings</code>
is on. 
  

<br/><br/><strong>Main:showAllStatistics</strong>  <input type="radio" name="15" value="on"><strong>On</strong>
<input type="radio" name="15" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Print all available statistics or only the minimal set at the end 
of the run.
  

<h3>Subruns</h3>

You can use <?php $filepath = $_GET["filepath"];
echo "<a href='ProgramFlow.php?filepath=".$filepath."' target='page'>";?>subruns</a> to carry out
several tasks in the same run. In that case you will need repeated
instances of the first setting below in your command file, and could
additionally use the second and third as well.

<br/><br/><table><tr><td><strong>Main:subrun  </td><td></td><td> <input type="text" name="16" value="-999" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>-999</strong></code>; <code>minimum = 0</code>)</td></tr></table>
The number of the current subrun, a non-negative integer, put as
first line in a section of lines to be read for this particular subrun.
  

<br/><br/><strong>Main:LHEFskipInit</strong>  <input type="radio" name="17" value="on"><strong>On</strong>
<input type="radio" name="17" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If you read several Les Houches Event Files that you want to see 
considered as one single combined event sample you can set this flag
<code>on</code> after the first subrun to skip (most of) the  
(re-)initialization step.
  

<br/><br/><table><tr><td><strong>Main:numberOfSubruns  </td><td></td><td> <input type="text" name="18" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
The number of subruns you intend to use in your current run.  
Unlike the two settings above, <code>Pythia</code> itself will not
intepret this number, but you could e.g. have a loop in your main
program to loop over subruns from 0 through 
<code>numberOfSubruns - 1</code>. 
  

<h3>Spares</h3>

For currently unforeseen purposes, a few dummy settings are made 
available here. The user can set the desired value in a "cards file"
and then use that value in the main program as desired.

<br/><br/><strong>Main:spareFlag1</strong>  <input type="radio" name="19" value="on"><strong>On</strong>
<input type="radio" name="19" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
  

<br/><br/><strong>Main:spareFlag2</strong>  <input type="radio" name="20" value="on"><strong>On</strong>
<input type="radio" name="20" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
  

<br/><br/><strong>Main:spareFlag3</strong>  <input type="radio" name="21" value="on"><strong>On</strong>
<input type="radio" name="21" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
  

<br/><br/><table><tr><td><strong>Main:spareMode1  </td><td></td><td> <input type="text" name="22" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareMode2  </td><td></td><td> <input type="text" name="23" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareMode3  </td><td></td><td> <input type="text" name="24" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareParm1 </td><td></td><td> <input type="text" name="25" value="0." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareParm2 </td><td></td><td> <input type="text" name="26" value="0." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareParm3 </td><td></td><td> <input type="text" name="27" value="0." size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareWord1  </td><td></td><td> <input type="text" name="28" value="void" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>void</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareWord2  </td><td></td><td> <input type="text" name="29" value="void" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>void</strong></code>)</td></tr></table>
  

<br/><br/><table><tr><td><strong>Main:spareWord3  </td><td></td><td> <input type="text" name="30" value="void" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>void</strong></code>)</td></tr></table>
  

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

if($_POST["1"] != "1000")
{
$data = "Main:numberOfEvents = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "2")
{
$data = "Main:numberToList = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "50")
{
$data = "Main:timesToShow = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "10")
{
$data = "Main:timesAllowErrors = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "on")
{
$data = "Main:showChangedSettings = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "off")
{
$data = "Main:showAllSettings = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "0")
{
$data = "Main:showOneParticleData = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
if($_POST["8"] != "off")
{
$data = "Main:showChangedParticleData = ".$_POST["8"]."\n";
fwrite($handle,$data);
}
if($_POST["9"] != "off")
{
$data = "Main:showChangedResonanceData = ".$_POST["9"]."\n";
fwrite($handle,$data);
}
if($_POST["10"] != "off")
{
$data = "Main:showAllParticleData = ".$_POST["10"]."\n";
fwrite($handle,$data);
}
if($_POST["11"] != "off")
{
$data = "Main:writeChangedSettings = ".$_POST["11"]."\n";
fwrite($handle,$data);
}
if($_POST["12"] != "currentSettings.cmnd")
{
$data = "Main:changedSettingsFile = ".$_POST["12"]."\n";
fwrite($handle,$data);
}
if($_POST["13"] != "off")
{
$data = "Main:writeAllSettings = ".$_POST["13"]."\n";
fwrite($handle,$data);
}
if($_POST["14"] != "allSettings.cmnd")
{
$data = "Main:allSettingsFile = ".$_POST["14"]."\n";
fwrite($handle,$data);
}
if($_POST["15"] != "off")
{
$data = "Main:showAllStatistics = ".$_POST["15"]."\n";
fwrite($handle,$data);
}
if($_POST["16"] != "-999")
{
$data = "Main:subrun = ".$_POST["16"]."\n";
fwrite($handle,$data);
}
if($_POST["17"] != "off")
{
$data = "Main:LHEFskipInit = ".$_POST["17"]."\n";
fwrite($handle,$data);
}
if($_POST["18"] != "0")
{
$data = "Main:numberOfSubruns = ".$_POST["18"]."\n";
fwrite($handle,$data);
}
if($_POST["19"] != "off")
{
$data = "Main:spareFlag1 = ".$_POST["19"]."\n";
fwrite($handle,$data);
}
if($_POST["20"] != "off")
{
$data = "Main:spareFlag2 = ".$_POST["20"]."\n";
fwrite($handle,$data);
}
if($_POST["21"] != "off")
{
$data = "Main:spareFlag3 = ".$_POST["21"]."\n";
fwrite($handle,$data);
}
if($_POST["22"] != "0")
{
$data = "Main:spareMode1 = ".$_POST["22"]."\n";
fwrite($handle,$data);
}
if($_POST["23"] != "0")
{
$data = "Main:spareMode2 = ".$_POST["23"]."\n";
fwrite($handle,$data);
}
if($_POST["24"] != "0")
{
$data = "Main:spareMode3 = ".$_POST["24"]."\n";
fwrite($handle,$data);
}
if($_POST["25"] != "0.")
{
$data = "Main:spareParm1 = ".$_POST["25"]."\n";
fwrite($handle,$data);
}
if($_POST["26"] != "0.")
{
$data = "Main:spareParm2 = ".$_POST["26"]."\n";
fwrite($handle,$data);
}
if($_POST["27"] != "0.")
{
$data = "Main:spareParm3 = ".$_POST["27"]."\n";
fwrite($handle,$data);
}
if($_POST["28"] != "void")
{
$data = "Main:spareWord1 = ".$_POST["28"]."\n";
fwrite($handle,$data);
}
if($_POST["29"] != "void")
{
$data = "Main:spareWord2 = ".$_POST["29"]."\n";
fwrite($handle,$data);
}
if($_POST["30"] != "void")
{
$data = "Main:spareWord3 = ".$_POST["30"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
