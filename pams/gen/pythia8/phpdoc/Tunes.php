<html>
<head>
<title>Tunes</title>
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

<form method='post' action='Tunes.php'>

<h2>Tunes</h2>

Since some physics aspects cannot be derived from first principles,
this program contains many parameters that represent a true 
uncertainty in our understanding of nature. Particularly afflicted
are the areas of hadronization and multiple interactions, which both
involve nonperturbative QCD physics. 

<p/>
Technically, PYTHIA  parameters can be varied independently of each 
other, but the physical requirement of a sensible description of a set
of data leads to correlations and anticorrelations between the 
parameters. Hence the need to produce tunes, not of one parameter at  
a time, but simultaneously for a group of them. A well-known such 
example is parton densities, where combined tunes to a wide range of data
have been produced, that can then be obtained prepackaged.  

<p/>
Given the many PYTHIA parameters to be tuned, it is convenient to 
divide the task into subtasks. Firstly, if we assume jet universality,
hadronization and final-state parton showers should be tuned to 
<i>e^+e^-</i> annihilation data, notably from LEP1, since this 
offers the cleanest environment. Secondly, with such parameters fixed, 
hadron collider data should be studied to pin down multiple interactions
and other further aspects, such as initial-state radiation. (Thirdly 
would come anything else, such as physics with photon beams, which 
involve further parameters, but that is beyond the current scope.)

<p/>
Sadly PYTHIA 8 did not yet take many steps along this long road. 
While the default values in PYTHIA 8 have been chosen "sensibly", 
so far there has not been a complete, consistent tuning of the program. 
For hadronization we can partly benefit from the LEP experience with
PYTHIA 6, since the basic hadronization scheme did not change.  
However, there never was a combined LEP tune, since each of the four 
LEP collaborations produced their own tunes to their own data. The 
situation is worse for multiple interactions, where PYTHIA 8 is more 
different from PYTHIA 6. Nevertheless, the PYTHIA 6 tunes to CDF data, 
performed by R.D. Field, have been used as a rough guide in picking 
reasonable default values.

<p/>
In the future we hope to see PYTHIA 8 tunes appear. Like with parton 
distributions, there is likely to be several tunes, because different 
sets of data will pull in different directions, by imperfections   
in the model or in the data, and by differences in the chosen
tuning strategies. We therefore propose to collect some of these tunes
here, in a prepackaged form. Of course, in all cases it is a matter
of setting values for parameters already defined elsewhere, so the
tunes offer no new functionality, only a more convenient setup. 

<p/>
If you set either the <code>Tune:ee</code> and <code>Tune:pp</code> 
modes below non-zero then all parameters used in the respective tune 
will be set accordingly when <code>pythia.init(...)</code> is called. 
You can check this by calling <code>pythia.settings.listChanged()</code> 
before and after initialization; before only the tune modes are 
nondefault, afterwards all the non-default-valued parameters in the 
tune appear. Therefore, for better or worse, you cannot combine a tune 
option with your own choices for some of the parameters used in the tune, 
since the values you set before <code>pythia.init(...)</code> would be 
overwritten at that point. 

<br/><br/><table><tr><td><strong>Tune:ee  </td><td>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Choice of tune to <ei>e^+e^-</ei> data, mainly for the hadronization
and timelike-showering aspects of PYTHIA. 
<br/>
<input type="radio" name="1" value="0" checked="checked"><strong>0 </strong>: no values are overwritten at initialization,  so you can set the individual parameters as you wish. <br/>
<input type="radio" name="1" value="101"><strong>101 </strong>: a tune by Marc Montull to the LEP 1 particle composition, as published in the RPP. The default PYTHIA parameter  values tend to produce more vector mesons than observed in data.  When more pseudoscalars are produced directly this also leads to a  reduced <ei>s/u</ei> ratio and diquark fraction. Also other parameters  related to the particle composition obtain changed values. No related  (re)tune to event shapes has been performed so far, however.   <br/>

<br/><br/><table><tr><td><strong>Tune:pp  </td><td>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>)</td></tr></table>
Choice of tune to <ei>pp / ppbar</ei> data, mainly for the 
multiple-interactions and initial-state-radiation aspects of PYTHIA. 
<note>Note:</note> Currently this is only a placeholder, since no
tunes alternative to the default values exist. 
<br/>
<input type="radio" name="2" value="0" checked="checked"><strong>0 </strong>: no values are overwritten at initialization,  so you can set the individual parameters as you wish. <br/>



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

if($_POST["1"] != "0")
{
$data = "Tune:ee = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "0")
{
$data = "Tune:pp = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
