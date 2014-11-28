<html>
<head>
<title>Update History</title>
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

<form method='post' action='UpdateHistory.php'>

<h2>Update History</h2>

These update notes describe major updates relative to the baseline 
PYTHIA 8.100 version. However, they are less extensive than the 
corresponding update notes for PYTHIA 6. There are three main 
reasons for this:
<ul>
<li>The manual contained on these html/php pages is kept up to date.
  (However, the "Brief Introduction" may not always be.)
</li>
<li>8.1 is a quite new code, so there are many minor changes that, 
  if all were to be documented, would hide the key ones.
</li>
<li>8.1 is not yet used for "mission critical" applications, 
  so there is less need to trace changed behaviour.
</li>
</ul>

<h3>Main news by version</h3>

<ul>
<li>8.101: 10 November 2007
<ul>
<li>New option to initialize with arbitrary beam directions<br/>
<code>pythia.init( idA, idB, pxA, pyA, pzA, pxB, pyB, pzB)</code>
</li>
<li>The <code>LHAevnt</code> and <code>LHAinit</code> classes have been 
joined into a new <code>LHAup</code> one, with new options that allow 
the writing of a Les Houches Event File.
</li>
</ul>
</li>

<li>8.102: 6 December 2007
<ul>
<li>Limited capability to use two different <code>Pythia</code> instances 
for signal + pileup event generation, see <code>main19.cc</code> for an 
example.
</li> 
<li>Added capability to set <?php $filepath = $_GET["filepath"];
echo "<a href='BeamParameters.php?filepath=".$filepath."' target='page'>";?>beam energy spread 
and beam vertex</a>.
<br/>
<b>Warning:</b> as a consequence, some settings names have been changed, 
see below.
</li>
</ul>
</li>

<li>8.103: 22 January 2008
<ul>
<li>Updated HepMC conversion routine.
</li>
<li>In the <code>Event</code> class the <code>=</code> and 
<code>=+</code> methods have been overloaded to allow the copying 
or appending of event records. Illustrated in <code>main19.cc</code>.
</li>
</ul>
</li>

<li>8.104: 14 February 2008
<ul>
<li>Updated configure scripts.
</li>
<li>The <code>SusyLesHouches</code> class updated to handle 
SLHA version 2.
</li>
<li>The <code>forceHadronLevel()</code> method introduced for standalone 
hadronization.
</li>
<li><code>main15.cc</code> illustrated how either full hadronization or 
only decays of some particles can be looped over for the rest of the 
event retained.
</li>
<li>The html and php page formatting improved with 
cascading style sheets.
</li>
<li>The static <code>ErrorMsg</code> class has been removed and 
its functionality moved into the non-static <code>Info</code> class,
in the renamed Info file.
</li>
</ul>
</li>

<li>8.105: 24 February 2008
<ul>
<li>Further reduction of the use of static, with related code changes.
This should allow to have several almost independent <code>Pythia</code> 
instances. Some static classes still remain, however, notably for
random number generation and particle properties.
</li>
<li>Several minor improvements and new options.
</li>
</ul>
</li>

<li>8.106: 11 March 2008
<ul>
<li>Improved handling of the Higgs width, relevant for massive and thereby
broad resonance shapes. 
</li>
</ul>
</li>

<li>8.107: 17 March 2008
<ul>
<li>Correction in the event record, so that the beam particles in line 
1 and 2 do not have any mother according to the <code>motherList</code>
method. Previously the "system" entry in line 0 was counted as their 
mother, which gave rise to an unexpected extra vertex in the conversion 
to the HepMC format.
</li>
</ul>
</li>

<li>8.108: 1 May 2008
<ul>
<li>Support for HepMC version 1 is removed, to simplify the code and 
reflect the evolution of the field.
</li>
<li>Status codes are stored in HepMC only as 1 for existing and 2 for
decayed or fragmented particles (whereas previously the original PYTHIA
codes were used for the latter).
</li>
<li>Parton densities are stored in HepMC as <i>xf(x,Q^2)</i> 
rather than the <i>f(x,Q^2)</i> used in (some) previous versions.
</li>
<li>The SusyLesHouches class has ben updated so that reading is fully
compatible with the SLHA2 standard. 
</li>
<li>The matrix elements for neutralino pair production have now been
completed and checked.
</li>
<li>A new compilation option <code>-Wshadow</code> is introduced and 
code is rewritten at all places where this option gave warnings.
</li>
<li>Minor library correction to allow compilation with gcc 4.3.0.</li>
<li>Ensure that <i>alpha_strong</i> does not blow up, by introducing 
a minimal scale somewhat above <i>Lambda_3</i> (roughly where
<i>alpha_strong = 10</i>). 
</li>
<li>New methods <code>isValence1()</code> and <code>isValence2()</code> 
in the <code>Info</code> class.  
</li>
</ul>
</li>

</ul>
 
<h3>Changes among settings names</h3>

New capabilities are still being added, meaning new settings names.
It then may become preferable to rename existing settings to form
new logical groups. Here is a list of thise changes that have been 
made since be 8.100 baseline version. 
<ul>
<li>A '*' is used as wildcard.
</li>
<li>Names within brackets denotes also new/changed functionality.
</li>
</ul>

<table cellspacing="5">
<tr> <td>8.100 setting  </td>       <td>has been moved to </td> </tr>
<tr> <td>Beams:*        </td>       <td>BeamRemnants:*    </td> </tr>
<tr> <td>Main:idA       </td>       <td>Beams:idA         </td> </tr>
<tr> <td>Main:idB       </td>       <td>Beams:idB         </td> </tr>
<tr> <td>Main:inCMframe </td>       <td>(Beams:frameType) </td> </tr>
<tr> <td>Main:eCM       </td>       <td>Beams:eCM         </td> </tr>
<tr> <td>Main:eA        </td>       <td>Beams:eA          </td> </tr>
<tr> <td>Main:eB        </td>       <td>Beams:eB          </td> </tr>
<tr> <td>Main:LHEF      </td>       <td>Beams:LHEF        </td> </tr>
</table>



</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
