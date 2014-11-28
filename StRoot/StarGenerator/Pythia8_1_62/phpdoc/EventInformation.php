<html>
<head>
<title>Event Information</title>
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

<form method='post' action='EventInformation.php'>

<h2>Event Information</h2>

The <code>Info</code> class collects various one-of-a-kind information, 
some relevant for all events and others for the current event. 
An object <code>info</code> is a public member of the <code>Pythia</code>
class, so if you e.g. have declared <code>Pythia pythia</code>, the
<code>Info</code> methods can be accessed by 
<code>pythia.info.method()</code>. Most of this is information that 
could also be obtained e.g. from the event record, but is here more
directly available. It is primarily intended for processes generated 
internally in PYTHIA, but many of the methods would work also for
events fed in via the Les Houches Accord.

<h3>List information</h3>

<a name="method1"></a>
<p/><strong>void Info::list() &nbsp;</strong> <br/>
a listing of most of the information set for the current event. 
  

<h3>The beams</h3>

<a name="method2"></a>
<p/><strong>int Info::idA() &nbsp;</strong> <br/>
  
<strong>int Info::idB() &nbsp;</strong> <br/>
the identities of the two beam particles. 
  

<a name="method3"></a>
<p/><strong>double Info::pzA() &nbsp;</strong> <br/>
  
<strong>double Info::pzB() &nbsp;</strong> <br/>
the longitudinal momenta of the two beam particles.
  

<a name="method4"></a>
<p/><strong>double Info::eA() &nbsp;</strong> <br/>
  
<strong>double Info::eB() &nbsp;</strong> <br/>
the energies of the two beam particles.
  

<a name="method5"></a>
<p/><strong>double Info::mA() &nbsp;</strong> <br/>
  
<strong>double Info::mB() &nbsp;</strong> <br/>
the masses of the two beam particles.
  

<a name="method6"></a>
<p/><strong>double Info::eCM() &nbsp;</strong> <br/>
  
<strong>double Info::s() &nbsp;</strong> <br/>
the CM energy and its square for the two beams. 
  

<h3>Initialization</h3>

<a name="method7"></a>
<p/><strong>bool Info::tooLowPTmin() &nbsp;</strong> <br/>
normally false, but true if the proposed <i>pTmin</i> scale was too low 
in timelike or spacelike showers, or in multiparton interactions. In the 
former case the <i>pTmin</i> is raised to some minimal value, in the 
latter the initialization fails (it is impossible to obtain a minijet 
cross section bigger than the nondiffractive one by reducing 
<i>pTmin</i>).
  

<h3>The event type</h3>

<a name="method8"></a>
<p/><strong>string Info::name() &nbsp;</strong> <br/>
  
<strong>int Info::code() &nbsp;</strong> <br/>
the name and code of the process that occured.
  

<a name="method9"></a>
<p/><strong>int Info::nFinal() &nbsp;</strong> <br/>
the number of final-state partons in the hard process.
  

<a name="method10"></a>
<p/><strong>bool Info::isResolved() &nbsp;</strong> <br/>
are beam particles resolved, i.e. were PDF's used for the process?
  

<a name="method11"></a>
<p/><strong>bool Info::isDiffractiveA() &nbsp;</strong> <br/>
  
<strong>bool Info::isDiffractiveB() &nbsp;</strong> <br/>
is either beam diffractively excited?
  

<a name="method12"></a>
<p/><strong>bool Info::isMinBias() &nbsp;</strong> <br/>
is the process a minimum-bias one?
  

<a name="method13"></a>
<p/><strong>bool Info::isLHA() &nbsp;</strong> <br/>
has the process been generated from external Les Houches Accord 
information?
  

<a name="method14"></a>
<p/><strong>bool Info::atEndOfFile() &nbsp;</strong> <br/>
true if a linked Les Houches class refuses to return any further 
events, presumably because it has reached the end of the file from 
which events have been read in.
  

<a name="method15"></a>
<p/><strong>bool Info::hasSub() &nbsp;</strong> <br/>
does the process have a subprocess classification?
Currently only true for minbias and Les Houches events, where it allows 
the hardest collision to be identified. 
  

<a name="method16"></a>
<p/><strong>string Info::nameSub() &nbsp;</strong> <br/>
  
<strong>int Info::codeSub() &nbsp;</strong> <br/>
  
<strong>int Info::nFinalSub() &nbsp;</strong> <br/>
the name, code and number of final-state partons in the subprocess
that occured when <code>hasSub()</code> is true. For a minimum-bias event 
the <code>code</code> would always be 101, while <code>codeSub()</code> 
would vary depending on the actual hardest interaction, e.g. 111 for 
<i>g g -> g g</i>. For a Les Houches event the <code>code</code> would 
always be 9999, while <code>codeSub()</code> would be the external 
user-defined classification code. The methods below would also provide 
information for such particular subcollisions.  
  

<h3>Hard process parton densities and scales</h3>

<a name="method17"></a>
<p/><strong>int Info::id1() &nbsp;</strong> <br/>
  
<strong>int Info::id2() &nbsp;</strong> <br/>
the identities of the two partons coming in to the hard process.
  

<a name="method18"></a>
<p/><strong>double Info::x1() &nbsp;</strong> <br/>
  
<strong>double Info::x2() &nbsp;</strong> <br/>
<i>x</i> fractions of the two partons coming in to the hard process.
  

<a name="method19"></a>
<p/><strong>double Info::y() &nbsp;</strong> <br/>
  
<strong>double Info::tau() &nbsp;</strong> <br/>
rapidity and scaled mass-squared of the hard-process subsystem, as 
defined by the above <i>x</i> values. 
  

<a name="method20"></a>
<p/><strong>double Info::pdf1() &nbsp;</strong> <br/>
  
<strong>double Info::pdf2() &nbsp;</strong> <br/>
parton densities <i>x*f(x,Q^2</i> )evaluated for the two incoming 
partons; could be used e.g. for reweighting purposes. 
  

<a name="method21"></a>
<p/><strong>double Info::QFac() &nbsp;</strong> <br/>
  
<strong>double Info::Q2Fac() &nbsp;</strong> <br/>
the <i>Q</i> or <i>Q^2</i> factorization scale at which the 
densities were evaluated.
  

<a name="method22"></a>
<p/><strong>bool Info::isValence1() &nbsp;</strong> <br/>
  
<strong>bool Info::isValence2() &nbsp;</strong> <br/>
<code>true</code> if the two hard incoming partons have been picked 
to belong to the valence piece of the parton-density distribution, 
else <code>false</code>. Should be interpreted with caution.
Information is not set if you switch off parton-level processing. 
  

<a name="method23"></a>
<p/><strong>double Info::alphaS() &nbsp;</strong> <br/>
  
<strong>double Info::alphaEM() &nbsp;</strong> <br/>
the <i>alpha_strong</i> and <i>alpha_electromagnetic</i> values used 
for the hard process.
  

<a name="method24"></a>
<p/><strong>double Info::QRen() &nbsp;</strong> <br/>
  
<strong>double Info::Q2Ren() &nbsp;</strong> <br/>
the <i>Q</i> or <i>Q^2</i> renormalization scale at which 
<i>alpha_strong</i> and <i>alpha_electromagnetic</i> were evaluated.
  

<h3>Hard process kinematics</h3>

<a name="method25"></a>
<p/><strong>double Info::mHat() &nbsp;</strong> <br/>
  
<strong>double Info::sHat() &nbsp;</strong> <br/>
the invariant mass and its square for the hard process.
  

<a name="method26"></a>
<p/><strong>double Info::tHat() &nbsp;</strong> <br/>
  
<strong>double Info::uHat() &nbsp;</strong> <br/>
the remaining two Mandelstam variables; only defined for <i>2 -> 2</i>
processes. 
  

<a name="method27"></a>
<p/><strong>double Info::pTHat() &nbsp;</strong> <br/>
  
<strong>double Info::pT2Hat() &nbsp;</strong> <br/>
transverse momentum and its square in the rest frame of a <i>2 -> 2</i>
processes. 
  

<a name="method28"></a>
<p/><strong>double Info::m3Hat() &nbsp;</strong> <br/>
  
<strong>double Info::m4Hat() &nbsp;</strong> <br/>
the masses of the two outgoing particles in a <i>2 -> 2</i> processes. 
  

<a name="method29"></a>
<p/><strong>double Info::thetaHat() &nbsp;</strong> <br/>
  
<strong>double Info::phiHat() &nbsp;</strong> <br/>
the polar and azimuthal scattering angles in the rest frame of 
a <i>2 -> 2</i> process.
  

<h3>Event weight and activity</h3>

<a name="method30"></a>
<p/><strong>double Info::weight() &nbsp;</strong> <br/>
weight assigned to the current event. Is normally 1 and thus 
uninteresting. However, there are several cases where one may have
nontrivial event weights. These weights must the be used e.g. when 
filling histograms. 
<br/>(i) In the <code><?php $filepath = $_GET["filepath"];
echo "<a href='PhaseSpaceCuts.php?filepath=".$filepath."' target='page'>";?>
PhaseSpace:increaseMaximum = off</a></code> default strategy,
an event with a differential cross-section above the assumed one 
(in a given phase-space point) is assigned a weight correspondingly
above unity. This should happen only very rarely, if at all, and so
could normally be disregarded. 
<br/>(ii) The <?php $filepath = $_GET["filepath"];
echo "<a href='UserHooks.php?filepath=".$filepath."' target='page'>";?>User Hooks</a> class offers 
the possibility to bias the selection of phase space points, which 
means that events come with a compensating weight, stored here. 
<br/>(iii) For Les Houches events some strategies allow negative weights, 
which then after unweighting lead to events with weight -1. There are 
also Les Houches strategies where no unweighting is done, so events 
come with a weight. Specifically, for strategies <i>+4</i> and
<i>-4</i>, the event weight is in units of pb. (Internally in mb, 
but converted at output.) 
  

<a name="method31"></a>
<p/><strong>double Info::weightSum() &nbsp;</strong> <br/>
Sum of weights accumulated during the run. For unweighted events this
agrees with the number of generated events. In order to obtain 
histograms normalized "per event", at the end of a run, histogram
contents should be divided by this weight. (And additionally 
divided by the bin width.) Normalization to cross section also
required multiplication by <code>sigmaGen()</code> below.
  

<a name="method32"></a>
<p/><strong>int Info::lhaStrategy() &nbsp;</strong> <br/>
normally 0, but if Les Houches events are input then it gives the 
event weighting strategy, see 
<?php $filepath = $_GET["filepath"];
echo "<a href='LesHouchesAccord.php?filepath=".$filepath."' target='page'>";?>Les Houches Accord</a>.
  

<a name="method33"></a>
<p/><strong>int Info::nISR() &nbsp;</strong> <br/>
  
<strong>int Info::nFSRinProc() &nbsp;</strong> <br/>
  
<strong>int Info::nFSRinRes() &nbsp;</strong> <br/>
the number of emissions in the initial-state showering, in the final-state
showering excluding resonance decys, and in the final-state showering
inside resonance decays, respectively.
  

<a name="method34"></a>
<p/><strong>double Info::pTmaxMPI() &nbsp;</strong> <br/>
  
<strong>double Info::pTmaxISR() &nbsp;</strong> <br/>
  
<strong>double Info::pTmaxFSR() &nbsp;</strong> <br/>
Maximum <i>pT</i> scales set for MPI, ISR and FSR, given the 
process type and scale choice for the hard interactions. The actual
evolution will run down from these scales.
  

<a name="method35"></a>
<p/><strong>double Info::pTnow() &nbsp;</strong> <br/>
The current <i>pT</i> scale in the combined MPI, ISR and FSR evolution.
Useful for classification in <?php $filepath = $_GET["filepath"];
echo "<a href='UserHooks.php?filepath=".$filepath."' target='page'>";?>user hooks</a>,
but not once the event has been evolved.  
  

<a name="method36"></a>
<p/><strong>double Info::mergingWeight() &nbsp;</strong> <br/>
combined CKKW-L weight assigned to the current event. If CKKW-L merging is 
performed, all histograms should be filled with this weight, as discussed in 
 <a href="MatrixElementMerging.html" target="page"> Matrix Element
Merging</a>.
  

<h3>Multiparton interactions</h3>

<a name="method37"></a>
<p/><strong>double Info::a0MPI() &nbsp;</strong> <br/>
The value of a0 when an x-dependent matter profile is used,
<code>MultipartonInteractions:bProfile = 4</code>.
  

<a name="method38"></a>
<p/><strong>double Info::bMPI() &nbsp;</strong> <br/>
The impact parameter <i>b</i> assumed for the current collision when
multiparton interactions are simulated. Is not expressed in any physical
size (like fm), but only rescaled so that the average should be unity 
for minimum-bias events (meaning less than that for events with hard
processes). 
  

<a name="method39"></a>
<p/><strong>double Info::enhanceMPI() &nbsp;</strong> <br/>
The choice of impact parameter implies an enhancement or depletion of
the rate of subsequent interactions, as given by this number. Again
the average is normalized be unity for minimum-bias events (meaning 
more than that for events with hard processes).  
  

<a name="method40"></a>
<p/><strong>int Info::nMPI() &nbsp;</strong> <br/>
The number of hard interactions in the current event. Is 0 for elastic
and diffractive events, and else at least 1, with more possible from
multiparton interactions.
  

<a name="method41"></a>
<p/><strong>int Info::codeMPI(int i) &nbsp;</strong> <br/>
  
<strong>double Info::pTMPI(int i) &nbsp;</strong> <br/>
the process code and transverse momentum of the <code>i</code>'th 
subprocess, with <code>i</code> in the range from 0 to
<code>nMPI() - 1</code>. The values for subprocess 0 is redundant with
information already provided above.  
  

<a name="method42"></a>
<p/><strong>int Info::iAMPI(i) &nbsp;</strong> <br/>
  
<strong>int Info::iBMPI(i) &nbsp;</strong> <br/>
are normally zero. However, if the <code>i</code>'th subprocess is
a rescattering, i.e. either or both incoming partons come from the 
outgoing state of previous scatterings, they give the position in the
event record of the outgoing-state parton that rescatters. 
<code>iAMPI</code> and <code>iBMPI</code> then denote partons coming from 
the first or second beam, respectively.
  

<a name="method43"></a>
<p/><strong>double Info::eMPI(i) &nbsp;</strong> <br/>
The enhancement or depletion of the rate of the <code>i</code>'th 
subprocess. Is primarily of interest for the 
<code>MultipartonInteractions:bProfile = 4</code> option, where the 
size of the proton depends on the <i>x</i> values of the colliding
partons. Note that <code>eMPI(0) = enhanceMPI()</code>.
  

<h3>Cross sections</h3>

Here are the currently available methods related to the event sample 
as a whole. While continuously updated during the run, it is recommended
only to study these properties at the end of the event generation, 
when the full statistics is available.

<a name="method44"></a>
<p/><strong>long Info::nTried() &nbsp;</strong> <br/>
  
<strong>long Info::nSelected() &nbsp;</strong> <br/>
  
<strong>long Info::nAccepted() &nbsp;</strong> <br/>
the total number of tried phase-space points, selected hard processes
and finally accepted events, summed over all allowed subprocesses.
The first number is only intended for a study of the phase-space selection
efficiency. The last two numbers usually only disagree if the user introduces 
some veto during the event-generation process; then the former is the number 
of acceptable events found by PYTHIA and the latter the number that also
were approved by the user. If you set <?php $filepath = $_GET["filepath"];
echo "<a href='ASecondHardProcess.php?filepath=".$filepath."' target='page'>";?>a 
second hard process</a> there may also be a mismatch. 
  

<a name="method45"></a>
<p/><strong>double Info::sigmaGen() &nbsp;</strong> <br/>
  
<strong>double Info::sigmaErr() &nbsp;</strong> <br/>
the estimated cross section and its estimated error,
summed over all allowed subprocesses, in units of mb. The numbers refer to
the accepted event sample above, i.e. after any user veto. 
  

<h3>Loop counters</h3>

Mainly for internal/debug purposes, a number of loop counters from
various parts of the program are stored in the <code>Info</code> class,
so that one can keep track of how the event generation is progressing.
This may be especially useful in the context of the  
<code><?php $filepath = $_GET["filepath"];
echo "<a href='UserHooks.php?filepath=".$filepath."' target='page'>";?>User Hooks</a></code> facility.

<a name="method46"></a>
<p/><strong>int Info::getCounter(int i) &nbsp;</strong> <br/>
the method that gives you access to the value of the various loop 
counters.
<br/><code>argument</code><strong> i </strong>  :  the counter number you want to access:
<br/><code>argumentoption </code><strong> 0 - 9</strong> :  counters that refer to the run as a whole, 
i.e. are set 0 at the beginning of the run and then only can increase.
  
<br/><code>argumentoption </code><strong> 0</strong> :  the number of successful constructor calls for the
<code>Pythia</code> class (can only be 0 or 1).
  
<br/><code>argumentoption </code><strong> 1</strong> :  the number of times a <code>Pythia::init(...)</code>
call has been begun.  
  
<br/><code>argumentoption </code><strong> 2</strong> :  the number of times a <code>Pythia::init(...)</code>
call has been completed successfully.  
  
<br/><code>argumentoption </code><strong> 3</strong> :  the number of times a <code>Pythia::next()</code>
call has been begun.  
  
<br/><code>argumentoption </code><strong> 4</strong> :  the number of times a <code>Pythia::next()</code>
call has been completed successfully.  
  
<br/><code>argumentoption </code><strong> 10 - 19</strong> :  counters that refer to each individual event, 
and are reset and updated in the top-level <code>Pythia::next()</code> 
method.  
  
<br/><code>argumentoption </code><strong> 10</strong> :  the number of times the selection of a new hard 
process has been begun. Normally this should only happen once, unless a 
user veto is set to abort the current process and try a new one.
  
<br/><code>argumentoption </code><strong> 11</strong> :  the number of times the selection of a new hard 
process has been completed successfully.  
  
<br/><code>argumentoption </code><strong> 12</strong> :  as 11, but additionally the process should
survive any user veto and go on to the parton- and hadron-level stages. 
  
<br/><code>argumentoption </code><strong> 13</strong> :  as 11, but additionally the process should 
survive the parton- and hadron-level stage and any user cuts. 
  
<br/><code>argumentoption </code><strong> 14</strong> :  the number of times the loop over parton- and
hadron-level processing has begun for a hard process. Is reset each
time counter 12 above is reached. 
  
<br/><code>argumentoption </code><strong> 15</strong> :  the number of times the above loop has successfully
completed the parton-level step.
  
<br/><code>argumentoption </code><strong> 16</strong> :  the number of times the above loop has successfully
completed the checks and user vetoes after the parton-level step.
  
<br/><code>argumentoption </code><strong> 17</strong> :  the number of times the above loop has successfully
completed the hadron-level step.
  
<br/><code>argumentoption </code><strong> 18</strong> :  the number of times the above loop has successfully
completed the checks and user vetoes after the hadron-level step.
  
<br/><code>argumentoption </code><strong> 20 - 39</strong> :  counters that refer to a local part of the 
individual event, and are reset at the beginning of this part.
  
<br/><code>argumentoption </code><strong> 20</strong> :  the current system being processed in 
<code>PartonLevel::next()</code>. Is almost always 1, but for double
diffraction the two diffractive systems are 1 and 2, respectively.
  
<br/><code>argumentoption </code><strong> 21</strong> :  the number of times the processing of the 
current system (see above) has begun.
  
<br/><code>argumentoption </code><strong> 22</strong> :  the number of times a step has begun in the 
combined MPI/ISR/FSR evolution downwards in <i>pT</i> 
for the current system.
  
<br/><code>argumentoption </code><strong> 23</strong> :  the number of times MPI has been selected for the 
downwards step above.
  
<br/><code>argumentoption </code><strong> 24</strong> :  the number of times ISR has been selected for the 
downwards step above.
  
<br/><code>argumentoption </code><strong> 25</strong> :  the number of times FSR has been selected for the 
downwards step above.
  
<br/><code>argumentoption </code><strong> 26</strong> :   the number of times MPI has been accepted as the 
downwards step above, after the vetoes.
  
<br/><code>argumentoption </code><strong> 27</strong> :   the number of times ISR has been accepted as the 
downwards step above, after the vetoes.
  
<br/><code>argumentoption </code><strong> 28</strong> :   the number of times FSR has been accepted as the 
downwards step above, after the vetoes.
  
<br/><code>argumentoption </code><strong> 29</strong> :  the number of times a step has begun in the 
separate (optional) FSR evolution downwards in <i>pT</i> 
for the current system.
  
<br/><code>argumentoption </code><strong> 30</strong> :  the number of times FSR has been selected for the 
downwards step above.
  
<br/><code>argumentoption </code><strong> 31</strong> :   the number of times FSR has been accepted as the 
downwards step above, after the vetoes.
  
<br/><code>argumentoption </code><strong> 40 - 49</strong> :  counters that are unused (currently), and
that therefore are free to use, with the help of the two methods below.
  
  
  

<a name="method47"></a>
<p/><strong>void Info::setCounter(int i, int value = 0) &nbsp;</strong> <br/>
set the above counters to a given value. Only to be used by you 
for the unassigned counters 40 - 49.
<br/><code>argument</code><strong> i </strong>  :  the counter number, see above.
  
<br/><code>argument</code><strong> value </strong> (<code>default = <strong>0</strong></code>) :  set the counter to this number;
normally the default value is what you want.
  
  

<a name="method48"></a>
<p/><strong>void Info::addCounter(int i, int value = 0) &nbsp;</strong> <br/>
increase the above counters by a given amount. Only to be used by you 
for the unassigned counters 40 - 49.
<br/><code>argument</code><strong> i </strong>  :  the counter number, see above.
  
<br/><code>argument</code><strong> value </strong> (<code>default = <strong>1</strong></code>) :  increase the counter by this amount;
normally the default value is what you want.
  
  

<h3>Parton shower history</h3>

The following methods are mainly intended for internal use,
e.g. for matrix-element matching.

<a name="method49"></a>
<p/><strong>void Info::hasHistory(bool hasHistoryIn) &nbsp;</strong> <br/>
  
<strong>bool Info::hasHistory() &nbsp;</strong> <br/>
set/get knowledge whether the likely shower history of an event 
has been traced.
  

<a name="method50"></a>
<p/><strong>void Info::zNowISR(bool zNowIn) &nbsp;</strong> <br/>
  
<strong>double Info::zNowISR() &nbsp;</strong> <br/>
set/get value of <i>z</i> in latest ISR branching.
  

<a name="method51"></a>
<p/><strong>void Info::pT2NowISR(bool pT2NowIn) &nbsp;</strong> <br/>
  
<strong>double Info::pT2NowISR() &nbsp;</strong> <br/>
set/get value of <i>pT^2</i> in latest ISR branching.
  

</body>
</html>

<!-- Copyright (C) 2012 Torbjorn Sjostrand -->
