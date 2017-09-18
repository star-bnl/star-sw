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
corresponding update notes for PYTHIA 6. On the other hand, 
whereas the PYTHIA 6 manual is a static document, the PYTHIA 8 
html/php pages are kept up to date for each subversion. 
 
<br/><br/> 
<b>Important note:</b>version 8.160 does introduce some elements of 
backwards incompatibility. Specifically, "multiple interactions", 
abbreviated MI, has been renamed "multiparton interactions", MPI. 
This affects many parts of the code and documentation. Also some 
features are deprecated, although remaining until the end of the 
8.1xx version series. 
 
 
<h3>Main news by version</h3> 
 
<ul> 
 
<li>8.105: 24 February 2008 
<ul> 
 
<li>New option to initialize with arbitrary beam directions<br/> 
<code>pythia.init( idA, idB, pxA, pyA, pzA, pxB, pyB, pzB)</code></li> 
 
<li>Added capability to set <?php $filepath = $_GET["filepath"];
echo "<a href='BeamParameters.php?filepath=".$filepath."' target='page'>";?>beam energy spread 
and beam vertex</a>. 
<br/> 
<b>Warning:</b> as a consequence, the old <code>Beams</code> group of 
settings has been moved to <code>BeamRemnants</code>, and 
<code>Beams</code> is now instead used for machine beam parameters. 
Therefore also some <code>Main</code> settings of this character have been 
regrouped, as follows: 
<table cellspacing="5"> 
<tr> <td>8.100 setting  </td> <td>has been moved to </td> </tr> 
<tr> <td><code>Main:idA          </code></td> 
     <td><code>Beams:idA         </code></td> </tr> 
<tr> <td><code>Main:idB          </code></td> 
     <td><code>Beams:idB         </code></td> </tr> 
<tr> <td><code>Main:inCMframe    </code></td> 
     <td>(<code>Beams:frameType</code>) </td> </tr> 
<tr> <td><code>Main:eCM          </code></td> 
     <td><code>Beams:eCM         </code></td> </tr> 
<tr> <td><code>Main:eA           </code></td> 
     <td><code>Beams:eA          </code></td> </tr> 
<tr> <td><code>Main:eB           </code></td> 
     <td><code>Beams:eB          </code></td> </tr> 
<tr> <td><code>Main:LHEF         </code></td> 
     <td><code>Beams:LHEF        </code></td> </tr> 
</table></li> 
 
<li>The <code>forceHadronLevel()</code> method introduced for standalone 
hadronization.</li> 
 
<li><code>main15.cc</code> illustrated how either full hadronization or 
only decays of some particles can be looped over for the rest of the 
event retained.</li> 
 
<li>The <code>LHAevnt</code> and <code>LHAinit</code> classes have been 
joined into a new <code>LHAup</code> one, with new options that allow 
the writing of a Les Houches Event File.</li> 
 
<li>The <code>SusyLesHouches</code> class updated to handle 
SLHA version 2.</li> 
 
<li>Updated HepMC conversion routine.</li> 
 
<li>The static <code>ErrorMsg</code> class has been removed and 
its functionality moved into the non-static <code>Info</code> class, 
in the renamed Info file.</li> 
 
<li>Further reduction of the use of static, with related code changes. 
This should allow to have several almost independent <code>Pythia</code> 
instances. Some static classes still remain, however, notably for 
random number generation and particle properties.</li> 
 
<li>Limited capability to use two different <code>Pythia</code> instances 
for signal + pileup event generation, see <code>main19.cc</code> for an 
example.</li> 
 
<li>In the <code>Event</code> class the <code>=</code> and 
<code>=+</code> methods have been overloaded to allow the copying 
or appending of event records. Illustrated in <code>main19.cc</code>.</li> 
 
<li>The html and php page formatting improved with 
cascading style sheets.</li> 
 
<li>Several minor improvements and new options, including updated configure 
scripts.</li> 
 
</ul> 
</li> 
 
<li>8.108: 1 May 2008 
<ul> 
 
<li>Correction in the event record, so that the beam particles in line 
1 and 2 do not have any mother according to the <code>motherList</code> 
method. Previously the "system" entry in line 0 was counted as their 
mother, which gave rise to an unexpected extra vertex in the conversion 
to the HepMC format.</li> 
 
<li>Support for HepMC version 1 is removed, to simplify the code and 
reflect the evolution of the field.</li> 
 
<li>Status codes are stored in HepMC only as 1 for existing and 2 for 
decayed or fragmented particles (whereas previously the original PYTHIA 
codes were used for the latter).</li> 
 
<li>Parton densities are stored in HepMC as <i>xf(x,Q^2)</i> 
rather than the <i>f(x,Q^2)</i> used in (some) previous versions.</li> 
 
<li>The SusyLesHouches class has been updated so that reading is fully 
compatible with the SLHA2 standard. </li> 
 
<li>Improved handling of the Higgs width, relevant for massive and thereby 
broad resonance shapes.</li> 
 
<li>The matrix elements for neutralino pair production have now been 
completed and checked.</li> 
 
<li>Ensure that <i>alpha_strong</i> does not blow up, by introducing 
a minimal scale somewhat above <i>Lambda_3</i> (roughly where 
<i>alpha_strong = 10</i>).</li> 
 
<li>New methods <code>isValence1()</code> and <code>isValence2()</code> 
in the <code>Info</code> class.</li> 
 
<li>Protection against division by zero in calculation of decay vertex 
(for zero-mass gluons with zero lifetime, where there should be no 
displacement).</li> 
 
<li>A new compilation option <code>-Wshadow</code> is introduced and 
code is rewritten at all places where this option gave warnings.</li> 
 
<li>Minor library correction to allow compilation with gcc 4.3.0.</li> 
 
</ul> 
</li> 
 
<li>8.114: 22 October 2008 
<ul> 
 
<li>New rescattering description operational (but still experimental) 
for the case that FSR is not interleaved, but saved until after MPI, 
ISR and beam remnants have been handled. This involves much new code 
in several classes.</li> 
 
<li>A new class <code>PartonSystems</code> is introduced to 
keep track of which partons in the event record belong to which 
subcollision system, plus some further information on each subsystem. 
It takes over functionality previously found as part of the 
<code>Event</code> class, but leaves room for future growth.</li> 
 
<li>Add optional model, wherein an increased <i>pT0</i> turnoff scale 
for MPI and ISR is used for above-average active events, i.e. events that 
already have several MPI's or ISR emissions.</li> 
 
<li>Freeze GRV 94L distribution at small <i>Q^2</i> to avoid blowup.</li> 
 
<li>The <code>pythia.readFile(...)</code> method can now alternatively take 
an <code>istream</code> as argument instead of a <code>filename</code>.</li> 
 
<li>Minor bug correction in <code>PartonLevel.cc</code>; the bug could 
(rarely) give a segmentation fault.</li> 
 
</ul> 
</li> 
 
<li>8.120: 10 March 2009 
<ul> 
 
<li>New rescattering description further developed, but not yet 
recommended for normal usage.</li> 
 
<li>Include new processes for Large Extra Dimensions and Unparticles, 
contributed by Stefan Ask. New test program <code>main28.cc</code> 
illustrates.</li> 
 
<li>Include further SUSY processes: neutralino-chargino and 
chargino-chargino pairs. The processes should be valid also 
in the case of non-minimal flavour violation and/or CP violation. 
Expanded machinery to keep track of SUSY parameters.</li> 
 
<li>Include backwards evolution of incoming photon as part of the 
<code>SpaceShower</code> initial-state radiation description. This 
allows you to simulate hard collisions where one of the incoming 
partons is a photon. New test program <code>main43.cc</code> 
illustrates.</li> 
 
<li>Allow separate mass and transverse momentum cuts when two hard 
subprocesses are generated in the same event.</li> 
 
<li>The default value for the border between short- and long-lived 
particles has been changed from 1 mm to 10 mm, to better conform with 
LHC standards, see  <?php $filepath = $_GET["filepath"];
echo "<a href='ParticleDecays.php?filepath=".$filepath."' target='page'>";?>here</a>. 
The default is still to let all unstable particles decay.</li> 
 
<li>New ISR matrix-element correction to <i>f &rarr; f gamma</i> 
in single <i>W</i> production.</li> 
 
<li>New method <code>Event::statusHepMC</code> returns the status 
code according to the HepMC conventions agreed in February 2009. 
The interface to HepMC now writes out status according to this 
convention.</li> 
 
<li>Add capability to link to FastJet, with expanded configure script 
and Makefile, and with <code>main61.cc</code> as new example.</li> 
 
<li>Update of <code>Makefile.msc</code>, with added support for latest 
Visual C++ Express edition and use of regexp to check nmake version.</li> 
 
<li>Update of <code>LHAFortran.h</code> and 
<code>Pythia6Interface.h</code>, to make the interface to Fortran 
routines work also under Windows. (Thanks to Anton Karneyeu.) 
 
<li>Updated and expanded worksheet.</li> 
 
<li>The manual pages in the <code>xmldoc</code> directory, and thereby 
also those of the <code>htmldoc</code> and <code>phpdoc</code> 
directories, have been significantly updated and expanded. In particular, 
in many places the class of each method is explicitly shown, as well as 
the type of the return value and of the arguments. This upgrade is not 
yet completed, but already covers the more relevant sections. </li> 
 
<li>The unary minus operator in the <code>Vec4()</code> returns a 
reference to a four-vector with all components negated, but leaves 
the original four-vector unchanged. Previously the four-vector itself 
was flipped.</li> 
 
<li>The <code>pPlus()</code> and <code>pMinus()</code> methods of a 
four-vector and an event-record particle are renamed <code>pPos()</code> 
and <code>pNeg()</code>, respectively.</li> 
 
<li>Include a further loop in <code>ProcessLevel</code>, so that a new 
process is generated in case of failures of a less severe nature.</li> 
 
<li>Introduce warning message for unexpected <code>meMode</code> in 
<code>ResonanceWidths</code>.</li> 
 
<li>Les Houches event reading framework has been rearranged for 
more flexibility. Some bugs corrected. Specifically, when scale 
is not set (<code>scale = -1.</code> in the Les Houches standard), 
PYTHIA did not attempt to set this scale itself, which typically 
lead to there not being any ISR or FSR. Now the 
<?php $filepath = $_GET["filepath"];
echo "<a href='CouplingsAndScales.php?filepath=".$filepath."' target='page'>";?>rules for normal 
1-, 2- and 3-body final states</a> are applied, with a trivial 
extension of the 3-body rules for higher multiplicities.</li> 
 
<li>Correct bug in the handling of parton densities, whereby it was 
not possible to switch to a new set, once a first initialization 
had been done.</li> 
 
<li>Correct bugs when several <code>Pythia::init</code> initialization 
calls are made in the same run, specifically in the case that pointers 
to external processes have been handed in. 
 
<li>Changes in <code>main03.cmnd</code> and <code>main04.cmnd</code> 
so that some nonstandard options are commented out rather than active. 
Related comments inserted also in some other <code>.cmnd</code> files, 
but there without any change in program execution.</li> 
 
<li>A few further minor bug fixes.</li> 
 
<li>Update year to 2009.</li> 
 
</ul> 
</li> 
 
<li>8.125: 16 June 2009 
<ul> 
 
<li>Hadronization and timelike-shower parameter default values updated 
according to a tune to LEP1 data by Hendrik Hoeth, using the 
Rivet + Professor framework.</li> 
 
<li>Many further SUSY production processes. SLHA readin expanded to cover 
also masses and decay modes. Example <code>main22.cc</code> updated, while 
<code>main33.cc</code> now superfluous and removed. </li> 
 
<li>Also further processes for extra dimensions.</li> 
 
<li>Stefan Ask joins as coauthor.</li> 
 
<li>The <code>TimeShower::shower(...)</code> method has gained 
a new argument, that forces the shower evolution to stop after 
a given number of branchings. A new method 
<code>TimeShower::pTLastInShower()</code> permits the last evolution 
<i>pT</i> to be read out. These extensions can be useful 
for matching studies. 
</li> 
 
<li>New optional argument <code>isInterleaved</code> added to 
<code>TimeShower::branch(...)</code>. Is false by default, but 
true when called from the parton level for interleaved evolution 
of several parton systems, optionally also with ISR and MPI.</li> 
 
<li>New methods <code>UserHooks::canSetResonanceScale()</code> 
and <code>UserHooks:scaleResonance(...)</code> allows an optional 
user choice of the maximum shower scale in resonance decays.</li> 
 
<li>A new method <code>SpaceShower::doRestart()</code> has been 
added, to help distinguish cases when a 
<code>SpaceShower::branch(...)</code> failure forces a complete 
restart of the evolution from ones where only the intended 
current branching has been vetoed. 
 
<li>When multiparton interactions are initialized, it is now 
possible to reduce both <i>pT0</i> and <i>pTmin</i> 
if necessary to find a valid solution where 
<i>sigma_jet &gt; sigma_nondiffractive</i>. Previously 
only the former would be reduced, which could lead to 
infinite loops if too large a <i>pTmin</i> was used. 
Thanks to Sami Kama for pointing out the problem. 
</li> 
 
<li>The rescattering machinery is now essentially completed, and can 
be used also by others than the authors. For now, however, it can only 
be recommended for dedicated studies, not e.g. for generic tunes. 
</li> 
 
<li>Timelike and spacelike showers, and beam remnant handling, 
are modified to handle rescattering partons. Specifically, a new machinery 
is introduced to trace the recoils from the combination of rescattering 
with showers and primordial <i>kT</i>. Can assign space- or timelike 
virtualities to intermediate particles to have energy and momentum 
conserved locally. This affects the <code>PartonLevel</code>, 
<code>MultipartonInteractions</code>, <code>TimeShower</code>, 
<code>SpaceShowe</code>r and <code>BeamRemnants</code> classes. 
Further details to appear in the upcoming article on rescattering. 
(Another change is a reversal to pre-8.114 order for non-interleaved FSR, 
wherein also FSR is treated before beam remnants are attached.) 
</li> 
 
<li>Four new status codes introduced, as part of the rescattering 
description: 
<br/>45 : incoming rescattered parton, with changed kinematics owing 
to ISR in the mother system (cf. status 34); 
<br/>46 : incoming copy of recoiler when this is a rescattered parton 
(cf. status 42); 
<br/>54 : copy of a recoiler, when in the initial state of a different 
system from the radiator; 
<br/>55 : copy of a recoiler, when in the final state of a different 
system from the radiator. 
</li> 
 
<li>New method <code>Info::tooLowPTmin()</code> can tell whether the 
<i>pTmin</i> scales for showers or multiparton interactions are too low. 
</li> 
 
<li>Pion beams allowed, both <i>pi^+</i>, <i>pi^-</i> and 
<i>pi^0</i>. New machinery for the latter, where the valence 
flavour content is chosen to be either <i>d dbar</i> or 
<i>u ubar</i> for each new event. One internal pion PDF 
implemented, with others from LHAPDF.</li> 
 
<li>Treatment of Pomeron-proton collisions begun.</li> 
 
<li>Phase-space handling of hard processes and multiparton interactions 
slightly expanded to better allow for harder PDF's than proton ones, 
e.g. for Pomerons.</li> 
 
<li>The program documentation has been expanded with an 
alphabetical index of all methods that are described on the 
webpages, see the <?php $filepath = $_GET["filepath"];
echo "<a href='ProgramMethods.php?filepath=".$filepath."' target='page'>";?>Program Methods</a> 
page. Also other sections of the documentation have been 
updated and expanded, including the worksheet.</li> 
 
<li>Several <code>list</code> methods have been made 
<code>const</code>. For the listing of events two new methods 
have been added, <code>Event::list()</code> and 
<code>Event::list(bool showScaleAndVertex, 
bool showMothersAndDaughters = false)</code>, 
that correspond to special cases of the general method.</li> 
 
<li>A new method <code>Pythia::LHAeventSkip(int nSkip)</code> 
permits a skip-ahead of the reading of external Les Houches 
Events, without the necessity to generate the intervening 
<code>nSkip</code> events in full. Makes use of the new 
<code>LHAup::skipEvent(int nSkip)</code> method to perform 
the operations. Mainly intended for debug purposes. 
 
<li>The <code>ClusterJet</code> jet finder now saves the last 
five clustering scales. Also a minor bug fix. Thanks to Nils 
Lavesson for this contribution.</li> 
 
<li>The <code>Particle::m2()</code> method now returns a negative 
number when the stored mass <i>m</i> is negative, as used to 
indicate spacelike virtualities. Also the 
<code>Particle::eCalc()</code>, <code>Particle::mT()</code> and 
<code>Particle::mT2()</code> methods have been modified. 
</li> 
 
<li>The <code>&lt;&lt;</code> method to print our a four-vector has 
been expanded with a fifth number, the invariant length, with a minus 
sign for spacelike vectors, and provided within brackets to allow 
a simple visual distinction.</li> 
 
<li>New methods <code>Rndm::dumpState(string fileName)</code> and 
<code>Rndm::readState(string fileName)</code> allows to write or 
read the state of the random number generator to or from a binary file. 
</li> 
 
<li>New method <code>double GammaReal(double x)</code> returns the 
value of the <i>Gamma</i> function for arbitrary real argument. 
Some cross sections for extra-dimensional processes rewritten to 
make use of it. 
</li> 
 
<li>New example program <code>main29.cc</code> shows how 
to set up a fictitious process of a heavy system decaying 
to two particles or partons, with decays traced to stable 
particles, as relevant for astroparticle applications.</li> 
 
<li>Main programs that illustrate the HepMC interface have 
been updated to use version 2.04, including units and excluding 
deprecated output formats.</li> 
 
<li>The <code>main32.cc</code> example extended also to handle 
Les Houches Event Files.</li> 
 
<li>The Makefile has been modified so that "make clean" only 
removes the current compilation and library files, while 
"make distclean" gives a more extensive reset and cleanup. 
Thanks to Nils Lavesson for this contribution. Some other 
minor Makefile corrections.</li> 
 
<li>Several main programs that use the 
<code>Main:timesToShow</code> mode have been corrected so as 
not to crash if this is set to zero. Also some other cosmetics 
changes in main programs that do not affect the running.</li> 
 
<li>Bug correction, in that previously a veto with user hooks 
was not propagated from parton showers inside resonance decays. 
</li> 
 
<li>Minor bug fix in <code>TimeShower</code> for kinematics with 
unequal beam-particle masses.</li> 
 
<li>Bug fix so that <code>PartonLevel:MPI = off</code> also works for 
minimum-bias events.</li> 
 
<li>Minor bug fix in the impact-parameter selection of multiparton 
interactions. Thanks to Sami Kama for pointing it out.</li> 
 
<li>String fragmentation for junction topology protected against 
numerical instability in boost.</li> 
 
<li>Bug correction in the handling of particle decays to partons, 
where the scale of the partons was set before the partons had been 
added to some arrays, leading to indexation out of bounds. 
Thanks to Vladislav Burylov for discovering this bug. 
</li> 
 
<li>Bug correction in the handling of particles with inhibited decay, 
where the decay vertex would be too far displaced, which could lead to 
infinite loop. Thanks to Sami Kama for debugging this. </li> 
 
<li>Check to avoid infinite loop in matrix-element handling of 
two-body decays.</li> 
 
<li>Bug correction to avoid infinite loops in Dalitz decay treatment. 
Some changes in the decay handling logic to allow a new try when the 
decay of a particle fails.</li> 
 
<li>Minor correction, so the pointer to the <code>Info</code> class 
is set also for user-written classes derived from <code>LHAup</code>. 
</li> 
 
<li>Correction for typo in the matrix element of the 
<code>Sigma3ff2HchgchgfftWW</code> class, for doubly charged Higgs 
production. Thanks to Merlin Kole for spotting it.</li> 
 
<li>Updated colour bookkeeping in junction-antijunction annihilation 
avoids later problems in <code>Pythia::check()</code>. </li> 
 
<li>Minor updates of the <code>Makefile.msc</code> file to work with 
Visual Studio 2008. Thanks to David Bailey for these modifications.</li> 
 
<li>Ensure that <code>nInit</code> in the <code>BeamParticle</code> 
class is set also for unresolved lepton beams.</li> 
 
<li>The <code>VetoEvolution</code> class, derived from 
<code>UserHooks</code>, is obsolete and has been removed.</li> 
 
</ul> 
</li> 
 
<li>8.130: 15 September 2009 
<ul> 
 
<li>New machinery that allows multiparton interactions inside diffractive 
systems. Also new optional Pomeron flux factors and Pomeron PDFs. 
New page on <?php $filepath = $_GET["filepath"];
echo "<a href='Diffraction.php?filepath=".$filepath."' target='page'>";?>diffraction</a> added, where 
further details are collected. Still not tuned, so to be used with 
caution.</li> 
 
<li>Make Peter Skands' "Tune 1" parameters for ISR and MPI default. 
The older simpler tune is still available as an option, see 
<code><?php $filepath = $_GET["filepath"];
echo "<a href='Tunes.php?filepath=".$filepath."' target='page'>";?>Tune:pp</a></code>. 
</li> 
 
<li>New possible choices for a second hard process: charmonium, 
bottomonium, top pair and single top.</li> 
 
<li>New code for pair production of generic colour-triplet scalar, 
fermion or vector. Largely written by Johan Bijnens, partly recycling 
existing code.</li> 
 
<li>Add user hooks possibility to veto event after a given number 
of multiparton interactions.</li> 
 
<li>Add instructions how PYTHIA 8 can be used from inside ROOT. 
Thanks to Andreas Morsch for providing the text and Rene Brun 
for a simple example.</li> 
 
<li>The <code>main21.cc</code> example extended with an option 
for a single-particle gun.</li> 
 
<li>Improvements and bug fixes in rescattering framework.</li> 
 
<li>New method <code>Hist::table(string fileName)</code> provides a 
more direct way to print a two-column table of histogram contents 
into a file than the current <code>Hist::table(ostream& os = cout)</code>. 
 </li> 
 
<li>Modify reading of external files so that a line only consisting of 
control characters counts as empty, the same way as a line only consisting 
of blanks already did. This includes carriage return, tabs and a few more, 
the ones represented by <code>\n \t \v \b \r \f \a</code>. Applies to 
Les Houches Event files, settings files and particle data files. </li> 
 
<li>Fix it so that the read-in of a Les Houches Event File for the 
hardest process can be combined with the facility to select a specified 
second hard process.</li> 
 
<li>New empty base class method <code>LHAup::fileFound()</code> 
allows the derived class <code>LHAupLHEF</code> to signal more 
clearly that a failed initialization is caused by a failure to open 
the desired file. </li> 
 
<li>Check that a pointer to an <code>LHAup</code> object has been set 
in <code>Pythia::LHAeventList()</code> and 
<code>Pythia::LHAeventSkip(...)</code> calls.</li> 
 
<li>Updated configure and Makefile, e.g. to build shared libraries 
on Mac OS X.</li> 
 
<li>The options with a direct link to hard-process generation 
in PYTHIA 6 has been removed. It is hardly ever used but 
complicates the build structure. (Owing to its usefulness for some 
debug work, it was reinstated in a limited form in version 8.135. 
Thus <code>main51.cc</code> now contains the complete interface, 
previously in separate files, and commented-out lines in 
<code>examples/Makefile</code> suggest how PYTHIA 6 could be linked.) 
</li> 
 
<li>New argument to <code>Info::errorMsg(...)</code> allows to 
show all error messages of a specific kind rather than only the 
first one, e.g. for initialization.</li> 
 
<li>Correction in decay table of righthanded Majorana neutrinos. 
Thanks to Arnaud Ferrari and Vladimir Savinov.</li> 
 
<li>Correction in expressions in the manual for <i>H^+-</i> couplings 
to an <i>h^0</i>, <i>H^0</i> or <i>A^0</i> and a <i>W^+-</i>. 
Thanks to Rikard Enberg.</li> 
 
<li>Fix for accessing uninitialized memory, caused by accessing the 
daughters of the incoming beams before these daughters actually existed. 
Thanks to David Bailey and Sami Kama. 
</li> 
 
<li>Uninitialized photon PDF inside the proton could give crazy results 
for processes with incoming photons. Thanks to Adam Davison.</li> 
 
<li>Bug fix such that finite lifetimes can be set also for particles 
produced in the hard process. To exemplify, the bug affected 
<i>tau</i> leptons produced in <i>Z^0</i> and <i>W^+-</i> 
decays. Thanks to Troels Petersen.</li> 
 
<li>Bug fix in <code>TimeShower</code>, that <code>beamOffset</code> 
could remain uninitialized. Thanks to Sami Kama.</li> 
 
<li>Minor correction to resonance decays: fail if the allowed mass 
range of a Breit-Wigner resonance is a small fraction of the total 
area under the resonance curve.</li> 
 
<li>Correction when all three valence quarks were kicked out from a 
proton and could give false messages that beam momentum had been 
used up.</li> 
 
<li>Clarify status codes needed for hadron-level standalone runs in 
order to avoid error messages, and modify the <code>main21.cc</code> 
example accordingly. </li> 
 
<li>Minor corrections in the processes for extra dimensions.</li> 
 
<li>Some other minor additions to existing facilities 
and minor bug fixes.</li> 
 
</ul> 
</li> 
 
<li>8.135: 10 January 2010 
<ul> 
 
<li>All usage of static member methods inside Pythia8 has now been 
eliminated. This simplifies for you to have several simultaneous 
<code>Pythia</code> instances that are run with different conditions. 
The three main classes affected by this are the <code>Settings</code> 
and <code>ParticleData</code> databases and the <code>Rndm</code> 
random-number generator. You can no longer address the methods of these 
classes directly, but have to address them via the <code>settings</code>, 
<code>particleData</code> and <code>rndm</code> instances in the 
respective <code>Pythia</code> object. Also some other smaller pieces 
of code are affected, e.g. Standard Model and SUSY couplings 
(the latter in new files). 
<br/><b>Note 1</b>: The documentation has been updated accordingly 
on these webpages, but the "A Brief Introduction to PYTHIA 8.1" 
still refers to the old behaviour of version 8.100. 
<br/><b>Note 2</b>: the interface to the external LHAPDF library remains 
static, since LHAPDF is written in Fortran and thus by definition 
static. 
<br/><b>Note 3:</b> if you want to have momentum smearing in 
<code>CellJet</code> you now need to send in a pointer to a 
random-number generator. 
</li> 
 
<li>Ten new proton PDF sets are made available internally: 
MRST LO* (2007), MRST LO** (2008), MSTW 2008 LO (central member), 
MSTW 2008 NLO (central member), CTEQ6L, CTEQ6L1, CTEQ6.6 (NLO, 
central member), CT09MC1, CT09MC2, and CT09MCS, see 
<?php $filepath = $_GET["filepath"];
echo "<a href='PDFSelection.php?filepath=".$filepath."' target='page'>";?>PDF Selection</a>. The Pomeron PDF data 
files have been renamed for consistency. Thanks to Tomas Kasemets 
for help with this [<a href="Bibliography.php" target="page">Kas10</a>]. 
</li> 
 
<li>New parameters <code>TimeShower:pTmaxFudgeMPI</code> and 
<code>SpaceShower:pTmaxFudgeMPI</code> introduced, to give the same 
functionality for multiparton interactions that 
<code>TimeShower:pTmaxFudge</code> and <code>SpaceShower:pTmaxFudge</code> 
do for the hardest.</li> 
 
<li>A few extensions of the <code>UserHooks</code> framework. 
New methods <code>UserHooks:canVetoISREmission()</code> and 
<code>UserHooks::doVetoISREmission(...)</code> allows the latest 
initial-state emission to be studied before being finalized, 
with the possibility to veto it. 
Similarly <code>Userhooks:canVetoFSREmission()</code> and 
<code>UserHooks::doVetoFSREmission(...)</code> can be used to 
veto the latest final-state emission.</li> 
 
<li>A number of loop counters have been introduced in the 
<code>Info</code> class, that offers some further information on 
the progress of the event generation, for use e.g. in conjunction 
with the <code>UserHooks</code> facility.</li> 
 
<li>The <code>Pythia::initTunes(...)</code> method is made public, so that 
it can be called before the normal call from <code>Pythia::init(...)</code>. 
That way it is possible to start out from a given tune and change a few 
of the parameters. 
</li> 
 
<li>Bug corrected in <code>LHAFortran.h</code> for hard-process input 
from Fortran commonblock. This did not work properly when the input 
was used in combination with a second hard process generated internally. 
Thanks to Mikhail Kirsanov and Roberto Chierici. 
 
<li>Insertion of missing initialization of <code>isInit</code> in the 
<code>Settings</code> and <code>ParticleData</code> constructors, and 
<code>nInit</code> for <code>BeamParticle</code>. 
Thanks to Leif L&ouml;nnblad.</li> 
 
<li>Updated <code>Makefile.msc</code> for Windows users.</li> 
 
<li>Fix of some non-optimal use of booleans, that give warnings on 
a Windows compiler. Thanks to Anton Karneyeu.</li> 
 
<li>New options for the <?php $filepath = $_GET["filepath"];
echo "<a href='SUSYLesHouchesAccord.php?filepath=".$filepath."' target='page'>";?> 
SUSY Les Houches Accord</a> such that, by default, 
particle and decay data are not overwritten for known Standard Model 
particles (including <i>Z^0</i>, <i>W^+-</i> and <i>t</i>, 
but excluding the Higgs).</li> 
 
<li>Bug fix in <code>SusyLesHouches</code>, where the reading of SLHA 
information embedded in an LHEF would not stop at the end of the header 
section.</li> 
  
<li>Bug correction for undefined secondary widths where decay products 
together are heavier than the mother. Also unit default secondary width 
values in the <code>DecayChannel</code> constructor.</li> 
  
<li>Documentation updated, including change of current year to 2010.</li> 
 
</ul> 
</li> 
 
<li>8.140: 16 July 2010 
<ul>
 
<li>Four new draft <?php $filepath = $_GET["filepath"];
echo "<a href='Tunes.php?filepath=".$filepath."' target='page'>";?>tunes</a> available.</li> 
 
<li>Introduction of a new scenario for production of Hidden-Valley 
particles, and interleaved showering in the QCD and HV sectors, see 
the new <?php $filepath = $_GET["filepath"];
echo "<a href='HiddenValleyProcesses.php?filepath=".$filepath."' target='page'>";?>Hidden Valleys</a> 
description. A longer physics writeup is available [<a href="Bibliography.php" target="page">Car10</a>]. 
</li> 
 
<li>Implementation of <i>2 &rarr; 3</i> phase space selection intended 
for QCD processes with massless partons. A new set of matching 
<?php $filepath = $_GET["filepath"];
echo "<a href='PhaseSpaceCuts.php?filepath=".$filepath."' target='page'>";?>phase space cut parameters</a>. 
The <?php $filepath = $_GET["filepath"];
echo "<a href='QCDProcesses.php?filepath=".$filepath."' target='page'>";?>ten different QCD <i>2 &rarr; 3</i> 
processes</a> have been implemented making use of this new 
possibility, so far without a complete handling of possible colour 
flows, however.</li> 
 
<li>New processes have been added for 
<?php $filepath = $_GET["filepath"];
echo "<a href='CompositenessProcesses.php?filepath=".$filepath."' target='page'>";?>contact interactions</a> in 
<i>q q &rarr; q q</i> and <i>q qbar &rarr; q qbar</i> scattering.</li> 
 
<li>A process has been added for TeV^-1 Sized 
<?php $filepath = $_GET["filepath"];
echo "<a href='ExtraDimensionalProcesses.php?filepath=".$filepath."' target='page'>";?>Extra Dimensions</a>, which 
involves the electroweak KK gauge bosons, i.e. <i>gamma_{KK}/Z_{KK}</i>, 
in one TeV^-1 sized extra dimension; see <code>main30.cc</code> for 
an example. This scenario is described in [<a href="Bibliography.php" target="page">Bel10</a>]. Thanks to 
Noam Hod and coworkers for contributing this code.</li> 
 
<li>In the Randall-Sundrum extra-dimensional scenario a new option has 
been added where SM fields can exist in the bulk rather than only on 
a brane. (Still under development.) Furthermore production of a 
Kaluza-Klein gluon state has been added, and the <code> main28.cc</code> 
test program extended.</li> 
 
<li>The scenario for monojets in Large Extra Dimensions has been expanded 
with an alternative for scalar graviton exchange instead of tensor one. 
</li> 
 
<li>New parameters for maximum scale of 
<?php $filepath = $_GET["filepath"];
echo "<a href='TimelikeShowers.php?filepath=".$filepath."' target='page'>";?>timelike showers</a>, 
<code>TimeShower:pTmaxMatch</code>, and the dampening of hard radiation, 
<code>TimeShower:pTdampMatch</code> and <code>TimeShower:pTdampFudge</code>, 
by analogy with corresponding ones for spacelike showers. Also new method 
<code>TimeShower:limitPTmax(...)</code> to implement alternative procedures. 
For dipoles stretched to the beam the new switch 
<code>TimeShower:dampenBeamRecoil</code> allows to dampen radiation 
close to the beam direction, with a changed default behaviour.</li> 
 
<li>Azimuthal anisotropies from coherence arguments have been introduced 
for the spacelike parton showers, see <code>SpaceShower:phiIntAsym</code> 
and <code>SpaceShower:strengthIntAsym</code>. 
Also azimuthal anisotropies from gluon polarization have been introduced 
for the spacelike parton showers, see <code>SpaceShower:phiPolAsym</code>, 
and updated for timelike parton showers, see 
<code>TimeShower:phiPolAsym</code>.</li> 
 
<li>Improvements for the matching to POWHEG LHEF-style input illustrated 
by the new <code>main71.cc</code> example. See also [<a href="Bibliography.php" target="page">Cor10</a>].</li> 
 
<li>A set of new processes <i>gamma gamma &rarr; f fbar</i>, with <i>f</i> 
quarks or leptons. Code for equivalent photon flux around an unresolved 
proton, with more to come. Thanks to Oystein Alvestad. 
</li> 
 
<li>A new option has been included to dampen the growth of the diffractive 
cross sections, see <?php $filepath = $_GET["filepath"];
echo "<a href='TotalCrossSections.php?filepath=".$filepath."' target='page'>";?>Total Cross 
Sections</a>.</li> 
 
<li>A new method <code>virtual int SigmaProcess::idSChannel()</code> 
has been introduced. If overloaded to return a nonzero value then a 
<i>2 &rarr; n</i> process will appear listed as a 
<i>2 &rarr; 1 &rarr; n</i> one. 
That is, an intermediate resonance with the requested identity will be 
inserted in the event record, even without appearing in the calculation 
of the matrix element proper. Thanks to Noam Hod for idea and code.</li> 
 
<li>A new method <code>SigmaProcess::convertM2()</code> has been introduced 
to optionally allow the <code>SigmaProcess::sigmaHat()</code> to return 
the squared matrix element rather than <i>d(sigmaHat)/d(tHat)</i> for 
<i> 2 &rarr; 2 </i> processes. Furthermore kinematics is stored in the new 
<code>mME</code> and <code>pME</code> vectors for alternative cross 
section encodings.</li> 
 
<li>Different encoding of the <i>f fbar &rarr; Z W</i> cross section, 
contributed by Merlin Kole, based on the cross section of 
Brown, Sahdev and Mikaelian. Gives a distinctly different cross section 
than the previous based on EHLQ (including their bug fix). 
In particular the problem with negative cross sections is now fixed.</li> 
 
<li>Minor updates: pass <code>xmlPath</code> to new MSTW and CTEQ PDFs; 
reset beams earlier to give cleaner documentation.</li> 
 
<li>The H1 Fit B LO parametrization to the Pomeron PDF has been included, 
and made new default for Pomerons. Thanks to Paul Newman for providing 
the data files.</li> 
 
<li>Three changes, in principle unrelated, but with the common objective 
to make the generation of a given event depend only on the values 
determined during the initialization stage and on the state of the 
random-number generator when the event is begun. The new default 
should ensure a reproducible stop-and-restart behaviour, convenient 
for debug purposes. Thanks to Michael Schmelling for stressing the 
desire for such a behaviour. 
<br/>(i) Introduction of a new option <code><?php $filepath = $_GET["filepath"];
echo "<a href='PhaseSpaceCuts.php?filepath=".$filepath."' target='page'>";?> 
PhaseSpace:increaseMaximum</a></code> that allows to switch between 
two strategies for handling the (hopefully rare) cases when the 
assumed maximum of the cross section function is exceeded during the 
event generation, with a changed default behaviour. In the old 
default the maximum could be increased if it was exceeded during the run, 
thereby introducing a memory of the previously generated events. 
<br/>(ii) Updated handling of random numbers with Gaussian distributions. 
The <code>gauss()</code> method now only generates one value at a time. 
Instead the new <code>gauss2()</code> method returns a pair of Gauss 
numbers, with related time savings. In the old approach one Gaussian 
number could be buffered, which introduced a memory. 
<br/>(iii) Minor bug fix in <code>MiniStringFragmentation</code>, where 
the popcorn baryon state was not reset for each new system, again 
giving a (flawed) memory.</li> 
 
<li>New directory <code>rootexample</code> with a simple code example 
how to use ROOT for histogramming in PYTHIA runs. See 
<?php $filepath = $_GET["filepath"];
echo "<a href='ROOTusage.php?filepath=".$filepath."' target='page'>";?>ROOT Usage</a> for details. Thanks to Rene Brun. 
</li> 
 
<li>In the <code>HepMCinterface</code> the checks for unhadronized 
quarks and gluons are not performed if hadronization has been switched 
off.</li> 
 
<li>The <code>UserHooks::initPtr(...)</code> has been expanded so that 
pointers to more classes (beam particles, random number, couplings, etc.) 
are sent in, thereby increasing the scope of possible user-written code. 
Also, the <code>UserHooks::doVetoProcessLevel(Event& process)</code> 
now allows the <code>process</code> event record to be modified, 
even if it is not recommended. The new <code>Info::pTnow()</code> 
method returns the current <i>pT</i> scale of the combined MPI, ISR 
and FSR evolution, which can be useful for some user hooks decisions. 
</li> 
 
<li>Histograms of the <code>Hist</code> class can now be booked 
with up to 1000 bins, instead of the previous maximum 100. All bins 
can be written to file, but the line-printer style printing will join 
nearby bins so at most 100 are printed, as before.  </li> 
 
<li>Updated <code>configure</code> and <code>Makefile</code> 
to allow 64-bit compilation and more options. Thanks to Mikhail Kirsanov, 
Rene Brun and Fons Rademakers.</li> 
 
<li>Bug fix in <code>ProcessLevel</code> for colour flow checks of 
junctions. Enables the decay of a neutralino to three quarks, e.g. 
Thanks to Nils-Erik Bomark.</li> 
 
<li>Bug fixes in <code>TimeShower</code>, where recoil partners in resonance 
decays of coloured particles, and recoil partners in QED dipoles when beams 
are not allowed to take a recoil, might not be correctly identified. </li> 
 
<li>Bug fix in <code>SpaceShower</code>: when used with a fixed 
<i>alpha_strong</i>, the threshold enhancement factor of 
<i>g &rarr; Q Qbar</i> became undefined. Thanks to Stefan Prestel.</li> 
 
<li>Bug fix in initialization of resonance widths, where the 
<code>minWidth</code> parameter could be used uninitialized, 
occasionally resulting in strange initialization problems when 
kinematics needs to force at least one resonance to be off-shell.</li> 
 
<li>Minor updates of <code>main06.cc</code>, <code>main10.cc</code> 
and <code>main23.cc</code>.</li> 
 
<li>Bug fix in mother pointer of multiparton interactions in 
diffractive systems.</li> 
 
</ul> 
</li> 
 
<li>8.142: 15 August 2010 
<ul> 
 
<li>The strategy for setting up tune values has been changed. 
Previously the <code>Tune:ee</code> and <code>Tune:pp</code> 
settings were only interpreted during the 
<code>Pythia::init(...)</code> stage. Now they are interpreted 
as soon as they are read. Specifically this means that it is 
possible to override any of the tune parameters by putting new 
commands below <code>Tune:ee</code> and <code>Tune:pp</code> 
in the <code>Pythia::readFile(...)</code> configuration file 
or in the list of <code>Pythia::readString(...)</code> commands. 
<br/>As a consequence of this change, the public 
<code>Pythia::initTunes(...)</code> method has been replaced by 
two private <code>Settings::initTuneEE(...)</code> and 
<code>Settings::initTunePP(...)</code> methods. 
<br/>If <code>Tune:ee</code> or <code>Tune:pp</code> are 
nonvanishing by default, then the corresponding tune variables 
are set also from the <code>Pythia</code> constructor, before any 
user changes are possible. Currently this is not relevant. 
</li> 
 
<li>Bug fix in the setup of the <code>Tune:pp = 5</code> and 
<code>= 6</code> options, which meant that diffractive cross 
sections were not reduced as advertised.</li> 
 
<li>The default value of <code>MultipartonInteractions:pT0Ref</code> 
has been slightly reduced so as to give a somewhat improved 
default description. It is not intended as a replacement for the 
specific tunes, however.</li> 
 
<li>Some comparisons between tunes and data, obtained with the 
Rivet package, have been posted on 
<a href="http://home.thep.lu.se/~richard/pythia81/"> 
http://home.thep.lu.se/~richard/pythia81/</a>. 
Further improvements can be expected from future Professor-based 
tunes. The current set of 2C, 2M, 3C and 3M "draft tunes" have 
deliberately been chosen different also to explore a range of 
possibilities. One not visible in the plots is the fraction 
of single and double diffractive events in the inelastic cross 
section. This is 32% in 2C and 2M, while it was reduced to 
21% in 3C and 3M, and a recent ATLAS study (ATLAS-CONF-2010-048) 
would suggest 28+-4%.</li> 
 
<li>Minor changes in <code>TimeShower</code> and 
<code>SpaceShower</code>, as protection if the <i>c</i> or 
<i>b</i> masses are set small.</li> 
 
<li>Bug fix in <code>SpaceShower</code>, for case when <i>phi</i> 
angle selection is biased to take into account interference effects. 
</li> 
 
<li>The <code>SigmaProcess::convertM2()</code> method now has 
been implemented also for <i>2 &rarr; 1</i> processes, so that it is 
possible to provide the squared matrix element instead of 
<i>sigmaHat</i> for semi-internal processes. A fixed-width 
Breit-Wigner is also inserted, since the matrix element is supposed 
not to include it.</li> 
 
<li>New method <code>SigmaProcess::setupForME()</code> does an 
extended conversion from the internal kinematics to an almost 
equivalent one, better adapted to the mass conventions of matrix-element 
calculation programs, such as massive incoming <i>b</i> quarks. 
New flags allow to determine whether the <i>c</i>, <i>b</i>, 
<i>mu</i> and <i>tau</i> should be considered massless or not 
in the calculations.</li> 
 
<li>Two new friend methods <code>table</code> of the <code>Hist</code> 
class allow to print a table with three columns, the first for the 
(common!) <i>x</i> values and the other two for the respective 
histogram values. A new method <code>takeSqrt</code> for the square 
root of histogram bin contents. 
</li> 
 
</ul> 
</li> 
 
<li>8.145: 10 November 2010 
<ul> 
 
<li><code>Couplings</code> is defined in <code>StandardModel.h</code> 
as a derived class of <code>coupSM</code> and has only one extra flag: 
<code>isSUSY</code> to check presence of extra couplings.Changed all 
pointers <code>CoupSM*</code> to <code>Couplings*</code> and removed 
explicit references to <code>CoupSUSY*</code>. The <code>coupSUSY</code> 
object is only initialised if SUSY couplings are present. The new pointer 
<code>couplingsPtr</code> points either to only SM couplings or SM+SUSY 
couplings based on SLHA data.</li> 
 
<li>New files <code>SusyResonanceWidths.h/cc</code> contains the 
<code>SusyResonanceWidths</code> class, which inherits from 
<code>ResonanceWidths</code> but typecasts the <code>couplingsPtr</code> 
to <code>(CoupSUSY*) coupSUSYPtr</code>.  This is the base class for 
all SUSY resonances. It contains the <code>ResonanceSquark</code> class 
for all squark decays. A new flag <code>SLHA:useDecayTable</code> 
to check if internal widths should be overwritten by those read in 
via SLHA.</li> 
 
<li>Added new functionality to <code>SusyLesHouches</code> for read-in 
of generic user blocks in the SLHA format, along with methods to 
extract parameters from them with typecasting controlled by the user. 
Intended for use with user-written semi-internal processes.</li> 
 
<li>Added <code>Sigma1qq2antisquark</code> cross section.</li> 
 
<li>Some new flags and modes in the <code>SUSY</code> and 
<code>SLHA</code> series offer further functionality.</li> 
 
<li>Several further changes as a consequence of the upgrade of the SUSY 
machinery.</li> 
 
<li>Bug/typo fixes in rotation matrices for SUSY couplings and for the 
processes <code>qqbar2squarkantisquark</code>, 
<code>qq2squarksquark</code> and more.</li> 
 
<li>Improved handling of colour junctions. Added new example program 
<code>main72.cc</code>, to illustrate read-in of color junction 
structures via LHEF (<code>main72.lhe</code>). The example used is SUSY 
with RPV.</li> 
 
<li>New Tune 4C introduced as <code>Tune:pp = 5</code>. The more crude 
(non-)tunes 3C and 3M are removed.</li> 
 
<li>New methods <code>Settings::getFlagMap(...)</code>, 
<code>Settings::getModeMap(...)</code>, 
<code>Settings::getParmMap(...)</code> and 
<code>Settings::getWordMap(...)</code> allows to return a map of all 
settings of the respective type that contain a specific string 
in its name. 
</li> 
 
<li>Improved description of excited <i>g^*/KK-gluon^*</i> production 
in the <code>Sigma1qqbar2KKgluonStar</code> and 
<code>ResonanceKKgluon</code> classes.</li> 
 
<li>Possibility added to let Hidden-Valley <i>gamma_v</i> have 
a nonzero mass and decay by mixing with the ordinary photon. 
Still experimental.</li> 
 
<li>Minor bug fix in handling of three-body phase space.</li> 
 
<li>Minor correction in <code>Sigma2ffbar2TEVffbar</code> class.</li> 
 
<li>Bug fix for decays of <i>W'</i> to a pair of heavy fermions when 
<i>gV != gA</i>. Thanks to M. Chizhov, see arXiv:0705.3944. </li> 
 
<li>Restore the older EHLQ-based encoding of the <i>f fbar &rarr; Z W</i> 
cross section, which was changed in version 8.140, since comparisons 
with Madgraph gives much better agreement with it than with the 
expression of Brown, Sahdev and Mikaelian.</li> 
 
<li>The <code>HepMCInterface</code> now also stores colour flow 
information for coloured particles.</li> 
 
<li>Pointer to <code>Couplings</code> made available for particle 
decays.</li> 
 
<li>Minor update in <code>main61.cc</code> for more elegant usage 
of FastJet, as suggested by Gregory Soyez.</li> 
 
</ul> 
</li> 
 
<li>8.150: 20 April 2011 
<ul> 
 
<li>Tune 4C is made default. It is based on first comparisons with 
LHC data [<a href="Bibliography.php" target="page">Cor10a</a>], and has also been checked independently 
[<a href="Bibliography.php" target="page">Buc11</a>] to give reasonable agreement with many distributions. 
</li> 
 
<li>The description of <i>tau</i> lepton decays has been significantly 
enhanced, to include helicity information related to the production 
process and hadronic currents fitted to data. A complete writeup is 
in preparation, while a summary can be found in [<a href="Bibliography.php" target="page">Ilt12</a>]. 
A new flag is introduced to revert to the old behaviour, for 
cross-checks, see <?php $filepath = $_GET["filepath"];
echo "<a href='ParticleDecays.php?filepath=".$filepath."' target='page'>";?>Particle Decays</a>. 
The new tau decay machinery is on by default. 
</li> 
 
<li>A new option <code>MultipartonInteractions:bProfile = 4</code> has 
been introduced for the impact-parameter profile of protons in the 
<?php $filepath = $_GET["filepath"];
echo "<a href='MultipartonInteractions.php?filepath=".$filepath."' target='page'>";?>Multiparton Interactions</a> 
framework, wherein low-<i>x</i> partons are spread over a larger area 
than high-<i>x</i> ones, see [<a href="Bibliography.php" target="page">Cor11</a>]. A new method 
<code>Info::eMPI(i)</code> gives back the enhancement factor related to 
the <code>i</code>'th interaction. The impact-parameter profile can now 
be selected separately for <?php $filepath = $_GET["filepath"];
echo "<a href='Diffraction.php?filepath=".$filepath."' target='page'>";?>diffraction</a>, 
but without a <code>bProfile = 4</code> option. 
</li> 
 
<li>The possibility of "hadronization" in the 
<?php $filepath = $_GET["filepath"];
echo "<a href='HiddenValleyProcesses.php?filepath=".$filepath."' target='page'>";?>Hidden Valley</a> sector 
has been added as a new option <code>HiddenValley:fragment = on</code>. 
This is based on a copy of the standard string fragmentation framework, 
but with the option of a completely separate "flavour" sector, and 
separately tunable longitudinal fragmentation functions and transverse 
momenta. For now only a simple flavour scenario is provided, where 
flavour-diagonal mesons can decay back into the normal visible sector 
while off-diagonal ones remain hidden. A writeup of the model is in 
preparation [<a href="Bibliography.php" target="page">Car11</a>]. Also some further Hidden Valley options 
have been added.</li> 
 
<li>Included possibility in <code>TimeShower</code> for QCD dipoles to 
have an adjustable normalization factor, via the new 
<code>bool isFlexible</code> and <code>double flexFactor</code> 
properties of <code>class TimeDipoleEnd</code>. This flexibility is used 
to treat radiation off colour topologies with epsilon tensors, in 
particular colour junctions which have all their partons in the 
final state. (An example is the BNV-SUSY decay ~chi0&rarr;uds.) For 
such topologies, the new treatment in PYTHIA 8 is that a 
half-strength dipole is spanned between all combinations of 
final-state quarks. For junctions with an incoming (anti)colour line (such 
as in the BNV-SUSY decay ~t*&rarr;cb), a full-strength dipole is 
instead spanned between the two daughters, with no radiation from the 
decaying object (in its CM). As will be described in a forthcoming 
paper with N. Desai, this should give the 
closest possible correspondence to the radiation patterns exhibited 
by higher-order matrix elements for this type of process.</li> 
 
<li>Corrected harmless bugs in <code>ProcessLevel::findJunctions</code> 
which caused junctions of types 3 and 4 (i.e., junctions with 1 
incoming color tag) to sometimes be incorrectly classified as types 
1 and 2 (with all color tags outgoing), respectively. 
Since the parity (even/odd) of the junction kind was still correct, 
however, this did not cause any problems at the hadronization 
stage.</li> 
 
<li>The junction-finder in <code>ProcessLevel::findJunctions</code> 
has been made more stable, and the following ordering of the color 
tags returned by <code>Event::endColJunction(iJun, iLeg)</code>  is 
now enforced: for junction kinds 3 and 4 (one tag 
incoming), <code>iLeg = 0</code> will return the 
incoming tag, while for kinds 5 and 6 (two tags incoming), 
<code>iLeg = 0</code> and <code>iLeg = 1</code> will return the 
incoming tags. Apart from this ordering, the color tags are 
ordered in ascending color tag number.</li> 
 
<li>Declarations of friend functions moved to sit outside the class 
it befriends. Thanks to Axel Naumann for pointing out the incorrect 
previous construction.</li> 
 
<li>The <code>rootexample</code> subdirectory becomes 
<code>rootexamples</code> as the old example is split into two, 
with improved documentation to clarify usage. Thanks to Axel Naumann 
and Bernhard Meirose for the new code.</li> 
 
<li>In the Les Houches Event File machinery, input type has been 
changed from <code>ifstream</code> to <code>istream</code> for more 
flexibility.</li> 
 
<li>R-hadron  handling is largely implemented, see the 
<?php $filepath = $_GET["filepath"];
echo "<a href='RHadrons.php?filepath=".$filepath."' target='page'>";?>R-hadrons</a> page, although some aspects 
still need polishing. A new class <code>RHadrons</code> takes 
care of the hadronization and decay. Particle data have been added 
for R-hadrons containing a gluino, stop or sbottom, but could 
alternatively be used for other long-lived coloured particles. 
Thanks to Bernhard Meirose for support. 
</li> 
 
<li>Calculations of SUSY decay widths included by 
  N. Desai, contained in the new source 
files <code>SusyResonanceWidth.cc</code> 
  and </code>.h</code>. Validation and addition of more modes is still in 
  progress, so this implementation should be considered preliminary 
  for now. For a list of available modes, see 
  the <?php $filepath = $_GET["filepath"];
echo "<a href='SUSYProcesses.php?filepath=".$filepath."' target='page'>";?>SUSY Processes</a> page. 
</li> 
 
<li>A first implementation of the SLHA-based QNUMBERS interface for 
  defining new exotic particles has been 
  included. See [<a href="Bibliography.php" target="page">Alw07</a>] and 
  the <?php $filepath = $_GET["filepath"];
echo "<a href='SUSYLesHouchesAccord.php?filepath=".$filepath."' target='page'>";?>SUSY Les Houches Accord</a> 
  page. </li> 
 
<li>Read-in of LHEF events containing Baryon Number Violating vertices 
  has been included, using colour junctions, see 
  the <?php $filepath = $_GET["filepath"];
echo "<a href='EventRecord.php?filepath=".$filepath."' target='page'>";?>Event Record</a> page. The advanced 
  shower model taking into account the full colour structure of such 
  events, developed by N. Desai and described above, 
  is turned on by default in such events. 
  Several test cases were used to 
  validate this implementation, but it is possible that problems could 
  still exist for some cases. Feedback is welcome. Thanks to 
  the MadGraph authors for providing several test cases.</li> 
 
<li>Some first steps taken to allow events containing colour sextets, 
  e.g., from semi-internal processes or LHEF interfaces, and/or from 
  SLHA <code>DECAY</code> tables. New 
  <code>colType</code> codes 3 and -3 are introduced to describe 
  sextets and antisextets, respectively. The state of the current 
  implementation is that hard processes containing such states can be 
  read in and decays generated (via <code>DECAY</code> tables). Parton 
  showers can be added, but the sextets themselves do not, as yet, 
  shower, and any undecayed sextets remaining at the hadronization 
  stage would lead to unpredictable problems if hadronization is 
  switched on. Thanks to J. Alwall for help with testing 
  this implementation. 
</li> 
 
<li>The functionality of the SLHA SUSY/BSM interface (see 
  the <?php $filepath = $_GET["filepath"];
echo "<a href='SUSYLesHouches.php?filepath=".$filepath."' target='page'>";?>SUSY Les Houches</a> page) has been 
  extended so that copies of all <code>BLOCK</code>s are now stored 
  internally as 
  strings, regardless of whether they correspond to "official" SLHA 
  blocks or not. Their contents can subsequently be queried by a set 
  of new templated member functions of the 
  class <code>SusyLesHouches</code>. Available query functions so far 
  include 
  <br/><code>template &lt;class T&gt; bool getEntry(string, T&);</code>, 
  <br/><code>template &lt;class T&gt; bool getEntry(string, int, T&);</code>, 
  <br/><code>template &lt;class T&gt; bool getEntry(string, int, int, T&); 
  </code>, and 
  <br/><code>template &lt;class T&gt; bool getEntry(string, int, 
  int, int, T&);</code>, 
  <br/>where the type of 
  the argument used in the call determines how to read the block 
  entries. Thus, if an LHEF, SLHA, or other card file is read in by 
  PYTHIA, the contents of any <code>BLOCK</code> in that file can 
  subsequently be queried run-time using these functions. This is 
  used, e.g., in the new interface between MADGRAPH 5 and PYTHIA 
  8. Thanks to the MADGRAPH team, and to J. Alwall in particular, for 
  help and debug on this new implementation. 
</li> 
 
</ul> 
</li> 
 
<li>8.153: 10 August 2011 
<ul> 
 
<li>The setup of tunes has been modified, see the 
<?php $filepath = $_GET["filepath"];
echo "<a href='Tunes.php?filepath=".$filepath."' target='page'>";?>Tunes</a> page for details. Specifically 
the <code>Tune:ee</code> and <code>Tune:pp</code> modes have 
acquired a new option <code>-1</code> for a forced restore to the 
default values of all settings used in the respective kind of 
tunes.</li> 
 
<li>The code for handling multiparton interactions in the scenario with 
an <i>x</i>-dependent width of the Gaussian matter profile 
[<a href="Bibliography.php" target="page">Cor11</a>], has been improved and updated. Tune 4Cx, which 
is based on this option, has been added as a further tune option.</li> 
 
<li>A possibility to bias the phase-space selection has been added, 
whereby some phase space regions can be oversampled, which is 
compensated by assigning a weight to each event. 
A new set of methods have been added to the <code>UserHooks</code> 
class to allow users to program how to bias the selection.</li> 
 
<li>New options added so that matrix-element corrections can be 
switched off after the first branching in ISR or FSR.</li> 
 
<li>Some new <code>Info</code> methods have been added to store 
information on latest ISR branching.  The <code>SpaceShower</code> 
class has also been corrected so that the latest <i>z = 1/2</i> 
when not defined by history. This avoids undefined values for 
azimuthal asymmetries. Thanks to Stefan Prestel for finding and 
sorting out this problem.</li> 
 
<li>The <code>Pythia::forceHadronLevel()</code> method now takes 
an argument that optionally means that existing junction information 
is not overwritten. Thanks to Leif L&ouml;nnblad for pointing out 
the usefulness of this.</li> 
 
<li>For particle decays to a varying number of hadrons the multiplicity 
increase with mass has been somewhat reduced, and especially for 
<code>meMode = 23</code> a separate even slower increase has been 
introduced.</li> 
 
<li>New possibility to force the tau polarization.</li> 
 
<li>Bug fix for the special case in which events containing SUSY 
particles are generated without proper initialization of SUSY decays. 
This can happen, e.g., if events containing SUSY particles are read 
in via external LHEF files, if those files do not contain readable 
SLHA spectra in their headers. In this case, a failed attempt to 
generate sparticle decays with ill-defined couplings previously 
led to crashes. The program now reverts to the old behaviour, 
based on static decay tables, in such cases, with the default 
being to decay all sparticles to gravitino + particle. An error 
message stating that the full SUSY treatment has been switched 
off and why is also issued. Thanks to N. Desai for this fix.</li> 
 
<li>The R-hadron machinery has been completed.</li> 
 
<li>Minor change in timelike showers: gluons which fall below the 
<i>pTmin</i> cutoff by the correction for mass effects are now 
eliminated, while previously they were kept. This reduces the 
number of gluons somewhat, but has no significant effects on the 
hadronic final state. (Prompted by R-hadron studies, where big mass 
effects else give bothersome low-energy gluons.)</li> 
 
<li>Corrected typos in two of the Upsilon wave function matrix 
elements, <code>Bottomonium:OUpsilon3P08</code> and 
<code>Bottomonium:Ochib03P01</code>. Thanks to Beate Heinemann for 
pointing it out.</li> 
 
<li>Implemented <i>f*</i> decay angle in <i>f f &rarr; f f*</i> processes, 
and fixed some bugs for the same process. Thanks to Piyali Banerjee 
for tests.</li> 
 
<li>Introduce possibility to separate production channels for 
TeV-sized extra dimensions, and a small code correction. Thanks to 
Noam Hod for code contribution.</li> 
 
<li>Bug fix for neutrino beams: since neutrinos are always lefthanded 
there should be a factor 2 in the cross section, relative to charged 
leptons, from the non-need to average over incoming spin states. This 
is now fixed by introducing a new PDF class <code>NeutrinoPoint</code> 
for (unresolved) neutrinos, with normalization 2 rather than 1 for 
charged leptons. Thanks to Ryosuke Sato.</li> 
 
<li>Bug fix for some (rarely used) particle settings, which could not 
be changed by users because they were read too early. Thanks to 
Andrew Altheimer and Gustaaf Brooijmans.</li> 
 
<li>Bug fix in handling of <code>tau</code> decays, where setting of 
decay vertices could write outside memory. Thanks to Steven Schramm.</li> 
 
<li>Minor expansion of the <code>BeamParticle</code> constructor.</li> 
 
<li>Minor bug fix in CTEQ 6L for uninitialized variables.</li> 
 
<li>Minor bug fix in fragmentation flavour combination to hadron.</li> 
 
<li>Some updates of the documentation, including new pages on 
MadGraph 5 as a generator for semi-internal processes (thanks to 
Johan Alwall) and on RIVET interfacing.</li> 
  
<li>Minor improvements of ROOT- and HepMC-related code and documentation.</li> 
 
<li>Some cleanup of code.</li> 
 
</ul> 
</li> 
 
<li>8.157: 10 November 2011 
<ul> 
 
<ii>Stefan Prestel joins as new member of the PYTHIA author team. 
He is mainly working on matching/merging issues, and has contributed 
the CKKW-L machinery mentioned below.</li> 
 
<li>Merging capabilities according to the CKKW-L scheme [<a href="Bibliography.php" target="page">Lon11</a>] 
have been added, see the new 
<?php $filepath = $_GET["filepath"];
echo "<a href='MatrixElementMerging.php?filepath=".$filepath."' target='page'>";?>Matrix Element Merging</a> 
page. It involves new classes to reconstruct and select how a 
matrix-element-generated partonic configuration could have been obtained 
by the PYTHIA evolution, and to perform trial showers (using a new 
copy of the normal parton-level machinery) that introduce the appropriate 
Sudakov suppression factors. New examples to illustrate various 
common tasks are found in <code>main81.cc</code>, <code>main82.cc</code>, 
<code>main83.cc</code> and <code>main84.cc</code>.</li> 
 
<li>The <code>main71.cc</code> program is now generalised for reading 
in arbitrary POWHEG LHE files, to implement a smooth matching between NLO 
matrix elements and the PYTHIA parton showers. Cuts are made on both 
ISR and FSR emissions, while previously only ISR was covered. The key 
assumptions are the <i>pT</i> definitions used for ISR and FSR, but several 
further options are available, as documented in the <code>main71.cmnd</code> 
file. Work is still ongoing to pick the best default options. </li> 
 
<li>Several expansions of the <code>UserHooks</code> class. 
For both <code>doVetoFSREmission</code> and <code>doVetoISREmission</code> 
a new argument <code>iSys</code> labels the system within which the 
radiation occurs. For <code>doVetoFSREmission</code> a further argument 
<code>inResonance</code> distinguishes FSR in resonance 
decays from that in the hard process itself. New methods 
<code>canVetoMPIEmission</code> and <code>doVetoMPIEmission</code> 
have been added to veto multiparton interactions in the same way as 
FSR and ISR. In <code>biasSelectionBy</code> at times incorrect values 
for <code>inEvent</code> has been corrected. Also some systematization 
of the use of <code>const</code>.</li> 
 
<li>A new model for hadron scattering is introduced, still at an early 
stage and therefore more intended for internal development than for the 
normal user. The basic idea is that a high-energy <i>pp</i> collision 
involves the fragmentation of multiparton strings that overlap in space 
(and time). Also the produced hadrons therefore initially overlap, and 
there is a strong likelihood that hadrons can scatter against each other 
until the system has expanded sufficiently. This can e.g. increase the 
transverse momentum of heavier hadrons at the expense of lighter ones. 
</li> 
 
<li>A new jet finder <code>SlowJet</code> offers simple access to the 
inclusive <i>kT</i>, anti-<i>kT</i>, and Cambridge/Aachen 
algorithms in a cylindrical coordinate frame. The jet reconstruction 
is then based on sequential recombination with progressive removal, 
using the <i>E</i> recombination scheme. The minimalistic 
<code>SlowJet</code> code is much slower than <code>FastJet</code> 
[<a href="Bibliography.php" target="page">Cac06</a>], and contains less options, but reconstructs the same 
jets if run under identical conditions. For details see the 
<?php $filepath = $_GET["filepath"];
echo "<a href='EventAnalysis.php?filepath=".$filepath."' target='page'>";?>Event Analysis</a> page.</li> 
 
<li>Starting in gcc 4.6, it is possible to switch off specific warnings 
around specific blocks of code. Although version 4.6 is some way off 
from being commonly found, this option has now been introduced to switch 
off <code>-Wshadow</code> warnings in <code>HepMCInterface.cc</code>. 
For other compilers, or earlier versions of gcc, the old behaviour 
is retained.</li> 
 
<li>Support for reading in gzipped LHEFs has been added in the 
<code>LesHouches</code> and <code>SusyLesHouches</code> classes. 
This also affects <code>configure</code> and <code>Makefile</code>s. 
Unless explicitly enabled, it should not affect anything. 
If enabled, it relies on the Boost and zlib libraries to function, 
so paths to these must be set appropriately, see the 
<code>README</code> file for details. The Boost header files can 
give very many shadow warnings, so <code>-Wshadow</code> is disabled 
if gzip support is enabled, as described above.</li> 
 
<li>Introduce use of the <code>HEPMC_HAS_UNITS</code> environment 
variable in the <code>HepMCInterface.cc</code> and <code>main32.cc</code> 
codes to automatically check whether GeV and mm can be set as relevant 
units. If yes, then it is set in <code>main32.cc</code>. If no, 
a conversion from GeV to MeV is done explicitly in 
<code>HepMCInterface.cc</code>. Note that, for early HepMC versions, 
this means a change of behaviour. Thanks to Andy Buckley.</li> 
 
<li>A new method <code>Info::lhaStrategy()</code> returns the 
Les Houches event weighting strategy where relevant, and 0 where not. 
For the strategies <i>+-4</i> the event weight and sum, 
<code>Info::weight()</code> and <code>Info::weightSum()</code>, 
is now in units of pb at output, as it should be at input. 
(While internally mb is used.)</li> 
 
<li>New flag <code>Check:abortIfVeto</code> allows the user to 
resume control over execution in case of a veto in the event 
generation process.</li> 
 
<li>New method <code>Event::at(i)</code> returns reference to 
<code>i</code>'th particle in the event record.</li> 
 
<li>Introduce the Fermi constant as one of the Standard Model 
parameters.</li> 
 
<li>Included automatic pre-initialization of SLHA blocks MASS and 
SMINPUTS using PYTHIA's SM parameters and particle data table 
values.</li> 
 
<li>SUSY: added sleptons (and sneutrinos) as resonances. 
Corrected 2-body decay widths of gluinos, squarks and gauginos. 
Added sleptonic decay modes to gaugino decays. 
Implemented 2-body decays of sleptons (and sneutrinos) 
into sleptons/leptons/gauginos. Corrected slepton couplings (they 
now look exactly like squark couplings, using a 6x6 slepton mixing 
matrix).</li> 
 
<li>New parameter <code>Diffraction:probMaxPert</code> introduce to 
give more flexibility in transition from a nonperturbative to a 
perturbative description of a diffractive system.</li> 
 
<li>Bug corrected in <code>SigmaEW.cc</code>, for the process 
<code>WeakSingleBoson:ffbar2ffbar(s:gm)</code>. Previously, all outgoing 
quark flavours in this process were erroneously assigned ID code 5 
(b quarks). This has now been corrected so the proper ratios are 
obtained, with <i>u : c : d : s : b = 4 : 4 : 1 : 1 : 1</i>, 
according to the squared quark charges.</li> 
   
<li>Changes in <code>FragmentationSystems</code> and 
<code>StringFragmentation</code> to reject systems with three or more 
interconnected junctions. This may happen in baryon-number-violating 
scenarios in pp collisions, if both of the beam baryon junctions are 
resolved and get connected via an antijunction created by a 
hard-process BNV vertex. In principle, this could be addressed by an 
extension of the existing junction fragmentation framework. However, 
since it happens rarely, for the time being an error message is printed 
and the fragmentation restarted. 
</li> 
 
<li>Change in <code>Event::copy</code> to safeguard against attempting 
to copy out-of-range entries. An attempt to copy a non-existing entry 
will now return -1.</li> 
 
<li>Bug fix in the machinery for the user to force the setting of 
tau polarization.</li> 
 
<li>Bug fix in the initial search for a maximum of the process 
cross section (only affecting rare cases).</li> 
 
<li>Corrected angular decay distributions for two compositeness 
processes, <code>Sigma2qq2qStarq</code> and 
<code>Sigma2qqbar2lStarlbar</code>.</li> 
    
<li>Extra check in <code>HiddenValleyFragmentation</code> to make sure 
that pointers which have not been assigned with <code>new</code> are not 
deleted.</li> 
 
<li>Some further minor changes.</li> 
 
</ul> 
</li> 
 
<li>8.160: 23 January 2012 
<ul> 
 
<li>The older term "multiple interactions" has been replaced by the 
new standard "multiparton interactions" one, and correspondingly the 
abbreviation MI by MPI. This affects everything: settings, class and 
method names, documentation, etc. It therefore becomes necessary to 
rewrite user code. However, so as to keep the immediate effort at a 
reasonable level, the old settings names are kept as aliases. Thus 
<code>PartonLevel:MI</code> and <code>PartonLevel:MPI</code> are 
equivalent, and similarly <code>MultipleInteractions:pT0Ref</code> 
and <code>MultipartonInteractions:pT0Ref</code>. Should you be using 
methods such as <code>double Info::pTMI(int i)</code> you need to 
edit the code, however. All the <code>example/main*</code> files 
have been updated accordingly.</li> 
 
<li>A new option <code>Beams:frameType = 5</code> has been added 
for the case where an external generator should provide LHA process 
information. The new <code>Pythia::setLHAupPtr(...)</code> method 
should then be used to link in this generator. The new switch 
<code>Beams:newLHEFsameInit</code> can be used to tell that a new 
LHEF should be used, but without the need for a new initialization. 
With these two changes, all the different ways of initializing 
can be covered by the <code>Pythia::init()</code> call with no 
arguments. The various <code>init(...)</code>options with arguments 
are deprecated and will be removed for PYTHIA 8.2.</li> 
 
<li> The <code>Pythia::stat()</code> method, with no arguments, 
replaces <code>Pythia::statistics(...)</code>, although the latter 
remains as a deprecated alternative.</li> 
 
<li>New settings <code>Init:...</code>, <code>Next:...</code>, 
and <code>Stat:...</code> can be used to steer some details of 
the operation of the <code>Pythia::init()</code>, 
<code>Pythia::next()</code> and  <code>Pythia::stat()</code>, 
respectively. In particular it affects the amount of printout 
at the various stages of operation. See 
<?php $filepath = $_GET["filepath"];
echo "<a href='MainProgramSettings.php?filepath=".$filepath."' target='page'>";?>here</a> for further details. 
This change involves several code changes, but in documentation 
rather than physics. Most of the <code>Main:...</code> settings 
are deprecated as a consequence.</li> 
 
<li>The sample main programs in the <code>examples</code> subdirectory 
have been updated. This includes a change to the new favoured 
methods and settings outlined above, plus some update of the 
physics contents. Some of the examples have been combined, 
some others have been added (e.g. for R-hadrons), and as a consequence 
some renumbering has been made. See the modified list of 
<?php $filepath = $_GET["filepath"];
echo "<a href='SampleMainPrograms.php?filepath=".$filepath."' target='page'>";?>sample main programs</a> for 
the new status. In particular note that (the new) <code>main61.cc</code> 
allows streamlined input and output in HepMC, like <code>main42.cc</code> 
(previously <code>main32.cc</code>), but additionally links to LHAPDF. </li> 
 
<li>The <code>examples/configure</code> script has new optional argument 
<code>--with-pythia8</code>. It can be used to set the new 
<code>PYTHIA8LOCATION</code> environment variable, which then is used 
in <code>examples/Makefile</code> to give the path to the PYTHIA 
library. Thereby it becomes possible to relocate (parts of) the 
<code>examples</code> directory and still obtain the correct path. 
Thanks to Mikhail Kirsanov.</li> 
 
<li>Polarization information has been included as a new 
<code>Particle</code> property, that can be set by 
<code>void pol(double polIn)</code> and obtained by 
<code>double pol()</code>. Default value is <code>9.</code>, 
in agreement with the Les Houches standard. Event listings have 
been expanded to optionally display this information. Currently 
polarization is not used internally.</li> 
 
<li>The matrix element merging machinery has been modified as follows. 
<br/>- Improved handling of the hard process, so that MadGraph5-produced 
LHE files do not produce problems (sometimes, the choices of MG4 on what 
to put into a LHEF have changed in MG5). 
<br/>- Improved handling of colour for easier handling of states with 
many quarks (the code has been tested for states with up to 5 
<i>q qbar</i> pairs and some gluons). 
<br/>- Improved checking which clusterings are allowed. Now, clusterings 
should immediately be rejected if they lead to unphysical states, 
without the need to explicitly construct these states. This was needed 
to get <i>t tbar</i> production, VBF and some simple SUSY processes 
running with reasonable generality. 
<br/>- Improved handling of incomplete histories. Now there is an 
additional switch allowing code to try to swap some colours when fewer 
clusterings than requested have been found. 
</li> 
 
<li>Updates in the handling of graviton resonances in scenarios with 
extra dimensions. 
<br/>- Now all G decays have correct angular distribution, which was not 
the case before for W/Z decays. 
<br/>- Added the possibility for G to only couple to longitudinal 
W/Z bosons, which affects both the width and angular distributions. 
<br/>- Added decays to Higgs pairs, <i>G &rarr; hh</i>. 
</li> 
 
<li>Bug fix in the <code>findJunctions()</code> function in 
<code>ProcessLevel.cc</code>. Changes introduced in version 
8.145 (to allow for junctions in baryon-number-violating 
processes) did not correctly handle the remapping of color 
tags that can be applied when adding beam remnants to events with 
multiparton interactions. A simpler and more stable algorithm is 
now applied for the simplest cases, with the more complex one only 
invoked for cases such as BNV, which are normally considered before 
remnants are added, hence avoiding this particular problem. Also some 
other changes in the handling of junction, including a safeguard to 
reject systems with three or more interconnected junctions.</li> 
 
<li>Bug fix and updates to the SLHA interface. Possibility of infinite 
loop during <code>ProcessLevel::initSLHA</code> corrected. Modifications 
to <code>SusyLesHouches</code> to allow interpretation of SLHA1 spectra 
with R-parity violation. Rather than reject spectra that do not conform 
fully to the SLHA2 standard for RPV, the interface will now first look 
for SLHA1 mixing matrices and attempt to translate those into SLHA2 ones 
if possible. In this case, warnings that the expected SLHA2 blocks were 
not found will be printed. It is up to the user to check that the 
derived SLHA2 information is correct. Corresponding additions to 
<code>SusyCouplings.cc</code> to extract mixing-matrix information 
from the relevant SLHA2 RPV blocks when RPV is switched on.</li> 
 
<li>Updated SLHA example <code>main24.cmnd</code> to use new example 
spectrum, <code>cmssm.spc</code>, corresponding to CMSSM point 
10.1.1, obtained with SOFTSUSY 3.3.0. Thanks to B. Allanach for 
providing the new spectrum file.</li> 
 
<li>A new <code>include/FastJet3.h</code> header file simplifies 
interfacing of FastJet to Pythia. For details see documentation in 
the file itself. Thanks to Gavin Salam for this contribution.</li> 
 
<li>The handling of diffraction has been made more flexible. 
Specifically the "total" Pomeron-proton cross section can now be made 
to depend on the mass of the diffractive system. This does not affect 
the diffractive cross section in pp collisions, which is set separately, 
but is used in the MPI machinery to affect the average number of 
interactions per Pomeron-proton collision. Furthermore, the allowed 
range for some parameters has been expanded. Thanks to Robert 
Ciesielski.</li> 
 
<li>New method <code>Pythia::forceTimeShower(...)</code> can be used to 
generate a single final-state cascade from a set of partons, without 
any knowledge of prior history. This is mainly intended for toy studies. 
The meaning of the <code>ProcessLevel:all</code> switch has been modified 
so that this parton-level function is available, but not any others 
at this level. 
</li> 
 
<li>ATLAS tune A2 now included. Thanks to Deepak Kar for 
providing it.</li> 
 
<li>Change in the handling of recoils of timelike showers in resonance 
decays, in cases of coloured resonances such as <i>t &rarr; b W</i>. In 
the first step the <i>W</i> always acts as recoiler to the <i>b</i>, 
but in subsequent step previously the <i>W</i> remained as recoiler to 
one dipole, while now all QCD-radiating partons recoil against another 
coloured parton. The old behaviour could give a (small) unphysical spike 
of radiation collinear with the colourless recoiler in the subsequent 
emissions (while the pattern of the first was and remains correct). 
The old behaviour can be recovered for checks, see 
<code>TimeShower:recoilToColoured</code>. 
Thanks to Yevgeny Kats for pointing out this issue (previously noted 
for Pythia 6 by several persons). 
</li> 
 
<li>Bug fixes for <code>rootexamples/Makefile</code> and restoration 
of some deleted information in <code>rootexamples/README</code>. 
Thanks to Axel Naumann and Bernhard Meirose.</li> 
 
<li>Bug fix, so that displaced vertices are possible in resonance 
decay chains. Thanks to Daniel Blackburn and Andy Buckley for 
pointing this out.</li> 
 
<li>Bug fix for <code>Info</code> counter 2.</li> 
 
<li>Bug fix so that sextet quarks now are read in correctly from 
Les Houches Event files; previously the (anti)colours were set 
to 0.</li> 
 
<li>Year updated to 2012 in copyright statements etc.</li> 
 
<li>Reformatting to reduce the number of code lines with more 
than 79 characters.</li> 
 
<li><code>Pythia::initSLHA()</code> moved from <code>public</code> 
to <code>private</code>. 
</li> 
 
<li>Remove warning message when tau polarization is set by hand.</li> 
 
<li>Several minor changes to reduce the number of warnings issued 
by the clang compiler. The origin of some warnings remains unclear 
so those remain to track down (could also be compiler bugs; note that 
we discuss warnings, not errors). Thanks to Randy MacLeod for bringing 
this up.</li> 
 
<li>The worksheet has been updated to be in step with Pythia 8.160.</li> 
 
<li>Several other minor corrections in the code and documentation.</li> 
 
</ul> 
</li> 
 
<li>8.162: 12 March 2012 
<ul> 
 
<li>A new option allows for several partons to share the recoil in 
final-state radiation, see<?php $filepath = $_GET["filepath"];
echo "<a href='TimelikeShowers.php?filepath=".$filepath."' target='page'>";?>Timelike 
Showers</a>. It is mainly intended to be used in the context of 
matching to matrix elements, and so only to be used in the first 
few branchings.</li> 
 
<li>Several new processes for LED dijet production, see 
<?php $filepath = $_GET["filepath"];
echo "<a href='ExtraDimensionalProcesses.php?filepath=".$filepath."' target='page'>";?>Extra Dimensions</a>.</li> 
 
<li>Small update of the <code>Sigma2ffbar2LEDllbar</code> and 
<code>Sigma2ffbar2LEDgammagamma</code> LED processes.</li> 
 
<li>New <code>Sigma2QCffbar2llbar</code> 
<?php $filepath = $_GET["filepath"];
echo "<a href='CompositenessProcesses.php?filepath=".$filepath."' target='page'>";?>contact interaction</a> 
process.</li> 
 
<li>Inclusion of a new method in the 
<?php $filepath = $_GET["filepath"];
echo "<a href='MatrixElementMerging.php?filepath=".$filepath."' target='page'>";?>matrix-element merging</a> 
framework to influence the construction of histories, e.g. to already 
in the construction of histories disallow paths that fail the 
<i>2 &rarr; 2</i> cuts.</li> 
 
<li>Further minor updates of the matrix-element merging code, 
mainly for improved clarity.</li> 
 
<li>Minor bug fix in the handling of beam and event information fed 
in from an <code>LHAup</code> instance.</li> 
 
<li>Minor bug fix for potential crashes from uninitialized variables 
for the merging machinery when merging is not used.</li> 
 
<li>Updated History class for matrix element merging, 
also avoiding some compiler warnings.</li> 
 
<li>Bug fixes in the handling of correlated mass choices in resonance 
decays, such as <i>H &rarr; Z^*0 Z^*0</i>.</li> 
 
<li>Bug fix when the process-level execution is switched off, 
where <code>Info::isResolved()</code> could be called before its 
value was initialized. Thanks to Christian Pulvermacher 
for finding this.</li> 
 
<li>Corrected matrix-element expression for mass selection in 
<i>A^0 &rarr; Z^*0 Z^*0</i> and <i>A^0 &rarr; W^*+ W^*-</i>.</li> 
 
<li>SM Higgs mass updated to 125 GeV and default width and branching 
ratios modified accordingly. Minor technical improvements of width 
calculation.</li> 
 
<li>The usage of nested classes has been removed from 
<code>SusyLesHouches</code>, since it could give compilation errors 
on some platforms. The new class names begin with <code>LH</code> 
and all classes have been put inside the <code>Pythia8</code> 
namespace.</li> 
 
<li>Minor Makefile updates. Thanks to Mikhail Kirsanov.</li> 
 
<li>Minor changes to avoid some clang compiler warnings on the 
Mac OS X platform.</li> 
 
</ul> 
</li> 
 
<li>8.163: 27 March 2012 
<ul> 
 
<li>New methods in the 
<?php $filepath = $_GET["filepath"];
echo "<a href='EventInformation.php?filepath=".$filepath."' target='page'>";?><code>Info</code></a> class, 
<code>id1pdf()</code>, <code>id2pdf()</code>, <code>x1pdf()</code> 
and <code>x2pdf()</code>, to denote the partons for which parton 
distribution values have been defined. Previously this was assumed 
to agree with the incoming partons to the hard process, the same 
methods without the <code>pdf</code> qualifier. However, now the 
POWHEG approach offers a counterexample. Also the reading and 
handling of Les Houches (and other) events, and the interface 
to HepMC, has been modified accordingly. 
</li> 
 
<li>The decay of Higgs and top resonances read in from Les Houches 
Event files is now performed with angular correlations as for 
internal processes. LHE files should normally contain all process-specific 
resonance decay chains and, if not, decays are made isotropic. 
The <i>H &rarr; WW/ZZ &rarr; f fbar f' fbar'</i> and 
<i>t &rarr; b W &rarr; b f fbar</i> correlations are process-independent, 
however, and thus can be handled internally. If part of the decay 
chain has already been set, e.g. <i>H &rarr; WW/Z></i> or 
<i>t &rarr; b W</i>, then the subsequent decays are still isotropic.</li> 
 
<li>Updated instructions how to link to HepMC, 
in <code>README.HepMC</code>.</li> 
 
<li>Bug fix in the turn-on of resolved diffraction for low 
CM energies. Thanks to Erwin Visser.</li> 
 
<li>Bug fix in the handling of string junctions at very high energies, 
caused by numerical errors. Thanks to Erwin Visser.</li> 
 
<li>Some other small changes, mainly aesthetics.</li> 
 
</ul> 
</li> 
 
<li>8.165: 8 May 2012 
<ul> 
 
<li>The MBR (Minimum Bias Rockefeller) model for single, double and 
central diffraction [<a href="Bibliography.php" target="page">Cie12</a>] is included as new option 
<?php $filepath = $_GET["filepath"];
echo "<a href='Diffraction.php?filepath=".$filepath."' target='page'>";?><code>Diffraction:PomFlux = 5</code></a>. 
It is specifically intended for <i>p p</i> and <i>pbar p</i> 
interactions, and is currently the only option that also supports 
central diffraction. Thus the basic machinery for Central Diffraction 
(a.k.a. Double Pomeron Exchange) has now been implemented. 
See <code>examples/main04.cc</code> for an example. 
Thanks to Robert Ciesielski for contributing the new code. 
</li> 
 
<li>For a <i>tau</i> lepton in an external process, by default the 
SPINUP number in the Les Houches Accord now is interpreted as giving 
the <i>tau</i> helicity, and is used for its decay.</li> 
 
<li>A <i>tau</i> coming from a <i>W</i> now defaults to being purely 
lefthanded when neither of the existing matrix elements apply.</li> 
 
<li>Decay mode <i>t &rarr; H+ b</i> included as an option.</li> 
 
<li>Four ATLAS tunes have been implemented as options for 
<code>Tune:pp</code>: A2-minbias-mstw2008lo, AU2-cteq6l1, AU2-mstw2008lo, 
and AU2-ct10. The new flag <code>Tune:preferLHAPDF</code> can be used 
to switch between the LHAPDF and the internal implementation of a 
PDF set, in cases where both are available.</li> 
 
<li>Reorder libraries in <code>examples/Makefile</code>, specifically 
move <code>LIBGZIP</code> so that it is properly linked when used. 
Thanks to Erik Schnetter.</li> 
 
<li>Minor modification so that LHAPDF can be used for PDFs in the hard 
process, with one of the built-in PDFs for the rest, even if LHAPDF is 
compiled so as to handle only one concurrent PDF set.</Li> 
 
<li>Bug fix, that <code>ParticleDecays:mixB = off</code> did not 
switch off <i>B0 - B0bar</i> and <i>Bs0 - Bs0bar</i> mixing. 
Thanks to James Catmore.</li> 
 
<li>Bug fix for the handling of gluon polarization of initial-state 
radiation, where an anisotropic azimuthal distribution was inadvertently 
generated in some <i>2 &rarr; 1</i> processes. The update also includes 
always setting the second daughter zero for the two partons coming in to 
a hard <i>2 &rarr; 1</i> process. Thanks to Antonio Policicchio.</li> 
 
<li>Bug fix, that the setting of the number of user hooks MPI steps did 
not use <code>UserHooks::canVetoMPIStep()</code> properly.</li> 
 
<li>Some other small changes, mainly documentation and aesthetics.</li> 
 
</ul> 
</li> 
 
<li>8.170: 21 September 2012 
<ul> 
 
<li>Streamline default behaviour and options for choice of GeV or MeV 
for output to the HepMC event format, see 
<?php $filepath = $_GET["filepath"];
echo "<a href='HepMCInterface.php?filepath=".$filepath."' target='page'>";?>HepMC Interface</a>. 
Also set the mass of HepMC particles explicitly, rather than having 
it calculated implicitly. Thanks to James Monk and Andy Buckley.</li> 
 
<li>The <i>tau</i> decay machinery has been further augmented with 
matrix elements and form factors for a variety of decay modes, such 
that all modes with a branching ratio above 0.1% are fully modeled. 
Several new classes and methods have been added to this end, 
Also, a <i>tau</i> pair coming from a <i>Z^0</i> decay is now 
handled by assuming the <i>Z^0</i> to be unpolarized when neither 
of the existing matrix elements apply. Taus coming from B baryons are 
handed as for B mesons.</li> 
 
<li>Flavour violating decays have been added to the squark, gluino, 
neutralino and chargino decay tables.</li> 
 
<li>Extend the <code>UserHooks::subEvent(...)</code> method so that it 
also works passably at the process level. Also new option for 
<code>UserHooks::omitResonanceDecays(...)</code>.</li> 
 
<li>New methods <code>UserHooks::canVetoPartonLevelEarly()</code> 
and <code>UserHooks::doVetoPartonLevelEarly( const Event&)</code> 
are intended to be used the same way as the existing ones without 
<code>Early</code> in their names, but allow veto right after 
the ISR + FSR + MPI evolution, before beam remnants are added and 
resonance decays are considered.</li> 
 
<li>Central diffraction now available for all 
<code><?php $filepath = $_GET["filepath"];
echo "<a href='Diffraction.php?filepath=".$filepath."' target='page'>";?>PomFlux</a></code> 
options, not only the MBR model. This has been constructed by analogy 
with the respective assumptions made for single diffraction, but 
includes some arbitrariness. Therefore the cross section is left 
easily rescaleable and, for backwards compatibility with tunes 
that does not contain it, easily possible to switch off, see 
the <?php $filepath = $_GET["filepath"];
echo "<a href='TotalCrossSections.php?filepath=".$filepath."' target='page'>";?>relevant section</a>. 
</li> 
 
<li>Reading of ALPGEN parameter and event files has been added, 
see <?php $filepath = $_GET["filepath"];
echo "<a href='AlpgenAndMLM.php?filepath=".$filepath."' target='page'>";?>ALPGEN and MLM Merging</a>. 
</li> 
 
<li>MLM matching has been added, as a first step for ALPGEN events, 
see <?php $filepath = $_GET["filepath"];
echo "<a href='AlpgenAndMLM.php?filepath=".$filepath."' target='page'>";?>ALPGEN and MLM Merging</a>. 
</li> 
 
<li>The CKKW-L merging machinery has been upgraded in a number of respects. 
<br/>- More thorough treatment of <code>pp>bb~e+e-veve~</code> with 
additional <i>b</i>-jets. 
<br/>- Corrected hard <i>mu_r</i> and <i>mu_f</i> choices for 
dijet and prompt photon. 
<br/>- More ways to define a hard process, e.g. with the LEPTONS and 
NEUTRINO tags. The merging will understand LHE files for mixed processes 
(e.g. <i>W+</i> and <i>W-</i> production together). 
<br/>- More merging scale definitions. 
<br/>- More freedom to generate all possible histories. 
<br/>- Internal check (and cut) on Les Houches events in 
<code>Pythia::mergeProcess</code> if merging scale value of the events 
is below the value given to Pythia by setting <code>Merging::TMS</code>. 
</li> 
 
<li>It now works to have R-parity violating decays of R-hadrons, 
i.e. baryon number violation in a vertex displaced from the primary 
one.</li> 
 
<li>The documentation of diffractive processes by the 
<code><?php $filepath = $_GET["filepath"];
echo "<a href='EventInformation.php?filepath=".$filepath."' target='page'>";?>Info</a></code> 
methods has been expanded and corrected. This also include the 
<code>list()</code> method.</li> 
 
<li>Particle masses and widths have been updated to agree with the 
2012 RPP values [<a href="Bibliography.php" target="page">Ber12</a>]. Thanks to Piotr Zyla for data file 
and James Catmore for program to update the PYTHIA tables from this 
input.</li> 
 
<li>New methods <code>jetAssignment</code> and <code>removeJet</code> 
added to the <code>SlowJet</code> class.</li> 
 
<li>Introduce angular correlation in decay chain 
<i>H &rarr; gamma Z0 &rarr; gamma f fbar</i>. Thanks to Tim Barklow and 
Michael Peskin.</li> 
 
<li>Introduce simple way to bias the selection of <i>2 &rarr; 2</i> 
processes towards larger <i>pT</i> values, with a compensatingly 
decreasing event weight, see 
<?php $filepath = $_GET["filepath"];
echo "<a href='PhaseSpaceCuts.php?filepath=".$filepath."' target='page'>";?>Phase Space Cuts</a>. Only offers a 
subset of the possibilities allowed by <code>UserHooks</code>, 
but simpler to use. The <code>main08.cc</code> program has been 
expanded to illustrate this possibility, and also expanded to 
include low-<i>pT</i> subsamples.</li> 
 
<li>The two remaining non-NLO tunes from [<a href="Bibliography.php" target="page">ATL12</a>] are now 
included.</li> 
 
<li>The <code><?php $filepath = $_GET["filepath"];
echo "<a href='EventInformation.php?filepath=".$filepath."' target='page'>";?>Info</a></code> methods 
<code>nTried, nSelected, nAccepted, sigmaGen</code> and 
<code>SigmaErr</code> now takes the code of an individual process 
as an optional argument.</li> 
 
<li>It is now possible to generate resonance decays, followed by 
showers and hadronization, without having them associated with any 
specific process. This is part of an expanded 
<?php $filepath = $_GET["filepath"];
echo "<a href='HadronLevelStandalone.php?filepath=".$filepath."' target='page'>";?>Hadron-Level Standalone</a> 
machinery, as before triggered by <code>ProcessLevel = off</code>, 
but additionally requiring <code>Standalone:allowResDec = on</code>. 
Input can either be directly into the <code>event</code> 
event record or via a (simplified) Les Houches Event File.</li> 
 
<li>New <code>configure</code> script options <code>--installdir</code>, 
<code>--prefix</code> and <code>--datadir</code> can be used to set the 
location(s) to which the library, header and data directories 
will be copied by a <code>make install</code> subsequent to the 
<code>make</code>. Thanks to Mikhail Kirsanov.</li> 
 
<li>Fix charge in antiparticle name when particle read in from SLHA 
file. Thanks to Johan Alwall.</li> 
 
<li>Pointers now only compared with == and != (not e.g. > 0), to avoid 
warnings in gcc 4.7.</li> 
 
<li>New check that version number of the code matches that of the 
XML files. If not, no events can be generated. Thanks to James Monk 
for suggestion.</li> 
  
<li>New check that mother and daughter indices have been set to give 
a consistent event history. Can be switched on/off with the new 
<code>Check:history</code> flag.</li> 
 
<li>A new method <code>LHAup::newEventFile</code> has been added to 
switch to reading in events from another LHE file without having to 
reinitialize the whole class. Lower-level routines like 
<code>openFile</code> and <code>closeFile</code> have been added to 
handle correct order of operations also when an intermediate gzip 
decompression step is involved.</li> 
 
<li><code>LHAup::eventLHEF()</code> can now be called with an optional 
argument <code>false</code>, to make event files somewhat smaller by 
reducing the amount of blanks.</li> 
 
<li>A new mode <code>Beams:nSkipLHEFatInit</code> introduced to 
skip ahead the first few events in a Les Houches Event File (cf. the 
<code>LHAup::skipEvent(nSkip)</code> method).</li> 
 
<li>Introduce a new pair of user hooks that can be used to reject 
the sequence of hard-process resonance decays, without rejecting 
the production of the primary resonances.</li> 
 
<li>The possibility of separate multiplicative prefactors to the 
renormalization and factorization default <i>pT^2</i> scale has been 
introduced for both timelike and spacelike showers.</li> 
 
<li>Bug fixes in history information for R-hadron production, which also 
fixes HepMC conversion in this case.</li> 
 
<li>Bug corrected in <code>SigmaSusy.cc</code>, for chargino+neutralino 
production. Indexing error for incoming quark states in the process 
<code>Sigma2qqbar2charchi0</code>, resulted in incorrect CKM factors.</li> 
 
<li>Corrected a bug in <code>SusyLesHouches.cc</code>, for NMSSM spectra. 
The unitarity check on the neutralino mixing matrix was faulty, leading 
to erroneous messages about unitarity violations and SUSY being switched 
off.</li> 
 
<li>Bug fixes in the handling of resolved and unresolved diffractive events. 
Thanks to Robert Ciesielski for debug.</li> 
 
<li>Do not set up FSR dipoles for <i>2 &rarr; 1</i> processes.</li> 
 
<li>Check that some channel open for resonance decays. Also further 
check whether resonance decay treatment should be invoked.</li> 
 
<li>Bug fix in reading of particle names from SLHA input.</li> 
 
<li>Change mass, width and decay mode(s) of D*_s(10431). Thanks to 
Michal Petran.</li> 
 
<li>Bug fix in leptoquark production (lepton sign in 
<i>q g &rarr; LQ l</i>).</li> 
 
<li>New argument added to <code>SpaceShower::reassignBeamPtrs</code> 
for diffractive event processing, as already available for 
<code>TimeShower</code>.</li> 
 
<li>Do not write warnings in <code>SpaceShower</code> for weights 
above unity if the evolution scale is below 1 GeV^2.</li> 
 
<li>Add default values for member variables in some constructors, 
and some related changes for <code>AlphaStrong</code> code.</li> 
 
<li>Warn if negative-energy parton in hadronization.</li> 
 
<li>The MPI <i>pT</i> values assumed in the beam remnant setting 
of primordial <i>kT</i> and colour reconnection probability were 
incorrect for diffractive events.</li> 
 
<li>The arrays with MPI information were not reset when parton or 
hadron level fails and a new try is made. Only affected few events.</li> 
 
<li>MPI statistics can not yet be accumulated for diffractive events, 
and therefore the relevant routine is no longer called.</li> 
 
<li>Bug fix in the double parton scattering suppression from 
energy-momentum conservation.</li> 
 
<li>Outgoing proton masses were not set in the event record for 
elastic scattering (but kinematics handling was correct).</li> 
 
<li>Bug fixes in the identification and documentation of junctions, 
previously leading to some unnecessarily rejected events. Also 
other improvements leading to fewer errors.</li> 
 
<li>Slightly increased values for FragmentationSystems:mJoin and 
StringFragmentation::FACSTOPMASS to reduce failure rate, without 
noticeably affecting event properties.</li> 
 
<li>The Les Houches cross section error is now taken into account 
in the final Pythia error for strategies +-3.  New methods 
<code>LHAup::xSecSum()</code> and <code>LHAup::xErrSum()</code> 
provide the necessary information. 
</li> 
 
<li>When a tau pair comes from a massless photon, in dipole shower 
evolution, for the decay description the mother photon is reassigned 
to have the sum of the tau momenta.</li> 
 
<li>Minor change in initialization sequence for user hooks, 
to allow for more flexibility.</li> 
 
<li>Do not print warnings when multiparton interaction weights are 
only slightly above unity.</li> 
 
<li>Do not write warnings for three known particles that are so close 
to threshold that widths are switched off to avoid trouble.</li> 
 
<li>Some minor typographical changes.</li> 
 
</ul> 
</li> 
 
<li>8.175: 18 February 2013 
<ul> 
 
<li>Richard Corke and Stefan Ask leave as active authors, and have 
new e-mail addresses.</li> 
 
<li>Jesper Roy Christiansen and Philip Ilten join as new authors.</li> 
 
<li>A severe bug found and corrected in the handling of junction 
fragmentation. For some string topologies it could lead to parts of 
partonic systems mainly aligned along the beam axis to obtain an 
effective tilt, thereby giving rise to a pair of opposing jets that 
should not be there. The bug was an unfortunate side consequence 
of the improvement of junction handling introduced in version 8.170, 
and do not affect versions prior to that. To the largest extent 
possible, it is recommended to avoid this version. Potential errors 
are likely to be most relevant for processes at low <i>pT</i>, 
whereas processes that already have a large scale proportionately 
are less affected. Sincere apologies, and thanks to Hannes Jung for 
discovering it.</li> 
 
<li>The initialization is aborted when the user tries to change 
the value of a non-existing variable in the Settings or 
ParticleData databases, and it becomes impossible to generate events. 
This may be rather brutal, as opposed to the former policy of 
ignoring such commands (except for a warning), but avoids that 
the user wastes time on a run that is likely not to give the wanted 
results. Thanks to Gavin Salam for stressing this point.</li> 
 
<li> Major updates of the merging code. CKKW-L code is made more flexible, 
and now includes additional options to facilitate merging of additional 
jets in MSSM processes. Unitarised ME+PS merging (UMEPS) is introduced 
as a new merging scheme for tree-level input. An example main program 
for UMEPS is  added. UMEPS is documented in the new section 
<?php $filepath = $_GET["filepath"];
echo "<a href='UMEPSMerging.php?filepath=".$filepath."' target='page'>";?>UMEPS Merging</a>. 
NLO merging methods are now functional. Two NLO merging schemes are 
implemented: NL<sup>3</sup> and unitarised NLO+PS merging (UNLOPS), 
both of which are illustrated with example main programs. NLO merging 
is documented in <?php $filepath = $_GET["filepath"];
echo "<a href='NLOMerging.php?filepath=".$filepath."' target='page'>";?>NLO Merging</a>. 
</li> 
 
<li>The machinery for the MLM-style matching of jets has been 
expanded to cover input either from ALPGEN, as before, or from 
Madgraph (or other LHEF input), and expanded to cover either a 
matching algorithm based on the one in ALPGEN or the one in Madgraph. 
See <?php $filepath = $_GET["filepath"];
echo "<a href='JetMatching.php?filepath=".$filepath."' target='page'>";?>Jet Matching</a> for 
details.</li> 
 
<li>Neutrons and antineutrons are now allowed as incoming 
beam particles. The neutron PDF is derived from the chosen proton 
PDF by simple isospin conjugation, and total/elastic/diffractive 
cross sections are assumed the same as for protons.</li> 
 
<li>Update in <code>Pythia.cc</code>, to allow user to override 
SLHA mass values by hand, controlled by the new flag 
<code>SLHA:allowUserOverride</code>. This was motivated by people 
wanting to be able to read in a base SLHA spectrum and then quickly 
scan over particular mass values manually. Corresponding documentation 
update in <code>SUSYLesHouchesAccord.xml</code>.</li> 
 
<li>Updates to the <code>ResonanceWidths</code> and 
<code>SUSYResonanceWidths</code> classes, to merge common 
initialization steps into the base class. Corresponding documentation 
update in <code>SemiInternalResonances.xml</code>. Also some cosmetic 
changes in <code>SUSYResonanceWidths</code> to improve conformity 
with <code>CODINGSTYLE</code> rules.</li> 
 
<li>The <code>SlowJet</code> jet finder is updated with an option 
for an alternative <i>R</i> separation based on 
<i>cosh(Delta y)-cos(Delta phi)</i>.</li> 
 
<li>Statistics information on the separate subprocesses among 
the Les Houches external input is improved.</li> 
 
<li>The sophisticated tau decay machinery has been expanded so that 
it can also handle production of taus in hypothetical 
lepton-number-violating processes, such as <i>H0 &rarr; tau+ mu-</i>. 
</li> 
 
<li>Branching ratios for most light hadrons, and the tau lepton, 
have been updated to agree with the 2012 Review of Particle Physics 
[<a href="Bibliography.php" target="page">Ber12</a>], by Anil Pratap Singh.</li> 
 
<li>Photon radiation can now be included in leptonic two-body decays 
of hadrons by setting the new switch 
<code>ParticleDecays:allowPhotonRadiation = on</code>. 
The lower shower cutoff <code>TimeShower:pTminChgL</code> has been 
reduced so as to let the simulated photon spectrum extend to lower 
energies. 
</li> 
 
<li>A new switch <code>MultipartonInteractions:bSelScale</code> has 
been introduced, to determine the relevant mass or <i>pT</i> scale 
of an event for the selection of impact parameter in the MPI 
framework. In spite of a changed default behaviour (the old being 
option 3), practical consequences are small for most processes. 
</li> 
 
<li>The new <code>UserHooks::retryPartonLevel()</code> method can be 
overloaded, so that the same hard process is reused for a new try 
on the parton level, rather than being rejected completely. 
Thanks to Christian Bauer for suggestion.</li> 
 
<li>A new method <code>Event::undoDecay(int i)</code> can be used 
to remove the daughters of a particle, and further descendents. 
Does not work for removing the daughters of a parton.</li> 
 
<li>The interface to <code>HepMC</code> has been updated. In particular 
the support for old version has been removed, thereby allowing 
significant simplifications. The list of methods to set and get the 
behaviour of the conversion routine has also been updated. The 
<code>set_event_scale</code> method now stores the renormalization 
scale (rather than the <i>pT</i> stored previously). 
The examples <code>main41</code>, <code>main42</code>, 
<code>main61</code>, <code>main62</code> and <code>main84</code> 
have been updated accordingly.</li> 
 
<li>Four new methods to interrogate the default values of the 
four different kinds of settings.</li> 
 
<li>The <code>Vec4</code> class has been expanded with methods to 
return true rapidity or pseudorapidity, and the <i>R</i> distance 
in <i>(y, phi)</i> or <i>(eta, phi)</i> cylindrical coordinates. 
Thanks to Andy Buckley.</li> 
 
<li>The copy and = constructors of the <code>Pythia</code> class 
are made private so that compilers will block use of them. 
Thanks to Andy Buckley.</li> 
 
<li>New option of the <code>Pythia</code> constructor, to omit 
the banner printout where necessary. Also new master switch 
<code>Print:quiet</code> to switch off most runtime program 
messages. Thanks to Andy Buckley.</li> 
 
<li>Updates in the machinery that restores all affected values 
to their defaults before setting a new tune.</li> 
 
<li>A new <code>Vect</code> class has been introduced for the 
<?php $filepath = $_GET["filepath"];
echo "<a href='SettingsScheme.php?filepath=".$filepath."' target='page'>";?>Settings</a> database. 
It can be used to store a vector of double-precision real values. 
</li> 
 
<li>Three new groupings of existing QCD processes: 
<code>SoftQCD:inelastic</code>, <code>HardQCD:hardccbar</code> 
and <code>HardQCD:hardbbbar</code>. </li> 
 
<li>New possibility to set the <i>a</i> and <i>b</i> parameters 
of the Lund fragmentation function separately for heavy flavour. 
Especially the <i>b</i> is expected to be universal, but 
Aurelien Martens and Eli Ben-Haim have found improved fits by 
relaxing this condition, so we formalize this possibility without 
recommeding it.</li> 
 
<li>Method <code>Pythia::getPDFPtr</code> is made public so it can 
be used also outside the <code>Pythia</code> class to access PDF 
sets. Internally the PDF bookkeeping is modified to allow for 
the simultaneous handling of several PDF sets. Thanks to 
Mathias Ritzmann for providing code changes.</li> 
 
<li>It is now possible to read the separate scale values of all 
outgoing particles, if this info is stored as a single line in 
an LHEF, begun by a single hashtag character. This is currently 
nonstandard and may evolve.</li> 
 
<li>Some improvements to the install targets code in the 
<code>Makefile</code> and error detection in 
<code>examples/configure</code>. Thanks to Mikhail Kirsanov.</li> 
 
<li>The CC and FC global environment variables can be set to specify 
the C++ and Fortran 77/90 compilers, otherwise set in 
<code>configure</code> and <code>examples/configure</code>. 
Thanks to Mikhail Kirsanov.</li> 
 
<li>Check of shell choice in <code>configure</code> removed. 
Thanks to Andy Buckley.</li> 
 
<li>Minor modifications in the Makefile and text for Root examples, 
relevant if shared libraries are used.</li> 
 
<li>Previously the whole <code>std</code> namespace has been made 
available in <code>PythiaStdlib.h</code>. Now this behaviour has 
been replaced by the already-existing alternative, where only a 
part of <code>std</code> is made available, and only inside the 
<code>Pythia8</code> namespace. Thus user code does not become 
reliant on the choices in PYTHIA. Some related smaller code 
rearrangements. Thanks to Andy Buckley.</li> 
 
<li>When the "second hard" machinery is used to set two hard processes, 
the old behaviour for the <code>SpaceShower:pTmaxMatch = 0</code> and 
<code>TimeShower:pTmaxMatch = 0</code> options was to use the combined 
final state of the two processes to decide what <i>pT_max</i> 
scale to use. This could have unexpected consequences. Now each process 
is analyzed separately, and therefore has a separate scale. Also 
for <code>MultipartonInteractions:pTmaxMatch = 0</code> the two 
interactions now are analyzed separately. It is then enough that 
one of them should limit <i>pT_max</i> of subsequent MPI's for 
such a restriction to be imposed. Thanks to Monika Jindal for 
discovering this bug.</li> 
 
<li>The <code>TimeShower:pTdampMatch</code> option had no effect if 
switched on, since some code was missing. The manual description 
of it and the more relevant <code>SpaceShower:pTdampMatch</code> 
has been updated.</li> 
 
<li>One minor bug fix and some corrections to make the code compile 
on Windows/VisualC++-2010. A simple error function parametrization 
has been added to <code>PhaseSpace.h</code> and 
<code>SigmaTotal.h</code>, to be uncommented in case 
<code>erf(x)</code> is not available by default. Thanks to Guy Barrand 
for providing the information.</li> 
 
<li>Break a closed dependency loop between <code>ParticleData.h</code> 
and <code>ResonanceWidths.h</code>. Thanks to Andreas Schaelicke and 
Andy Buckley.</li> 
 
<li>In the <code>examples/mainXX.cc</code> programs, class instances 
that have been created with a <code>new</code> are explicitly 
<code>delete</code>d at the end of the run. Useful if trying to track 
memory leaks.</li> 
 
<li>Bug fix for R-parity violating decays of R-hadrons, to treat the 
special case of sequential resonance decays after the RPV one. 
Thanks to Mariangela Lisanti for pointing out this bug.</li> 
 
<li>Changed the maximum allowed value for <code>Tune:pp</code>. 
Thanks to Yevgeny Kats.</li> 
 
<li>Bug fix in the <i>z</i>-value weight for QED ISR emissions.</li> 
 
<li>Some destructors moved for improved consistency.</li> 
 
<li>Constants <code>DEBUG</code> renamed <code>DBSUSY</code> in the 
<code>SusyCouplings</code> and <code>SusyResonanceWidths</code> 
classes to avoid confusion. Thanks to Guy Barrand.</li> 
 
<li>Many typos corrected and some other minor improvements of the 
online documentation.</li> 
 
<li>The year has been updated to 2013 in files.</li> 
 
</ul> 
</li> 
 
<li>8.176: 18 April 2013 
<ul> 
 
<li>Introduce weak shower, whereby <i>W^+-</i> and <i>Z^0</i> 
gauge bosons can be produced interleaved with ordinary QCD and 
QED emissions. Currently only available for final-state radiation, 
and off by default, see 
<?php $filepath = $_GET["filepath"];
echo "<a href='TimelikeShowers.php?filepath=".$filepath."' target='page'>";?>Timelike Showers</a>. 
A new status code 56 denotes <i>W^+-</i> and <i>Z^0</i> gauge 
bosons produced by the FSR parton showers.</li> 
 
<li>The gauge bosons produced by showers decay at the end of the 
parton-level step. New identity codes 93 for a <code>Zcopy</code> 
and 94 for a <code>Wcopy</code> can be used to set the decay modes 
of shower-produced gauge bosons separately from hard-process ones, 
but do not appear in event listings.</li> 
 
<li>The <code>Particle</code> class has been updated with a pointer 
to the mother <code>Event</code> class, replacing the previous pointer 
to the <code>ParticleData</code> class. Thereby several old methods 
in the <code>Event</code> class can obtain new equivalents for the 
<code>Particle</code> one. That is, a call <code>event.method(i)</code> 
instead can be written <code>event[i].method()</code>. The duplicated 
methods are <code>statusHepMC</code>, <code>iTopCopy</code>, 
<code>iBotCopy</code>, <code>iTopCopyId</code>, <code>iBotCopyId</code>, 
<code>motherList</code>, <code>daughterList</code>, <code>sisterList</code>, 
<code>isAncestor</code> and <code>undoDecay</code>. Omitted is 
<code>sisterListTopBot</code>, while <code>sisterList</code> obtains an 
optional argument. A new method <code>index</code> is introduced. 
The now redundant <code>Event</code> class methods will be removed in 
PYTHIA version 8.2. Thanks to Andy Buckley for suggestion. 
</li> 
 
<li>The initialization of SUSY couplings and particle data has been 
moved from the <code>Pythia</code> and <code>ProcessLevel</code> classes 
to a new dedicated <code>SLHAinterface</code> class.</li> 
 
<li>Some SUSY 3-body decays have been implemented with Matrix Element 
weighting.</li> 
 
<li>New method <code>Info::scalup()</code> returns the Les Houches 
<code>SCALUP</code> variable. This method is now used instead of 
<code>QFac()</code> in <code>main31.cc</code>, to set maximal scale for 
shower radiation in case that the event contains no POWHEG emission, and 
thereby sidesteps a potential bug when PDF information is provided in the 
Les Houches Event File. Also some default values have been changed in 
<code>main31.cmnd</code>. Thanks to James Monk.</li> 
 
<li>New parameter <code>LesHouches:mRecalculate</code> can be used to 
prompt the recalculation of a particle mass from its four-momentum. 
This may be required for Les Houches input events where the nominal 
mass of the particle species is stored in the mass slot, rather than 
the actual mass of the current particle, as foreseen.</li> 
 
<li>Extra checks that tracing of junctions in particular and partonic 
colour singlets in general are only performed when the event record 
contains coloured unhadronized partons. Thanks to James Catmore.</li> 
 
<li>Minor correction related to GZIP support. Thanks to James Monk.</li> 
 
<li>Recalculate the phase space of diffractive processes for each event 
when variable beam energies are allowed. Thanks to Oldrich Kepka for 
finding the bug.</li> 
 
<li>Fix a bug whereby a gluon in diffractive systems would obtain a 
light-quark mass.</li> 
 
<li>The <code>Pythia::check()</code> method can also check that each 
particle obeys <i>E^2 - p^2 = m^2</i> (within numerical precision). 
Speed up the checks on mother-daughter history by streamlining the 
handling of simple cases.</li> 
 
</ul> 
</li> 
 
<li>8.180: 20 September 2013 
<ul> 
 
<li>Nishita Desai joins as new author.</li> 
 
<li>All the header files <code>include/*.h</code> have been moved to 
<code>include/Pythia8/*.h</code> to better comply with conventions 
in other libraries (<code>*</code> = generic name). Thus also all 
code lines <code>#include "*.h"</code> that refer to PYTHIA files 
have been changed to <code>#include "Pythia8/*.h"</code>. Thanks to 
Andy Buckley for suggestion.</li> 
 
<li>The class that converts PYTHIA events to the HepMC format 
is renamed from <code>I_Pythia8</code> to <code>Pythia8ToHepMC</code>, 
the files where the class resides from <code>HepMCInterface.h/.cc</code> 
to <code>Pythia8ToHepMC.h/.cc</code>, and the subdirectory of the 
<code>.cc</code> file from <code>hepmcinterface</code> to 
<code>pythia8tohepmc</code>. This clarifies the role of the class 
and avoids potential future name clashes with other interfaces. 
Makefiles and example main runs have been modified accordingly. 
Thanks to Andy Buckley.</li> 
 
<li>The configure/Makefile step has been extended such that a 
<code>bin/pythia8-config</code> script is constructed, to simplify 
subsequent linking of PYTHIA 8 and other libraries to your main 
program. See the <code>README</code> file for further instructions, 
the new "Compling programs against PYTHIA 8" section. 
Thanks to Andy Buckley for code and text.</li> 
 
<li>The "minbias" event class has been renamed into the 
(inelastic) "nondiffractive" one, since the "minbias" denomination 
should be reserved for an experimental procedure. Needless to say, 
also the separation into diffraction or not could be made by 
experimental procedures, but here it is more generally accepted that 
there is an underlying theoretical description that takes precedence 
in cases of conflict. Thus the <code>SoftQCD:minBias</code> flag 
has been remaned <code>SoftQCD:nonDiffractive</code>, and the 
<code>Info::isMinBias()</code> method has been renamed 
<code>Info::isNonDiffractive()</code>. The old names have been retained 
as alternatives for now. Thanks to Andy Buckley.</li> 
 
<li>Four PDF sets have been added from the NNPDF2.3 QCD+QED family: 
the central sets for LO with <i>alpha_s(M_Z) = 0.130</i>, 
LO with <i>alpha_s(M_Z) = 0.119</i>, NLO with <i>alpha_s(M_Z) = 0.119</i>, 
and NNLO with <i>alpha_s(M_Z) = 0.119</i>. 
Thanks to Juan Rojo and Stefano Carrazza for providing the code.</li> 
 
<li>The <code>fjcore</code> version 3.0.5 code from <code>FastJet</code> 
has been included with the PYTHIA distribution. <code>SlowJet</code> now 
by default becomes a front end to <code>fjcore</code>, but the old 
behaviour is retained as an option. The new approach is faster, 
but the older one still offers a few extra possibilities. Note that 
the new <code>useFJcore</code> argument to the <code>SlowJet</code> 
constructor has been included as a new last-but-one argument, 
since the last one is likely to be used much more infrequently 
and logically depends on the value set for <code>useFJcore</code>. 
Thanks to Matteo Cacciari, Gavin Salam and Gregory Soyez for making 
the <code>fjcore</code> code freely distributable.</li> 
 
<li>New method <code>constituents(i)</code> in <code>FastJet</code> 
returns a vector with the indices of the particles belonging to the 
<i>i</i>'th jet.</li> 
 
<li>The global recoil option for final-state showers has been extended 
with a few more options to improve possibility for matching to 
output from other programs, such as aMC@NLO.</li> 
 
<li>Further extensions of the weak-showering machinery, but still 
experimental. Now also includes <i>W/Z</i> emission in the initial 
state.</li> 
 
<li>Introduce new <code>qqbar</code> option for 
<code>SigmaProcess::inFlux()</code> and update documentation.</li> 
 
<li>Improve the description of <code>meMode</code> for resonances 
and illustrate how to force specific Higgs branching ratios in 
<code>main16.cmnd</code>.</li> 
 
<li>Added two previously missing SUSY processes for associated 
production of a gluino with a neutralino or chargino: 
<code>SUSY:qqbar2chi0gluino</code> and 
<code>SUSY:qqbar2chi+-gluino</code>, using the cross section 
expressions in [<a href="Bibliography.php" target="page">Fuk11</a>]. Validated against PYTHIA 6 and 
XSUSY.</li> 
 
<li>Added direct slepton production from <i>q qbar</i> annihilation, 
<code>SUSY:qqbar2sleptonantislepton</code>. Validated against 
PYTHIA 6.</li> 
 
<li>Corrected some slepton and squark EW couplings in 
<code>SusyCouplings</code>.</li> 
 
<li>Generalized the cross section formulae for 
<code>SUSY:qg2squarkgluino</code> to the case of non-minimal flavor 
violation, using the expressions in [<a href="Bibliography.php" target="page">Fuk11</a>]. Validated 
against PYTHIA 6 and XSUSY. The new implementation is compatible 
with both SLHA2 and SLHA1, whereas the older was only compatible 
with SLHA1.</li> 
 
<li>Generalized the cross section formulae for 
<code>SUSY:qqbar2gluinogluino</code> to the case of non-minimal 
flavour violation, using the expressions in [<a href="Bibliography.php" target="page">Fuk11</a>]. 
(Also corrected a factor 2 in that paper on the color factor for 
<i>st</i> and <i>su</i> interference terms.) The new 
implementation is compatible with both SLHA2 and SLHA1, whereas 
the older was only compatible with SLHA1.</li> 
 
<li>Changed the default <code>meMode</code> for SLHA DECAY modes to 100, 
to allow off-shell decays. Enabled the user to choose a different 
<code>meMode</code> if desired, via a new mode, <code>SLHA:meMode</code>. 
If values different from 100 are selected (e.g., 103), modes that are 
extremely far off shell (defined as needing a fluctuation of more than 
100 times the root-sum-square of the widths of the mother and daughter 
particles) will still default back to 100, though the user may of course 
still switch them off if so desired. Added documentation about this in 
<code>SusyLesHouchesAccord.xml</code>. Deleted the parameter 
<code>SLHA:minDecayDeltaM</code>, which would prevent the possibility 
of off-shell decays unless set to a large negative value.</li> 
 
<li>Added calculation of decay lengths to SLHA DECAY modes.</li> 
 
<li>Updated documentation of SUSY processes and SLHA-related 
parameters. Additional reference to the new cross sections included in 
the bibliography.</li> 
 
<li>Allow six-flavour running of <i>alpha_s</i> above the top threshold, 
and implement option with CMW rescaling of the Lambda values, for use 
in coherent showers.</li> 
 
<li>The merging code has been updated, with corrected handling of massive 
partons. This amends problems in <i>tt&#772;</i>, and makes clusterings of 
massive emissions <i>W, Z, ~g</i> possible. The bookkeeping for the hard 
process has been upgraded to accommodate more general BSM processes. The 
treatment of incomplete histories has been upgraded, so that histories which 
allow for at least one clustering will be used in the unitarisation. Also, 
states that do not allow for any clustering will not be subject to the 
(internal, optional) cut on the input events any longer, to guarantee full 
phase space coverage. This is important for processes for which a 
non-negligible fraction of states lead to incomplete histories, e.g. 
<i>tt&#772;W</i> production. Further, functionality for reclustering 
FSR off SUSY particles has been added. A new setting allows more easily 
to switch between 4-flavour and 5-flavour merging schemes.</li> 
 
<li>The <code>main85.cc</code> - <code>main88.cc</code> have obtained 
support for zipped LHE input files, and have nicer printing of the 
inclusive cross section at the end.</li> 
 
<li>Introduced new mode <code>LesHouches:setLifetime</code> so that 
the lifetime information in Les Houches input can be replaced by 
the standard PYTHIA selection procedure. By default this is applied to 
<i>tau</i> leptons, since some matrix-element generators do not set 
this lifetime. Thanks to James Monk and Thorsten Kuhl.</li> 
 
<li>A new <code>examples/main46.cc</code> illustrates how to write a 
ProMC file with PYTHIA events. Thanks to Sergei Chekanov.</li> 
 
<li>The <code>Vect</code> class has been renamed <code>PVec</code> 
and a new <code>MVec</code> class has been introduced along the same 
lines, see the <?php $filepath = $_GET["filepath"];
echo "<a href='SettingsScheme.php?filepath=".$filepath."' target='page'>";?>Settings</a> database. 
They can be used to store a vector of double-precision real or integer 
values, respectively, i.e. of parms or modes, hence the names. 
Thanks to Abram Krislock.</li> 
 
<li>Two new vectors of integers <code>SUSY:idVecA</code> and 
<code>SUSY:idVecB</code> have been introduced to allow a more flexible 
selection of final states in SUSY processes, see the 
<?php $filepath = $_GET["filepath"];
echo "<a href='SUSYProcesses.php?filepath=".$filepath."' target='page'>";?>SUSY process selection</a> page. 
The selection machinery has been expanded to cope. 
Thanks to Abram Krislock.</li> 
 
<li>New switch <code>PartonLevel:earlyResDec</code> to give 
first simple option where resonances are allowed ro decay before 
colour reconnection and beam remnants are added.</li> 
 
<li>Renamed the <code>mass(...)</code> methods to <code>mSel(...)</code> 
in the <code>Particle</code>, <code>ParticleData</code> and 
<code>ParticleDataEntry</code> classes, to avoid users mistaking 
<code>Particle::mass()</code> for <code>Particle::m()</code>.</li> 
 
<li>Fixed a missing (unused) argument in some derived classes when 
the <code>mRecalculate</code> parameter was introduced in Les Houches 
code in version 8.176.</li> 
 
<li>Removed several unused class variables from header files, based 
on new compiler information.</li> 
 
<li>Fixed a bug in <code>Sigma2ffbar2HposHneg</code>, where 
<code>int(...)</code> was used instead of <code>abs.(...)</code>. 
Thanks to Vittorio Zecca.</li> 
 
<li>Fixed missing re-initialization of Breit-Wigner mass generation 
parameters for externally set new resonance widths. 
Thanks to Vittorio Zecca.</li> 
 
<li>Fixed several places where division by zero or adressing of 
arrays out-of-bounds could occur. While some were trivial, others 
revealed underlying errors, notably in the handling of some SUSY 
processes and resonances. New checks against unphysical SLHA input 
have been implemented. Also some bug fixes in the new weak-shower 
ISR component, the sphericity analysis, the event undo-decay option, 
and more. Thanks to Vittorio Zecca.</li> 
 
<li>Bug fix in the <i>tau</i> decay description for decay chains like 
<i>H0 &rarr; A0 A0 &rarr; 4 tau</i>, caused by an erroneous assignment of 
the number of spin states. Thanks to Brock Tweedie.</li> 
 
<li>Add setting of some <code>TimeShower</code> variables that might 
remain uninitialized. Thanks to Christian Pulvermacher. 
</li> 
 
<li>Extra check in <code>TimeShower</code> for incoming massive 
particles, where a recalculation of kinematics to an equivalent 
massless case could give a slightly negative effective dipole mass 
(with endpoint masses subtracted). Thanks to Paolo Torrielli. 
</li> 
 
<li>Updated manual on how to implement new showers, mainly detailing 
new optional method arguments. Thanks to Mathias Ritzmann. 
</li> 
 
<li>Corrected bug in <code>SigmaSUSY</code> which caused flavour violating 
slepton production (for R-parity conserving cases too).</li> 
 
<li>Corrected bug in <code>SusyCouplings</code> where <i>tan(beta)</i> 
was not read from LHA file for RPV spectrum.</li> 
 
<li>Modified the contact interaction term normalization of the 
<i>f fbar &rarr; l lbar</i> process to agree with the one used in 
PYTHIA 6. Documentation of the <code>ContactInteractions:Lambda</code> 
updated to reflect this. Thanks to Daniel Hayden.</li> 
 
<li>Introduced copy constroctor for the <code>Event</code> class.</li> 
 
<li>Reset the Higgs mass range when its mass is changed in 
<code>examples/main14.cc</code>.</li> 
 
<li>Remove obsolete <code>examples/main91.cc</code> and associated 
<code>Makefile</code> code. 
</li> 
 
<li>Catch cases where junction topologies are sent to the ministring 
machinery, which is not set up to handle it, and therefore could 
give segmentation faults. Very rare.</li> 
 
</ul> 
</li> 
 
<li>8.183: 23 January 2014 
<ul> 
 
<li>A potentially severe bug has been found in version 8.180, 
in that by mistake the <code>SpaceShower:vetoWeakJets</code>, 
<code>SpaceShower:dopTDampMass</code>, <code>TimeShower:dopTDampMass</code> 
were given a default <code>on</code> value rather than <code>off</code>. 
Its main effect is to reduce the emission of two or more hard 
jets for processes 221 and 222, i.e. <i>gamma^*/Z^0/W^+-</i> 
production: implicitly it thereby also softens the <i>pT</i> 
spectrum of the gauge boson. Other processes should be unaffected.
</li> 
 
<Li>The emission of <i>W</i> and <i>Z</i> weak gauge bosons in 
ISR and FSR parton showers is now fully implemented [<a href="Bibliography.php" target="page">Chr14</a>]. 
Note extra optional arguments for a few <code>TimeShower</code> and 
<code>SpaceShower</code> methods.</li> 
 
<li>The worksheet, intended to help beginners get going, has been
updated and expanded. Notably with new exercises on CKKW-L merging 
and Higgs production and decay. Also some new comments on platform 
requirements and how to meet them. An <code>examples/main80</code> 
program has been added for the merging exercise.</li> 
</li> 
 
<li>The LO PDF sets in the NNPDF2.3 QCD+QED family are now valid down to 
<i>Q^2 = 1 GeV^2</i>, the NLO and NNLO sets are positive definite, 
and all go down to <i>x = 10^-9</i>. Grid file names have been updated 
to agree with the LHAPDF ones. New <code>main54.cc</code> test program. 
Thanks to Juan Rojo and Stefano Carrazza for the updates.</li> 
 
<li>Three new <?php $filepath = $_GET["filepath"];
echo "<a href='Tunes.php?filepath=".$filepath."' target='page'>";?><i>e^+e^-</i> tunes</a> to some 
LEP data, one by Peter Skands and two by Nadine Fischer. Also the
more comprehensive <?php $filepath = $_GET["filepath"];
echo "<a href='Tunes.php?filepath=".$filepath."' target='page'>";?>Monash 2013 tune</a> 
[<a href="Bibliography.php" target="page">Ska14</a>], which covers both <i>e^+e^-</i> and 
<i>pp/pbarp</i> data.</li> 

<li>Some moderate changes in the merging machinery, as follows.
Added the D-parameter of the <i>kT</i> measure as an input. Added 
flag to not remove states with incomplete histories from NLO input 
(normally, these are taken from tree-level samples instead). This is 
necessary for a correct treatment of inputs with "diagram subtraction", 
as is e.g. needed for the definition of <i>p p &rarr; tW</i>.
Updated the bookkeeping of the hard process in the merging again, so
that the "hard process candidates" are not switched around too often.
Updated the main files with proper header for the standard
<code>access( string, F_OK)</code> function.</li> 
 
<li>Changed status code for partons in the hardest interaction of
the <code>SoftQCD</code> processes, from being in the 30'ies range
to being in the 20'ies range. Thus a nondiffractive event is always
set to have one 20'ies hard interaction. Diffractive events can have
zero, one or two hard interactions, the latter possible only for 
double diffraction. A low-mass diffractive system need not contain
any perturbative interaction, but a nonperturbatively kicked-out
quark or gluon is defined by the new 24 status code.</li> 
 
<li>New flag <code>HiggsSM:NLOWidths</code>, on by default, enables
rescaling of the internal PYTHIA partial widths of the SM Higgs 
to the values recommended by the LHCXSWG.</li> 
 
<li>New possibility to pick a fixed factorization scale for ISR 
(and also for FSR) for debug purposes.</li> 
 
<li>New processes for s-channel exchange of a <i>gamma^*/Z^0</i> 
or <i>W^+-</i>, written as <i>2 &rarr; 2</i> processes, whereas the 
already existing processes separate production and decay of a resonance. 
</li> 
 
<li>New parameter <code>StringZ:aExtraSQuark</code> allows an optional 
softening in hadronization for strange quarks.</li> 
 
<li>New method <code>vector&lt;int&gt; Info::codesHard()</code> returns a 
vector of the hard-process codes set up for current run.</li> 
 
<li>New optional extra argument for the <code>Hist::table</code> methods 
allow output of underflow and overflow. Thanks to Mathias Ritzmann.</li> 
 
<li>Two extra safety checks for the <code>Event</code> class. 
Thanks to Mathias Ritzmann.</li> 
 
<li>Allow a leptoquark also to couple to the top quark. Thanks to 
Katherine Leney.</li> 

<li>Updated problematic return statement in <code>JetMatching.h</code>.
Thanks to Simon de Visscher.</li> 
 
<li>New addresses for Nishita Desai and Philip Ilten.</li> 
 
<li>Changes in <code>TimeShower</code>, <code>(Susy)ResonanceWidths</code>, 
<code>SigmaProcess</code> and other classes to avoid undefined variables. 
Thanks to Vittorio Zecca.</li> 

<li>Major cleanup of code and documentation files: replace tabs, remove 
blank characters at the end of code lines, keep one blank character at the
end of documentation lines, restrict to a maximum of 79 characters per line
wherever feasible, and update year to 2014.</li>
 
</ul> 
</li> 
 
<li>8.185: 15 March 2014 
<ul> 

<li>Bug found in the handling of ISR in diffractive systems,  whereby the 
maximal scale of shower emissions was set to  the mass of the diffractive 
system, rather than the <i>pT</i> of the hardest interaction. This bug 
was introduced in version 8.183, when the status code of the hardest MPI 
was changed, and notably gave too many high-<i>pT</i> particles, and 
also a bit too high multiplicity. In versions up to and including 8.180, 
instead the <i>pT</i> of the second MPI was not strictly limited by the 
<i>pT</i> of the first, which also gave an overestimate, but smaller.</li> 
 
<li>Added the FxFx matching and shower-kT matching schemes as new options,
see <?php $filepath = $_GET["filepath"];
echo "<a href='JetMatching.php?filepath=".$filepath."' target='page'>";?>Jet Matching</a>. Still under validation,
though. Part of the new code is contributed by Simon de Visscher.</li>

<li>A number of minor improvements of the merging machineries for 
increased flexibility, including removed hardcoded maximum number of jets, 
updated bookkeeping to allow for a event-by-event definition of the 
hard process for merging, removed incomplete histories warning, small 
change in mass treatment for reclustering, and more. Also removed internal 
setting of <i>K</i> factors for <code>main88.cc</code>.</li> 

<li>New documentation on the 
<?php $filepath = $_GET["filepath"];
echo "<a href='POWHEGMerging.php?filepath=".$filepath."' target='page'>";?>POWHEG Merging</a> implemented in 
<code>examples/main31.cc</code>.</li> 

<li>Higgs particle names (re)simplified to MSSM standard. New 
<code>SLHAInterface</code> code added to reset the names appropriately 
(SLHA2 conventions) when in the NMSSM, CPV or RPV scenarios. PDG codes 
45 and 46 reserved as placeholders for further Higgs states.</li>
 
<li>SUSY decay channels now have <code>meMode = 0</code>, except those to 
Gravitinos, for which no code has been implemented (they stay 103). 
By default, the width of SUSY particles is formally set to 1.0,  but all 
decay channels start out with zero branching fractions. The proper BRs 
should then be calculated by PYTHIA, and/or read in via SLHA decay tables,
and/or set by hand by the user.</li> 

<li>Expanded SLHA input support for NMSSM type models, plus a few 
further minor SUSY improvements and fixes.</li> 

<li>Significant extensions to the 
<?php $filepath = $_GET["filepath"];
echo "<a href='OniaProcesses.php?filepath=".$filepath."' target='page'>";?>charmonium and bottomonium</a> 
machineries, as follows.
<br/>Many more colour-octet states, according to a new labelling 
convention <i>9 9 n_Q n_state n_r n_L n_J</i>, where <i>n_Q</i> is 
the flavour of the state (4 or 5),  <i>n_state</i> is 0 for 3S1, 
1  for 1S0 and 2  for 3PJ states, while <i>n_r</i>, <i>n_L</i> 
and <i>n_J</i> are the same as in the PDG convention. They are
assigned masses 10 MeV above the respective colour singlet state.
<br/>Data on processes which only differ by the radial excitation 
number <i>n_r</i> are grouped into vectors for on/off switches 
and long-dinstance matrix elements, but share the same process code
and short-distance matrix elements. This grouping involves some upgrades 
to the cross section bookkeeping machinery.
<br/>Matrix elements for 3DJ processes are now implemented.
<br/>The mass splitting between all colour octet and singlet state pairs 
can be set by one common parameter.
</li> 

<li>Introduce new switch <code>TimeShower:weightGluonToQuark</code> that 
allows a few alternative approaches to set the <i>g &rarr; q qbar</i> 
branching rate, notably for heavy quarks, and similarly for 
<i>gamma &rarr; f fbar</i>. Associated scale-setting freedom regulated
by new <code>TimeShower:scaleGluonToQuark</code> parameter.
</li> 

<li>New/improved capabilities to accept LesHouches input of events
without any incoming beams, e.g. for resonances with decay chains, 
where PYTHIA should add showers and hadronization, see
<?php $filepath = $_GET["filepath"];
echo "<a href='LesHouchesAccord.php?filepath=".$filepath."' target='page'>";?>Les Houches Accord</a> and 
<?php $filepath = $_GET["filepath"];
echo "<a href='HadronLevelStandalone.php?filepath=".$filepath."' target='page'>";?>Hadron-Level Standalone</a>.
The <code>Standalone:allowResDec</code> switch is removed and its 
functionality largely taken over by the 
<code>ProcessLevel:resonanceDecays</code> one.</li> 

<li>New possibility to comment out a section of lines in a command file,
when read in by <code>Pythia::readFile(...)</code>. The first line of 
the commented section should begin by <code>/*</code> and the last
begin by <code>*/</code>. Best is to keep them on separate lines,
optionally followed by comments, but no commands.</li> 
 
<li>A new <code>FVec</code> class has been introduced to store a vector 
of flags, see the <?php $filepath = $_GET["filepath"];
echo "<a href='SettingsScheme.php?filepath=".$filepath."' target='page'>";?>Settings</a> database.
</li> 

<li>Minor update in <code>configure</code> to be used by
<code>pythia8-config</code>. Thanks to Andy Buckley.</li> 

<li>Minor update in the <code>configure</code> options for FastJet.</li> 

<li>Minor update of the Monash 2013 tune.</li> 

<li>In the joining of nearby partons, <i>q + g &rarr; q</i> , 
in preparation of the hadronization step, it is ensured that the quark 
mother is listed as <code>mother1()</code>, even when this is not the
order of the mothers in the event record.</li> 

<li>The histogram table printing routines have acquired a new argument
that allows the <i>x</i> value at the beginning of each bin to be
printed, instead of the midpoint one.</li> 
 
<li>Implement (some of the) functionality of the <code>doForceWidth</code> 
switch for resonances, that has been missing so far, and improve the
documentation thereof. Thanks to Xiaohu Sun.</li> 

<li>Update some of the hadron-level code to handle several
junction-antijunction topologies in a single event.</li> 

<li>Bug fix for MPI's with an <i>x</i>-dependent impact parameter profile
(<code>bProfile = 4</code>) and the factorization scale of a hard process as 
the maximal scale (<code>pTmaxMatch = 1</code>), that this maximum was not
respected, so that an MPI could be harder than the hard process.  
Thanks to Stefano Camarda.</li> 

<li>A few minor bug fixes in the new weak-shower machinery.</li> 

<li>Minor fix in the setting of the pointer of a particle data entry to 
the full particle data table.</li> 

<li>Some other minor fixes.</li> 
 
</ul> 
</li> 
 
<li>8.186: 8 July 2014 
<ul> 

<li>A new user hook to help experimentation with colour reconnection,
in particular in relation to resonance decays.</li> 

<li>New set of parameters <code>StringFlav:probQQ1toQQ0join</code>,
used to pick diquark spin when two already produced quarks are to be
joined into a diquark.</li> 

<li>Possibility to map the bookkeeping of exotic unresolved incoming 
beams in Les Houches event files onto neutrino beams, see
<code>LesHouches:idRenameBeams</code>. Primarily intended for dark matter 
annihilation. Thanks to Leila Ali Cavasonza and Jiang-Hao Yu.
</li> 

<li>A correction to the graviton (<i>G^*</i>) + parton hard processes 
(5003-5005) to account for off-shell <i>G^*</i> production. 
The previous formula assumed the graviton was on-shell and could 
greatly overestimate the low-mass tail of the <i>G^*</i>.</li> 

<li>Bug fixes in <code>examples/JetMatching.h</code>, for uninitialized
boolean, and number of partons to match for heavy flavour handling. 
The former bug was introduced with the new matching schemes in version
8.185 and affected matching to ALPGEN, since no events were rejected.
Thanks to Chris Brust.</li> 

<li>Differentiate the reading of ALPGEN masses, so that vanishing 
<i>c</i> or <i>b</i> masses in ALPGEN are not unwittingly used
where inappropriate. Also protect the <i>c</i> and <i>b</i> masses 
from being set too low in <code>TimeShower</code> and 
<code>SpaceShower</code>, so that flavour thresholds are not messed up.</li> 

<li>Reduce number of maximum violation warnings in <code>SpaceShower</code> 
by leaving some headroom for branching weights.</li> 

<li>Bug fix that <code>hasVetoed</code> in <code>Pythia.cc</code> was not
reset correctly in some specific cases. Thanks to Christian Bauer and
Calvin Berggren.</li> 

<li>Bug fix in PartonLevel.cc where a reference could become invalid.
Thanks to Thomas R&ouml;ssler.</li> 

<li>Improvements and bug fixes in junction handling.</li> 

<li>Bug fix to not reuse colour indices in double diffraction.</li> 

<li>Bug fix in the calculation of the average mass threshold of 
open decay channels of a resonance. Reduced 
<code>ResonanceWidths::MASSMIN</code> parameter value.</li> 

<li>Correct typo in left-right symmetry documentation. Thanks to 
Nady Bakhet.</li> 

<li>Some other minor fixes. Thanks to Pere Mato for two of them.</li> 

</ul> 
</li> 
</ul> 
 
</body>
</html>
 
<!-- Copyright (C) 2014 Torbjorn Sjostrand --> 
