<html>
<head>
<title>Particle Properties</title>
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

<form method='post' action='ParticleProperties.php'>

<h2>Particle Properties</h2>

A <code>Particle</code> corresponds to one entry/slot in the 
event record. Its properties therefore is a mix of ones belonging 
to a particle-as-such, like its identity code or four-momentum, 
and ones related to the event-as-a-whole, like which mother it has. 

<p/>
What is stored for each particle is 
<ul>
<li>the identity code,</li> 
<li>the status code,</li> 
<li>two mother indices,</li>
<li>two daughter indices,</li> 
<li>a colour and an anticolour index,</li> 
<li>the four-momentum and mass,</li> 
<li>the production vertex and proper lifetime,</li>
<li>a pointer to the particle kind in the particle data tables.</li>
</ul>
From these, a number of further quantities may be derived.

<h3>Basic methods</h3>

The following member functions can be used to extract the information:

<p/><code>method&nbsp; </code><strong> id() &nbsp;</strong> <br/>
the identity of a particle, according to the PDG particle codes 
[<a href="Bibliography.php" target="page">Yao06</a>].
  

<p/><code>method&nbsp; </code><strong> status() &nbsp;</strong> <br/>
status code. The status code includes information on how a particle was 
produced, i.e. where in the program execution it was inserted into the 
event record, and why. It also tells whether the particle is still present 
or not. It does not tell how a particle disappeared, whether by a decay, 
a shower branching, a hadronization process, or whatever, but this is 
implicit in the status code of its daughter(s). The basic scheme is:
<ul>
<li>status = +- (10 * i + j)</li>
<li> +          : still remaining particles</li>
<li> -          : decayed/branched/fragmented/... and not remaining</li>
<li> i =  1 - 9 : stage of event generation inside PYTHIA</li>
<li> i = 10 -19 : reserved for future expansion</li>
<li> i >= 20    : free for add-on programs</li>
<li> j = 1 - 9  : further specification</li>
</ul>
In detail, the list of used or foreseen status codes is: 
<ul>
<li>11 - 19 : beam particles</li> 
  <ul>
  <li>11 : the event as a whole</li> 
  <li>12 : incoming beam</li>
  <li>13 : incoming beam-inside-beam (e.g. <i>gamma</i> 
           inside <i>e</i>)</li>
  <li>14 : outgoing elastically scattered</li> 
  <li>15 : outgoing diffractively scattered</li>
  </ul>
<li>21 - 29 : particles of the hardest subprocess</li>
  <ul>
  <li>21 : incoming</li>
  <li>22 : intermediate (intended to have preserved mass)</li>
  <li>23 : outgoing</li>
  </ul>
<li>31 - 39 : particles of subsequent subprocesses</li>
  <ul>
  <li>31 : incoming</li>
  <li>32 : intermediate (intended to have preserved mass)</li> 
  <li>33 : outgoing</li> 
  </ul>
<li>41 - 49 : particles produced by initial-state-showers</li>
  <ul>
  <li>41 : incoming on spacelike main branch</li>
  <li>42 : incoming copy of recoiler</li>
  <li>43 : outgoing produced in timelike sidebranch of shower</li>
  <li>44 : outgoing shifted by the branching</li>
  </ul>
<li>51 - 59 : particles produced by final-state-showers</li>
  <ul>
  <li>51 : outgoing produced by parton branching</li>
  <li>52 : outgoing copy of recoiler, with changed momentum</li>  
  <li>53 : copy of recoiler when this is incoming parton, 
           with changed momentum</li>  
  </ul>
<li>61 - 69 : particles produced by beam-remnant treatment</li>
  <ul>
  <li>61 : incoming subprocess particle with primordial <i>kT</i> 
           included</li>
  <li>62 : outgoing subprocess particle with primordial <i>kT</i> 
           included</li>
  <li>63 : outgoing beam remnant</li>  
  </ul>
<li>71 - 79 : partons in preparation of hadronization process</li>
  <ul>
  <li>71 : copied partons to collect into contiguous colour singlet</li>  
  <li>72 : copied recoiling singlet when ministring collapses to
           one hadron and momentum has to be reshuffled</li>
  <li>73 : combination of very nearby partons into one</li>
  <li>74 : combination of two junction quarks (+ nearby gluons) 
           to a diquark</li>  
  <li>75 : gluons split to decouple a junction-antijunction pair</li> 
  <li>76 : partons with momentum shuffled to decouple a 
           junction-antijunction pair </li>
  <li>77 : temporary opposing parton when fragmenting first two 
           strings in to junction (should disappear again)</li>
  <li>78 : temporary combined diquark end when fragmenting last 
           string in to junction (should disappear again)</li>
  </ul>
<li>81 - 89 : primary hadrons produced by hadronization process</li>
  <ul>
  <li>81 : from ministring into one hadron</li>
  <li>82 : from ministring into two hadrons</li>
  <li>83, 84 : from normal string (the difference between the two 
           is technical, whether fragmented off from the top of the 
           string system or from the bottom, useful for debug only)</li>
  <li>85, 86 : primary produced hadrons in junction frogmentation of 
           the first two string legs in to the junction, 
           in order of treatment</li>
  </ul>
<li>91 - 99 : particles produced in decay process, or by Bose-Einstein 
  effects</li>
  <ul>
  <li>91 : normal decay products</li>
  <li>92 : decay products after oscillation <i>B0 &lt;-> B0bar</i> or 
           <i>B_s0 &lt;-> B_s0bar</i></li>
  <li>93, 94 : decay handled by external program, normally
           or with oscillation</li>
  <li>99 : particles with momenta shifted by Bose-Einstein effects
           (not a proper decay, but bookkept as an <i>1 -> 1</i> such,
           happening after decays of short-lived resonances but before
           decays of longer-lived particles)</li>
  </ul>
<li>101 - 199 : reserved for future expansion</li>
<li>201 - : free to be used by anybody</li>   
</ul>
  

<p/><code>method&nbsp; </code><strong> mother1(), mother2() &nbsp;</strong> <br/>
the indices in the event record where the first and last mothers are 
stored, if any. There are five allowed combinations of <code>mother1</code> 
and <code>mother2</code>:
<ol>
<li><code>mother1 = mother2 = 0</code>: for lines 0 - 2, where line 0 
represents the event as a whole, and 1 and 2 the two incoming 
beam particles; </li>
<li><code>mother1 = mother2 > 0</code>: the particle is a "carbon copy" 
of its mother, but with changed momentum as a "recoil"  effect, 
e.g. in a shower;</li>
<li><code>mother1 > 0, mother2 = 0</code>: the "normal" mother case, where 
it is meaningful to speak of one single mother to several products, 
in a shower or decay;</li>
<li><code>mother1 &lt; mother2</code>, both > 0, for 
<code>abs(status) = 81 - 86</code>: primary hadrons produced from the 
fragmentation of a string spanning the range from <code>mother1</code> 
to <code>mother2</code>, so that all partons in this range should be 
considered mothers;</li>
<li><code>mother1 &lt; mother2</code>, both > 0, except case 4: particles 
with two truly different mothers, in particular the particles emerging 
from a hard <i>2 -> n</i> interaction.</li>
</ol>    
<br/><b>Note 1:</b> in backwards evolution of initial-state showers, 
the mother may well appear below the daughter in the event record. 
<br/><b>Note 2:</b> the <code>motherList(i)</code> method of the 
<code>Event</code> class returns a vector of all the mothers, 
providing a uniform representation for all five cases. 
  

<p/><code>method&nbsp; </code><strong> daughter1(), daughter2() &nbsp;</strong> <br/>
the indices in the event record where the first and last daughters 
are stored, if any. There are five allowed combinations of 
<code>daughter1</code> and <code>daughter2</code>:
<ol>
<li><code>daughter1 = daughter2 = 0</code>: there are no daughters 
(so far);</li>
<li><code>daughter1 = daughter2 > 0</code>: the particle has a 
"carbon copy" as its sole daughter, but with changed momentum 
as a "recoil" effect, e.g. in a shower;</li> 
<li><code>daughter1 > 0, daughter2 = 0</code>: each of the incoming beams 
has only (at most) one daughter, namely the initiator parton of the 
hardest interaction; further, in a <i>2 -> 1</i> hard interaction, 
like <i>q qbar -> Z^0</i>, or in a clustering of two nearby partons, 
the initial partons only have this one daughter;</li> 
<li><code>daughter1 &lt; daughter2</code>, both > 0: the particle has 
a range of decay products from <code>daughter1</code> to 
<code>daughter2</code>;</li> <li><code>daughter2 &lt; daughter1</code>, 
both > 0: the particle has two separately stored decay products (e.g. 
in backwards evolution of initial-state showers).</li>
</ol>
<br/><b>Note 1:</b> in backwards evolution of initial-state showers, the 
daughters may well appear below the mother in the event record. 
<br/><b>Note 2:</b> the mother-daughter relation normally is reciprocal,
but not always. An example is hadron beams (indices 1 and 2), where each 
beam remnant and the initiator of each multiple interaction has the 
respective beam as mother, but the beam itself only has the initiator 
of the hardest interaction as daughter.
<br/><b>Note 3:</b> the <code>daughterList(i)</code> method of the 
<code>Event</code> class returns a vector of all the daughters, 
providing a uniform representation for all five cases. With this method, 
also all the daughters of the beams are caught, with the initiators of 
the basic process given first,  while the rest are in no guaranteed order 
(since they are found by a scanning of the event record for particles
with the beam as mother, with no further information). 
  

<p/><code>method&nbsp; </code><strong> col(), acol() &nbsp;</strong> <br/>
the colour and anticolour tags, Les Houches Accord [<a href="Bibliography.php" target="page">Boo01</a>] 
style (starting from tag 101 by default, see below).
  

<p/><code>method&nbsp; </code><strong> px(), py(), pz(), e() &nbsp;</strong> <br/>
the particle four-momentum components, alternatively extracted as a 
<code>Vec4 p()</code>.
  

<p/><code>method&nbsp; </code><strong> m() &nbsp;</strong> <br/>
the particle mass.
  

<p/><code>method&nbsp; </code><strong> scale() &nbsp;</strong> <br/>
the scale at which a parton was produced, which can be used to restrict 
its radiation to lower scales in subsequent steps of the shower evolution. 
Note that scale is linear in momenta, not quadratic (i.e. <i>Q</i>, 
not <i>Q^2</i>). 
  

<p/><code>method&nbsp; </code><strong> xProd(), yProd(), zProd(), tProd() &nbsp;</strong> <br/>
the production vertex coordinates, in mm or mm/c, alternatively extracted 
as a <code>Vec4 vProd()</code>. The initial process is assumed to occur 
at the origin.
<br/><b>Note:</b>the <code>Vec4</code> has components px(), py(), 
pz() and e(), which of course should be reinterpreted as above. 
  

<p/><code>method&nbsp; </code><strong> tau() &nbsp;</strong> <br/>
the proper lifetime, in mm/c; is assigned for all hadrons with
positive nominal <i>tau</i>, <i>tau_0 > 0</i>, even if not 
decayed by PYTHIA (because of one veto or another).
  

<p/>
The same method names are overloaded to take an argument, in which case 
the corresponding property is set accordingly.

<h3>Further methods</h3>

 There are a few alternative methods for input:

<p/><code>method&nbsp; </code><strong> statusPos(), statusNeg() &nbsp;</strong> <br/>
sets the status sign positive or negative, without changing the absolute value.
  

<p/><code>method&nbsp; </code><strong> statusCode(code) &nbsp;</strong> <br/>
changes the absolute value but retains the original sign. 
  

<p/><code>method&nbsp; </code><strong> mothers(m1, m2) &nbsp;</strong> <br/>
sets both mothers in one go.
  

<p/><code>method&nbsp; </code><strong> daughters(d1, d2) &nbsp;</strong> <br/>
sets both daughters in one go.
  

<p/><code>method&nbsp; </code><strong> cols(c, ac) &nbsp;</strong> <br/>
sets both colour and anticolour in one go.
  

<p/><code>method&nbsp; </code><strong> p( px, py, pz, e) &nbsp;</strong> <br/>
sets the four-momentum in one go; 
alternative input as a <code>Vec4</code> object.
  

<p/><code>method&nbsp; </code><strong> vProd(  xProd, yProd, zProd, tProd) &nbsp;</strong> <br/>
sets the production vertex in one go; alternative input as a 
<code>Vec4</code> 
object.
  

<p/>
In addition, a number of derived quantities can easily be obtained 
(but cannot be set), such as:

<p/><code>method&nbsp; </code><strong> idAbs() &nbsp;</strong> <br/>
the absolute value of the particle identity code.
  

<p/><code>method&nbsp; </code><strong> statusAbs() &nbsp;</strong> <br/>
the absolute value of the status code.
  

<p/><code>method&nbsp; </code><strong> isFinal() &nbsp;</strong> <br/>
true for a remaining particle, i.e. one with positive status code, 
else false. Thus, after an event has been fully generated, it 
separates the final-state particles from intermediate-stage ones. 
(If used earlier in the generation process, a particle then 
considered final may well decay later.)  
  

<p/><code>method&nbsp; </code><strong> m2() &nbsp;</strong> <br/>
squared mass.
  

<p/><code>method&nbsp; </code><strong> mCalc(), m2Calc() &nbsp;</strong> <br/>
(squared) mass calculated from the four-momentum; should agree 
with <code>m(), m2()</code> up to roundoff.
  

<p/><code>method&nbsp; </code><strong> eCalc() &nbsp;</strong> <br/>
energy calculated from the mass and three-momentum; 
should agree with <code>e()</code> up to roundoff.
  

<p/><code>method&nbsp; </code><strong> pT(), pT2() &nbsp;</strong> <br/>
(squared) transverse momentum.
  

<p/><code>method&nbsp; </code><strong> mT(), mT2() &nbsp;</strong> <br/>
(squared) transverse mass.
  

<p/><code>method&nbsp; </code><strong> pAbs(), pAbs2() &nbsp;</strong> <br/>
(squared) three-momentum size.
  

<p/><code>method&nbsp; </code><strong> theta(), phi() &nbsp;</strong> <br/>
polar and azimuthal angle.
  

<p/><code>method&nbsp; </code><strong> thetaXZ() &nbsp;</strong> <br/>
angle in the <i>(p_x, p_z)</i> plane, between <i>-pi</i> and 
<i>+pi</i>, with 0 along the <i>+z</i> axis 
  

<p/><code>method&nbsp; </code><strong> pPlus(), pMinus() &nbsp;</strong> <br/>
<i>E +- p_z</i>. 
  

<p/><code>method&nbsp; </code><strong> y(), eta() &nbsp;</strong> <br/>
rapidity and pseudorapidity.
  

<p/><code>method&nbsp; </code><strong> xDec(), yDec(), zDec(), tDec() &nbsp;</strong> <br/>
the decay vertex coordinates, in mm or mm/c, alternatively extracted as 
a <code>Vec4 vDec()</code>; this decay vertex is calculated from the 
production vertex, the proper lifetime and the four-momentum assuming 
no magnetic field or other detector interference; it can be used to 
decide whether a decay should be performed or not, and thus is defined
also for particles which PYTHIA did not let decay.
  

<p/>
Each Particle contains a pointer to the respective 
<code>ParticleDataEntry</code> object in the 
<?php $filepath = $_GET["filepath"];
echo "<a href='ParticleDataScheme.php?filepath=".$filepath."' target='page'>";?>particle data tables</a>. 
This gives access to properties of the particle species as such. It is 
there mainly for convenience, and should be thrown if an event is 
written to disk, to avoid any problems of object persistency. Should 
an event later be read back in, the pointer will be recreated from the 
<code>id</code> code if the normal input methods are used. (Use the
<?php $filepath = $_GET["filepath"];
echo "<a href='EventRecord.php?filepath=".$filepath."' target='page'>";?><code>Event::restorePtrs()</code></a> method 
if your persistency scheme bypasses the normal methods.) This pointer is 
used by the following member functions:

<p/><code>method&nbsp; </code><strong> name() &nbsp;</strong> <br/>
the name of the particle, as a string.
  

<p/><code>method&nbsp; </code><strong> nameWithStatus() &nbsp;</strong> <br/>
as above, but for negative-status particles the name is given in 
brackets to emphasize that they are intermediaries.
  

<p/><code>method&nbsp; </code><strong> spinType() &nbsp;</strong> <br/>
<i>2 *spin + 1</i> when defined, else 0.
  

<p/><code>method&nbsp; </code><strong> charge(), chargeType() &nbsp;</strong> <br/>
charge, and three times it to make an integer.
  

<p/><code>method&nbsp; </code><strong> isCharged(), isNeutral() &nbsp;</strong> <br/>
charge different from or equal to 0.
  

<p/><code>method&nbsp; </code><strong> colType() &nbsp;</strong> <br/>
0 for colour singlets, 1 for triplets, 
-1 for antitriplets and 2 for octets.
  

<p/><code>method&nbsp; </code><strong> m0() &nbsp;</strong> <br/>
the nominal mass of the particle, according to the data tables.
  

<p/><code>method&nbsp; </code><strong> mWidth(), mMin(), mMax() &nbsp;</strong> <br/>
the width of the particle, and the minimum and maximum allowed mass value
for particles with a width, according to the data tables.
  

<p/><code>method&nbsp; </code><strong> mass() &nbsp;</strong> <br/>
the mass of the particle, picked according to a Breit-Wigner 
distribution for particles with width, and thus different each 
time called. 
  

<p/><code>method&nbsp; </code><strong> constituentMass() &nbsp;</strong> <br/>
will give the constituent masses for quarks and diquarks, 
else the same masses as with <code>m0()</code>.
  

<p/><code>method&nbsp; </code><strong> isResonance() &nbsp;</strong> <br/>
particles where the decay is to be treated as part of the hard process,
typically with nominal mass above 20 GeV (<i>W^+-, Z^0, t, ...</i>). 
  

<p/><code>method&nbsp; </code><strong> mayDecay() &nbsp;</strong> <br/>
flag whether particle has been declared unstable or not, offering 
the main user switch to select which particle species to decay.
  

<p/><code>method&nbsp; </code><strong> canDecay() &nbsp;</strong> <br/>
flag whether decay modes have been declared for a particle, 
so that it could be decayed, should that be requested.
  

<p/><code>method&nbsp; </code><strong> doExternalDecay() &nbsp;</strong> <br/>
particles that are decayed by an external program.
  

<p/><code>method&nbsp; </code><strong> isVisible() &nbsp;</strong> <br/>
particles with strong or electric charge, or composed of ones having it,  
which thereby should be considered visible in a normal detector.
  

<p/><code>method&nbsp; </code><strong> doForceWidth() &nbsp;</strong> <br/>
resonances that have code to recalculate the width in <code>mWidth</i>
from the nominal mass value <code>m0</code>, but where nevertheless the 
stored <code>mWidth</i> value is used.
  

<p/><code>method&nbsp; </code><strong> isLepton() &nbsp;</strong> <br/>
true for a lepton or an antilepton (including neutrinos).
  

<p/><code>method&nbsp; </code><strong> isQuark() &nbsp;</strong> <br/>
true for a quark or an antiquark.
  

<p/><code>method&nbsp; </code><strong> isGluon() &nbsp;</strong> <br/>
true for a gluon.
  

<p/><code>method&nbsp; </code><strong> isHadron() &nbsp;</strong> <br/>
true for a hadron (made up out of normal quarks and gluons, 
i.e. not for R-hadrons and other exotic states).
  

<p/><code>method&nbsp; </code><strong> particleData() &nbsp;</strong> <br/>
a reference to the ParticleDataEntry.
  

<p/>
There are some further methods, inherited from <code>Vec4</code>, 
to rotate and boost the four-momentum.

<p/>
Not part of the event class proper, but obviously tightly linked,
are the metods <code>m(Particle, Particle)</code> and 
<code>m2(Particle, Particle)</code> to calculate the (squared) 
invariant mass of two particles.

<p/>
The 
<?php $filepath = $_GET["filepath"];
echo "<a href='EventRecord.php?filepath=".$filepath."' target='page'>";?><code>Event</code></a> 
class also contains a few methods defined for individual particles, 
but these may require some search in the event record and therefore 
cannot be defined as a <code>Particle</code> method.

<p/>
Currently there is no information on polarization states.

</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->

