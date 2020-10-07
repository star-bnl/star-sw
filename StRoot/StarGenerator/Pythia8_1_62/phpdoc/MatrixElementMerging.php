
<html>
<head>
<title>Matrix Element Merging</title>
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

<form method='post' action='MatrixElementMerging.php'>

<h2>Matrix Element Merging</h2>

CKKW-L merging [<a href="Bibliography.php" target="page">Lon01</a>] allows for a consistent merging of parton 
showers with
matrix elements to include multiple well-separated partons. The
algorithm implemented  in PYTHIA is described in [<a href="Bibliography.php" target="page">Lon11</a>]. To
perform matrix element merging,  the user has to supply LHE
files [<a href="Bibliography.php" target="page">Alw07</a>] for the hard process  and the corresponding
process with up to N additional jets.

<p/> The usage of the merging procedure is illustrated in a few
example main  programs
(<code>main81.cc</code>, <code>main82.cc</code>, 
<code>main83.cc</code> and <code>main84.cc</code>, together with  the
input files <code>main81.cmnd</code>, <code>main82.cmnd</code> and 
<code>main84.cmnd</code>). These examples should of course only serve
as  an illustration, and as such will not make use of the merging in
all  possible ways. For full generality, the example programs link to
LHAPDF,  FastJet and HepMC. Of course the user is welcome to  remove
these dependencies. To remove the FastJet dependence, the functions
calculating example observables have to be deleted. Removing the
LHAPDF  dependence requires changing the cmnd input files to choose an
inbuilt PDF,  as outlined in the
<a href="PDFSelection.html" target="page">PDF documentation</a>.  The
HepMC dependence can be removed by erasing the code allowing for HepMC
output.

<p/> Three very short LHE files (<code>w+_production_lhc_0.lhe</code>,
<code>w+_production_lhc_1.lhe</code>, <code>w+_production_lhc_2.lhe</code>)
are included in the distribution. These files are not intended for
physics  studies, but only serve as input for the example main
programs. For  realistic studies, the user has to supply LHE files.

<p/> In the generation of LHE files, the value of the factorisation
scale used in  the PDFs is not important, since the cross section will
be multiplied by ratios  of PDFs to adjust to the PYTHIA starting
scales. The same is true for the  renormalisation scale (and starting
value <i>&alpha;<sub>s</sub>(M<sub>Z</sub>)</i>)  used to evaluate
<i>&alpha;<sub>s</sub></i>. Coupling and scale choices by the user 
will be transferred to the merging routines.

<p/> LHE files should be regularised with a jet measure, and
additional cuts on  the partons in the LHEF generation avoided as much
as possible. This means  that the merging scale is always a more
stringent cut than all other cuts  on the partons. Of course, if the
hard process itself is divergent, a cut  needs to be made. However,
this should be chosen in such a way as to not  exclude regions that
will be available to the matrix elements with additional jets. An
example is QCD  di-jet production with additional jets: Say the 
<i>2 -> 2</i> process is regularised with a <i>pTmin</i> cut
of <code>pTminCut = 100</code> GeV, and the 
<i>2 - >3</i> sample is regularised with a <i>kTmin</i>-cut of 
<code>kTminCut = 50</code> GeV. This would mean that when reclustering
the  emission in the <i>2 -> 3</i> sample, we could end up with a 
<i>pT = pTminNow</i> of the <i>2 -> 2</i> configuration with 
<i>pTminCut > pTminNow</i>, which is excluded in the 
<i>2 -> 2</i>  sample. Thus, the <i>2 -> 3</i> sample will include a
Sudakov factor  not included in the <i>2 -> 2</i> sample, resulting
in merging scale  dependencies. Such dependencies can be avoided if
the cuts on the hard  process are minimal. 

<p/> Of course, additional cuts on electroweak particles are
allowed. These  should be the same for all samples with
 <i>0 &lt;= n &lt;= N</i>  additional partons.

<p/> If it is not possible to generate LHE files with minimal cuts,
the user can choose to use the <code>MergingHooks</code> structures in
order to decide how much influence to attribute to parton shower
histories in which the reclustered lowest multiplicity process does
not pass the matrix element cuts. This is  described below.

<p/> When generating LHE files, please refrain from explicitly putting
unstable  particles (e.g. massive gauge bosons) in the final state. 
Rather, specify a  resonance by its decay products, e.g. if Les Houches 
events for the <i>pp &rarr; Z + jets &rarr; e+e- + jets</i> process
are desired, generate the matrix element events with the <i>Z</i> decay
included. From a physical  point of view, on-shell final massive gauge
bosons should not be considered  part of a hard process, since only
the boson decay products will be detectable.  Furthermore,
non-narrow-width approximation contributions are not present if  the
ME generator only produces on-shell bosons. Interference effects
between  different production channels for the decay products would
also be neglected.  These points seem an unnecessary restriction on
the accuracy of the ME  calculation.  In addition, there is a
technical reason for this strategy. Since  some matrix element
generators choose to put additional information on  intermediate
bosons into Les Houches events, depending on if they pass a certain
criterion (e.g. being close to the mass shell), without exact
knowledge of this  criterion, the only feasible way of bookkeeping the
hard process is by  identifying outgoing decay products. The code
implemented in PYTHIA8 uses this  strategy and may become unreliable
otherwise.

<p/> For all merging purposes, processes with different charge of
outgoing leptons are considered different processes. That means
e.g. that <i>e+&nu;<sub>e</sub>+ jets</i> and 
<i>e-&nu;&#772;<sub>e</sub> + jets</i>
are considered independent processes. If the user wishes to generate
distributions including effects of more than one  process, merged
samples for all independent processes should be generated  separately
and added afterwards.

<p/> When the matrix element merging is used to produce HepMC
[<a href="Bibliography.php" target="page">Dob01</a>] files to be analysed  with RIVET [<a href="Bibliography.php" target="page">Buc10</a>], 
special care  needs to taken in how the cross section is read by RIVET 
(see below).

<p/> To specify the merging conditions, additionally information on
the merging scale value and the functional definition of the merging
scale is needed. A few  standard definitions of merging scales are
available. This makes the user  interface less cumbersome.

<p/> Different choices intrinsic to the CKKW-L merging procedure might
be relevant for the user as well. The base
class <code>MergingHooks</code> gives the user the opportunity to
define the functional form of the merging scale.  In the following,
the usage of the merging machinery to consistently include LHE files
with additional jets into PYTHIA  will be discussed.

<br/><br/><hr/>
<h3>Merging with merging scale defined in kT</h3>

<p/> The quickest way to include processes with additional jets is to
produce LHE files with one of the standard ways to define the merging
scale. The following switches are provided:

<br/><br/><strong>Merging:doKTMerging</strong>  <input type="radio" name="1" value="on"><strong>On</strong>
<input type="radio" name="1" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If the additional jets in the LHE files have been regulated by
a <i>kT</i> cut, the user can supply the merging scale definition by
setting this flag to on. <i>kT</i> here and below means cutting on
Durham <i>kT</i> for <i>e+e-</i>  collisions, and cutting on 
longitudinally invariant <i>kT</i> for hadronic  collisions.
  

</p>
Since currently, a few slightly different definitions of
longitudinally invariant <i>kT</i> are considered, a specific form
can be chosen by setting the switch

<br/><br/><table><tr><td><strong>Merging:ktType  </td><td>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>; <code>minimum = 1</code>; <code>maximum = 3</code>)</td></tr></table>
Precise functional definition of longitudinally
invariant <ei>kT</ei>. For e+e- collisions, <ei>Durham kT</ei> is
always defined by the square root of <ei>min{ 2*min[ </sub>
E<sub>i</sub><sup>2</sup>, E<sub>j</sub><sup>2</sup>] * [ 1 -
cos&theta;<sub>ij</sub>] }</ei>, so that this switch will have no effect.
<br/>
<input type="radio" name="2" value="1" checked="checked"><strong>1 </strong>:  Longitudinally invariant <ei>kT</ei> is defined by the  square root of the minimum of minimal jet kinematic <ei>pT</ei> (<ei>p<sub>Tkin,min</sub> = min{ p<sub>T,i</sub> } </ei>) and <ei>p<sub>Tlon,min</sub> =  min{ min[ p<sub>T,i</sub><sup>2</sup>, p<sub>T,j</sub><sup>2</sup>] * [ (&Delta;y<sub>ij</sub>)<sup>2</sup> + (&Delta;&phi;<sub>ij</sub>)<sup>2</sup> ] / D<sup>2</sup> }</ei> ,  i.e.  <ei>kT = min{ &radic;p<sub>Tkin,min</sub>, &radic;p<sub>Tlon,min</sub> }</ei>  for hadronic collisions. Note that the true rapidity of partons is used. <br/>
<input type="radio" name="2" value="2"><strong>2 </strong>: Longitudinally invariant <ei>kT</ei> is defined by the  square root of the minimum of minimal jet kinematic <ei>pT</ei> (<ei>p<sub>Tkin,min</sub> = min{ p<sub>T,i</sub> } </ei>) and <ei>p<sub>Tlon,min</sub> =  min{ min[ p<sub>T,i</sub><sup>2</sup>, p<sub>T,j</sub><sup>2</sup>] * [ (&Delta;&eta;<sub>ij</sub>)<sup>2</sup> + (&Delta;&phi;<sub>ij</sub>)<sup>2</sup> ] / D<sup>2</sup> }</ei>, i.e.  <ei>kT = min{ &radic;p<sub>Tkin,min</sub>, &radic;p<sub>Tlon,min</sub> }</ei>  for hadronic collisions. Note that the pseudorapidity of partons is used. <br/>
<input type="radio" name="2" value="3"><strong>3 </strong>:  Longitudinally invariant <ei>kT</ei> is defined by the  square root of the minimum of minimal jet kinematic <ei>pT</ei> (<ei>p<sub>Tkin,min</sub> = min{ p<sub>T,i</sub> } </ei>) and <ei>p<sub>Tlon,min</sub> =  min{ min[ p<sub>T,i</sub><sup>2</sup>, p<sub>T,j</sub><sup>2</sup>] * [ cosh(&Delta;&eta;<sub>ij</sub>) - cos(&Delta;&phi;<sub>ij</sub>) ] / D<sup>2</sup> } </ei>,  i.e.  <ei>kT = min{ &radic;p<sub>Tkin,min</sub>, &radic;p<sub>Tlon,min</sub> }</ei>  for hadronic collisions. <br/>

<br/><br/><table><tr><td><strong>Merging:nJetMax  </td><td></td><td> <input type="text" name="3" value="0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>; <code>minimum = 0</code>)</td></tr></table>
Maximal number of additional jets in the matrix element
  

<br/><br/><table><tr><td><strong>Merging:TMS </td><td></td><td> <input type="text" name="4" value="0.0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.0</strong></code>)</td></tr></table>
The value of the merging scale. The name is inspired by the scale in
evolution equations, which is often called 't', and the suffix 'MS' stands 
for merging  scale.  In the particular case of <i>kT</i>-merging, this
would be the value of the <i>kT</i>-cut  in GeV. For any merging scale
definition, this input is considered the actual value of the merging
scale.
  

<br/><br/><table><tr><td><strong>Merging:Process  </td><td></td><td> <input type="text" name="5" value="void" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>void</strong></code>)</td></tr></table>
The string specifying the hard core process, in MG/ME notation. If
e.g. <i>W + jets</i> merging should be performed, set this to 
<code>pp>e+ve</code> (<i>without white spaces or  quotation marks</i>). 
This string may contain resonances in the MG/ME notation, e.g. for merging 
<i>pp&rarr;Z W<sup>+</sup>&rarr;q q&#772; e+&nu;<sub>e</sub> + jets</i>, 
the string <code>pp>(z>jj)(w+>e+ve)</code> would be applicable. 
  

<br/><br/><strong>Merging:doMGMerging</strong>  <input type="radio" name="6" value="on"><strong>On</strong>
<input type="radio" name="6" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
Even easier, but highly non-general, is to perform the merging with
MadGraph/MadEvent-produced LHE files, with a merging scale defined by
a <i>kT</i> cut.  For this, set this switch to on. The merging scale 
value will be read from  the +1 jet LHE file by searching for the
string <code>ktdurham</code>, and  extracting the value from <code>
value  = ktdurham</code>. Also, the hard  process will be read from
the +0 jet LHE file, from the line containing  the string <code>@1</code> 
(the tag specifying the first process in the  MadGraph process card). 
For this to work, PYTHIA should be initialised on LHE files called 
<code>NameOfYourLesHouchesFile_0.lhe</code> (+0 jet sample) and
<code>NameOfYourLesHouchesFile_1.lhe</code> (+1 jet sample) and the
same naming convention for LHE files with two or more additional jets.
  

<p/> Since for this option, the merging scale value is read from the
LHEF, no merging scale value needs to be supplied by setting <strong>
Merging:TMS </strong>.  Also, the hard process is read from LHEF, the
input <strong>  Merging::Process</strong> does not have to be defined.
However, the maximal number of merged jets still has to be supplied by
setting <strong>Merging:nJetMax</strong>.

<h4>Histogramming the events</h4> After the event has been processed,
histograms for observables of interest need to be filled. In order to
achieve good statistical accuracy for all jet  multiplicities and all
subprocesses contributing to one jet multiplicity,  generally a fixed
number of unit-weighted events is read from each Les Houches  Event
file. To then arrive at the correct prediction, for each of these
events,  histogram bins should be filled with the corresponding cross
section, or  weighted with unit weight and normalised at the end to
the generated cross  section for each jet multiplicity separately.

<p/> Still another, even more important, event weight that has to
applied on an  event-by-event basis is the CKKW-L-weight. This
corrective weight is the main  outcome of the merging procedure and
includes the correct no-emission  probabilities, PDF weights and
&alpha;<sub>s</sub> factors. This means that the merging
implementation will generate weighted events. The CKKW-L-weight can be
accessed by the following function:

<p/><strong> double Info::mergingWeight() &nbsp;</strong> <br/>
Returns the CKKW-L weight for the current event.

<p/> Note that to avoid confusion, this function does not include the
the weight of a phase space point (given
by <strong>Info::weight()</strong>). This weight will differ from
unity when reading in weighted Les Houches events. In this case, the
full weight with which to fill histogram bins is
<strong>Info::mergingWeight() * Info::weight()</strong>.

<p/> Finally, to arrive at a correct relative normalisation of the
contributions from different number of additional jets in the matrix
element, each histogram should be rescaled with the accepted cross
section given by 
<strong>Info::sigmaGen()</strong>. The accepted cross section includes
the  effect of vetoes generating Sudakov form factors for the matrix
elements, and  is in general only known after the run.

<p/> This final step can of course be skipped if the accepted cross
section had been estimated before the histgramming run, and  histogram
bins had instead been filled with the weight
<strong>Info::mergingWeight() * &sigma;<sub>est</sub>(number of
additional jets in current ME sample)</strong>. This is the way HepMC
events should be weighted to produce correct relative weights of
events (see below, and particularly  examine the example program
<code>main84.cc</code>).

<p/> Examples how to use these options are given in <code>main81.cc</code> 
(<i>kT</i> merging) and <code>main84.cc</code> (automatic MG/ME merging 
for RIVET usage). 

<br/><br/><hr/>
<h3>Merging with user-defined merging scale function</h3>

<p/> For all other merging scale definitions, the procedure is
slightly more  complicated, since the user has to write a small piece
of code defining the  merging scale. To allow for a user defined
procedure, set the input

<br/><br/><strong>Merging:doUserMerging</strong>  <input type="radio" name="7" value="on"><strong>On</strong>
<input type="radio" name="7" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
General user defined merging on/off.
  

</p>
Then, set
the <strong>Merging:nJetMax</strong>, <strong>Merging:TMS</strong>
and <strong>Merging:Process</strong> input as before.

<p/> Since during execution, PYTHIA needs to evaluate the merging
scale with the  definition of the user, the user interface is designed
in a way similar to the 
<code>UserHooks</code> strategy. The class controlling the merging
scale  definition is called <code>MergingHooks</code>. 

<h4>Initialisation</h4>

<p/> To initialise the merging with user-defined merging scale, we
should construct a class derived from <code>MergingHooks</code>, with
a constructor and destructor

<p/>
<a name="method1"></a>
<p/><strong>MergingHooks::MergingHooks() &nbsp;</strong> <br/>
  
<a name="method2"></a>
<p/><strong>virtual MergingHooks::~MergingHooks() &nbsp;</strong> <br/>
  
The constructor and destructor do not need to do anything.

<p/> For the class to be called during execution, a pointer to an
object of the class should be handed in with the
<br/><code><a href="ProgramFlow.html" target="page">
Pythia::setMergingHooksPtr( MergingHooks*)</a></code> method.

An examples of this procedure are given in <code>main82.cc</code>.

<h4>Defining a merging scale</h4>

<p/> Then, in the spirit of the <code>UserHooks</code> class, the user
needs to  supply the process to be merged by defining a methods to
evaluate the merging scale variable.

<a name="method3"></a>
<p/><strong>virtual double MergingHooks::tmsDefinition(const Event& event) &nbsp;</strong> <br/>
This method will have to calculate the value of the merging scale
defined in  some variable from the input event record. An example of
such a function is  given in <code>main82.cc</code>.
  

<p/> The base class <code>MergingHooks</code> contains many functions
giving  information on the hard process, to make the definition of the
merging scale as easy as possible:

<a name="method4"></a>
<p/><strong>int MergingHooks::nMaxJets() &nbsp;</strong> <br/>
Return the maximum number of additional jets to be merged.
  

<a name="method5"></a>
<p/><strong>int MergingHooks::nHardOutPartons() &nbsp;</strong> <br/>
Returns the number of outgoing partons in the hard core process.
  

<a name="method6"></a>
<p/><strong>int MergingHooks::nHardOutLeptons() &nbsp;</strong> <br/>
Returns the number of outgoing leptons in the hard core process.
  

<a name="method7"></a>
<p/><strong>int MergingHooks::nHardInPartons() &nbsp;</strong> <br/>
Returns the number of incoming partons in the hard core process.
  

<a name="method8"></a>
<p/><strong>int MergingHooks::nHardInLeptons() &nbsp;</strong> <br/>
Returns the number of incoming leptons in the hard core process.
  

<a name="method9"></a>
<p/><strong>int MergingHooks::nResInCurrent() &nbsp;</strong> <br/>
The number of resonances in the hard process reconstructed from the
current event. If e.g. the ME configuration was 
<i>pp -> (w+->e+ve)(z -> mu+mu-)jj</i>, and the ME generator put 
both intermediate bosons into the LHE file, this will return 2.
  

<a name="method10"></a>
<p/><strong>double MergingHooks::tms() &nbsp;</strong> <br/>
 Returns the value used as the merging scale.
  

<p/> Filling output histograms for the event then proceeds along the
lines described above in "Histogramming the events".

<p/> The full procedure is outlined in <code>main82.cc</code>. Special 
care needs to be  taken when the output is stored in the form of HepMC 
files for RIVET usage.

<h4>Defining a cut on lowest jet multiplicity events</h4>

<p/> It can sometimes happen that when generating LHE files, a fairly
restrictive cut has been used when generating the lowest multiplicity
matrix element  configurations. Then, it can happen that states that
are (in the generation of a parton shower history) constructed by
reclustering from higher multiplicity  configurations, do not pass
this matrix element cut.

<p/> Consider as an example  pure QCD dijet merging, when up to one
additional jet should be merged.  Three-jet matrix element
configurations for which the reclustered two-jet state does not pass
the cuts applied to the two-jet matrix element would never have  been
produced by showering the two-jet matrix element. This means that the
three-jet matrix element includes regions of phase space that would
never have  been populated by the parton shower. Thus, since the
matrix element phase space is larger than the shower phase space,
merging scale dependencies are expected.  A priori, this is not
troublesome, since the aim of matrix element merging is  to include
regions of phase space outside the range of the parton shower
approximation into the shower. An example is the inclusion of
configurations  with only unordered histories.

<p/> Clearly, if the parton shower phase space is very constrained by
applying  stringent cuts to the two-jet matrix element, merging scale
dependencies can  become sizable, as was e.g. seen in [<a href="Bibliography.php" target="page">Lon11</a>]
when forcing shower emissions to be ordered both in the evolution
variable and in rapidity. To  influence the effect of large phase
space differences for shower emissions and matrix element
configurations due to LHEF generation cuts, the user has to  write a
small piece of code overwriting method

<a name="method11"></a>
<p/><strong>virtual double MergingHooks::dampenIfFailCuts(const Event&event) &nbsp;</strong> <br/>
multiplicity  reclustered state as an input Event. From this input
event, the user can then check if matrix element cuts are
fulfilled. The return value will be internally multiplied to the
CKKW-L weight of the current event. Thus, if the user wishes  to
suppress contributions not passing particular cuts, a number smaller
than  unity can be returned.
  

<p/> Note that this method gives the user access to the lowest
multiplicity state,  which ( e.g. in the case of incomplete histories)
does not have to be a <i>2 &rarr; 2</i> configuration. Also, changing the
weight of the current event by  hand is of course a major intervention
in the algorithm, and should be  considered very carefully. Generally,
if this facility would have to be used extensively, it is certainly
preferable to be less restrictive when applying  additional,
non-merging-scale-related cuts to the matrix element. 

<p/> An example how to force a cut on lowest multiplicity reclustered
states for pure QCD matrix element configurations is given by
<code>main83.cc</code> (to be used with e.g. <code>main82.cmnd</code>).

<h4>Influencing the construction of all possible histories</h4>

<p/> Even more powerful - and dangerous - is influencing the construction
of histories directly. This should only be attempted by expert users. If you 
believe manipulations completely unavoidable, we advise you to take great care 
when redefining the following functions.

<a name="method12"></a>
<p/><strong>virtual bool MergingHooks::canCutOnRecState() &nbsp;</strong> <br/>
In the base class this method returns false. If you redefine it
to return true then the method <code>doCutOnRecState(...)</code>
will be called for each reclustered state encountered in the generation of
all possible histories of the matrix element state.  
  

<a name="method13"></a>
<p/><strong>virtual bool MergingHooks::doCutOnRecState(const Event&event) &nbsp;</strong> <br/>
This routine will be supplied internally with every possible reclustered 
event that can be reached by reclustering any number of partons in
the matrix element input state. The new, reclustered, states can then be 
analysed. If the method returns false, no further clusterings of the 
reclustered state will be attempted, thus disallowing all history branches
which contain the disallowed state.   
  

<p/> 
Clearly, these methods are highly intrusive. It could e.g. happen that no 
history is allowed, which would make merging impossible. One example where 
this method could be useful is if cuts on the core <i>2 -> 2</i> processes
have to be checked, and the method 
<code>MergingHooks::dampenIfFailCuts(const Event& event)</code> is not 
sufficiently effective.

<br/><br/><hr/>
<h3>Matrix element merging and HepMC output for RIVET</h3>

An example how to produce matrix element merged events to be analysed
with RIVET is given by <code>main84.cc</code>. 

<p/> The main issue is that the output of separate RIVET runs can not
in general be combined. To perform a matrix element merging, we
however need to runs over  different LHE files. The solution to this
problem (so far) is to only perform  one RIVET run for all matrix
elements, i.e. print the events for all ME parton  multiplicities,
with the correct weights, to a single HepMC file. Since the correct
weight includes the cross section of the different samples after
Sudakov vetoes --- which is not a priori known --- the cross sections
have to be  estimated in a test run, before the actual production run
is performed. Finally, the cross section of the last event in the
HepMC file has to be taken as the  full merged cross section
<i>sigma_merge = Sum_{i=0}^N Sum_{j=0}*^{nEvents} 
sigma_est(i)*wckkwl(j)</i>.

<p/> This procedure is outlined in <code>main84.cc</code>. 

<br/><br/><hr/>
<h3>Further variables</h3>

For more advanced manipulations of the merging machinery, all
parameter  changes that were investigated in [<a href="Bibliography.php" target="page">Lon11</a>] are
supplied. Please  check [<a href="Bibliography.php" target="page">Lon11</a>] for a detailed discussion of
the switches.

<p/> These switches allow enthusiastic users to perform a systematic
assessment of the merging prescription. Apart from this, we advise the
non-expert user to keep the default values.

<br/><br/><strong>Merging:includeMassive</strong>  <input type="radio" name="8" value="on" checked="checked"><strong>On</strong>
<input type="radio" name="8" value="off"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>on</strong></code>)<br/>
If on, use the correct massive evolution variable and massive
splitting kernels in the reconstruction and picking of parton shower
histories of the matrix  element. If off, reconstruct evolution
scales, kinematics and splitting kernels  as if all partons were
massless.
  

<br/><br/><strong>Merging:enforceStrongOrdering</strong>  <input type="radio" name="9" value="on"><strong>On</strong>
<input type="radio" name="9" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If on, preferably pick parton shower histories of the matrix element
which  have strongly ordered consecutive splittings, i.e. paths in
which consecutive reclustered evolution scales are separated by a
user-defined factor.
  

<br/><br/><table><tr><td><strong>Merging:scaleSeparationFactor </td><td></td><td> <input type="text" name="10" value="1.0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1.0</strong></code>; <code>minimum = 1.0</code>; <code>maximum = 10.0</code>)</td></tr></table>
The factor by which scales should differ to be classified as strongly
ordered.
  

<br/><br/><strong>Merging:orderInRapidity</strong>  <input type="radio" name="11" value="on"><strong>On</strong>
<input type="radio" name="11" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If on, preferably pick parton shower histories of the matrix element
with  consecutive splittings ordered in rapidity and <i>pT</i>.
  

<br/><br/><strong>Merging:pickByFullP</strong>  <input type="radio" name="12" value="on" checked="checked"><strong>On</strong>
<input type="radio" name="12" value="off"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>on</strong></code>)<br/>
If on, pick parton shower histories of the matrix element by the full
shower  splitting kernels, including potential ME corrections and
Jacobians from joined evolution measures.
  

<br/><br/><strong>Merging:pickByPoPT2</strong>  <input type="radio" name="13" value="on"><strong>On</strong>
<input type="radio" name="13" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If on, pick parton shower histories of the matrix element by the
shower  splitting kernels divided by the evolution <i>pT</i>.
  

<br/><br/><strong>Merging:pickBySumPT</strong>  <input type="radio" name="14" value="on"><strong>On</strong>
<input type="radio" name="14" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If on, exclusively pick parton shower histories of the matrix element
for which have the smallest sum of scalar evolution <i>pT</i> for consecutive
splittings has been calculated.
  

<br/><br/><strong>Merging:includeRedundant</strong>  <input type="radio" name="15" value="on"><strong>On</strong>
<input type="radio" name="15" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If on, then also include PDF ratios and <i>&alpha;<sub>s</sub></i> 
factors in the  splitting probabilities used for picking a parton shower 
history of the matrix  element, when picking histories by the full shower
splitting probability. As argued in  [<a href="Bibliography.php" target="page">Lon11</a>], this should not
be done since a reweighting with PDF ratios and <i>&alpha;<sub>s</sub></i>
factors will be performed. However, it can give useful insight in how
sensitive the results  are to the prescription on how to choose PS
histories.
  

<br/><br/><table><tr><td><strong>Merging:nonJoinedNorm </td><td></td><td> <input type="text" name="16" value="1.0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1.0</strong></code>; <code>minimum = 0.0</code>; <code>maximum = 10.0</code>)</td></tr></table>
Normalisation factor with which to multiply splitting probability for
splittings without joined evolution equation.
  

<br/><br/><table><tr><td><strong>Merging:fsrInRecNorm </td><td></td><td> <input type="text" name="17" value="1.0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1.0</strong></code>; <code>minimum = 0.0</code>; <code>maximum = 10.0</code>)</td></tr></table>
Normalisation factor with which to multiply splitting probability for
final state splittings with an initial state recoiler.
  

<br/><br/><table><tr><td><strong>Merging:aCollFSR </td><td></td><td> <input type="text" name="18" value="1.0" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>1.0</strong></code>; <code>minimum = 0.0</code>; <code>maximum = 10.0</code>)</td></tr></table>
Factor with which to multiply the scalar <i>pT</i> of a final state
splitting, when choosing the history by the smallest sum of scalar
<i>pT</i>. Default value taken from Herwig++ [<a href="Bibliography.php" target="page">Tul09</a>].
  

<br/><br/><table><tr><td><strong>Merging:aCollISR </td><td></td><td> <input type="text" name="19" value="0.9" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.9</strong></code>; <code>minimum = 0.0</code>; <code>maximum = 10.0</code>)</td></tr></table>
Factor with which to multiply the scalar <i>pT</i> of an initial state
splitting, when choosing the history by the smallest sum of scalar
<i>pT</i>. Default value taken from Herwig++ [<a href="Bibliography.php" target="page">Tul09</a>].
  

<br/><br/><table><tr><td><strong>Merging:unorderedScalePrescrip  </td><td>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>; <code>minimum = 0</code>; <code>maximum = 1</code>)</td></tr></table>
When the parton shower history of the matrix element contains a
sequence of splittings which are not ordered in evolution <ei>pT</ei> 
(called an unordered history), this sequence is interpreted as a combined
emission. Then, a decision on which starting scale for trial emissions
off reconstructed states in this sequence of unordered splittings has
to be made. Two options are available:
<br/>
<input type="radio" name="20" value="0" checked="checked"><strong>0 </strong>:  Use larger of the two reconstructed (unordered) scales as  starting scale. <br/>
<input type="radio" name="20" value="1"><strong>1 </strong>:  Use smaller of the two reconstructed (unordered) scales as  starting scale. <br/>

<br/><br/><table><tr><td><strong>Merging:unorderedASscalePrescrip  </td><td>  &nbsp;&nbsp;(<code>default = <strong>1</strong></code>; <code>minimum = 0</code>; <code>maximum = 1</code>)</td></tr></table>
Prescription which scale to use to evaluate <ei>&alpha;<sub>s</sub></ei> 
weight for  splittings in a sequence of splittings which are not ordered 
in evolution <ei>pT</ei>.
<br/>
<input type="radio" name="21" value="0"><strong>0 </strong>:  Use the combined splitting scale as argument in <ei>&alpha;<sub>s</sub></ei>, for both splittings. <br/>
<input type="radio" name="21" value="1" checked="checked"><strong>1 </strong>:  Use the true reconstructed scale  as as argument in <ei>&alpha;<sub>s</sub></ei>, for each splitting separately. <br/>

<br/><br/><table><tr><td><strong>Merging:incompleteScalePrescrip  </td><td>  &nbsp;&nbsp;(<code>default = <strong>0</strong></code>; <code>minimum = 0</code>; <code>maximum = 2</code>)</td></tr></table>
When no complete parton shower history (i.e. starting from a 
<ei>2 &rarr; 2</ei> process)  for a matrix element with additional jets 
can be found, such a configuration is said to have an incomplete history. 
Since in incomplete histories, not all  shower starting scales are 
determined by clusterings, a prescription for setting the starting scale 
of trial showers in incomplete histories is needed. Three options are 
provided.
<br/>
<input type="radio" name="22" value="0" checked="checked"><strong>0 </strong>:  Use factorisation scale as shower starting scale for  incomplete histories. <br/>
<input type="radio" name="22" value="1"><strong>1 </strong>:  Use <ei>sHat</ei> as shower starting scale for   incomplete histories. <br/>
<input type="radio" name="22" value="2"><strong>2 </strong>:  Use <ei>s</ei> as shower starting scale for   incomplete histories. <br/>

<br/><br/><strong>Merging:allowColourShuffling</strong>  <input type="radio" name="23" value="on"><strong>On</strong>
<input type="radio" name="23" value="off" checked="checked"><strong>Off</strong>
 &nbsp;&nbsp;(<code>default = <strong>off</strong></code>)<br/>
If on, this will allow the algorithm to swap one colour index in the state,
when trying to find all possible clusterings, if no clustering has been
found, but more clusterings had been requested. In this way, some incomplete
histories can be avoided. Generally, we advise the non-expert user to not
touch this switch, because a slight change in the colour structure can change
the radiation pattern. To however study the sensitivity of the predictions on
these effects, allowing for colour reshuffling can be useful.
  


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
$data = "Merging:doKTMerging = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "1")
{
$data = "Merging:ktType = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "0")
{
$data = "Merging:nJetMax = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "0.0")
{
$data = "Merging:TMS = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "void")
{
$data = "Merging:Process = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "off")
{
$data = "Merging:doMGMerging = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "off")
{
$data = "Merging:doUserMerging = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
if($_POST["8"] != "on")
{
$data = "Merging:includeMassive = ".$_POST["8"]."\n";
fwrite($handle,$data);
}
if($_POST["9"] != "off")
{
$data = "Merging:enforceStrongOrdering = ".$_POST["9"]."\n";
fwrite($handle,$data);
}
if($_POST["10"] != "1.0")
{
$data = "Merging:scaleSeparationFactor = ".$_POST["10"]."\n";
fwrite($handle,$data);
}
if($_POST["11"] != "off")
{
$data = "Merging:orderInRapidity = ".$_POST["11"]."\n";
fwrite($handle,$data);
}
if($_POST["12"] != "on")
{
$data = "Merging:pickByFullP = ".$_POST["12"]."\n";
fwrite($handle,$data);
}
if($_POST["13"] != "off")
{
$data = "Merging:pickByPoPT2 = ".$_POST["13"]."\n";
fwrite($handle,$data);
}
if($_POST["14"] != "off")
{
$data = "Merging:pickBySumPT = ".$_POST["14"]."\n";
fwrite($handle,$data);
}
if($_POST["15"] != "off")
{
$data = "Merging:includeRedundant = ".$_POST["15"]."\n";
fwrite($handle,$data);
}
if($_POST["16"] != "1.0")
{
$data = "Merging:nonJoinedNorm = ".$_POST["16"]."\n";
fwrite($handle,$data);
}
if($_POST["17"] != "1.0")
{
$data = "Merging:fsrInRecNorm = ".$_POST["17"]."\n";
fwrite($handle,$data);
}
if($_POST["18"] != "1.0")
{
$data = "Merging:aCollFSR = ".$_POST["18"]."\n";
fwrite($handle,$data);
}
if($_POST["19"] != "0.9")
{
$data = "Merging:aCollISR = ".$_POST["19"]."\n";
fwrite($handle,$data);
}
if($_POST["20"] != "0")
{
$data = "Merging:unorderedScalePrescrip = ".$_POST["20"]."\n";
fwrite($handle,$data);
}
if($_POST["21"] != "1")
{
$data = "Merging:unorderedASscalePrescrip = ".$_POST["21"]."\n";
fwrite($handle,$data);
}
if($_POST["22"] != "0")
{
$data = "Merging:incompleteScalePrescrip = ".$_POST["22"]."\n";
fwrite($handle,$data);
}
if($_POST["23"] != "off")
{
$data = "Merging:allowColourShuffling = ".$_POST["23"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2012 Torbjorn Sjostrand -->
