<html>
<head>
<title>Semi-Internal Processes</title>
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

<form method='post' action='SemiInternalProcesses.php'>

<h2>Semi-Internal Processes</h2>

Normally users are expected to implement new processes via the
<?php $filepath = $_GET["filepath"];
echo "<a href='LesHouchesAccord.php?filepath=".$filepath."' target='page'>";?>Les Houches Accord</a>. Then 
you do all flavour, colour and phase-space selection externally, 
before your process-level events are input for further processing
by PYTHIA. However, it is also possible to implement a 
new process in exactly the same way as the internal PYTHIA
ones, thus making use of the internal phase space selection machinery
to sample an externally provided cross-section expression. 
This page gives a brief summary how to do that. If you additionally 
want to introduce a new resonance species, with its own internal 
width calculations, you will find further instructions 
<?php $filepath = $_GET["filepath"];
echo "<a href='SemiInternalResonances.php?filepath=".$filepath."' target='page'>";?>here</a>.

<p/>
Should you actually go ahead, it is strongly recommended to shop around 
for a similar process that has already been implemented, and to use that 
existing code as a template. Look for processes with the same combinations
of incoming flavours and colour flows, rather than the shape of the 
cross section itself. With a reasonable such match the task should be 
of medium difficulty, without it more demanding. 

<p/>
PYTHIA is rather good at handling the phase space of 
<i>2 -> 1</i> and <i>2 -> 2</i> processes, is more primitive for 
<i>2 -> 3</i> ones and does not at all address higher multiplicities. 
This limits the set of processes that you can implement in this 
framework. The produced particles may be resonances, however, so it is 
possible to end up with bigger "final" multiplicities through sequential 
decays, and to include further matrix-element weighting in those decays.   

<p/>
There are two steps involved in implementing a process:
<br/>1) writing a new class,  where the matrix elements are implemented, 
including information on incoming and outgoing flavours and colours, and 
<br/>2) making the process available.
<br/>We consider these two aspects in turn. An example where it all comes
together is found in <code>main25.cc</code>.

<h3>The Cross Section Class</h3> 

The matrix-element information has to be encoded in a new class.
The relevant code could either be put before the main program in the
same file, or be stored separately, e.g. in a matched pair
of <code>.h</code> and <code>.cc</code> files. The latter may be more
convenient, in particular if the cross sections are lengthy, or if you 
intend to build up your own little process library, but of course
requires that these additional files are correctly compiled and linked.

<p/>
The class has to be derived either from  
<code>Sigma1Process</code>, for <i>2 -> 1</i> processes, from 
<code>Sigma2Process</code>, for <i>2 -> 2</i> ones, or from 
<code>Sigma3Process</code>, for <i>2 -> 3</i> ones. (The 
<code>Sigma0Process</code> class is used for elastic, diffractive
and minimum-bias events, and is not recommended for use beyond that.) 
These are in their turn derived from the <code>SigmaProcess</code> 
base class. 

<p/>
The class can implement a number of methods. Some of these are 
compulsory, others strongly recommended, and the rest are to be
used only when the need arises to override the default behaviour. 
The methods are:

<p/>
A <b>constructor</b> for the derived class obviously must be available.
Here you are quite free to allow a list of arguments, to set
the parameters of your model, or even to create a set of closely
related but distinct processes. For instance, <i>g g -> Q Qbar</i>, 
<i>Q = c</i> or <i>b</i>, is only coded once, and then the 
constructor takes the quark code (4 or 5)  as argument, 
to allow the proper amount of differentiation. 

<p/>
A <b>destructor</b> is only needed if you plan to delete the process 
before the natural end of the run, and require some special behaviour 
at that point. If you call such a destructor you will leave a pointer 
dangling inside the <code>Pythia</code> object you gave it in to,
if that still exists. 

<p/><code>method&nbsp; </code><strong> void initProc() &nbsp;</strong> <br/>
is called once during initalization, and can then be used to set up
parameters, such as masses and couplings, and perform calculations 
that need not be repeated for each new event, thereby saving time. 
This method needs not be implemented, since in principle all 
calculations can be done in <code>sigmaHat</code> below.

<p/><code>method&nbsp; </code><strong> void sigmaKin() &nbsp;</strong> <br/>
is called once a kinematical configuration has been determined, but 
before the two incoming flavours are known. This routine can therefore 
be used to perform calculations that otherwise might have to be repeated 
over and over again in <code>sigmaHat</code> below. For instance 
a flavour-independent cross section calculation for a <i>q g</i> 
initial state would be repeated 20 times in <code>sigmaHat</code>, 
five times for the five quark flavours allowed in the incoming beams, 
times twice to include antiquarks, times twice since the (anti)quark 
could be in either of the two beams. You could therefore calculate the 
result once only and store it as a private data member of the class. 
It is optional whether you want to use this method, however, or put 
everything in <code>sigmaHat</code>.

<p/><code>method&nbsp; </code><strong> double sigmaHat() &nbsp;</strong> <br/>
is the key method for cross section calculations and returns a cross section
value, as further described below. It is called when also a preliminary set 
of incoming flavours has been picked, in addition to the kinematical ones 
already available for <code>sigmaKin</code>. Typically <code>sigmaHat</code> 
is called inside a loop over all allowed incoming flavour combinations,
stored in <code>id1</code> and <code>id2</code>, with fixed kinematics, 
as already illustrated above. The sum over the different flavour combinations 
provides the total cross section, while their relative size is used to make 
a selection of a specific incomimg state.
<br/>For a <i>2 -> 1</i> process, the returned value should be 
<i>sigmaHat(sHat)</i>, where <code>mH</code> (= <i>mHat</i>), 
<code>sH</code> (= <i>sHat</i>) and <code>sH2</code> (= <i>sHat^2</i>) 
are available to be used. 
<br/>For a <i>2 -> 2</i> process, instead
<i>d(sigmaHat)/d(tHat)</i> should be returned, based on
provided <code>mH, sH, sH2, tH, tH2, uH, uH2, m3, s3, m4, s4</code> and 
<code>pT2</code> values (<code>s3 = m3*m3</code> etc.). 
<br/>For a <i>2 -> 3</i> process, instead <i>|M|^2</i> should be 
returned, with normalization such that <i>|M|^2 / (2 sHat)</i> integrated 
over the three-body phase space gives the cross section. Here no standard 
set of variables exist. Instead the obvious ones, 
<code>mH, sH, m3, s3, m4, s4, m5, s5</code>, are complemented by the 
four-vectors <code>p3cm, p4cm, p5cm</code>, from which further invariants 
may be calculated. The four-vectors are defined in the cm frame of the 
subcollision, with incoming partons along the <i>+-z</i> axis.   
<br/>In either case, <i>alpha_s</i> and <i>alpha_em</i> have already 
been calculated, and are stored in <code>alpS</code> and <code>alpEM</code>. 
Also other standard variables may be used, like 
<code>CoupEW::sin2thetaW()</code>, and related flavour-dependent
vector and axial couplings in <code>CoupEW</code> and CKM combinations
in <code>VCKM</code>. 
<br/>In case some of the final-state particles are resonances, their 
squared masses have already been selected according to a Breit-Wigner
with a linearly running width <i>Gamma(m) = Gamma(m_0) * m / m_0</i>.
More precisely, the mass spectrum is weighted according to
<i>w_BW(m^2) d(m^2)</i>, where
<br/><i>
w_BW(m^2) = (1/pi) * (m * Gamma(m)) / ( (m^2 - m_0^2)^2 + (m * Gamma(m))^2 ) . 
</i><br/> 
If you would like to have another expression, the above weights are stored 
in <code>runBW3</code>, <code>runBW4</code> and <code>runBW5</code>, 
respectively. If you divide out one of these factors, you just remain with 
a phase space selection <i>d(m^2)</i> for this particle,
and can multiply on your desired shape factor instead. Unfortunately, the 
Monte Carlo efficiency will drop if your new mass distribution differs 
dramatically from the input one. Therefore it does make sense to adjust the 
database value of the width to be slightly (but not too much) broader 
than the distribution you have in mind. Also note that, already by default, 
the wings of the Breit-Wigner are oversampled (with a compensating lower 
internal weight) by partly sampling like <i>(a + b/m^2 + c/m^4) d(m^2)</i>,
where the last term is only used for <i>gamma^*/Z^0</i>.

<p/><code>method&nbsp; </code><strong> void setIdColAcol() &nbsp;</strong> <br/>
is called only once an initial state and a kinematical configuration has 
been picked. This routine must set the complete flavour information and 
the colour flow of the process. This may involve further random choices,
between different possible final-state flavours or between possible 
competing colour flows. Private data members of the class may be used to 
retain some information from the previous steps above.
<br/>When this routine is called the two incoming flavours have already 
been selected and are available in <code>id1</code> and <code>id2</code>, 
whereas the one, two or three outgoing ones either are fixed for a given 
process or can be determined from the instate (e.g. whether a <i>W^+</i> 
or <i>W^-</i> was produced).  There is also a standard method in 
<code>VCKM</code> to pick a final flavour from an initial one with CKM 
mixing. Once you have figured out the value of
<code>id3</code> and, the case being, <code>id4</code> and 
<code>id5</code>, you store these values permanently by a call
<code>setId( id1, id2, id3, id4, id5)</code>, where the last two may be 
omitted if irrelevant. 
<br/>Correspondingly, the colours are stored with
<code>setColAcol( col1, acol1, col2, acol2, col3, acol3, col4, acol4, 
col5, acol5)</code>, where the final ones may be omitted if irrelevant.
Les Houches style colour tags are used, but starting with number 1
(and later shifted by the currently requested offset). The 
input is grouped particle by particle, with the colour index before the 
anticolour one. You may need to select colour flow dynamically, depending 
on the kinematics, when several distinct possibilities exist. Trivial 
operations, like swapping colours and anticolours, can be done with 
existing methods.
<br/>When the <code>id3Mass()</code> and <code>id4Mass()</code>
methods have been used, the order of the outgoing particles may be 
inconsistent with the way the <i>tHat</i> and <i>uHat</i>
variables have been defined. A typical example would be a process like
<i>q g -> q' W</i> with <i>tHat</i> defined between incoming and 
outgoing quark, but where <code>id3Mass() = 24</code> and so the
process is to be stored as <i>q g -> W q'</i>. One should then put
the variable <code>swapTU = true</code> in <code>setIdColAcol()</code>
for each event where the <i>tHat</i> and <i>uHat</i> variables 
should be swapped before the event kinematics is reconstructed. This 
variable is automatically restored to <code>false</code> for each new 
event.

<p/><code>method&nbsp; </code><strong> double weightDecayFlav( Event& process) &nbsp;</strong> <br/>
is called to allow a reweighting of the simultaneous flavour choices of 
resonance decay products. Is currently only used for the 
<i>q qbar -> gamma*/Z^0 gamma*/Z^0</i> process, and will likely not
be of interest for you. 

<p/><code>method&nbsp; </code><strong> double weightDecay( Event& process, int iResBeg, int iResEnd) &nbsp;</strong> <br/>
is called when the basic process has one or several resonances, after each 
set of related resonances in <code>process[i]</code>,
<code>iResBeg</code> &lt;= <code>i </code> &lt;= <code>iResEnd</code>, 
has been allowed to decay. The calculated weight, to be normalized 
to the range between 0 and 1, is used to decide whether to accept the 
decay(s) or try for a new decay configuration. The base-class version of
this method returns unity, i.e. gives isotropic decays by default. 
This method may be called repeatedly for a single event. For instance, in 
<i>q qbar -> H^0 Z^0</i> with <i>H^0 -> W^+ W^-</i>, a first call 
would be made after the <i>H^0</i> and <i>Z^0</i> decays, and then 
depend only on the <i>Z^0</i> decay angles since the <i>H^0</i> 
decays isotropically. The second call would be after the <i>W^+ W^-</i> 
decays and then involve correlations between the four daughter fermions.

<p/><code>method&nbsp; </code><strong> string name() &nbsp;</strong> <br/>
returns the name of the process, as you want it to be shown in listings.

<p/><code>method&nbsp; </code><strong> int code() &nbsp;</strong> <br/>
returns an integer identifier of the process. This has no internal function, 
but is only intended as a service for the user to rapidly (and hopefully
uniquely) identify which process occured in a given event. Numbers below 
10000 are reserved for internal PYTHIA use. 

<p/><code>method&nbsp; </code><strong> string inFlux() &nbsp;</strong> <br/>
this string specifies the combinations of incoming partons that are 
allowed for the process under consideration, and thereby which incoming
flavours <code>id1</code> and <code>id2</code> the <code>sigmaHat()</code> 
calls will be looped over. It is always possible to pick a wider flavour 
selection than strictly required and then put to zero cross sections in 
the superfluous channels, but of course this may cost some extra execution 
time. Currently allowed options are:
<br/>* <code>gg</code>: two gluons. 
<br/>* <code>qg</code>: one (anti)quark and one gluon.
<br/>* <code>qq</code>: any combination of two quarks, two antiquarks or
a quark and an antiquark.
<br/>* <code>qqbarSame</code>: a quark and its antiquark;
this is a subset of the above <code>qq</code> option.
<br/>* <code>ff</code>: any combination of two fermions, two antifermions 
or a fermion and an antifermion; is the same as <code>qq</code> for 
hadron beams but also allows processes to work with lepton beams.
<br/>* <code>ffbarSame</code>: a fermion and its antifermion; is the 
same as <code>qqbarSame</code> for hadron beams but also allows processes 
to work with lepton beams.
<br/>* <code>ffbarChg</code>: a fermion and an antifermion that combine 
to give charge +-1.
<br/>* <code>fgm</code>: a fermion and a photon (gamma).
<br/>* <code>ggm</code>: a gluon and a photon.
<br/>* <code>gmgm</code>: two photons.

<p/><code>method&nbsp; </code><strong> bool convert2mb() &nbsp;</strong> <br/>
it is assumed that cross sections normally come in dimensions such that
they, when integrated over the relevant phase space, obtain the dimension
GeV^-2, and therefore need to be converted to mb. If the cross section 
is already encoded as mb then <code>convert2mb()</code> should be 
overloaded to instead return <code>false</code>.

<p/><code>method&nbsp; </code><strong> int id3Mass(), int id4Mass(), int id5Mass() &nbsp;</strong> <br/>
are the one, two or three final-state flavours, where masses are to be 
selected before the matrix elements are evaluated. Only the absolute value 
should be given. For massless particles, like gluons and photons, one need 
not give anything, i.e. one defaults to 0. The same goes for normal light 
quarks, where masses presumably are not implemented in the matrix elements.  
Later on, these quarks can still (automatically) obtain constituent masses, 
once a <i>u</i>, <i>d</i> or <i>s</i> flavour has been selected. 

<p/><code>method&nbsp; </code><strong> int resonanceA(), int resonanceB() &nbsp;</strong> <br/>
are the codes of up to two <i>s</i>-channel resonances contributing to 
the matrix elements. These are used by the program to improve the phase-space 
selection efficiency, by partly sampling according to the relevant 
Breit-Wigners. Massless resonances (the gluon and photon) need not be 
specified.

<p/><code>method&nbsp; </code><strong> bool isSChannel() &nbsp;</strong> <br/>
normally the choice of renormalization and factorization scales in 
<i>2 -> 2</i> and <i>2 -> 3</i> processes is based on the assumption 
that <i>t</i>- and <i>u</i>-channel exchanges dominates the 
cross section. In cases such as <i>f fbar -> gamma* -> f' fbar'</i> a 
<i>2 -> 2</i> process actually ought to be given scales as a 
<i>2 -> 1</i> one, in the sense that it proceeds entirely through 
an <i>s</i>-channel resonance. This can be achieved if you override the
default <code>false</code> to return <code>true</code>. See further the
page on <?php $filepath = $_GET["filepath"];
echo "<a href='CouplingsAndScales.php?filepath=".$filepath."' target='page'>";?>couplings and scales</a>.

<p/><code>method&nbsp; </code><strong> int idTchan1(), int idTchan2() &nbsp;</strong> <br/>
the <i>2 -> 3</i> phase space selection machinery is rather primitive,
as already mentioned. The efficiency can be improved in processes that
proceed though <i>t</i>-channel exchanges, such as 
<i>q qbar' -> H^0 q qbar'</i> via <i>Z^0 Z^0</i> fusion, if the identity 
of the  <i>t</i>-channel-exchanged particles on the two side of the 
event are provided. Only the absolute value is of interest.

<p/><code>method&nbsp; </code><strong> double tChanFracPow1(), double tChanFracPow2() &nbsp;</strong> <br/>
in the above kind of <i>2 -> 3</i> phase-space selection, the
sampling of <i>pT^2</i> is done with one part flat, one part weighted
like <i>1 / (pT^2 + m_R^2)</i> and one part  like 
<i>1 / (pT^2 + m_R^2)^2</i>. The above values provide the relative
amount put in the latter two channels, respectively, with the first 
obtaining the rest. Thus the sum of <code>tChanFracPow1()</code> and
<code>tChanFracPow2()</code> must be below unity. The final results
should be independent of these numbers, but the Monte Carlo efficiency 
may be quite low for a bad choice. Here <i>m_R</i> is the mass of the 
exchanged resonance specified by <code>idTchan1()</code> or 
<code>idTchan2()</code>. Note that the order of the final-state 
listing is important in the above <i>q qbar' -> H^0 q qbar'</i> example, 
i.e. the <i>H^0</i> must be returned by <code>id3Mass()</code>,
since it is actually the <i>pT^2</i> of the latter two that are 
selected independently, with the first <i>pT</i> then fixed  
by transverse-momentum conservation.

<p/><code>method&nbsp; </code><strong> useMirrorWeight() &nbsp;</strong> <br/>
in <i>2 -> 3</i> processes the phase space selection used here
involves a twofold ambiguity basically corresponding to a flipping of 
the positions of last two outgoing particles. These are assumed equally 
likely by default, <code>false</code>, but for processes proceeding entirely 
through <i>t</i>-channel exchange the Monte Carlo efficiency can be 
improved by making a preselection based on the relative propagator
weights, <code>true</code>.  

<p/><code>method&nbsp; </code><strong> int gmZmode() &nbsp;</strong> <br/>
allows a possibility to override the global mode 
<?php $filepath = $_GET["filepath"];
echo "<a href='ElectroweakProcesses.php?filepath=".$filepath."' target='page'>";?><code>WeakZ0:gmZmode</code></a> 
for a specific process. The global mode normally is used to switch off 
parts of the <i>gamma^*/Z^0</i> propagator for test purposes. The
above local mode is useful for processes where a <i>Z^0</i> really is
that and nothing more, such as <i>q qbar -> H^0 Z^0</i>. The default
value -1 returned by <code>gmZmode()</code> ensures that the global
mode is used. 

<h3>Access to a process</h3> 

Once you have implemented a class, it is straightforward to make use of 
it in a run. Assume you have written a new class <code>MySigma</code>, 
which inherits from <code>Sigma1Process</code>, <code>Sigma2Process</code> 
or <code>Sigma3Process</code>, which in their turn inherit from 
<code>SigmaProcess</code>. You then create an instance of this class
and hand it in to a <code>pythia</code> object with 
<pre>
      SigmaProcess* mySigma = new MySigma();
      pythia.setSigmaPtr( mySigma); 
</pre>
If you have several processes you can repeat the procedure any number
of times. When <code>pythia.init(...)</code> is called these processes 
are initialized along with any internal processes you may have switched on, 
and treated in exactly the same manner. The  <code>pythia.next()</code> 
will therefore generate a mix of the different kinds of processes without 
distinction. See also the <?php $filepath = $_GET["filepath"];
echo "<a href='ProgramFlow.php?filepath=".$filepath."' target='page'>";?>Program Flow</a> 
description.

<p/>
If the code should be of good quality and general usefulness, it would
be simple to include it as a permanently available process in the 
standard program distribution. The final step of that integration ought to 
be left for the PYTHIA authors, but here is a description of what is 
required.
 
<p/>
A flag has to be defined, that allows the process to be switched on;
by default it should always be off. The name of the flag should be 
chosen of the type <code>model:process</code>. Here the 
<code>model</code> would be related to the general scenario considered,
e.g. <code>Compositeness</code>, while <code>process</code> would
specify instate and outstate, separated by a 2 (= to), e.g.
<code>ug2u*g</code>. 
When several processes are implemented and "belong together" it is 
also useful to define a <code>model:all</code> switch that affects
all the separate processes. 

<p/>
The flags should normally be stored in the <code>ProcessSelection.xml</code>
file or one of its daughters for a specific kind of processes. This is to 
make them easily found by users. You could create and use your own 
<code>.xml</code> file, so long as you then add that name to the 
list of files in the <code>Index.xml</code> file. (If not,
the flags would never be created and the program would not work.)  

<p/>
In the <code>ProcessContainer.c</code> file, the
<code>SetupContainers::init()</code> method needs to be expanded to
create instances of the processes switched on. This code is fairly
repetitive, and should be easy to copy and modify from the code 
already there. The basic structure is 
<br/>(i) check whether a process is requested by the user and, if so, 
<br/>(ii) create an instance of the matrix-element class, 
<br/>(iii)create a container for the matrix element and its associated 
phase-space handling, and 
<br>(iv) add the container to the existing process list.  

<p/>
Two minor variations are possible. One is that a set of related 
processes are lumped inside the the same initial check, i.e. are 
switched on all together. The second is that the matrix-element 
constructor may take arguments, as specified by you (see above). 
If so, the same basic matrix element may be recycled for a set of 
related processes, e.g. one for a composite <i>u</i> and one for 
a composite <i>d</i>. Obviously these variations may be combined.

</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
