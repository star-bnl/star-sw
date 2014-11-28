<html>
<head>
<title>Standard-Model Parameters</title>
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

<form method='post' action='StandardModelParameters.php'>

<h2>Standard-Model Parameters</h2>

<h3>The strong coupling</h3> 

The <code>AlphaStrong</code> class is used to provide a first- or 
second-order running <i>alpha_strong</i> (or, trivially, a 
zeroth-order fixed one). Formulae are the standard ones found in 
[<a href="Bibliography.php" target="page">Yao06</a>]. The second-order expression used, eq. (9.5),
may be somewhat different in other approaches (with differences
formally of higher order), so do not necessarily expect perfect
agreement, especially not at small <i>Q^2</i> scales. The starting 
<i>alpha_strong</i> value is defined at the <i>M_Z</i> mass scale.
The <i>Lambda</i> values are matched at the <i>b</i> and <i>c</i> 
flavour thresholds, such that <i>alpha_strong</i> is continuous.
For second-order matching an approximate iterative method is used.
 
<p/>
Since we allow <i>alpha_strong</i> to vary separately for 
hard processes, timelike showers, spacelike showers and  multiple 
interactions, the relevant values can be set in each of these classes. 
The default behaviour is everywhere first-order running.
 
<p/>
The <i>alpha_strong</i> calculation is initialized by 
<code>init( value, order)</code>, where <code>value</code> 
is the <i>alpha_strong</i> value at <i>M_Z</i> and <code>order</code> 
is the order of the running, 0, 1 or 2.   Thereafter the value can be 
calculated by <code>alphaS(scale2)</code>, where 
<code>scale2</code> is the <i>Q^2</i> scale in GeV^2. 

<p/>
For applications inside shower programs, a second-order <code>alpha_s</code> 
value can be obtained as the product of the two functions 
<code>alphaS1Ord(scale2)</code> and <code>alphaS2OrdCorr(scale2)</code>, 
where the first gives a simple first-order running (but with the 
second-order <i>Lambda</i>) and the second the correction factor, 
below unity, for the second-order terms. This allows a compact handling 
of evolution equations.

<h3>The electromagnetic coupling</h3> 

<p/>
The <code>AlphaEM</code> class is used to generate a running
<i>alpha_em</i>. The input <code>StandardModel:alphaEMmZ</code>
value at the <i>M_Z</i> mass is matched to a low-energy behaviour
with running starting at the electron mass threshold. The matching
is done by fitting an effective running coefficient in the region
betweeen the light-quark treshold and the charm/tau threshold. This
procedure is approximate, but good enough for our purposes. 

<p/>
Since we allow <i>alpha_em</i> to vary separately for 
hard processes, timelike showers, spacelike showers and  multiple 
interactions, the choice between using a fixed or a running 
<i>alpha_em</i> can be made in each of these classes. 
The default behaviour is everywhere first-order running.
The actual values assumed at zero momentum transfer and 
at <i>M_Z</i> are only set here, however. 

<br/><br/><table><tr><td><strong>StandardModel:alphaEM0 </td><td></td><td> <input type="text" name="1" value="0.00729735" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.00729735</strong></code>; <code>minimum = 0.0072973</code>; <code>maximum = 0.0072974</code>)</td></tr></table>
The <i>alpha_em</i> value at vanishing momentum transfer
(and also below <i>m_e</i>). 
  

<br/><br/><table><tr><td><strong>StandardModel:alphaEMmZ </td><td></td><td> <input type="text" name="2" value="0.00781751" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.00781751</strong></code>; <code>minimum = 0.00780</code>; <code>maximum = 0.00783</code>)</td></tr></table>
The <i>alpha_em</i> value at the <i>M_Z</i> mass scale. 
Default is taken from [<a href="Bibliography.php" target="page">Yao06</a>].
  

<p/>
The <i>alpha_em</i> calculation is initialized by 
<code>init(order)</code>, where <code>order</code> is the order of 
the running, 0 or 1, with -1 a special option to use the fix value
provided at <i>M_Z</i>.   Thereafter the value can be 
calculated by <code>alphaEM(scale2)</code>, where 
<code>scale2</code> is the <i>Q^2</i> scale in GeV^2. 

<h3>The electroweak couplings</h3> 

There are two degrees of freedom that can be set, related to the 
electroweak mixing angle:

<br/><br/><table><tr><td><strong>StandardModel:sin2thetaW </td><td></td><td> <input type="text" name="3" value="0.2312" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.2312</strong></code>; <code>minimum = 0.225</code>; <code>maximum = 0.240</code>)</td></tr></table>
The weak mixing angle, as used in all <i>Z^0</i> and <i>W^+-</i>
masses and couplings, except for the vector couplings of fermions
to the <i>Z^0</i>, see below. Default is the MSbar value from
[<a href="Bibliography.php" target="page">Yao06</a>].
  

<br/><br/><table><tr><td><strong>StandardModel:sin2thetaWbar </td><td></td><td> <input type="text" name="4" value="0.2315" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.2315</strong></code>; <code>minimum = 0.225</code>; <code>maximum = 0.240</code>)</td></tr></table>
The weak mixing angle, as used to derive the vector couplings of fermions
to the <i>Z^0</i>, in the relation 
<i>v_f = a_f - 4 e_f sin^2(theta_W)bar</i>. Default is the
effective-angle value from [<a href="Bibliography.php" target="page">Yao06</a>].
  

<p/>
These and various couplings can be read out from the static 
<code>CoupEW</code> class:<br/>
<code>CoupEW::sin2thetaW()</code> gives the weak mixing angle set above.<br/>
<code>CoupEW::cos2thetaW()</code> gives 1 minus it.<br/>
<code>CoupEW::sin2thetaWbar()</code> gives the weak mixing angle as used
in fermion couplings.<br/>
<code>CoupEW::ef(idAbs)</code> gives the electrical charge. Note that this 
and subsequent routines should be called with a positive 
<code>idAbs</code>.<br/>
<code>CoupEW::vf(idAbs)</code> gives the vector coupling to 
<i>Z^0</i>.<br/>
<code>CoupEW::af(idAbs)</code> gives the axial vector coupling.<br/>
<code>CoupEW::t3f(idAbs)</code> gives the weak isospin of lefthanded quarks, 
i.e. <i>a_f/2</i>.<br/> 
<code>CoupEW::lf(idAbs)</code> gives the lefthanded coupling, i.e.
<i>(v_f + a_f/2)/2</i> (other definitions may differ by a factor 
of 2).<br/> 
<code>CoupEW::rf(idAbs)</code> gives the righthanded coupling, i.e.
<i>(v_f - a_f/2)/2</i> (with comment as above).<br/> 
<code>CoupEW::ef2(idAbs)</code> gives <i>e_f^2</i>.<br/>
<code>CoupEW::vf2(idAbs)</code> gives <i>v_f^2</i>.<br/>
<code>CoupEW::af2(idAbs)</code> gives <i>a_f^2</i>.

<h3>The quark weak-mixing matrix</h3>

The absolute values of the Cabibbo-Kobayashi-Maskawa matrix elements are 
set by the following nine real values taken from [<a href="Bibliography.php" target="page">Yao06</a>] - 
currently the CP-violating phase is not taken into account in this 
parametrization. It is up to the user to pick a consistent unitary 
set of new values whenever changes are made.  

<br/><br/><table><tr><td><strong>StandardModel:Vud </td><td></td><td> <input type="text" name="5" value="0.97383" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.97383</strong></code>; <code>minimum = 0.973</code>; <code>maximum = 0.975</code>)</td></tr></table>
The <i>V_ud</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vus </td><td></td><td> <input type="text" name="6" value="0.2272" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.2272</strong></code>; <code>minimum = 0.224</code>; <code>maximum = 0.230</code>)</td></tr></table>
The <i>V_us</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vub </td><td></td><td> <input type="text" name="7" value="0.00396" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.00396</strong></code>; <code>minimum = 0.0037</code>; <code>maximum = 0.0042</code>)</td></tr></table>
The <i>V_ub</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vcd </td><td></td><td> <input type="text" name="8" value="0.2271" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.2271</strong></code>; <code>minimum = 0.224</code>; <code>maximum = 0.230</code>)</td></tr></table>
The <i>V_cd</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vcs </td><td></td><td> <input type="text" name="9" value="0.97296" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.97296</strong></code>; <code>minimum = 0.972</code>; <code>maximum = 0.974</code>)</td></tr></table>
The <i>V_cs</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vcb </td><td></td><td> <input type="text" name="10" value="0.04221" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.04221</strong></code>; <code>minimum = 0.0418</code>; <code>maximum = 0.0426</code>)</td></tr></table>
The <i>V_cb</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vtd </td><td></td><td> <input type="text" name="11" value="0.00814" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.00814</strong></code>; <code>minimum = 0.006</code>; <code>maximum = 0.010</code>)</td></tr></table>
The <i>V_td</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vts </td><td></td><td> <input type="text" name="12" value="0.04161" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.04161</strong></code>; <code>minimum = 0.039</code>; <code>maximum = 0.043</code>)</td></tr></table>
The <i>V_ts</i> CKM matrix element.
  

<br/><br/><table><tr><td><strong>StandardModel:Vtb </td><td></td><td> <input type="text" name="13" value="0.9991" size="20"/>  &nbsp;&nbsp;(<code>default = <strong>0.9991</strong></code>; <code>minimum = 0.99907</code>; <code>maximum = 0.9992</code>)</td></tr></table>
The <i>V_tb</i> CKM matrix element.
  

<p/>
These couplings can be read back out in a few alternative forms:<br/>
<code>VCKM::Vgen(genU, genD)</code> gives the CKM mixing element for
up-type generation index <code>genU</code> (1, 2 or 3) and
down-type generation index <code>genD</code>.<br/>
<code>VCKM::V2gen(genU, genD)</code> gives the square of the above.<br/>
<code>VCKM::Vid(id1, id2)</code> gives the CKM mixing element between
two quark flavours <code>id1</code> and <code>id2</code>. The sign of 
the flavours is irrelevant, since the process may be either of the type
<i>q qbar' -> W</i> or <i>q g -> W q'</i>. Flavour combinations 
with no CKM mixing (e.g. <i>u u</i>) are given a vanishing value.<br/>
<code>VCKM::V2id(id1, id2)</code> gives the square of the above.<br/>
<code>VCKM::V2sum(id)</code> gives the sum of squares that a given
flavour can couple to, excluding the top quark. Is close to unity
for the first two generations.<br/>
<code>VCKM::V2pick(id)</code> picks a CKM partner quark (with the same 
sign as <code>id</code>) according to the respective squared elements,
again excluding the top quark from the list of possibilities.

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

if($_POST["1"] != "0.00729735")
{
$data = "StandardModel:alphaEM0 = ".$_POST["1"]."\n";
fwrite($handle,$data);
}
if($_POST["2"] != "0.00781751")
{
$data = "StandardModel:alphaEMmZ = ".$_POST["2"]."\n";
fwrite($handle,$data);
}
if($_POST["3"] != "0.2312")
{
$data = "StandardModel:sin2thetaW = ".$_POST["3"]."\n";
fwrite($handle,$data);
}
if($_POST["4"] != "0.2315")
{
$data = "StandardModel:sin2thetaWbar = ".$_POST["4"]."\n";
fwrite($handle,$data);
}
if($_POST["5"] != "0.97383")
{
$data = "StandardModel:Vud = ".$_POST["5"]."\n";
fwrite($handle,$data);
}
if($_POST["6"] != "0.2272")
{
$data = "StandardModel:Vus = ".$_POST["6"]."\n";
fwrite($handle,$data);
}
if($_POST["7"] != "0.00396")
{
$data = "StandardModel:Vub = ".$_POST["7"]."\n";
fwrite($handle,$data);
}
if($_POST["8"] != "0.2271")
{
$data = "StandardModel:Vcd = ".$_POST["8"]."\n";
fwrite($handle,$data);
}
if($_POST["9"] != "0.97296")
{
$data = "StandardModel:Vcs = ".$_POST["9"]."\n";
fwrite($handle,$data);
}
if($_POST["10"] != "0.04221")
{
$data = "StandardModel:Vcb = ".$_POST["10"]."\n";
fwrite($handle,$data);
}
if($_POST["11"] != "0.00814")
{
$data = "StandardModel:Vtd = ".$_POST["11"]."\n";
fwrite($handle,$data);
}
if($_POST["12"] != "0.04161")
{
$data = "StandardModel:Vts = ".$_POST["12"]."\n";
fwrite($handle,$data);
}
if($_POST["13"] != "0.9991")
{
$data = "StandardModel:Vtb = ".$_POST["13"]."\n";
fwrite($handle,$data);
}
fclose($handle);
}

?>
</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
