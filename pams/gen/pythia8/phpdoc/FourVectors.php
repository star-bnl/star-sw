<html>
<head>
<title>Four-Vectors</title>
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

<form method='post' action='FourVectors.php'>

<h2>Four-Vectors</h2>

The <code>Vec4</code> class gives an implementation of four-vectors. 
The member function names are based on the assumption that these 
represent momentum vectors. Thus one can get or set 
<code>px()</code>, <code>py()</code>, <code>pz()</code> and 
<code>e()</code>, but not <i>x, y, z</i> or <i>t</i>. (When 
production vertices are defined in the particle class, this is 
partly circumvented by new methods that hide a <code>Vec4</code>.) 
All four values can be set in the constructor, or later by the 
<code>p</code> method, with input in the order 
<code>(px, py, pz, e)</code>.
 
<p/>
The <code>Particle</code> object contains a <code>Vec4 p</code> that 
stores the particle four-momentum, and another <code>Vec4 vProd</code> 
for the production vertex. Therefore a user would not normally access the 
<code>Vec4</code> class directly, but by using the similarly-named methods 
of the <code>Particle</code> class, see
<?php $filepath = $_GET["filepath"];
echo "<a href='ParticleProperties.php?filepath=".$filepath."' target='page'>";?>Particle Properties</a>.
(The latter also stores the particle mass separately, offering an element 
of redundancy, helpful in avoiding some roundoff errors.)
However, you may find some knowledge of the four-vectors
convenient, e.g. as part of some simple analysis code based
directly on the PYTHIA output, say to define the four-vector sum 
of a set of particles. 

<p/>
A set of overloaded operators are defined for four-vectors, so that 
one may naturally add, subtract, multiply or divide four-vectors with 
each other or with double numbers, for all the cases that are 
meaningful. Of course the equal sign also works as expected.
The &lt;&lt; operator is overloaded to write out the values of the
four components of a <code>Vec4</code>.

<p/>
A number of methods provides output of derived quantities, such as:
<ul>
<li><code>mCalc(), m2Calc()</code> the (squared) mass, calculated from
the four-vectors. If <i>m^2 &lt; 0</i> the mass is given with a
minus sign, <i>-sqrt(-m^2)</i>.
<li><code>pT(), pT2()</code> the (squared) transverse momentum.</li> 
<li><code>pAbs(), pAbs2()</code> the (squared) absolute momentum.</li>  
<li><code>theta()</code> the polar angle, in the range 0 through
<i>pi</i>.</li> 
<li><code>phi()</code> the azimuthal angle, in the range <i>-pi</i> 
through <i>pi</i>.</li> 
<li><code>thetaXZ()</code> the angle in the <i>xz</i> plane, in the 
range <i>-pi</i> through <i>pi</i>, with 0 along the <i>+z</i>
axis.</li> 
<li><code>pPlus(), pMinus()</code> the combinations <i>E+-p_z</i>.</li> 
</ul>

<p/>
There are also some <code>friend</code> methods that take two or three
four-vectors as argument:
<ul>
<li><code>m(Vec4&, Vec4&), m2(Vec4&, Vec4&)</code> the (squared) 
invariant mass.</li> 
<li><code>dot3(Vec4&, Vec4&)</code> the three-product. </li> 
<li><code>cross3(Vec4&, Vec4&)</code> the cross-product.</li>  
<li><code>theta(Vec4&, Vec4&), costheta(Vec4&, Vec4&)</code> the 
(cosine) of the opening angle between the vectors.</li> 
<li><code>phi(Vec4&, Vec4&), cosphi(Vec4&, Vec4&)</code> the 
(cosine) of the azimuthal angle between the vectors around the
<i>z</i> axis, in the range 0 through <i>pi</i>.</li> 
<li><code>phi(Vec4&, Vec4&, Vec4&), cosphi(Vec4&, Vec4&, Vec4&)</code> 
the (cosine) of the azimuthal angle between the first two vectors 
around the direction of the third, in the range 0 through <i>pi</i>.</li> 
</ul>

<p/>
Some member functions can be used to modify vectors, including some
for rotations and boosts:
<ul>
<li><code>rescale3(factor), rescale4(factor)</code> multiply the 
three-vector or all components by this factor.</li> 
<li><code>flip3(), flip4()</code> flip the sign of the 
three-vector or all components.</li> 
<li><code>rot(theta, phi)</code> rotate by this polar and azimuthal
angle.</li> 
<li><code>rotaxis(phi, nx, ny, nz), rotaxis(phi, n)</code> rotate 
by this azimuthal angle around the axis provided either by the 
three-vector <code>(nx, ny, nz)</code> or the four-vector 
<code>n</code>.</li> 
<li><code>bst(betaX, betaY, betaZ), bst(betaX, betaY, betaZ, gamma)</code>
boost the particle by this <i>beta</i> vector. Sometimes it may be
convenient also to provide the <i>gamma</i> value, especially for large
boosts where numerical accuracy may suffer.</li>  
<li><code>bst(Vec4&), bstback(Vec4&)</code> boost with a 
<i>beta = p/E</i> or <i>beta = -p/E</i>, respectively. 
</ul>

<p/>
For a longer sequence of rotations and boosts, and where several 
<code>Vec4</code> are to be rotated and boosted in the same way, 
a more efficient approach is to define a <code>RotBstMatrix</code>, 
which forms a separate auxiliary class. You can build up this matrix
by successive calls to the methods 
<ul>
<li><code>rot(theta, phi)</code> rotate by this polar and azimuthal
angle.</li>
<li><code>rot(Vec4& p)</code> rotate so that a vector originally along 
the <i>+z</i> axis becomes parallel with <i>p</i>. More specifically,
rotate by <i>-phi</i>, <i>theta</i> and <i>phi</i>, with angles
defined by <i>p</i>.</li>
<li><code>bst(betaX, betaY, betaZ)</code> boost the particle by this 
<i>beta</i> vector.</li>  
<li><code>bst(Vec4&), bstback(Vec4&)</code> boost with a 
<i>beta = p/E</i> or <i>beta = -p/E</i>, respectively. </li> 
<li><code>bst(Vec4& p1, Vec4& p2)</code> boost so that <i>p_1</i> 
is transformed to <i>p_2</i>. It is assumed that the two vectors 
obey <i>p_1^2 = p_2^2</i>.</li> 
<li><code>toCMframe(Vec4& p1, Vec4& p2)</code> boost and rotate to the 
rest frame of <i>p_1</i> and <i>p_2</i>, with <i>p_1</i> along
the <i>+z</i> axis.</li>
<li><code>fromCMframe(Vec4& p1, Vec4& p2)</code> rotate and boost from the 
rest frame of <i>p_1</i> and <i>p_2</i>, with <i>p_1</i> along
the <i>+z</i> axis, to the actual frame of <i>p_1</i> and <i>p_2</i>,
i.e. the inverse of the above.</li>
<li><code>rotbst(RotBstMatrix&)</code> combine an existing matrix with
another one.</li> 
<li><code>invert()</code> invert the matrix, which corresponds to an 
opposite sequence and sign of rotations and boosts.</li> 
<li><code>reset()</code> reset to no rotation/boost; default at 
creation.</li> 
</ul>

</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
