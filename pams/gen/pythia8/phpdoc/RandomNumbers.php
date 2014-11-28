<html>
<head>
<title>Random Numbers</title>
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

<form method='post' action='RandomNumbers.php'>
    
<h2>Random Numbers</h2>

This page describes the random-number generator in PYTHIA and 
how it can be replaced by an external one.
 
<h3>Internal random numbers</h3>

The <code>Rndm</code> class generates random numbers, using the 
Marsaglia-Zaman-Tsang algorithm [<a href="Bibliography.php" target="page">Mar90</a>]. It is purely static, 
i.e. only exists in one copy, so that one cannot run several copies, 
each with the same random number sequence, by mistake. 

<p/>
Random numbers <code>R</code> uniformly distributed in 
<code>0 &lt; R &lt; 1</code> are obtained with
<pre>
   Rndm::flat();
</pre>
There are also methods to generate according to an exponential, to 
<i>x * exp(-x)</i>, to a Gaussian, or picked among a set of 
possibilites, which make use of <code>flat()</code>.

<p/>
If the random number generator is not initialized before, it will be
so the first time it is asked to generate a random number, and then
with the default seed, 19780503. You can initialize, or reinitialize,
with your own choice of seed with a 
<pre>
   Rndm::init(seed);
</pre>
Here values <code>0 &lt; seed &lt; 900 000 000</code> gives so many 
different random number sequences, while <code>seed = 0</code> will call 
the <code>Stdlib time(0)</code> function to provide a "random" 
<code>seed</code>, and <code>seed &lt; 0</code> will revert back to 
the default <code>seed</code>.

<p/>
The <code>Pythia</code> class defines <?php $filepath = $_GET["filepath"];
echo "<a href='RandomNumberSeed.php?filepath=".$filepath."' target='page'>";?>a 
flag and a mode</a>, that allows the <code>seed</code> to be set in 
the <code>Pythia::init</code> call. That would be the standard way for a 
user to pick the random number sequence in a run.

<h3>External random numbers</h3>

<code>RndmEngine</code> is a base class for the external handling of 
random-number generation. The user-written derived class is called 
if a pointer to it has been handed in with the 
<code>pythia.rndmEnginePtr()</code> method. Since the default 
Marsaglia-Zaman-Tsang algorithm is quite good, chances are that any 
replacement would be a step down, but this may still be required by 
consistency with other program elements in big experimental frameworks.

<p/>
There is only one pure virtual method in <code>RndmEngine</code>, to 
generate one random number flat in the range between 0 and 1: 
<pre>
  virtual double flat() = 0;
</pre>
Note that methods for initialization are not provided in the base 
class, in part since input parameters may be specific to the generator
used, in part since initialization can as well be taken care of 
externally to the <code>Pythia</code> code.

<p/>
An example illustrating how to run with an external random number
generator is provided in <code>main24.cc</code>.

</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
