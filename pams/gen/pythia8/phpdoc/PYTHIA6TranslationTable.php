<html>
<head>
<title>PYTHIA 6 Translation Table</title>
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

<form method='post' action='PYTHIA6TranslationTable.php'>

<h2>PYTHIA 6 Translation Table</h2>

For those more familiar with PYTHIA 6 than PYTHIA 8, here comes a table
that shows the approximate correspondence between some commonly used
variables in the two programs.The list can be expanded to meet explicit 
needs (channel your suggestions via the Monte Carlo responsible of your 
collaboration), but there is no question of ever providing anywhere near 
a complete coverage.

<h3>Selecting properties of event generation</h3>

For PYTHIA 8 you should use the <code>pythia->readString("command")</code> 
to give in the commands listed below, assuming that you have access to a 
pointer <code>pythia</code> to an instance of the <code>Pythia</code> class. 

<table cellspacing="5">

<tr> <th>PYTHIA 6 </th> <th>PYTHIA 8             </th> 
<th> Comment </th> </tr>

<tr> <td>MSEL = 1  </td> <td>SoftQCD:minBias = on</td> 
<td> soft and hard QCD events (for <i>pp/pbarp</i>)</td> </tr>

<tr> <td>MSEL = 2  </td> <td>SoftQCD:all = on</td> 
<td> as above, plus elastic and diffractive (for <i>pp/pbarp</i>)</td> </tr>

<tr> <td>MSTP(61) = 0  </td> <td>PartonLevel:ISR = off</td> 
<td> no initial-state radiation </td> </tr>

<tr> <td>MSTP(71) = 0  </td> <td>PartonLevel:FSR = off</td> 
<td> no final-state radiation </td> </tr>

<tr> <td>MSTP(81) = 0  </td> <td>PartonLevel:MI = off</td> 
<td> no multiple parton-parton interactions </td> </tr>

<tr> <td>MSTP(111) = 0  </td> <td>HadronLevel:all = off</td> 
<td> no hadronization and no decays </td> </tr>

</table>

<h3>Information about generated event</h3>

Several PYTHIA 6 variables are stored in two places, and then both are 
given below. For PYTHIA 8 it is assumed that you have access to a pointer 
<code>pythia</code> to an instance of the <code>Pythia</code> class.
 
<table cellspacing="5">

<tr> <th>PYTHIA 6 </th> <th>PYTHIA 8             </th> 
<th> Comment </th> </tr>

<tr> <td>msti(1), mint(1)  </td> <td> pythia->info.code()  </td> 
<td> process ID (but changed numbering) </td> </tr>

<tr> <td>pari(13), vint(43) </td> <td> pythia->info.mHat() </td> 
<td> invariant mass of the hard subprocess </td> </tr>                     

<tr> <td>pari(17), vint(47) </td> <td> pythia->info.pTHat() </td> 
<td> transverse momentum of the hard subprocess (2 -> 2)</td> </tr>       

<tr> <td>pari(21), vint(51) </td> <td> pythia->info.QRen()  </td> 
<td> renormalization scale Q of the hard subprocess (default definition changed)</td> </tr>

<tr> <td>vint(57) </td> <td> pythia->info.alphaEM()</td> 
<td> electromagnetic coupling constant in the hard subprocess </td> </tr>

<tr> <td>vint(58) </td> <td> pythia->info.alphaS()</td> 
<td> strong coupling constant in the hard subprocess </td> </tr>

<tr> <td>msti(15), mint(15) </td> <td> pythia->info.id1()   </td> 
<td> ID of the first incoming parton </td> </tr>

<tr> <td>msti(16), mint(16) </td> <td> pythia->info.id2()   </td> 
<td> ID of the second incoming parton </td> </tr>

<tr> <td>pari(33), vint(41) </td> <td> pythia->info.x1()    </td> 
<td> momentum fraction x of the first incoming parton </td> </tr>

<tr> <td>pari(34), vint(42) </td> <td> pythia->info.x2()    </td> 
<td> momentum fraction x of the second incoming parton </td> </tr>

<tr> <td>pari(23), vint(53) </td> <td> pythia->info.QFac()  </td> 
<td> factorization scale Q of the hard subprocess (default definition changed) </td> </tr>

<tr> <td>pari(29), vint(39) </td> <td> pythia->info.pdf1()  </td> 
<td> x1*f(x1) (PDF density 1) </td> </tr>

<tr> <td>pari(30), vint(40) </td> <td> pythia->info.pdf2()  </td> 
<td> x2*f(x2) (PDF density 2) </td> </tr>

<tr> <td>pari(7), vint(97) </td> <td> pythia->info.weight()</td> 
<td> event weight (normally unity) </td> </tr>

</table>

</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
