<html>
<head>
<title>Histograms</title>
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

<form method='post' action='Histograms.php'>
    
<h2>Histograms</h2>

The <code>Hist</code> class gives a simple implementation of 
one-dimensional histograms, useful for quick-and-dirty testing, 
without the need to link to more sophisticated packages. 
For this reson it is used in many of the sample main programs
found in the <code>examples</code> subdirectory.


<p/>
A Histogram is declared by a    
<p/><code>class&nbsp; </code><strong> Hist name( title, numberOfBins, xMin, xMax) &nbsp;</strong> <br/>
where
<br/><code>argument</code><strong> title </strong>  :  
is a string with the title of the histogram at output,
  
<br/><code>argument</code><strong> numberOfBins </strong>  :  
is the number of bin the <i>x</i> range will be subdivided into, 
  
<br/><code>argument</code><strong> xMin </strong>  :  
is the lower edge of the histogram,
  
<br/><code>argument</code><strong> xMax </strong>  :  
is the upper edge of the histogram.
  
  

<p/>
For instance
<pre>
   Hist ZpT( "Z0 pT spectrum", 100, 0., 100.);
</pre>
Alternatively you can first declare it and later define it:
<pre>
   Hist ZpT;
   ZpT.book( "Z0 pT spectrum", 100, 0., 100.);
</pre>

Once declared, its contents can be added by repeated calls to 
<code>fill</code>
<p/><code>method&nbsp; </code><strong> fill( xValue, weight) &nbsp;</strong> <br/>
where
<br/><code>argument</code><strong> xValue </strong>  : 
is the <i>x</i> position where the filling should occur, and
  
<br/><code>argument</code><strong> weight </strong> (<code>default = <strong>1.</strong></code>) : 
is the amount of weight to be added at this <i>x</i> value.
  
  

<p/>
For instance
<pre>
   ZpT.fill( 22.7, 1.); 
</pre>
Since the weight defaults to 1 the last argument could have been 
omitted in this case.   

<p/>
A set of overloaded operators have been defined, so that histograms 
can be added, subtracted, divided or multiplied by each other. Then the
contents are modified accordingly bin by bin. Thus the relative
deviation between two histograms can be found as
<pre>
  diff = (data - theory) / (data + theory);
</pre>
assuming that <code>diff</code>, <code>data</code> and <code>theory</code>
have been booked with the same number of bins and <i>x</i> range. That 
responsibility rests on the user; some checks are made for compatibility, 
but not enough to catch all possible mistakes. 

<p/>
Also overloaded operations with double real numbers are available. 
Again these four operations are defined bin by bin, i.e. the 
corresponding amount is added to, subtracted from, multiplied by or
divided by each bin. The double number can come before or after the
histograms, with obvious results. Thus the inverse of a histogram 
<code>result</code> is given by <code>1. / result</code>. 
The two kind of operations can be combined, e.g.
<pre>
  allpT = ZpT + 2. * WpT
</pre>
Finally, also the <code>+=, -+, *=, /=</code> are overloaded, with 
the right-hand side being either a histogram or a real number. 

<p/>
A histogram can be printed by making use of the overloaded &lt;&lt; 
operator, e.g.:
<pre>
   cout &lt;&lt; ZpT;
</pre>
The printout format is inspired by the old HBOOK one. To understand 
how to read this format, consider the simplified example 
<pre>
                                    
        3.50*10^ 2  9                     
        3.00*10^ 2  X   7               
        2.50*10^ 2  X  1X               
        2.00*10^ 2  X6 XX                
        1.50*10^ 2  XX5XX                 
        1.00*10^ 2  XXXXX                
        0.50*10^ 2  XXXXX        

          Contents 
            *10^ 2  31122
            *10^ 1  47208
            *10^ 0  79373

          Low edge  -- 
            *10^ 1  10001 
            *10^ 0  05050
</pre>
The key feature is that the <code>Contents</code> and  
<code>Low edge</code> have to be read vertically. For instance, 
the first bin has the contents 
<code>3 * 10^2 + 4 * 10^1 + 7 * 10^0 = 347</code>. Correspondingly,
the other bins have contents 179, 123, 207 and 283. The first bin 
stretches from <code>-(1 * 10^1 + 0 * 10^0) = -10</code> to the 
beginning of the second bin, at <code>-(0 * 10^1 + 5 * 10^0) = -5</code>. 

<p/>
The visual representation above the contents give a simple impression 
of the shape. An <code>X</code> means that the contents are filled up 
to this level, a digit in the topmost row the fraction to which the 
last level is filled. So the 9 of the first column indicates this bin 
is filled 9/10 of the way from <code>3.00*10^2 = 300</code> to 
<code>3.50*10^2 = 350</code>, i.e. somewhere close to 345, 
or more precisely in the range 342.5 to 347.5.

<p/>
The printout also provides some other information, such as the
number of entries, i.e. how many times the histogram has been filled,
the total weight inside the histogram, the total weight in underflow 
and overflow, and the mean value and root-mean-square width (disregarding
underflow and overflow). The mean and width assumes that all the
contents is in the middle of the respective bin. This is especially
relevant when you plot a integer quantity, such as a multiplicity.
Then it makes sense to book with limits that are half-integers, e.g.
<pre>
   Hist multMI( "number of multiple interactions", 20, -0.5, 19.5);
</pre>
so that the bins are centered at 0, 1, 2, ..., respectively.  This also 
avoids ambiguities which bin gets to be filled if entries are
exactly at the border between two bins. Also note that the 
<code>fill( xValue)</code> method automatically performs a cast 
to double precision where necessary, i.e. <code>xValue</code> 
can be an integer. 

<p/>
Some further metods are:
<ul>
<li><code>getBinContent(iBin)</code> returns the value in bin 
<code>iBin</code>, ranging from 1 through <code>nBin</code>,
with <code>0</code> for underflow and <code>nBin + 1</code> for
overflow.</li>
<li><code>getEntries()</code> returns the number of entries.</li>
<li><code>table(ostream& = cout)</code> prints a two-column table,
where the first gives the center of each bin and the second the
corresponding bin contents. This may be useful for plotting e.g. with 
Gnuplot.</li>
<li><code>null()</code> resets bin contents.</li>
<li><code>name( title)</code> resets the title to the new string.</li>
<li><code>sameSize( Hist&)</code> checks that the number of bins and
upper and lower limits are the same as in the histogram in the 
argument.</li>
<li><code>takeLog(true)</code> take 10-logarithm of contents 
bin by bin.</li>
<li><code>takeLog(false)</code> take <i>e</i>-logarithm of contents 
bin by bin.</li>
</ul>

</body>
</html>

<!-- Copyright (C) 2008 Torbjorn Sjostrand -->
