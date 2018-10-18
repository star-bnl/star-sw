#!/usr/local/bin/perl

$dir = $ARGV[0];
if ($dir eq '.' or $dir eq "") {$dir = `pwd`;}

open(files,"ls $dir|");

if (-e "index.html")
	{
	print "Are you sure you want to rewrite index.html in $dir folder??? [y/n]\n";

	$answer = <STDIN>;
	chomp $answer;

	if ($answer ne 'y') {exit;}
	}

open(html, ">index.html");

print html <<EOF;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">

<html>
	<head>
		<title>Irakli Chakaberia</title>
	</head>
	<body>
		<a href="../index.html">UP</a><br>
EOF

$i = 1;

while (<files>)
	{
	chomp;
	if ($_ eq "index.html") {next;}
#	print html "		$_<br>\n";
	print html "		<a href=\"$_\"><img width=350 src=\"$_\"></a>\n";
	if ($i % 4 eq 0) {print html "		<br><hr>\n";}
	$i++;
	}

print html <<EOF;
	</body>
</html>
EOF

close(html);
