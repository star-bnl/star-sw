#!/afs/rhic/local/bin/perl
#
# hilite - hilite lines containing recognized keywords
#
if( $ARGV[0] eq '-i' ){
	$mopts = 'oi';
}
else {
	$mopts = 'o';
}
#
if( $ENV{'HILITE_REGEXP'} eq "" ) {
	$HILITE_REGEXP = 'error|Command not found'
		.'|chmod \+x'
		.'|ar: Warning: creating lib'
		.'|ar: creating lib'
		.'|^ar '
		.'|ld -shared'
		.'|Can.t locate file for:'
		.'|Unresolved:'
		.'|^make.*Error 1|^make.*Stop\.$';
}
#		.'|\.h: No such file or directory'
else {
	$HILITE_REGEXP = $ENV{'HILITE_REGEXP'}
}
# print "HILITE_REGEXP = ",$HILITE_REGEXP,"\n";
#
while (<STDIN>) {
	print "[1m" if /$HILITE_REGEXP/o;
	print;
	print "[m" if /$HILITE_REGEXP/o;
}
exit;
