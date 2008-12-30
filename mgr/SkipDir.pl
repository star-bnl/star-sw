#!/usr/local/bin/perl

#
# (c) J. Lauret 2007-2009
#
# Initially written in 2007 for generating the SKIP_DIRS
# command based on the content of the AutoBuild config
# file, expanded 2008/12 to also generate a deletion 
# command for cleaning up.
# 
#

# Some files for output
$OF1="defineSD.csh";
$OF2="deleteSD.csh";
    
if ( -e $OF1){  unlink($OF1);}
if ( -e $OF2){  unlink($OF2);}

if ( ! open(FI,".ABrc_$^O") ){
    die "No .ABrc_$^O found\n";
}
# else
while ( defined($l=<FI>) ){
    $l =~ s/#.*//;
    if ($l =~ m/(SKIP_DIRS=)(.*)/){
	$line .= $2;
    }
}
close(FI);

$line =~ s/\s+/ /g;
chop($line); 



open(FO1,">$OF1");
open(FO2,">$OF2");

print FO1 "#\!/bin/csh\n";
print FO2 "#\!/bin/csh\n";


print FO1 "setenv SKIP_DIRS \"$line\"\n";
close(FO1);
chmod(0755,$OF1);
print "$OF1 is ready\n";

$count = 0;
@items = split(" ",$line); 
foreach $dir (@items){ 
    foreach $pat ( (".".$ENV{STAR_HOST_SYS}."/*/StRoot/$dir",".".$ENV{STAR_HOST_SYS}."/*/*$dir*.so") ){
	@N = glob($pat); 
	if ($#N != -1){ 
	    if ( -f $N[0]){
		print FO2 "echo \"Removing file $pat\" && /bin/rm -f  $pat\n";
	    } else {
		print FO2 "echo \"Removing dir  $pat\" && /bin/rm -fr $pat\n";
	    }
	    $count += $#N+1;
	}; 
    }
}
if ( $count != 0){
    print FO2 "echo \"$count items deleted\"\n";
    close(FO2);
    chmod(0755,$OF2);
    print "$OF2 is ready\n";
} else {
    close(FO2);
    unlink($OF2);
    print "Removing $OF2 (nothing to do / all clean)\n";
}







