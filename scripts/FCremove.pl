#!/usr/bin/env perl

#
# Get a file list and mark all the files as un-available in DDB.
# Note that the format as been made flexible to accept a file
# list formatted by an HPSS index dump (i.e. one every 2 lines
# having some numbers and path starting with ".")
#
# Syntax is
#
# % FCremove.pl [-s] [-c] FileList.lis  [StorageType] [Site] [Node]
#
# The default is to mark files with available=0
#   Use -s to mark the file as sanity=0 instead of available=0 .
#   Use -c to cancel marking files (i.e. restore with 
#                                   availability or sanity to 1)
#
# The last 3 arguments are non-mandatory and may revert
# to default.
#

use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use FileCatalog;

$sanity = 0;
$cancel = 0;
$FILIN  = "";
$STORE  = "";
$SITE   = "";
$NODE   = "";

if ($#ARGV == -1){
    die "Syntax is : FCremove.pl [-s] [-c] FileList.lis [StorageType] [Site] [Node]\n";
}


foreach $ARG (@ARGV){
    if ($ARG eq "-s"){
	$sanity = 1;
    } elsif ($ARG eq "-c"){
	$cancel = 1;

    } elsif (substr($ARG,0,1) eq "-"){
	print "Unknown option $ARG\n";

    } else {
	# Take things in order
	if ($FILIN eq ""){ $FILIN = $ARG;}
	elsif ($STORE eq ""){ $STORE = $ARG;}
	elsif ($SITE  eq ""){ $SITE  = $ARG;}	
	elsif ($NODE  eq ""){ $NODE  = $ARG;}
    }
}



print "Will scan for files in $FILIN ";
print " storage=$STORE" if ($STORE ne "");
print " site   =$SITE"  if ($SITE  ne "");
print " NODE   =$NODE"  if ($NODE  ne "");
print "\nWill mark ".($sanity?"sanity":"available")." ".($cancel?"ON":"OFF")."\n";



open(FI,$FILIN) || die "Give a file name as input\n";

# Instantiate Catalog
$fileC = new FileCatalog();

my ($user,$passwd) = $fileC->get_connection($SITE."::Admin");
if ( ! defined($user) ){   $user   = "";}
if ( ! defined($passwd) ){ $passwd = "";}

if ( $user eq ""){
    print "Password : ";
    chomp($passwd = <STDIN>);
    $fileC->connect_as($SITE."::Admin","FC_admin",$passwd) || die "Cannot connect as FC_admin\n";
} else {
    if ( $passwd eq ""){
	print "Password for $user : ";
        chomp($passwd = <STDIN>);
    }
    $fileC->connect_as($SITE."::Admin",$user,$passwd)      || die "Cannot connect as $user\n";
}



# Turn off module debugging and script debugging
$fileC->debug_off();


while ( defined($file = <FI>)){
    chomp($file);
    next if ($file eq "");

    # this format is a dump format from HPSS 
    if ( $file =~ m/(.*: \.)(\/.*)/){
	$file = $2;
    }


    # Do not allow for spaces as separator
    @items = split(" ",$file);
    if ($#items > 0){ $file = $items[0];}

    # File list sent by the HPSS team i.e. FSS dump-lists
    # have file path starting with a "." . Strip it out.
    if ( substr($file,0,1) eq "."){
	$file = substr($file,1,length($file)-1);
    }

    if ($file =~ m/(.*\/)(.*)/){
	($path,$file) = ($1,$2);
	if ( $file =~ /::/){
	    # Catalog format with :: separator
	    ($tmp,$file) = split("::",$file);
	    $path .= $tmp;
	} else {
	    chop($path);
	}
	#print "$path $file\n";
	$fileC->clear_context();
	$fileC->set_context("path=$path",
			    "filename=$file");
	$fileC->set_context("storage=$STORE") if ($STORE ne "");
	$fileC->set_context("site=$SITE")     if ($SITE  ne "");
	$fileC->set_context("node=$NODE")     if ($NODE  ne "");
	$fileC->set_context("all=1")          if ($cancel);


	if ($sanity){
	    $fileC->set_context("sanity=".($cancel?0:1));
	} else {
	    $fileC->set_context("available=".($cancel?0:1));
	}
	@all = $fileC->run_query("path","filename");
	if( $#all != -1 ){
	    # There is no need for a foreach loop here because
	    # path/filename can only be a unique result
	    #foreach (@all){
	    #($path,$file) = split("::",$_);
	    if ( $sanity ){
		print "Marking $path $file sanity=".($cancel?1:0)."\n";
		$fileC->update_location("sanity",$cancel?1:0,1);
	    } else {
		print "".($cancel?"Enabling":"Disabling")." $path $file\n";
		$fileC->update_location("available",$cancel?1:0,1);
	    }
	    #}
	} else {
	    print "Did not find $path/$file (already marked ?)\n";
	}
    }
}

$fileC->destroy();
