#!/opt/star/bin/perl -w
#  Program Name            : HPSS_USER.PL
#    Original Author       : JLAURET
#    Date                  :  7-DEC-1998
#    Program Description   : (see .txt, an output of hpss_user.pl -h)
#                          :
#  References
#    Files open for Input  : hpss.pm module and  (public domain)
#                            Date::Manip DBI (with mysql built-in)
#
# Usage examples :
#
#   [perl] hpss_user.pl -t "+3 days"  -f input_example.lis
#   [perl] hpss_user.pl -t "05/10/99" -r //treat-as-rr.rcf.bnl.gov//dev/null/ -f input_example2.lis
#   [perl] hpss_user.pl -r //treat-as-rr.rcf.bnl.gov//dev/null/ blop.dat 
#   [perl] hpss_user.pl -r //treat-as-rr.rcf.bnl.gov//dev/null/ blop.dat,blip.dat 
#

use lib "/opt/star/lib";
use hpss;


hpss_toggle_debug() if ($ENV{DCDEBUG});

#
# Block is for testing in-situ
#
#if ( ! defined($ENV{DC_NEWREV}) ){
#    die "New revision is being deployed. Please try again later\n";
#}


 # This late addition prevents user having AFS directory
 # from using the DataCarousel.
 $home = $ENV{HOME};
 $home =~ s#/afs/rhic.bnl.gov/star/users/#/star/u/#; print "home $home\n";
 if($home =~ m|^/afs/|){
     print 
	 "Sorry, but due to an authentication problem, the DataCarousel\n",
	 "will not work with AFS based home directories.\n";
     die "\n";
 }


 # Check user's home file and configuration access file
 # First, the .shosts
 $sline1 ="carousel.rcf.bnl.gov starrdat"; 
 $sline2 ="hpssbat.rcf.bnl.gov starrdat";  
 &CheckFileFor("$home/.shosts","0600",$sline1,$sline2);


 # Host keys
 $sline1 = "hpssbat.rcf.bnl.gov ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAzH3wPMdP0BU0XrpKF+yt0m80+Kaa/AU24PtwO/sgwMO0Wqfr1N8ioXoBxpMO7WqHG1rXAM+Nk4Xjlwbeak2ca+ECliss2QtRo2qfvr/d7Od2hhXjgU0Ru0F614xevLoG4FZAd8y3Cl1BlM6RcAuaE0fDfX3giZxAMuxynAzRsoM=";
 $sline2 = "carousel.rcf.bnl.gov ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAzH3wPMdP0BU0XrpKF+yt0m80+Kaa/AU24PtwO/sgwMO0Wqfr1N8ioXoBxpMO7WqHG1rXAM+Nk4Xjlwbeak2ca+ECliss2QtRo2qfvr/d7Od2hhXjgU0Ru0F614xevLoG4FZAd8y3Cl1BlM6RcAuaE0fDfX3giZxAMuxynAzRsoM=";
 &CheckFileFor("$home/.ssh/known_hosts","0600",$sline1,$sline2);

 # Default is 1
 $needed_entry = 1;

 # .netrc is a bit more tricky. Left separated.
 $cline1 ="machine hpss.rcf.bnl.gov";   
 $cline2 ="login starrdat";
 if ( -e "$home/.netrc") {
     open(NFILE,"$home/.netrc");
     @ALL = <NFILE>;
     close(NFILE);
     for($i=0 ; $i < $#ALL ; $i++){
	 chomp($nline1 = $ALL[$i+0]);
	 chomp($nline2 = $ALL[$i+1]);
	 $nline1 =~ s/^\s*(.*?)\s*$/$1/;
	 $nline1 =~ s/\s+/ /g;	 
	 $nline2 =~ s/^\s*(.*?)\s*$/$1/;
	 $nline2 =~ s/\s+/ /g;
	 if ($nline1 eq $cline1 && $nline2 eq $cline2) {
	     $needed_entry = 0;
	 }
     }
     $OP = ">>$home/.netrc";
 } else {
     $OP = ">$home/.netrc";
 }
 open(NFILE,$OP);
 if ($needed_entry) {
     print "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n";
     print "Adding $cline2 to your .netrc file\n";
     print "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n";
     print NFILE "$cline1\n$cline2\npassword ReadData\n\n";
 }
 close(NFILE); 
 system("/bin/chmod 0600 $home/.netrc");


 # Don't waste time
 if($#ARGV == -1){	
     hpss_user_help();	
     exit;
 }



 # Get arguments as defined in HPSS.pm, check it out
 ($filin,$filout,$group,$date,$lifetime) = hpss_get_args();

 if($filin ne ""){
     if( hpss_check_all(0) ){
	 print "hpss_user >> opening database \n";

	 if ( $dbObj = hpss_open_database(%DataBase) ){
	     print "hpss_user >> treating records\n";
	     $saved = hpss_add_record($dbObj,\%DataBase,$filin,$filout,$group,$date,$lifetime);

	     print "hpss_user >> closing database \n";
	     $status= hpss_close_database($dbObj,%DataBase);

	     # Done ... Last checks.
	     if($saved != 0){	
		 print "$saved records entered in DataBase\n";
	     } else {
		 print "Nothing saved nor updated\n";
	     }
	     if(! $status){	print "Database Close Error !!\n";}
	 }
     } else {
	 print "Your input was not accepted\n";
     }
 }


# Routine for checking the presence of one line (or more) in a
# given file and set protection afterward.
sub CheckFileFor
{
    my($file,$prot,@lines)=@_;
    my(@ALL,$sline,$cline);
    my($i,$OP,$needed_entry);

    $needed_entry = 0;

    if( -e $file){
	$OP = ">>$file";
	open(SFILE,$file);
	while( defined($sline=<SFILE>) ) {
	    chomp($sline);
	    $sline =~ s/^\s*(.*?)\s*$/$1/;
	    $sline =~ s/\s+/ /g;
	    for($i = 0; $i <= $#lines; $i++){
		$cline = $lines[$i];
		#print ($sline eq $cline)."$cline\n";
		if( $sline eq $cline){
		    $needed_entry = $needed_entry | ($i+1);
		}
	    }
	}
	close(SFILE);
    } else {
	$OP = ">$file";
    }

    $modified = 0;
    for($i=0 ; $i <= $#lines ; $i++){
	if( ! ($needed_entry & ($i+1)) ){
	    $modified = 1;
	    open(SFILE,$OP);
	    print "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n";
	    print "Adding $lines[$i] to your $file file\n";
	    print "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n";
	    print SFILE "$lines[$i]\n";
	    close(SFILE);
	}
    }
    
    if($modified){  system("/bin/chmod $prot $file");}
}


1;


#
# Copyright 1998-2000 (c) Jerome Lauret <jlauret at mail.chem.sunysb.edu>
# Copyright 2000-2011 (c) Jerome Lauret <jlauret at bnl.gov>
# Distributed under the GNU General Public License
#
