#!/opt/star/bin/perl -w
#
#  
#
#     read_mail.pl - script to read production emails and move
#          jobfiles if job crashed from archive to jobfiles to resubmit them  
#  L.Didenko
#
#  Modified J.lauret for inclusion of auto-submit in one script.
#        feature may be disabled by setting up the $SSUBM flag to 0.
#        or using 2nd argument.
#        Assumed syntax :
#           % readMail.pl [ProductionLevel] [{0|1}]
###############################################################################


my $mail_line;
my $status_line;
my $job_file = "none";
my $jbStat = "n/a";
my @parts;
my $nodeID = "n/a";
my $job_line; 
my @wrd;
my $date_line;
my ($sec,$min,$hour,$mday,$mon);


# Added J.Lauret June 11th 2001. Merged from the auto_submit.pl script.
# Merging is necessary since we are now running SMRSH (more practical
# in one script anyway).
my $prodl;
if( defined($ARGV[0]) ){   
    $prodl = $ARGV[0];
} else {
    # default value
    $prodl="P01he";                                # production level
}

# root for a structure for an implied directory structure made of a
# 'jobfiles' directory and an 'archive' directory. This structure MUST
# be under nfs-tree since there would be a token issue otherwise.
my $SOURCE="/star/u/starreco/$prodl/requests/daq";

my $SUBMIT="/usr/local/bin/crs_submit.pl";         # crs submit script
my $SFLAG=0;                                       # flag for job sub. Auto set
my $SSUBM=1;                                       # set to 0 to disable submit

if( defined($ARGV[1]) ){  $SSUBM=$ARGV[1];}


# Some date for a mail file output.
($sec,$min,$hour,$mday,$mon) = localtime();

foreach my $int ( $mon,$mday ){
    $int < 10 and $int = '0'.$int;
    $thisday .= $int;
}


# This script also loses Email content. Not very good for
# debugging purposes so ...
open(FO,">>mbox.piped");



$outfile = "mail" . "_" .$thisday . "_" . "out"; 

while (<>) {
    chomp($mail_line = $_);
    print FO "$mail_line\n";

    if ($mail_line =~ /Date/) {
	$date_line = $mail_line;
    } 

    if ($mail_line =~ /job_\d+/) {
	$SFLAG += 1; # must be at least 2

	$status_line = $mail_line;
 
	if ( $status_line =~ /done/) {
	    $jbStat = "done";
	    @wrd = split (" ",$status_line);
	    $nodeID = $wrd[3];        

	} elsif ( $status_line =~ /staging failed/) {
	    $jbStat = "staging failed";
	    @wrd = split (" ",$status_line);
	    $nodeID = $wrd[4]; 

	} elsif ($status_line =~ /aborted/) {
	    $jbStat = "aborted";
	    @wrd = split (" ",$status_line);
	    $nodeID = $wrd[3]; 

	} elsif ($status_line =~ /killed/) {
	    $jbStat = "killed";
	    @wrd = split (" ",$status_line);
	    $nodeID = $wrd[3]; 

	} elsif ($status_line =~ /file not found/) {
	    $jbStat = "file not found";
	    #$nofiles_count++;
	    $nodeID = "n/a"; 

	} elsif ($status_line =~ /crashed/) {
	    $jbStat = "crashed";
	    @wrd = split (" ",$status_line);
	    $nodeID = $wrd[3]; 
	}
	
    } elsif ($mail_line =~ /Description/) {
	$SFLAG += 1; # must be at least 2

	@parts = split (":", $mail_line);
	$job_file = $parts[1];
    }

}
close(FO);

open (OUT,">> $outfile") or die "Can't open $outfile";
print OUT $date_line, "\n";
print OUT "JobInfo:   %  $jbStat  %  $nodeID  %  $job_file\n"; 
close (OUT);


if ($SFLAG == 2 && $SSUBM){
    # Now, the logic for file submission. Simple and fast ...
    opendir(JDIR,"$SOURCE/jobfiles/");

    while ( defined($file = readdir(JDIR)) ){
	if( $file =~ /$prodl/){
	    $lock = "$SOURCE/jobfiles/$file.lock";
	    if( ! -e "$lock" ){
		if ( open(FO,">lock") ){
		    system("$SUBMIT $SOURCE/jobfiles/$file");
		    rename("$SOURCE/jobfiles/$file","$SOURCE/archive/$file");
		    unlink($lock);
		    # Just for the heck of it, output submit debugging
		    &ASLog("Job $SOURCE/jobfiles/$file submitted");
		    last;
		} else {
		    &ASLog("Lock $lock creation failed");
		}
	    }
	}
    }
    close(JDIR);
}

exit;


# Subroutines ...
sub ASLog
{
    my($line)=@_;

    if ( open(FL,">>autosubmit.log") ){
	print localtime()." $line\n";
	close(FL);
    }
}
