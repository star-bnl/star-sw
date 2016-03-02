#!/opt/star/bin/perl -w
#
#  $Id:
#
#  $Log:   read_mail.pl - script to read production emails and move
#          jobfiles if job crashed from archive to jobfiles to resubmit them  
#  L.Didenko
#
#  Modified J.lauret for inclusion of auto-submit in one script.
#        feature may be disabled by setting up the $SSUBM flag to 0.
#        or using 2nd argument.
#        Assumed syntax :
#           % readMail.pl [ProductionLevel] [{0|1}]
#
#  Configuration read from a file named readMail.conf if exists.
#  Supported keywords :
#       prodtag=XXXX       
#       qnum=XXX
#       dropq=XXX
#       autosub={0|1}
#
# By default, qnum=0 means ANY Email coming from the CRS system release
# a new job to the queue. If this parameter is specified, only Emails
# coming from that queue release a new job in that same queue for production
# version prodtag.
#
###############################################################################


my $mail_line;
my $status_line;
my $job_status;
my $job_date;
my $nowdate;
my $job_file = "none";
my $jbStat = "n/a";
my @parts = ();
my $nodeID = "n/a";
my $job_line; 
my @wrd = ();
my $date_line;
my ($sec,$min,$hour,$mday,$mon);
my $qnum=0;
my $drop=0;
my $AUTOS=1;
my $daqsize = 0;
my $jobID = 0;
my $procID = 0;
my $jobinx = 0;
my @prt = ();

# Added J.Lauret June 11th 2001. Merged from the auto_submit.pl script.
# Merging is necessary since we are now running SMRSH (more practical
# in one script anyway).
# BTW, SMRSH seems to have some nasty side effects which will be later
# resolved i.e. it now thinks it is different script if an argument
# is passed ...
my $prodl;
if( defined($ARGV[0]) ){   
    $prodl = $ARGV[0];
} else {
    # default value for production level. DO NOT change this
    # but rather use the starreco:./readMail.conf file to
    # change the default.
    $prodl ="P01hg";
}


# ... so, also read the production tag from a conf file. 
# Warning : from a cronjob, $ENV{HOME} is null.
$HOME = $ENV{HOME};
if($HOME eq ""){ $HOME = ".";}
if( -e "$HOME/readMail.conf"){
    if (open(FI,"$HOME/readMail.conf") ){
	while( defined($line = <FI>) ){
	    chomp($line);
	    if($line ne ""){
		@items = split("=",$line);
		if( $items[0] =~ m/prodtag/i){
		    $prodl   = $items[1];
		} elsif ( $items[0] =~ m/qnum/i){   # qnum
		    $qnum    = $items[1];
		} elsif ( $items[0] =~ m/drop/i){   # dropq
		    $drop    = $items[1];
		} elsif ( $items[0] =~ m/auto/i){   # autosub
		    $AUTOS   = $items[1];
		}

	    }
	}
	close(FI);
    }
}


# root for a structure for an implied directory structure made of a
# 'jobfiles' directory and an 'archive' directory. This structure MUST
# be under nfs-tree since there would be a token issue otherwise.
my $SOURCE="/star/u/starreco/$prodl/requests/daq";
my $SUBMIT="/usr/local/bin/crs_submit.pl";         # crs submit script
my $PRIO=100;                                      # default job submit priority
my $SFLAG=0;                                       # flag for job sub. Auto set
my $SSUBM=1;                                       # set to 0 to disable submit
my $MAXMBXSIZE=4194304;                            # 4 MB limit

if( defined($ARGV[1]) ){  $SSUBM=$ARGV[1];}


# Some date for a mail file output.
($sec,$min,$hour,$mday,$mon,$yr) = localtime();

my $year = $yr + 1900 ;
   $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min  < 10) { $min  = '0'.$min  };
if( $sec  < 10) { $sec  = '0'.$sec  };

$thisday = $year."-".$mon."-".$mday; 
$nowdate = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec; 

$outfile = "mail" . "_" .$thisday . "_" . "out"; 
$QFLAG   = 1==1;
@OUTPUT  = undef;   # will hold all lines we receive


while ( defined($mail_line = <STDIN>) ) {
    chomp($mail_line);
    push(@OUTPUT,$mail_line);

    $status_line = $mail_line;
    @wrd = ();
    @prt = ();

       if( $status_line =~ /JobID_processID/) {
	   $job_status = $status_line;
	    @wrd = split ("%",$status_line);
            $jobinx = $wrd[1];
            $jbStat = $wrd[2];
            @prt = split ("_",$jobinx);
            $jobID = $prt[0];
            $procID = $prt[1]; 
	    if($jbStat eq "daq_transferred") {
            $daqsize = $wrd[3];
        }else{
        $daqsize = 0;
        }         
       }   

    if ($mail_line =~ /Date/) {
	$date_line = $mail_line;
    } elsif ($mail_line =~ m/(in queue )(\d+)( with priority)/){
	# turn submission ON/OFF based on queue number
	if( $qnum != $2 && $qnum != 0){
	    $QFLAG = 1==0;
	}
    }

    if ($mail_line =~ /job_\d+/) {
	$SFLAG += 1; # must be at least 2

      } elsif ($mail_line =~ /Description/) {
	$SFLAG += 1; # must be at least 2
	@parts = ();
	@parts = split (":", $mail_line);
	$job_file = $parts[1];
    }
 }

# Now revert to treating special cases
if( defined($job_status) ){
    open (OUT,">> $outfile") or die "Can't open $outfile";
    print OUT "Datetime:  ", $nowdate, "\n";
    print OUT $date_line, "\n"; 
    print OUT "JobInfo: % $jobID % $procID % $jbStat % $daqsize\n"; 
    close (OUT);
}

#+
# Output all we received back to a log
# This script also used to lose Email content.
#-
if ( -e "mbox.piped"){
    # If gets too big, move and delete old
    my @info = stat("mbox.piped");
    if ( $info[7] >= $MAXMBXSIZE){
	unlink("mbox.piped.old") if ( -e "mbox.piped.old");
	rename("mbox.piped","mbox.piped.old");
    }
}
if ( ! -e "mbox.piped"){ 
    open(FO,">mbox.piped"); 
    print FO "Begin on ".localtime()."\n";
    close(FO);
}
chomp($HOST= `/bin/hostname -s`);
$k   = 0; while ( -e "/tmp/mbox$$"."$k.pipe" ){ $k++;}
$FLO = "/tmp/mbox$$"."$k.pipe";
if ( open(FO,">$FLO") ){
    print FO "\n[$0 ".localtime()." on $HOST Pid=$$ Idx=$k]\n";
    foreach $line (@OUTPUT){  print FO "$mon$mday$hour$min$sec $line\n";}
    close(FO);
    system("/bin/cat $FLO >>mbox.piped && /bin/rm -f $FLO");
} # if not open, I guess we will lose it for now


# SFLAG -> The job received was indeintified as a CRS job not some
#          other Emails.
# QFLAG -> The queue selection passed
# SSUBM -> AutoSub (arg2 or hard-coded value, bacward compat)
# AUTOS -> Configuration file parameter says OK
#
if ($SFLAG == 2 && $QFLAG && $SSUBM && $AUTOS){
    # Now, the logic for file submission. Simple and fast ...
    opendir(JDIR,"$SOURCE/jobfiles/");

    while ( defined($file = readdir(JDIR)) ){
	if( $file =~ /$prodl/ && $file !~ /\.lock/){
	    $lock = "$SOURCE/jobfiles/$file.lock";
	    if( ! -e "$lock" ){
		if ( open(FO,">$lock") ){
                    $cmd = "$SUBMIT $SOURCE/jobfiles/$file $PRIO ";
		    if( $qnum !=0){
			$cmd .= " $qnum ";
			if( $drop != 0){
			    $cmd .= " $drop";
			}
		    } 

		    system($cmd);
		    rename("$SOURCE/jobfiles/$file","$SOURCE/archive/$file");
		    # Just for the heck of it, output submit debugging
		    &ASLog("Job $SOURCE/jobfiles/$file submitted ($qnum/$drop)");
		    unlink($lock);
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
	print FL localtime()." $line\n";
	close(FL);
    }
}
