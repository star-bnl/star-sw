#!/opt/star/bin/perl
#
# $Id: dbloaddaq.pl,v 1.1 1999/07/07 13:19:30 wenaus Exp $
#
# $Log: dbloaddaq.pl,v $
# Revision 1.1  1999/07/07 13:19:30  wenaus
# real data log
#
#
######################################################################
#
# dbloaddaq.pl
#
# T. Wenaus 7/99
#
# Load the run database from the old shift log and the data files
#
# Usage: dbloaddaq.pl
#

use lib "/star/u2d/wenaus/datadb";
use File::Find;
use File::Basename;
use LWP::Simple;
use Time::Local;

require "dbsetup.pl";

my $debugOn=0;

for ($ii=0; $ii<@ARGV; $ii++) {
    if ( $ARGV[$ii] eq "droprun" ) {
    }
}

# connect to the DB
&StDbConnect();

# scan the old log file and load new entries
$content = get("http://redford.star.bnl.gov/staronline/shifts/shiftLogReport.htm");
open(SAVE,">log.txt");
print SAVE $content;
close SAVE;
@content = split(/\r/,$content);
$nl=0;
foreach $line ( @content ) {
    $nextl = $content[$nl+1];
    $nextl =~ s/\n//g;
    $nextl =~ s/&nbsp;/ /g;
    $line =~ s/\n//g;
    $nextl =~ s/<[a-zA-Z0-9\/]+>//g;    
    $nextl =~ s/^\s*//;
    if ( $line =~ m/Author/i ) {
        $author = $nextl;
        $nrun='';
        $nevents=0;
        $date='';
        $gain='';
        $ped='';
        $type='';
        $trig='';
        $activity='';
        $mag='';
        $raw='';
        $gcorr='';
        $zerosupp='';
    } elsif ( $line =~ m/<h3>comment<\/h3>/i ) {
        # Comment, which is the last field. Pick it up and commit.
        $inComment = 1;
        $nc=0;
        @comment=0;
        while ( $inComment ) {
            $comment[$nc] = $content[$nl+$nc+1];
            $comment[$nc] =~ s/\n//g;
            if ( $comment[$nc] =~ m/<\/p>/ ) {
                $inComment=0;
            }
            $comment[$nc] =~ s/<p>//;
            $comment[$nc] =~ s/<\/p>//;
            $nc++;     
        }
        if ( $date ne '' ) {
            if ( $gcorr =~ m/on/i && $gain eq '' ) { $gain='corrected' }
            if ( $time =~ m/PM/i ) {$pm = 1} else {$pm = 0}
            $time =~ s/\s*[AP]M\s*//;
            ( $mo, $dy, $yr ) = split(/\//,$date);
            ( $hr, $min, $sec ) = split(/:/,$time);
            if ( ($pm) && $hr < 12 ) {$hr = $hr +12}
            $ctime = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                                      $yr+1900,$mo,$dy,$hr,$min,$sec);
            if ( $debugOn ) {
                print "=== RUN=\"$nrun\" AUTHOR=\"$author\"";
                print " DATE=$date";
                print " TIME=$time";
                print " CTIME=$ctime";
                print " ZERO=$zerosupp";
                print " GCORR=$gcorr";
                print " RAW=$raw";
                print " MAG=$mag";
                print " FIELD=$field";
                print " ACT=$activity";
                print " TRG=$trig";
                print " TYP=$type";
                print " PED=$ped";
                print " GAIN=$gain";
                print " THRHI=$thrhi";
                print " THRLO=$thrlo";
                print " SEQHI=$seqhi";
                print " SEQLO=$seqlo";
                print "\n";
            }
            $fullcomment='';
            foreach $cm ( @comment ) {
# include blank lines                if ( ! ($cm =~ m/^\s*$/) ) {
                    $fullcomment.=$cm."\n";
#                }
            }

            # add the log entry
            &dbadd();
        }
    } elsif ( $line =~ m/<b>T1:/i || $line =~ m/<b>Run_Num/i ) { #run no.
        $nrun = $nextl;
        if ( $nrun =~ m/^0+$/ ) {$nrun=''}
    } elsif ( $line =~ m/>Date:/i ) {
        $date = $nextl;
    } elsif ( $line =~ m/>Time:/i ) {
        $time = $nextl;
    } elsif ( $line =~ m/Number_of_Events:/i ) {
        $nevents = $nextl;
    } elsif ( $line =~ m/>ZeroSuppSelected:/i ) {
        $zerosup = $nextl;
    } elsif ( $line =~ m/>GainCorrSelected:/i ) { # gain corrected
        $gcorr = $nextl;
    } elsif ( $line =~ m/>GainCorrSelected1:/i ) { # raw format
        $raw = $nextl;
        if ( $raw =~ m/on/i ) { $raw='Y' }
    } elsif ( $line =~ m/>MagnetOn:/i ) {
        $mag = $nextl;
        if ( $mag =~ m/on/i ) {
            $field=100;
        } else {
            $field=0;
        }
    } elsif ( $line =~ m/>activityName:/i ) {
        $activity = $nextl;
    } elsif ( $line =~ m/>TriggerType:/i ) {
        $trig = $nextl;
        if ( $trig =~ m/ped/i ) {$trig='ped'}
        if ( $trig =~ m/cos/i ) {$trig='cosmic'}
        if ( $trig =~ m/laser/i ) {$trig='laser'}
        if ( $trig =~ m/gas/i ) {$trig='zdc'}
        if ( $trig =~ m/coll/i ) {$trig='collision'}
        if ( $trig =~ m/central/i ) {$trig='central'}
        if ( $trig =~ m/fee/i ) {$trig='fee-pulser'}
        if ( $trig =~ m/pad/i ) {$trig='pad-pulser'}
    } elsif ( $line =~ m/>Run_Typ/i ) {
        $type = $nextl;
        $type =~ s/other//i;
        if ( $type =~ m/ped/i ) {$type='ped'}
        if ( $type =~ m/phy/i ) {$type='phy'}
        if ( $type =~ m/gain/i ) {$type='gain'}
        if ( $type eq '' ) {$type='test'}
    } elsif ( $line =~ m/>OtherRun_Ped/i ) {
        $ped = $nextl;
        $ped =~ s/blank//gi;
    } elsif ( $line =~ m/>OtherRun_Gain/i ) {
        $gain = $nextl;
        $gain =~ s/blank//gi;
    } elsif ( $line =~ m/>asic_thresh_hi/i ) {
        $thrhi = $nextl;
    } elsif ( $line =~ m/>asic_thresh_lo/i ) {
        $thrlo = $nextl;
    } elsif ( $line =~ m/>asic_seq_hi/i ) {
        $seqhi = $nextl;
    } elsif ( $line =~ m/>asic_seq_lo/i ) {
        $seqlo = $nextl;
    }
    $nl++;
}

# scan the DAQ data files and load new files
@daqfiles = </disk1/star/daq/*.daq>;
if ( @daqfiles>0 ) {
    foreach $daqf ( @daqfiles ) {
        if ( $daqf =~ m/([0-9]{2})([0-9]{2})([0-9]{2})\.([0-9]+)\.daq$/ ) {
            $nrun = $4;
            $yr = $1;
            if ( $yr eq '00' ) {$yr=100}
            $mo = $2;
            $dy = $3;
            $ctime = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                              $yr+1900,$mo,$dy,0,0,0);
            my ($fmode, $uid, $gid, $filesize, 
                $readTime, $writeTime, $createTime) =
                    (stat($daqf))[2,4,5,7,8,9,10];
            $sizeMB = $filesize/1000000;
            printf("Data: %-28s %6dMB\n",$daqf,$sizeMB) if $debugOn;
            &dbaddfile();
        }
    }
}

# finished
&StDbDisconnect();
exit;

############################################################

sub dbaddfile {
    ## First check whether an entry exists for this run number
    $sql="select name from $RunT where name='$nrun'";
    $cursor =$dbh->prepare($sql) 
        || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;
    $nrow = 0;
    while(@fields = $cursor->fetchrow) {
        $nrow++;
        my $cols=$cursor->{NUM_OF_FIELDS};
        for($i=0;$i<$cols;$i++) {
            my $fvalue=$fields[$i];
            my $fname=$cursor->{NAME}->[$i];
            $fn=lc($fname);
            $val{$fn} = $fvalue;
        }
    }
    if ($nrow>0) {
        # Update
#        print "Already exists in DB. Update.\n";
#        $sql="update $RunT set ";
#        $sql.="name='".$nrun."',";
#        $sql.="ctime=$ctime,";
#        $sql.="starttime=$ctime";
#        $sql.=" where name='$nrun'";
    } else {
        # New entry
        $sql="insert into $RunT set ";
        $sql.="name='".$nrun."',";
        $sql.="ctime=$ctime,";
        $sql.="starttime=$ctime";
        print "$sql\n" if $debugOn;
        $rv = $dbh->do($sql) || die $dbh->errstr;
        if ( $nrun > 100 ) {
            print "======== $daqf $sizeMB MB===========================\n";
            $content = `/star/u2d/wenaus/bin/daqscan $daqf`;
            print $content;
        }
    }
}


############################################################

sub dbadd {
    ## First check whether an entry exists for this run number
    if ($nrun ne '') {
        $sql="select name from $RunT where name='$nrun'";
    } else {
        $sql="select name,ctime from $RunT where name='$nrun' and ctime='$ctime'";
    }
    $cursor =$dbh->prepare($sql) 
        || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;
    $nrow = 0;
    while(@fields = $cursor->fetchrow) {
        $nrow++;
        my $cols=$cursor->{NUM_OF_FIELDS};
        for($i=0;$i<$cols;$i++) {
            my $fvalue=$fields[$i];
            my $fname=$cursor->{NAME}->[$i];
            $fn=lc($fname);
            $val{$fn} = $fvalue;
            print "$fn=$fvalue " if $debugOn;
        }
        print "\n" if $debugOn;
    }
    if ($nrow>0) {
#        print "Already exists in DB. Update.\n";
#        $sql="update $RunT set ";
#        &setfields();
#        $sql.=" where name='$nrun' and ctime='$ctime'";
    } else {
        $sql="insert into $RunT set ";
        &setfields();
    }
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
}

###################################################################

sub setfields {
    $sql.="user='".$author."',";
    $sql.="name='".$nrun."',";
    $sql.="ctime=$ctime,";
    if ($nevents >0) {$sql.="nevents=".$nevents.","}
    $sql.="type='".$type."',";
    $sql.="trig='".$trig."',";
    $sql.="stage='daq',";
    $sql.="zerosup='".$zerosup."',";
    $sql.="pedmode='".$ped."',";
    $sql.="rawformat='".$raw."',";
    $sql.="gainmode='".$gain."',";
    if ( $field ne '' ) {$sql.="field=".$field.",";}
    if ( $thrlo ne '' ) {$sql.="thrlo=".$thrlo.",";}
    if ( $thrhi ne '' ) {$sql.="thrhi=".$thrhi.",";}
    if ( $seqlo ne '' ) {$sql.="seqlo=".$seqlo.",";}
    if ( $seqhi ne '' ) {$sql.="seqhi=".$seqhi.",";}
    $sql.="title='".$activity."',";
    $sql.="comment=".$dbh->quote($fullcomment);
}
