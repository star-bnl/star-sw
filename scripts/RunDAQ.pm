#
# Written J.Lauret , July 2001
#
# Perl macro for fastOffline communication
# Changing this will change EVERY scripts
# accessing this schema. Please, be carefull.
#
# Open/Close
#      rdaq_open_rdatabase          open raw-ddb
#      rdaq_close_rdatabase         close raw-ddb
#      rdaq_open_odatabase          open o-ddb
#      rdaq_close_odatabase         close the o-ddb
#
# Function requiering open/close call
#      rdaq_raw_files               return a list of raw file from r-ddb
#      rdaq_add_entry               add one entry in the o-ddb
#      rdaq_add_entries             add entries in the o-ddb
#      rdaq_delete_entries          delete entries from the o-ddb
#      rdaq_check_entries           check entries and return an array of entries
#                                   which are suspect (i.e. does not pass the
#                                   expected conditions).
#      rdaq_list_field              Returns all possible values for give record
#      rdaq_set_location            Set the disk location field for a give record
#      rdaq_get_location            gets the output directory location if any.
#
#      rdaq_last_run                return the last run number from the o-ddb
#      rdaq_get_files               get a list of files with $status from o-ddb
#      rdaq_get_ffiles              get a list of characteristic ...
#      rdaq_get_orecords            basic function allowing ANY field selection
#
#      rdaq_set_files               set a list of files from o-ddb to $status
#                                   Accept both format returned by get_files
#                                   and get_ffiles.
#      rdaq_set_xstatus             Set XStatus for a given XStatus id
#      rdaq_set_chain               Set chain according to list
#      rdaq_set_execdate            Set the file(s) execute date (should be
#                                   passed in GMT)
#
# Each rdaq_set_XXX has an equivalent rdaq_set_XXX_where method which accepts
# a finer grain selection.
#
#
# Utility (no need for any ddb to be opened)
#      rdaq_file2hpss               return an HPSS path+file (several methods)
#      rdaq_status_string           returns a status string from a status val
#      rdaq_bits2string             Returns a string from a bitfield.
#      rdaq_mask2string             Returns detector set from detector BitMask.
#      rdaq_trgs2string             Return trigger Setup name
#      rdaq_string2trgs             Return trigger Setup Index from its name
#      rdaq_ftype2string            Return file type/flavor name
#      rdaq_string2ftyype           Return file type/flavor Index from its name
#      rdaq_toggle_debug            Turn ON/OFF SELECT of raw data.
#      rdaq_html_mode               Turn ON/OFF HTML message printing
#
#      rdaq_set_dlevel              Set debug level
#      rdaq_set_message             Add a message to the message stack (debug)
#      rdaq_get_message             Get N message from the message stack
#
# DEV ONLY *** MAY BE CHANGED AT ANY POINT IN TIME ***
#      rdaq_set_files_where         Not in its final shape.
#      rdaq_update_entries          For maintainance ONLY
#
#
# Internal
#      GetBitValue                  Returns the bit position for a given variable
#                                   saved in a given table. If the second argument
#                                   is 1, the value is added to the list of possible
#                                   ones.
#      Record_n_Fetch               Base routine to save a string record in an
#                                   associated array and return the index.
#
#
# HowTo add a field
#  Adding a field with a plain type is easy. Just ALTER the table, modify
#  VALUES() and add a ?, modify the fetcher routine to make this field
#  appear at the proper place, eventually modify the MAINTAINER only
#  routine update_entries to initialize the column, and save ... Status
#  column is always expected to be the last field. For readability,
#  add fields before EntryDate (see the NOW()+0,0,0).
#
#  For a BITWISE field, do the same AND, in addition, add a
#  hash value for that column. The value MUST be a valid existing
#  table you can create using
#  > create table $TBLName (id INT NOT NULL AUTO_INCREMENT, Label CHAR(50) NOT NULL,
#  PRIMARY KEY(id), UNIQUE(Label));
#  The fields of that extra table are expected to be EXACTLY as above i.e. there
#  are all standardized to avoid proliferation of routines. THERE is nothing else
#  to do at this level.
#
#  WARNING:
#   - Log path requires CHAR(255) at least for FOLocations (bummer!!)
#   - Chains would survive with CHAR(50) but increased by hand to 255 anyhow
#
#
#  If you have build a script based on get_ffiles() or get_orecords(), you will
#  need to take into account the fact that the number of fields is larger. The
#  last field of DAQInfo is expected to be 'Status'. Please, preserve this ...
#
# The full DAQInfo table description is as follow (last dumped, Dec 2001). Other
# tables are of trivial format (i.e. and int id and a char label).
#
#    +-------------+---------------------+------+-----+---------+-------+
#    | Field       | Type                | Null | Key | Default | Extra |
#    +-------------+---------------------+------+-----+---------+-------+
#  0 | file        | char(255)           |      | PRI |         |       |
#  1 | runNumber   | int(10)             |      |     | 0       |       |
#  2 | NumEvt      | int(10)             | YES  |     | NULL    |       |
#  3 | BeginEvt    | int(10)             | YES  |     | NULL    |       |
#  4 | EndEvt      | int(10)             | YES  |     | NULL    |       |
#  5 | Current     | float(16,8)         | YES  |     | NULL    |       |
#  6 | scaleFactor | float(16,8)         | YES  |     | NULL    |       |
#  7 | BeamE       | float(16,8)         | YES  |     | NULL    |       |
#  8 | Collision   | char(10)            | YES  |     | NULL    |       |
#  9 | DetSetMask  | bigint(20) unsigned | YES  |     | 0       |       |
# 10 | TrgSetup    | bigint(20) unsigned | YES  |     | 0       |       |
# 11 | TrgMask     | bigint(20) unsigned | YES  |     | 0       |       |
#
# 12 | ftype       | int(11)             | YES  |     | 0       |       |
# 13 | EntryDate   | timestamp(14)       | YES  |     | NULL    |       |
# 14 | ExecDate    | timestamp(14)       | YES  |     | NULL    |       |
# 15 | UpdateDate  | timestamp(14)       | YES  |     | NULL    |       |
#
# 16 | DiskLoc     | int(11)             | YES  |     | 0       |       |
# 17 | Chain       | int(11)             | YES  |     | 0       |       |
#
# 18 | XStatus1    | int(11)             | YES  |     | 0       |       | --> This one is
#                                                                             reserved for ezTree
# 19 | XStatus2    | int(11)             | YES  |     | 0       |       | --> unused for now
#      ... as many Status as needed for pre-passes
# 20 | Status      | int(11)             | YES  |     | 0       |       |
#    +-------------+---------------------+------+-----+---------+-------+
#
# BEWARE: Adding a column implies code changes where a tag 'MOD HERE'
# can be found.
#
#
#
use Carp;
use DBI;
use Date::Manip ();

package RunDAQ;
require 5.000;
require Exporter;
@ISA = qw(Exporter);

@EXPORT= qw(
	    rdaq_open_rdatabase rdaq_close_rdatabase
	    rdaq_open_odatabase rdaq_close_odatabase

	    rdaq_raw_files rdaq_add_entry rdaq_add_entries
	    rdaq_set_location rdaq_get_location
	    rdaq_delete_entries

	    rdaq_check_entries rdaq_list_field
	    rdaq_last_run

	    rdaq_get_files rdaq_get_ffiles rdaq_get_orecords
	    rdaq_set_files rdaq_set_files_where
	    rdaq_set_xstatus rdaq_set_xstatus_where
	    rdaq_set_execdate rdaq_execdate_where
	    rdaq_set_chain rdaq_set_chain_where

	    rdaq_file2hpss rdaq_mask2string rdaq_status_string
	    rdaq_bits2string
	    rdaq_trgs2string  rdaq_string2trgs
	    rdaq_ftype2string rdaq_string2ftype
	    rdaq_toggle_debug rdaq_html_mode rdaq_set_dlevel rdaq_scaleToString

	    rdaq_set_files_where rdaq_update_entries

	    rdaq_set_message rdaq_get_message rdaq_purge_message

	    );


#
# Database information
#
#$DDBSERVER = "onlsun1.star.bnl.gov";
$DDBSERVER = "onldb.starp.bnl.gov";
$DDBUSER   = "starreco";
$DDBPASSWD = "";
$DDBPORT   = 3501;
$DDBNAME   = "RunLog";

$RDBOBJ    = undef;
$ODBOBJ    = undef;

$dbhost    = "duvall.star.bnl.gov";
$dbuser    = "starreco";
$dbpass    = "";
$DBTABLE   = "DAQInfo";
$dbname    = "operation";

$HPSSBASE  = "/home/starsink/raw/daq";        # base path for HPSS file loc.
$DELAY     = 60;                              # delay backward in time in seconds

# Required tables on $DDBSERVER
# List for Year2
#@REQUIRED  = ("daqFileTag","daqSummary",
#	      "triggerSet","detectorSet",
#	      "beamInfo","magField");
# List for Year3
@REQUIRED  = ("daqFileTag","daqSummary",
	      "l0TriggerSet","detectorSet",
	      "beamInfo","magField");

@EXPLAIN=();

#
# There should be NO OTHER configuration below this line but
# only composit variables or assumed fixed values.
#
$DEBUG     = 0;
$DLEVEL    = 0;
$PHTML     = 0;

# Build ddb ref here.
$DDBREF    = "DBI:mysql:$DDBNAME:$DDBSERVER:$DDBPORT";

#
# Those fields will be rounded in a get_orecords() and
# list_field() querry. It will NOT be rounded in a future
# to be implemented of set_files() or delete entries.
#
$ROUND{"scaleFactor"} = 1;
$ROUND{"BeamE"}       = 1; # does not work with 1


#
# Those fields are indicative of a bitmask operation.
# A bitwise operation will affect the functions as
# described above. The value of this hash array is
# the table name containing the bit position ...
#
$BITWISE{"TrgMask"}    = "FOTriggerBits";
$BITWISE{"DetSetMask"} = "FODetectorTypes";

#
# For list_field, we may want to select from secondary
# tables instead of from $DBTABLE. We can do this only
# if the fields are associated with a threaded table.
# Those tables are assumed to be id,Label. The index
# 0 or 1 indicates if we select by id or label in the
# list_field() routine returned values.
$THREAD0{"TrgSetup"} = "FOTriggerSetup";
$THREAD0{"ftype"}    = "FOFileType";
$THREAD1{"Chain"}    = "FOChains";
$THREAD1{"runNumber"}= "FOruns";


#
# Insert an element in the o-database.
# We accept only one entry. INEFFICIENT but provided for backward
# compatibility. Note the next method allowing for bundle inserts.
#
sub rdaq_add_entry
{
    my($obj,@values)=@_;
    my($sth);

    if(!$obj){ return 0;}
    $sth = $obj->prepare("INSERT IGNORE INTO $DBTABLE ".
			 "VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,NOW(),0,0, 0,0,0,0,0)"); # MOD HERE
    if ( $sth ){
	if ( ! $sth->execute(@values) ){
	    print "<!-- Could not execute with $#values params [".join(",",@values)."] -->\n"
		if ($DEBUG);
	}
	$sth->finish();
    } else {
	print "<!-- Could not prepare statement in rdaq_add_entry() -->\n" if ($DEBUG);
    }

    1;
}

# enter records as returned by rdaq_raw_files
# Returns the number of added entries.
sub rdaq_add_entries
{
    my($obj,@records)=@_;
    my($sth,$line,@values);
    my($count);

    $count=0;
    if(!$obj){ return 0;}

    if($#records != -1){
	$sth = $obj->prepare("INSERT INTO $DBTABLE ".
			     "VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,NOW(),0,0, 0,0,0,0,0)"); # MOD HERE
	if($sth){
	    foreach $line (@records){
		@values = split(" ",$line);
		if ($sth->execute(@values)){
		    $count++;
		} else {
		    if ( $sth->err == 1062){
			&info_message("add_entries",0,"Skipping already in $values[0]\n");
		    } else {
			&info_message("add_entries",3,"Failed to add [$line] ".
				      $sth->err." = ".
				      $sth->errstr."\n");
		    }
		}
	    }
	    $sth->finish();
	} else {
	    &info_message("add_entries",3,"Failed to prepare sentence\n");
	}
    }
    $count;
}


#
# Update a few field. This routine serves whenever we import a new information
# in a column and want to have that specific field updated.
#
# This has to usually be done manually but some programming would allow going
# back in time and update all records. Possibly an expensive operation.
#
sub rdaq_update_entries
{
    my($obj,@records)=@_;
    my($sth,$line,@values);
    my($count);

    $count=0;
    if(!$obj){  return 0;}

    if($#records != -1){
	$sth = $obj->prepare("UPDATE $DBTABLE SET scaleFactor=?, ".
			     "DetSetMask=?, TrgSetup=?, TrgMask=?, ftype=?".
			     "WHERE file=?");

	if($sth){
	    foreach $line (@records){
		@values = split(" ",$line);
		# print "<!-- DEBUG ".$values[0]." ".$values[11]." -->\n";
		if($sth->execute($values[6],
				 $values[9],$values[10],$values[11],$values[12],
				 $values[0]) ){
		    $count++;
		} else {
		    &info_message("update_entries",1,"Failed to update ".$sth->errstr()."\n");
		}
	    }
	    $sth->finish();
	} else {
	    &info_message("update_entries",3,"prepare() failed\n");
	}
    }
    $count;
}


#
# This method may be needed later.
# FastOffline do not make use of it at all (when a record is in,
# it is in forever).
#
sub rdaq_delete_entries
{
    my($obj,@files)=@_;
    my($sth,$line,@values);
    my($count);

    $count=0;
    if(!$obj){ return 0;}

    $sth = $obj->prepare("DELETE FROM $DBTABLE WHERE file=?");
    if(!$sth){ return 0;}

    foreach $line (@files){
	#print "$line\n";
	@values = split(" ",$line);
	#print "[$values[0]]\n";
	if( $sth->execute($values[0]) ){
	    print "Successful deletion of $values[0]\n";
	    $count++;
	}
    }
    $sth->finish();
    1;
}

#
# Sets the output directory location for this entry
#
sub rdaq_set_location
{
    my($obj,$loc,@values)=@_;
    my($file,$val);
    my($sth,$sth2);

    if(!$obj){  return 0;}

    # sort out if there is something in the hash array for that
    # location. Special case is undef which is reserved for
    # 'not stored on disk'.
    if( defined($loc) ){
	$val = &Record_n_Fetch("FOLocations",$loc);
    } else {
	$val = 0;
    }

    $file= shift(@values);
    $sth = $obj->prepare("UPDATE $DBTABLE SET DiskLoc=? WHERE file=?");
    return $sth->execute($val,$file);
}


#
# Get the output directory location
#
sub rdaq_get_location
{
    my($obj,@values)=@_;
    my($sth,$val);

    if(!$obj){  return 0;}

    $file= shift(@values);
    $sth = $obj->prepare("SELECT FOLocations.Label FROM FOLocations,$DBTABLE ".
			 "WHERE FOLocations.id=$DBTABLE.DiskLoc AND ".
			 "$DBTABLE.file=?");
    if($sth){
	$sth->execute($file);
	if( ! defined($val = $sth->fetchrow() ) ){
	    $val = 0;
	}
	$sth->finish();
    }
    $val;
}


#
# On August 3rd, we noticed that some entries were in but should not
# have been. After some discussion, it appeared that this is caused by
# a hand-shaking problem between offline/online and especially, a
# problem when a run information is copied in the database but then
# only after marked as bad.
# FastOffline wants those entries but the final table should not have
# them. Therefore, we implemented a method to boostrap the information
# in our  $DBTABLE and compare it to the initial expectations.
#
# $since is a number of minutes
#
sub rdaq_check_entries
{
    my($obj,$since)=@_;
    my($tref);
    my(@all);

    #$tref = Date::Manip::DateCalc("today","-$since minutes");
    #$tref = Date::Manip::UnixDate($tref,"%Y%m%e%H%M%S");

    undef;
}






#
# Select the top element of the o-database
#
sub rdaq_last_run
{
    my($obj)=@_;
    my($sth,$val);

    if(!$obj){ return 0;}
    $sth = $obj->prepare("SELECT file FROM $DBTABLE ORDER BY runNumber DESC, file DESC LIMIT 1");
    $sth->execute();
    if( $sth ){
	$val = $sth->fetchrow();
	if ( defined($val) ){
	    $sth->finish();
	    $val =~ /(.*_)(\d+_)(.*_)(\d+)/;
	    $val = $2.$4;
	    $val =~ s/_/./;
	    $val;
	} else {
	    $val = 0;
	}
    } else {
	0;
    }
}

#
# Get a list of files recently added
# to the database
#
sub rdaq_raw_files
{
    my($obj,$from,$limit)=@_;
    my($sth,$cmd,$llimit);
    my(@all,@res,$tres);
    my($tref,$kk,$gotit);
    my($rskip,$xrows,$xinfo);

    if(!$obj){ return 0;}

    # Default values
    $llimit = 0;
    if( ! defined($from) ){ $from = "";}
    if( ! defined($limit)){ $llimit = $limit= -1;}
    if( $from eq 0){ $from = "";}

    # An additional time-stamp selection will be made to minimize
    # a problem with database hand-shaking. This will affect only
    # the test runs with max file sequence = 1.
    #$tref = Date::Manip::DateCalc("today","-1 minute");
    #$tref = Date::Manip::UnixDate($tref,"%Y%m%e%H%M%S");
    $sth = $obj->prepare("select FROM_UNIXTIME( UNIX_TIMESTAMP(NOW()) - $DELAY )+0");
    $sth->execute();
    $tref= $sth->fetchrow();
    $sth->finish();


    # We will select on RunStatus == 0
    # Year2
    #$rval  = 9;
    #$cmd  = "SELECT daqFileTag.file, daqSummary.runNumber, daqFileTag.numberOfEvents, daqFileTag.beginEvent, daqFileTag.endEvent, magField.current, magField.scaleFactor, beamInfo.yellowEnergy+beamInfo.blueEnergy, CONCAT(beamInfo.blueSpecies,beamInfo.yellowSpecies) FROM daqFileTag, daqSummary, magField, beamInfo  WHERE daqSummary.runNumber=daqFileTag.run AND daqSummary.runStatus=0 AND daqSummary.destinationID In(1,2,4) AND magField.runNumber=daqSummary.runNumber AND magField.entryTag=0 AND beamInfo.runNumber=daqSummary.runNumber AND beamInfo.entryTag=0";
    # One more table runStatus, daqSummary.runStatus=0 gone, entryTag=5 for hardwired values
    $rval  = 9;
    $cmd   = "SELECT daqFileTag.file, daqSummary.runNumber, daqFileTag.numberOfEvents,daqFileTag.beginEvent, daqFileTag.endEvent, magField.current,magField.scaleFactor, beamInfo.yellowEnergy+beamInfo.blueEnergy,CONCAT(beamInfo.blueSpecies,beamInfo.yellowSpecies) FROM daqFileTag,daqSummary, magField, beamInfo,runStatus WHERE daqSummary.runNumber=daqFileTag.run AND magField.runNumber=daqSummary.runNumber AND runStatus.runNumber=daqFileTag.run AND beamInfo.runNumber=daqSummary.runNumber AND daqSummary.destinationID In(1,2,4) AND runStatus.rtsStatus=0  AND runStatus.shiftLeaderStatus<=1  AND magField.entryTag In(0,5) AND beamInfo.entryTag In(0,5)";

    @EXPLAIN = ("File","RunNumber","numberOfEvents","beginEvent","endEvent",
		"magField current","magField scaleFactor",
		"yellow+blue Energy","yellow+blue species");


    # Optional arguments
    $xinfo = "runNumber unspecified";
    if( $from ne ""){
	# start from some run number
	if( index($from,"\.") != -1){
	    # recent format returns file sequence
	    @res = split(/\./,$from);
	    #$cmd .= " AND (daqSummary.runNumber > $res[0] OR ".
	    #" (daqSummary.runNumber=$res[0] AND daqFileTag.fileSequence > $res[1]))";
	    # Changed for Year4 -- FileSequence is complex ....
	    $cmd  .= " AND (daqSummary.runNumber >= $res[0]) ";
	    $xinfo = "runNumber >= $res[0]";
	} elsif ( $from =~ /=/){
	    $cmd  .= " AND daqSummary.runNumber $from";
	    $xinfo = "runNumber $from";
	} else {
	    # old expected a run number only
	    $cmd .= " AND daqSummary.runNumber > $from";
	    $xinfo = "runNumber > $from";
	}
	$cmd .= " AND daqFileTag.entryTime <= $tref";
    }
    # limit can be a string litteral like 10,100
    if($llimit != -1){
	$cmd .= " LIMIT $limit";
    }

    print "<!-- $cmd -->\n" if ($DEBUG);
    $sth  = $obj->prepare($cmd);
    if ( ! $sth->execute() ){ 
	&info_message("raw_files",3,"Could not execute statement\n");
	return undef;
    }
    # else
    $rskip = $gotit = $kk=0;
    $xrows = $sth->rows();

    if ($DEBUG){
	&info_message("raw_files",3,"Expected number of rows = $xrows ($xinfo)\n");
	if ( $xrows == 0 ){
	    # Then verify what could be wrong with the querry
	    my($mcmd,$ccmd);
	    my($vsth,$cnt);
	    $mcmd = $ccmd = $cmd;
	    $mcmd =~ s/AND daqSummary.destinationID.*\)//;
	    &info_message("raw_files",3,"$mcmd\n");
	    $vsth = $obj->prepare($mcmd);
	    $cnt  = $vsth->rows();
	    &info_message("raw_files",3,"Without selector on destination,status,field tag -> $cnt\n");
	}
    }

    while( @res = $sth->fetchrow_array() ){
	$gotit++;
	# Massage the results to return a non-ambiguous information
	# We are still lacking
	if ( ($#res+1) != $rval){
	    die "Database has all fields but some are empty ".($#res+1)." received, expected $rval\n";
	}

	# skip scaler files added in 2006
	if ( $res[0] =~ /\.sca/ || $res[0] =~ m/run\d+\.\d+\.dat/ ){  next;}
	# even more exotic files are appearing in the DB from 2007 onwards
	if ( $res[0] !~ /\.daq/){
	    if ($DEBUG){
		&info_message("raw_files",3,"File not matching expected name pattern ".join(";",@res)."\n");
	    } else {
		&info_message("raw_files",3,"File not matching expected name pattern [$res[0]]\n");
	    }
	    $rskip++;
	    next;
	}

	# &info_message("raw_files",3,"Got a valid entry ".join(";",@res)."\n") if ($DEBUG);

	#for ($ii = 0 ; $ii <= $#res ; $ii++){
	#    print "$ii --> $res[$ii]\n";
	#}
	$tres = &rdaq_fetcher($obj,@res);
	if ( defined($tres) ){
	    push(@all,$tres);
	    $kk++;
	    if( $kk % 10000 == 0){
		# always output debug lines in HTML comment format
		# since this may be used in a CGI.
		print "<!-- Fetched $kk records -->\n" if ($DEBUG);
	    }
	} else {
	    &info_message("raw_files",3,"Incomplete information (will skip)\n");
	}
    }
    $sth->finish();

    # print some more info in debug mode - sanity checks mainly
    if ($DEBUG ){
	&info_message("raw_files",2,"Nothing known or returned for $from\n") if (!$gotit );
	if ($xrows != ($#all+1+$rskip)){
	    &info_message("raw_files",2,"Boo\n");
	}
    }

    @all;
}

# fetcher for currently missing elements and information in
# database. This routine is internal only and may be
# rehsaped at any time. However the final returned values
# should remain the same.
sub rdaq_fetcher
{
    my($obj,@res)=@_;
    my($ii,$stht,$sthl,$sths);
    my(@init,@items,$line,$run,$mask);

    # save a copy
    push(@init,@res);

    # Add a default BeamBeam at the last element.
    # Will later be in beamInfo table. Use global
    # variable for spead.
    # THIS IS NOW IN THE DATABASE. Moi : Jul 20th 2001
    # push(@res,"AuAu");


    # Dataset selection, the DetectorTypes was filled by hand.
    $stht = $obj->prepare("SELECT detectorTypes.name FROM detectorTypes, detectorSet ".
			  "WHERE detectorSet.detectorID=detectorTypes.detectorID AND ".
			  "detectorSet.runNumber=?");

    # Trigger label
    # Year2
    # $sthl = $obj->prepare("SELECT triggerLabel,numberOfEvents FROM triggerSet ".
    #			  "WHERE runNumber=? ORDER BY triggerLabel DESC");
    # Year3
    $sthl = $obj->prepare("SELECT name,numberOfEvents FROM l0TriggerSet ".
			  "WHERE runNumber=? ORDER BY name DESC");

    # Trigger Setup
    $sths = $obj->prepare("SELECT glbSetupName FROM runDescriptor ".
			  "WHERE runNumber=? ORDER BY glbSetupName DESC");


    #
    # Sort out Dataset information. This will have to remain
    # as is.
    #
    $run = $res[1];
    &Record_n_Fetch("FOruns","$run");

    if( ! defined($DETSETS{$run}) ){
	#&info_message("fetcher",3,"Checking DataSet for run $run\n");
	$stht->execute($run);
	$mask = 0;
	if( ! $stht ){
	    &info_message("fetcher",3,"$run cannot be evaluated. No DataSET info.\n");
	    return undef;
	} else {
	    while( defined($line = $stht->fetchrow() ) ){
		$mask |= (1 << &GetBitValue("DetSetMask",$line,1));
	    }
	    #&info_message("fetcher",0,"We have built mask=$mask");
	    #print "<!-- We have built $run mask=$mask -->\n";
	}

	# Note" detectors "daq" and "trg" will lead to mask=0 and a warning
	# Ideally, we should skip those ... but there are so many. So decided
	# to give a warning for debugging but save "unknown" (val=0).
	if ($mask == 0 ){
	    &info_message("fetcher",1,
			  "Reading $run table=detectorTypes,detectorSet was 0 [$mask] (daq/trg run only)\n");
	    #return undef;
	} # else {
	$DETSETS{$run} = $mask;
	#}
    } else {
	$mask = $DETSETS{$run};
    }
    push(@res,$mask);
    push(@EXPLAIN,"detectorTypes (mask)");

    #
    # This block is for the TriggerSetup
    #
    if( ! defined($TRGSET{$run}) ){
	#&info_message("fetcher",3,"Checking TrgMask for run $run -> ");
	$mask = 0;
	$sths->execute($run);
	if( ! $sths ){
	    &info_message("fetcher",3,"$run cannot be evaluated. No TriggerSetup info.\n");
	    return undef;
	} else {
	    $mask = "";
	    while( defined($line = $sths->fetchrow()) ){
		$mask .= $line.".";
	    }
	    chop($mask);
	    $mask = &Record_n_Fetch("FOTriggerSetup",$mask);
	}
	if ($mask eq ""){
	    &info_message("fetcher",1,
			  "Reading $run TrgSet table=runDescriptor field=glbSetupName leaded to [$mask]");
	    return undef;
	} else {
	    $TRGSET{$run} = $mask;
	}
    } else {
	$mask = $TRGSET{$run};
    }
    push(@res,$mask);
    push(@EXPLAIN,"glbSetupName");

    #
    # Now, add to this all possible trigger mask
    #
    if( ! defined($TRGMASK{$run}) ){
	#&info_message("fetcher",3,"Checking TrgMask for run $run -> ");
	$mask = "";
	$sthl->execute($run);
	if( ! $sthl ){
	    &info_message("fetcher",3,"$run cannot be evaluated. No TriggerLabel info.\n");
	    return undef;
	} else {
	    while( @items = $sthl->fetchrow_array() ){
		if($items[1] != 0){
		    #if (  $run == 12050037 ){
		    #	print "  $items[0] found -> ".&GetBitValue("TrgMask",$items[0])."\n";
		    #}
		    # mask can only go to 64 bits in perl (bummer)
		    #$mask += (1 << &GetBitValue("TrgMask",$items[0]));
		    $mask .= "[".&GetBitValue("TrgMask",$items[0])."].";
		}
	    }
	    chop($mask);
	}
	if ($mask eq ""){
	    &info_message("fetcher",1,
			  "Reading $run TrgMask table=l0TriggerSet leaded to [$mask]\n");
	    #return undef;
	} #else {
	    #if ( $run == 12050037 ){
	    #	print "<!-- For $run TrgMask table=l0TriggerSet mask=$mask -->\n";
	    #	die;
	    #}
	$TRGMASK{$run} = $mask;
	#}

    } else {
	$mask = $TRGMASK{$run};
    }
    # if we want to ensure that only good-runs (i.e. marked as such) are
    # taken, we can return 'undef' if mask==0. However, if we need Fastoffline
    # to check this run as we go, we want them ...
    push(@res,$mask);
    push(@EXPLAIN,"l0TriggerSet (mask)");


    # File name is the first field 0
    if( $res[0] =~ m/(st_)(\w+)(_\d+_raw)/ ){
	push(@res,&Record_n_Fetch("FOFileType",$2));
    } else {
	push(@res,0);
    }
    push(@EXPLAIN,"FOFileType (parsed)");


    #print "Returning from rdaq_fetcher() with ".($#res+1)." values\n" if($DEBUG);
    $mask = "";
    for ($ii=0 ; $ii <= $#res ; $ii++){
	$mask .= "$ii $EXPLAIN[$ii] -> [$res[$ii]] ; ";
	if ($res[$ii] eq ""){
	    &info_message("fetcher",2,
			  "Element $ii is NULL (not expected)\n");
	    &info_message("fetcher",0,
	    		  "I received ".($#init+1)." elements and ended with ".($#res+1)."\n");
	    &info_message("fetcher",2,
			  "$mask\n");

	    &info_message("fetcher",2,"BOGUS records for run=$res[1]\n");
	    return undef;
	}
    }

    return join(" ",@res);
}




# This is going to be entirely mysql-based
# so no need for a handler.
sub rdaq_open_rdatabase
{
    my($obj);
    my($i,$cmd,$sth);

    &info_message("open_rdatabase",2,"Opening $DDBREF\n") if ($DEBUG);
    $obj = DBI->connect($DDBREF,$DDBUSER,$DDBPASSWD,
			{PrintError  => 0, AutoCommit => 1,
			 ChopBlanks  => 1, LongReadLen => 200});
    if(!$obj){
	&info_message("open_rdatabase",1,"Failed to open $DDBREF using user=$DDBUSER\n") if ($DEBUG);
	return 0;
    }

    # Else fine, it is opened
    # check presence of tables
    for($i=0 ; $i <= $#REQUIRED ; $i++){
	$sth = $obj->prepare("SELECT * FROM $REQUIRED[$i] LIMIT 1");
	if(! $sth->execute() ){
	    &info_message("open_database",3,"Required Database $REQUIRED[$i] does not exists\n");
	    $obj->disconnect();
	    return 0;
	}
	$sth->finish();
    }

    # return object
    &info_message("open_rdatabase",2,"All OK, DB is opened\n") if ($DEBUG);
    return $obj;
}
sub rdaq_close_rdatabase
{
    my($obj)=@_;
    if(!$obj){
	return 0;
    } else {
	$obj->disconnect();
	1;
    }
}

sub rdaq_close_odatabase
{
    my($obj)=@_;

    if(!$obj){
	return 0;
    } else {
	$obj->disconnect();
	1;
    }
}

sub rdaq_open_odatabase
{
   if ( ! defined($ODBOBJ) ){
       $DBOBJ = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass,
			   {PrintError  => 0, AutoCommit => 1,
			    ChopBlanks  => 1, LongReadLen => 200});
       return $DBOBJ;
   } else {
       return $DBOBJ;
   }
}

#
# Scans o-database and returns a list of
# files which have status $status.
# The parameters are
#    status    may be -1 for all status
#    limit     -1 for no limit
#    mode      0 for the file name only,
#              all fields are otherwise
#              returned.
#    conds     A reference to an hash array
#              for extraneous condition selection.
#
#    ftype     Late addition. Has to be something
#              like 'laser' 'physics' 'pulser'.
#              Default is 'physics'. Late addition.
#
# The list will be given in a descending ordered
# array (first file is last saved to HPSS).
#
# Return full list (i.e. all columns from o-ddb)
#
sub rdaq_get_ffiles
{
    my($obj,$status,$limit,@ftypes)=@_;
    return &rdaq_get_files($obj,$status,$limit, 1,undef,@ftypes);
}

# Changed on 2006 and added $SEL - A few external script
# calling this method need to adapt.
sub rdaq_get_files
{
    my($obj,$status,$limit,  $mode,$SEL,@ftypes)=@_;
    my($el,%Conds);

    # Default values will be sorted out here.
    if( ! defined($limit) ){  $limit = 0;}
    if( ! defined($mode)  ){  $mode  = 0;}
    # if( ! defined($status)){  $status= 0;}
    # if( ! defined($ftype) ){  $ftype = 1;}

    if ( defined($SEL) ){
	# consider it as a hash reference and transfer
	# into %Conds
	foreach $el (keys %$SEL){   $Conds{$el} = $$SEL{$el};}
    }

    # We MUST pass a reference to a hash.
    $Conds{"Status"} = $status if defined($status);
    if ( $#ftypes != -1){
	if ( $#ftypes > 0 ){
	    # switch to OR syntax
	    $Conds{"ftype"}  = join("|",@ftypes);
	} else {
	    $Conds{"ftype"}  = $ftypes[0];
	}
    }

    return &rdaq_get_orecords($obj,\%Conds,$limit,$mode);
}

#
# Because of a later version of this (evoluated from get_files()),
# and for backward compatibility, we need to support the options
# described above .
# This basic fundamental function DOES NOT support default values
# so it needs to be sorted out prior to this call.
#
sub rdaq_get_orecords
{
    my($obj,$Conds,$limit,$mode)=@_;
    my($cmd,$el,$val,$tmp,$sth);
    my(@Values);
    my($file,@files,@items);
    my($flag,$comp,$savev);

    #print "In here\n";
    #@files = undef;
    if( ! $obj ){           return undef;}
    if( ! defined($mode) ){ $mode = 1;}

    # basic selection
    $cmd = "SELECT * FROM $DBTABLE";

    #$DEBUG = defined($ENV{RunDAQ_DEBUG})||0;

    # backward compatibility is status selection where -1 = all
    # may be achieved by skipping hash element.
    foreach $el (keys %$Conds){
	$comp= "=";
	$val = $$Conds{$el};

	# do NOT build a querry for a 'all' keyword
	if( ! defined($val) ){
	    &info_message("get_orecords",3,"[$el] has an undef value");
	    next;
	}
	if( $el eq "Status" && $val == -1){ next;}
	if( $el eq "Oper"   && $val == -1){ next;}


	# Sort out possible comparison operators
	$test= substr($val,0,1);
	#print "<!-- DEBUG:: test=$test for [$el] [$val] -->\n";
	$comp= ">=" if($test eq ">");
	$comp= "<=" if($test eq "<");
	$comp= "!=" if($test eq "!");
	if($comp ne "="){
	    # this re-assigns an array reference and may strip
	    # the condition
	    $val   = substr($val,1,length($val));
	}
	$savev = $val;


	# Sort out now the kind of field we are working with
	$flag = 1;

	if( defined($ROUND{$el}) ){                # Round OFF value selection
	    $val = "ROUND($el,$ROUND{$el})";
	} elsif ( defined($BITWISE{$el}) ){	   # BITWISE selection
	    $flag= 0;
	    $tmp = (split(":",$val))[0];
	    if( $tmp == 0){
		$val = "($el = 0)";
	    } else {
		#
		# TODO: this is NOT correct -  9. will math .9. in a list but
		# also 19. etc ...
		#
		$val = "(($el & (1 << $tmp)) OR ( LOCATE(\"[$tmp]\",$el) != 0 ))";
	    }
	} elsif ( index($val,"|") != -1){          # OR syntax in selection
	    #print "<!-- Received $val -->\n";
	    @items = split(/\|/,$val);
	    $val = "(";
	    foreach $tmp (@items) {
		$val .= "$el=$tmp OR " if ($tmp ne "");
	    }
	    $val = substr($val,0,length($val)-3).")";
	    #print "<!-- $val -->\n";
	    undef(@items);
	    $flag = 0;
	} else {                                   # Default selection
	    $val = $el;
	}


	# check WHERE keyword presence or not
	if($cmd !~ /WHERE/){
	    $cmd .= " WHERE $val";
	} else  {
	    $cmd .= " AND $val";
	}
	# check if selection is completely defined
	# or not.
	if( $flag){
	    $cmd .= "$comp?";
	    push(@Values,$savev);
	} else {
	    # the syntax is a bit operation or a OR/AND
	    # syntax and therefore is complete
	}

    }

    # order
    $cmd .= " ORDER BY runNumber DESC, file DESC";
    if( $limit > 0){
	# allow for float argument, integerize
	my($l) = int($limit);
	if ($l < 1){ $l = 1;}
	$cmd .= " LIMIT $l ";
    }


    print "<!-- DEBUG : [$cmd] [@Values] -->\n" if ($DEBUG);
    $sth = $obj->prepare($cmd);
    $sth->execute(@Values);
    if ($sth){
	while ( @items = $sth->fetchrow_array() ){
	    #print "<!-- Querry returns ".join("::",@items)." -->\n"   if ($DEBUG);
	    if($mode == 0){
		print "<!-- Mode is 0, simple return $items[0] -->\n" if ($DEBUG);
		$file = $items[0];
	    } else {
		$file = "";
		for ($i=0 ; $i <= $#items ;$i++){
		    if ( ! defined($items[$i]) ){ $items[$i] = "?";}
		}
		$file = join(" ",@items);
		print "<!-- Returning ".join("::",@items)." -->\n"    if ($DEBUG);
		chomp($file);
	    }
	    push(@files,$file);
	}
	print "<!-- No more returned values -->\n"                    if ($DEBUG);
    }

    return @files;
}




# This method is a backward support for the preceeding method
# which allowed setting status without condition. Now, we
# also support WHERE Status= cases.
sub rdaq_set_files
{
    my($obj,$status,@files)=@_;
    return &rdaq_set_files_where($obj,$status,-1,@files);
}
# Set the status for a list of files
sub rdaq_set_files_where
{
    my($obj,$status,$stscond,@files)=@_;
    return &__set_files_where($obj,"Status",$status,$stscond,@files);
}



# Set chain options or update
sub rdaq_set_chain
{
    my($obj,$chain,@files)=@_;
    return 0 if ( ! defined($chain) );
    return &rdaq_set_chain_where($obj,$chain,-1,@files);
}
sub rdaq_set_chain_where
{
    my($obj,$chain,$stscond,@files)=@_;
    return 0 if ( ! defined($chain) );
    return &__set_files_where($obj,"Chain",$chain,$stscond,@files);
}


# Setting the ExecDate - val could be 0 or undef which would equate
# to NOW()+0. This was added for convenience as updating a timestamp
# field without knowing the date format could be problematic (requires
# additional calculations)
sub rdaq_set_execdate
{
    my($obj,$val,@files)=@_;
    # $DEBUG = 1;
    return &rdaq_set_execdate_where($obj,$val,-1,@files);
}
sub rdaq_set_execdate_where
{
    my($obj,$val,$stscond,@files)=@_;
    if ( ! defined($val) ){ $val = "NOW()";}
    if ( "$val" eq "0" ){   $val = "NOW()";}
    # print "<!-- In set_execdate_where() calling with $val=[$val] and cond=[$stscond] -->\n" if ($DEBUG);
    return &__set_files_where($obj,"ExecDate",$val,$stscond,@files);
}



# Set Xstatus flag
sub rdaq_set_xstatus
{
    my($obj,$id,$status,@files)=@_;
    return &rdaq_set_xstatus_where($obj,$id,$status,-1,@files);
}
sub rdaq_set_xstatus_where
{
    my($obj,$id,$status,$stscond,@files)=@_;
    return &__set_files_where($obj,"XStatus$id",$status,$stscond,@files);
}



# Common interface
sub __set_files_where
{
    my($obj,$field,$value,$stscond,@files)=@_;
    my($TimeReset);
    my(@items);
    #my($sth,$success,$cmd);
    my($sth,$cmd);
    my(@FS);

    if(!$obj){         return 0;}
    if($#files == -1){ return 0;}

    #$success = 0;

    # If not numerical and they are indirect fields defined via
    # index/value tables, record and/or fetch the value now.
    # print "DEBUG Received field=$field\n";
    if ( defined($THREAD0{$field}) ){
	$value = &Record_n_Fetch($THREAD0{$field},$value);
	print "<!-- Value for $field $THREAD0{$field} case-0 is now $value -->\n" if ($DEBUG);
    } elsif ( defined($THREAD1{$field}) ){
	$value = &Record_n_Fetch($THREAD1{$field},$value);
	print "<!-- Value for $field $THREAD1{$field} case-1 is now $value -->\n" if ($DEBUG);
    }

    # any change would the UpdateDate field
    # $cmd = "UPDATE $DBTABLE SET $field=$value, UpdateDate=NOW()+0 WHERE file=? ";
    $TimeReset = ", UpdateDate=NOW() ";

    if ( $field eq "Status" && $value == 3){  $TimeReset = "";}
    $cmd = "UPDATE $DBTABLE SET $field=$value $TimeReset WHERE file=? ";

    if ($stscond != -1){
	$cmd .= " AND $field=$stscond";
    }
    print "<!-- Cmd=$cmd -->\n" if ($DEBUG);

    $sth = $obj->prepare($cmd);
    if($sth){
	foreach $file (@files){
	    # support for list of files or full list.
	    $file = (split(" ",$file))[0];
	    if($sth->execute($file)){
		push(@FS,$file);
		#$success++;
	    }
	}
	$sth->finish();
    }
    return @FS;
}




#
# Returns all possible values for a given field
# BEWARE of some querries which may return a long-long list ...
#
# $cache is a caching value in second - 0 is no caching
#
sub rdaq_list_field
{
    my($obj,$field,$limit,$cache)=@_;
    my($cmd,$sth,@tmp);
    my($val,$pval,$cfile);
    my($i,@all);

    if(!$obj){ return 0;}
    if( ! defined($limit) ){ $limit = 0;}
    if( ! defined($cache) ){ $cache = 0;}


    # Cache values introduced 2007
    $cfile = "/tmp/RunDAQ.$field.$limit.cache";
    if ( $cache && -e $cfile ){
	if ( time() - (stat($cfile))[9] <= $cache){
	    open(FI,$cfile);
	    while ( defined($val = <FI>) ){
		chomp($val);
		push(@all,$val);
	    }
	    close(FI);
	    return @all;
	}
    }


    # The association of DISTINCT and ROUND is apparently
    # unsafe. It works for 'scaleFactor' but not for 'BeamE' (??).
    # We will therefore make the unicity ourselves.
    if( defined($ROUND{$field}) ){
	$cmd   = "SELECT DISTINCT ROUND($field,$ROUND{$field})".
	    " AS SELECTED FROM $DBTABLE";
	$cmd  .= " ORDER BY SELECTED DESC";
    } elsif ( defined($BITWISE{$field}) ) {
	# this works fine
	$cmd   = "SELECT CONCAT(id,':',Label) FROM $BITWISE{$field}";
	$field = "id";
	$cmd  .= " ORDER BY $field DESC";
    } elsif ( defined($THREAD0{$field}) ){
	$cmd   = "SELECT id FROM $THREAD0{$field}";
	$cmd  .= " ORDER BY Label DESC";
    } elsif ( defined($THREAD1{$field}) ){
	$cmd   = "SELECT Label FROM $THREAD1{$field}";
	$cmd  .= " ORDER BY Label DESC";
    } else {
	$cmd   = "SELECT DISTINCT $field AS SELECTED FROM $DBTABLE";
	$cmd  .= " ORDER BY $field DESC";
    }

    #print "<!-- $cmd -->\n";

    $sth = $obj->prepare($cmd);
    if($sth){
	if($sth->execute()){
	    #print "Execute = success. Fetching.\n";
	    $i   = 0;
	    $pval= "";
	    while ( @tmp = $sth->fetchrow_array() ){
		#print "Debug :: @tmp\n";
		chomp($val = join("",@tmp));
		if( $val ne $pval){
		    $pval = $val;
		    push(@all,$val);
		    $i++;
		}
		if($i == $limit){ last;}
	    }
	} else {
	    &info_message("list_field",3,"Execute failed for $field\n");
	}
	$sth->finish();
    } else {
	&info_message("list_field",3,"[$cmd] could no be prepared\n");
    }

    # write cache
    if ( $cache ){
	if ( -e $cfile ){  unlink($cfile);}
	open(FO,">$cfile");
	foreach $val (@all){
	    print FO "$val\n";
	}
	close(FO);
    }
    @all;
}


# --------------------
# Utility routines.
# --------------------
#
# Returns the bit placement for a BITWISE field.
# A return value of 0 will mean bit 0 set to 1
# and indicate an unknown trigger (missing info
# in the table).
#
sub GetBitValue
{
    my($field,$el,$warn)=@_;
    my($tbl,$rv);

    if( $field eq ""){                       return 0; }
    if( $el    eq ""){                       return 0; }
    if( ! defined($warn) ){                 $warn = 0; }

    # Get the table name from the BITWISE hash configuration
    if( ! defined($tbl = $BITWISE{$field})){ return 0; }

    $rv = &Record_n_Fetch($tbl,$el);
    if ( $warn && $rv >= 64){
	&info_message("GetBitValue",0,"Bit $rv >= 64 in a mask will overflow\n");
    }
    return $rv;
}

#
# Fundamental routine saving/fetching the id of a record
# defined by its descriptor or label. Requires a database
# object handler.
# Returns 0 if any failures.
# Hashes the values to save later processing time.
# $mode if 1, disables insertion of new values.
#
sub Record_n_Fetch
{
    my($tbl,$el)=@_;
    my($obj,$sthc,$val,$rv);

    # cannot insert a null value
    #print "DEBUG Field=$tbl Value = $el\n";
    if($el eq ""){                   return 0;}
    if( sprintf("%s",$el) eq "0"){   return 0;}

    #print "Record_n_Fetch :: $tbl $el\n"  if ($tbl eq "FOFileType");
    print "Record_n_Fetch :: $tbl $el\n"  if ($tbl eq "FOChains");

    if( ! defined($rv = $RFETCHED{"$tbl-$el"}) ){
	# Return value
	#if($tbl eq "FOruns"){
	#    print "<!-- Adding $el -->";
	#}
	$rv  = 0;
	$obj = rdaq_open_odatabase();

	if(!$obj){ return $rv;}


	# Quick and dirty insert. Need quotes as value could be string
	$sthc = $obj->prepare("INSERT IGNORE INTO $tbl VALUES(0,'".$el."')");
	$sthc->execute();
	$sthc->finish();


	# fetch now.
	$sthc = $obj->prepare("SELECT $tbl.id FROM $tbl ".
			      "WHERE $tbl.Label=?");
	if($sthc){
	    if ( $sthc->execute($el) ){
		if( defined($val = $sthc->fetchrow()) ){
		    $RFETCHED{"$tbl-$el"} = $val;
		    $rv = $val;
		}
	    #} else {
	    #	print "<!-- Record_n_Fetch Failed statement with value [$el] -->\n"
	    #	    if ($DEBUG);
	    }
	    $sthc->finish();
	}

	# close database
	rdaq_close_odatabase($obj);
    }
    #print "Record_n_Fetch :: returning $rv\n" if ($tbl eq "FOFileType");
    return $rv;
}

sub GetRecord
{
    my($tbl,$el,$fld)=@_;
    my($obj,$sth,$val,$rv);

    if($el eq ""){  return 0;}
    if($el eq 0){   return 0;}
    if( ! defined($fld) ){
	$fld = "Label";
	$sel = "id";
    } elsif ( $fld eq "ID"){
	$fld = "id";
	$sel = "Label";
    } else {
	$fld = "Label";
	$sel = "id";
    }


    $rv = 0;
    if( ! defined($rv = $RFETCHED{"$tbl-$el"}) ){
	$obj = rdaq_open_odatabase();
	if(!$obj){ return $rv;}
	$sth = $obj->prepare("SELECT $tbl.$fld FROM $tbl ".
			      "WHERE $tbl.$sel=?");
	if($sth){
	    $sth->execute($el);
	    if( defined($val = $sth->fetchrow()) ){
		$RFETCHED{"$tbl-$el"} = $val;
		$rv = $val;
	    }
	    $sth->finish();
	}
	rdaq_close_odatabase($obj);
    }
    $rv;
}


# BACKWARD Compatibility only, this was replaced by bits2string()
# DO NOT USE ANYMORE.
sub rdaq_mask2string
{
    my($val)=@_;
    return rdaq_bits2string("DetSetMask",$val);
}

#
# Any bits 'val' from field column 'field'
# will be associated to a string. Noet that
# the column needs to have a BITWISE association
# entry for this to work.
#
sub rdaq_bits2string
{
    my($field,$val)=@_;
    my($str,@items);
    my($oobj,$sth);

    if( ! defined($BITWISE{$field}) ){  return "unknown";}
    if( ! defined($val) ){              return "unknown";}

    if( ! defined($BITS2STRING{"$field-$val"}) ){
	my(@Vals,%AVals);

	$str = "";
	if ( index("$val","]") != -1 ){
	    @Vals = split(/\./,$val);
	    print "<!-- We splitted to $#Vals -->\n"   if ( $DEBUG );
	    foreach $el (@Vals){
		$el =~ s/\[//; $el =~ s/\]//;
		print "<!-- we found $el -->\n"        if ( $DEBUG );
		$AVals{$el} = 1;
	    }
	} else {
	    print "<!-- We did not split [$val] -->\n" if ( $DEBUG );
	}


	$oobj= rdaq_open_odatabase();
	#print "SELECT * FROM $BITWISE{$field}\n";
	$sth = $oobj->prepare("SELECT * FROM $BITWISE{$field} ORDER BY Label ASC");

	if($sth){
	    $sth->execute();
	    while( @items = $sth->fetchrow_array() ){
		if ( $#Vals != -1){
		    if ( defined($AVals{$items[0]}) ){
			print "<!-- $val $items[0] $items[1] (in string)  -->\n" if ($DEBUG);
			$str .= "$items[1].";
		    } else {
			print "<!-- $val $items[0] $items[1] (not in str) -->\n" if ($DEBUG);
		    }
		} else {
		    # Normal mask
		    print "<!-- $val $items[0] $items[1] ".(1 << $items[0])." -->\n" if ($DEBUG);
		    $str .= "$items[1]." if( $val & (1 << $items[0]) );
		}
	    }
	    chop($str);
	}
	rdaq_close_odatabase($oobj);
	if($str eq ""){ $str = "unknown";}
	$BITS2STRING{"$field-$val"} = $str;
    }
    $BITS2STRING{"$field-$val"};
}




#
# Accept a raw name, return a fully specified HPSS path
# file name.
# Mode 0 -> return 'path/file' (default)
# Mode 1 -> return 'path file' (i.e. with space)
# Mode 2 -> return 'path file year month'
# Mode 3 -> return 'path file year month DayOfYear'
#           month is calculated.
#
# May implement other modes ...
#
sub rdaq_file2hpss
{
    my($file,$mode)=@_;
    my($Hfile,$code);
    my($y,$dm,$n,@items);

    # default
    if( ! defined($mode) ){ $mode = 0;}

    # reduce the a file list (all characteristics) to a file-only
    $file  = (split(" ",$file))[0];

    # parse the damned file name. This is really trivial but
    # good to put it in a module so we can bacward support Y1
    # convention if necessary.
    #            -----v  may be a | list
    $file =~ m/(st_)(\w+_)(\d+)(_.*)/;
    $code = $3;


    print "<!-- Getting $code -->\n" if ($DEBUG);
    ($y,$dm,$n) = $code =~ m/(\d+)(\d{3,})(\d{3,})/;
    print "<!-- Now $y $dm $n -->\n"   if ($DEBUG);

    $y += 1999;


    if($y <= 2000){
	# The default path is to store by month
	# We are NOT taking care of exceptions ...
	@items = Date::Manip::Date_NthDayOfYear($y,$dm);
	my($y1path)=sprintf("%s/%s/%2.2d",$HPSSBASE,$y,$items[1]);

	if($mode == 1){
	    "$y1path $file";
	} elsif ($mode == 2){
	    "$y1path $file $y $items[1]";
	} else {
	    "$y1path/$file";
	}
    } elsif ($y <= 2005) {
	# the default option is to store by day-of-year
	if($mode==1){
	    "$HPSSBASE/$y/$dm $file";
	} elsif ($mode == 2 || $mode == 3){
	    @items = Date::Manip::Date_NthDayOfYear($y,$dm);
	    ($mode == 2)?
		"$HPSSBASE/$y/$dm $file $y $items[1]":
		"$HPSSBASE/$y/$dm $file $y $items[1] $dm";
	} else {
	    "$HPSSBASE/$y/$dm/$file";
	}
    } else {
	# the default since 2005+ is to store by day-of-year
	# and run number
	if($mode==1){
	    "$HPSSBASE/$y/$dm/$code $file";
	} elsif ($mode == 2 || $mode == 3){
	    @items = Date::Manip::Date_NthDayOfYear($y,$dm);
	    ($mode == 2)?
		"$HPSSBASE/$y/$dm/$code $file $y $items[1]":
		"$HPSSBASE/$y/$dm/$code $file $y $items[1] $dm";
	} else {
	    "$HPSSBASE/$y/$dm/$code/$file";
	}
    }
}

#
# Returns the status string for a given entry.
# Status level are set according to the explaination and convention below.
# There is no interface for
#
sub rdaq_status_string
{
    my($sts)=@_;
    my($str);

    $str = "Unknown";
    $str = "new"       if($sts == 0);
    $str = "Submitted" if($sts == 1);
    $str = "Processed" if($sts == 2);
    $str = "QADone"    if($sts == 3);   # i.e. + QA
    $str = "Skipped"   if($sts == 4);

    $str = "SCalib"    if($sts == 5);   # submitted for calibration
    $str = "FCalib"    if($sts == 6);   # submitted for calibration, ezTree

    $str = "External"     if($sts == 7);# submitted for external processing
    $str = "RandomReject" if($sts == 8);# was rejected from pick by random Num
    $str = "StatusHeld"   if($sts == 9);# was rejected from pick by random Num

    $str = "Bad"       if($sts == 111);
    $str = "Died"      if($sts == 666);

    $str;
}


#
# Converts a scale factor to a string following STAR production conventions
# like "FullField", "HalfField" etc ...
#
sub rdaq_scaleToString
{
    my($val)=@_;
    my($stf,$frmt,$vv);

    if ( defined($ROUND{"scaleFactor"}) ){
	# preserve formatting / rounding
	$frmt = sprintf("%%.%df",$ROUND{"scaleFactor"});
	#print "RunDAQ DEBUG $frmt\n";
	$vv   = sprintf($frmt,$val);
    } else {
	$vv   = $val;
    }


    $stf = "Unknown";
    $stf = "FieldOff"          if ($vv ==  0.0);
    $stf = "FullField"         if ($vv ==  1.0);
    $stf = "HalfField"         if ($vv ==  0.5);
    $stf = "ReversedFullField" if ($vv == -1.0);
    $stf = "ReversedHalfField" if ($vv == -0.5);

    #print "RunDAQ DEBUG scale = $val -- $vv --> $stf\n";
    $stf;
}


sub rdaq_trgs2string
{
    my($val)=@_;

    $rv = &GetRecord("FOTriggerSetup",$val);
    if($rv eq 0){
	return "unknown";
    } else {
	$rv;
    }
}

#
# This method returns a number based on a string value
# For example, rdaq_string2trgs("ppProductionMinBias") would return
# the associated id number for that trigger setup name. These methods
# are used to make it easier for users to access those "dynamic" values
# (first triggerSetup would get id 1, second id 2 etc ... so
# depends on year not on a convention)
#
sub rdaq_string2trgs
{
    my($val)=@_;

    $rv = &GetRecord("FOTriggerSetup",$val,"ID");
    if($rv eq 0){
	return 0;
    } else {
	$rv;
    }
}

sub rdaq_ftype2string
{
    my($val)=@_;

    $rv = &GetRecord("FOFileType",$val);
    if($rv eq 0){
	return "unknown";
    } else {
	$rv;
    }
}

#
# Like string2trgs, but returns the id for the file type.
# File type is arbitrary and made from a parsing of the full
# filename. They will be things like 'express', 'zerobias'
# etc ... Parsing allows for infinit combo without having to
# worry of harcoded values. However, the FastOffline interface
# would need alteration if any new "type" of files appear.
#
sub rdaq_string2ftype
{
    my($val)=@_;

    $rv = &GetRecord("FOFileType",$val,"ID");
    if($rv eq 0){
	return 0;
    } else {
	$rv;
    }
}


sub rdaq_set_dlevel
{
    my($level)=@_;
    $DLEVEL = $level;
}


#
# Some utility - Displays inforative message
#
sub	info_message
{
    my($routine,$l,@messages) = @_;
    my($mess);

    if ($l < $DLEVEL){ return;}

    foreach $mess (@messages){
	if ($PHTML){
	    chomp($mess);
	    if ($mess =~ /;/){
		my(@items) = split(/;/,$mess);
		printf "FastOffl :: <TT>%10.10s</TT> :<BR>\n<BLOCKQUOTE>\n",$routine;
		foreach (@items){
		    print "$_<BR>\n";
		}
		print "</BLOCKQUOTE>\n";
	    } else {
		printf "FastOffl :: <TT>%10.10s</TT> : %s<BR>\n",$routine,$mess;
	    }
	} else {
	    # default mode
	    printf "FastOffl :: %10.10s : %s",$routine,$mess;
	}
    }
}


sub    rdaq_toggle_debug
{
    my($arg)=@_;

    if ( ! defined($arg) ){
	$DEBUG = ! $DEBUG;
    } else {
	$DEBUG = ($arg==1);
    }
    return $DEBUG;
}

sub    rdaq_html_mode
{
    my($arg)=@_;

    if ( ! defined($arg) ){
	$PHTML = ! $PHTML;
    } else {
	$PHTML = ($arg==1);
    }
    return $PHTML;
}


#
#
# New method to set delay
#
sub    rdaq_set_delay
{
    my($t)=@_;
    $DELAY = $t if ( defined($t) );
    return $DELAY;
}

#
# We will have
#  FOMessages: id, Itime, Category, Message, Variablemsg
#
# Category is arbitrary.
#
#
sub  rdaq_set_message
{
    my($fac,$mess,$var)=@_;
    my($obj,$val,$sth1,$sth2);

    if ( $obj = rdaq_open_odatabase() ){
	$sth1 = $obj->prepare("SELECT id FROM FOMessages WHERE Message='?' AND Variablemsg='?'");
	$sth2 = $obj->prepare("INSERT INTO FOMessages VALUES(0,NOW(),?,?,?)");
	if ( ! defined($fac) ){  $fac  = "-";}
	if ( ! defined($mess) ){ $cat  = "";}
	if ( ! defined($var)){   $var  = "";}
	if ( $sth1 ){
	    $sth1->execute($mess,$var);
	    if( ! defined($val = $sth1->fetchrow() ) ){
		if ( $sth2 ){
		    $sth2->execute($fac,$mess,$var);
		    $sth2->finish();
		}
	    }
	    $sth1->finish();
	}
    }
}
sub  rdaq_get_message
{
    my($limit,$fld,$sel,$ufield)=@_;
    my($obj,$cmd,$sth,$sts,$key,$ii);
    my(@tmp,@rec,%Rec);

    my(@fields)=("Itime","Category","Message","Variablemsg");
    my($Debug)=0;

    if ( ! defined($limit) ){ $limit = 10;}
    if ( ! defined($sel) ){   $sel   = "";}
    if ( ! defined($fld) ){   $fld   = -1;}
    if ( ! defined($ufield) ){$ufield= -1;}

    # force type for int
    $limit = int($limit);
    $fld   = int($fld);

    push(@rec,";ModuleDebug;Begin; [$limit] [$fld] [$sel] [$ufield]") if ($Debug);

    if ( $obj = rdaq_open_odatabase() ){
	if ( $ufield != -1){
	    $cmd = "SELECT DISTINCT $fields[$ufield] FROM FOMessages";
	} else {
	    $cmd = "SELECT ".join(",",@fields)." FROM FOMessages";

	    if ( $fld != -1){
		# protect string arg - further protect by using prepare with ?
		$sel = (split(" ",$sel))[0];
		$sel =~ s/%.*//;
		$sel =~ s/-/ /g;   # this is an internal convention
		$cmd .= " WHERE $fields[$fld]=?";
	    }
	    $cmd .= " ORDER BY ITime DESC";
	}
	#+
	# ATTENTION - See http://www.star.bnl.gov/HyperNews-star/protected/get/dbdevel/47.html
	# This may break
	#-
	#$cmd .= " ORDER BY ITime DESC LIMIT ?";
	#
	# YES, broken 2007
	#
	#
	#
	## now prepare
	#$sth = $obj->prepare($cmd);
	
	$cmd .= " LIMIT ".(10*$limit) if ($limit != 0);
	$sth = $obj->prepare($cmd);


	push(@rec,";ModuleDebug;Prepare statement; $cmd + [$sel] [$limit]") if ($Debug);
	$ii = 0;

	# can't do the same sorting using MySQL
	# The limit x10 is purely arbitrary as we will count the records we need
	# and stop when we get the proper requested number.
	#
	# Note the logic: if a selector is used, all records otherwise unique
	# based on key Message/Variablemsg
	#

	if ( $sel ne ""){
	    $sts = $sth->execute($sel);
	} else {
	    $sts = $sth->execute();
	}

	if ( $sts ){
	    push(@rec,";ModuleDebug;Execute;Status was OK, ready to execute") if ($Debug);
	    #print "Execute  $ii < $limit ";
	    while ( (@tmp = $sth->fetchrow_array()) && ($ii < $limit || $limit ==0) ){
		#push(@rec,";ModuleDebug;Execute;$#tmp values returned") if ($Debug);
		if ($#tmp == 0){
		    # mode all field distinct
		    push(@rec, $fields[$ufield].";".$tmp[0] );
		    $ii++;
		} else {
		    $key = $tmp[2]." ".$tmp[3];
		    # print "Got [$key] ";
		    if ( ! defined($Rec{$key}) || $sel ne "" ){
			chomp($Rec{$key} = join(";",@tmp));
			push(@rec,$Rec{$key});
			$ii++;
		    }
		}
	    }
	} else {
	    push(@rec,";ModuleDebug;Execute;Error was [".$obj->errstr."]") if ($Debug);

	}
	return @rec;
    } else {
	return undef;
    }
}

sub rdaq_purge_message
{
}


1;

#
# April 2007
#  Added several methods for handling message synchronization /
#  reporting amongst components.
#
#  CREATE TABLE FOMessages (id INT NOT NULL AUTO_INCREMENT, Itime TIMESTAMP
#     NOT NULL DEFAULT CURRENT_TIMESTAMP, Category CHAR(15), Message CHAR(80),
#     Variablemsg TEXT, PRIMARY KEY (id));
#
#
# Dec 2001
#  Changed the meaning of TriggerSetup from trgSetupName to
#  glbSetupName. Seemed more appropriate and what people are
#  accustom too. Added rdaq_trgs2string() interface.
#  Also improved speed in runNumber get_list_field by using
#  THREAD arrays. Only 1239 entries to scan for runNumber for
#  example instead of 113940 (2 order of magnitude up).
#

