# FileCatalog.pm
#
# Written by Adam Kisiel, service work November-December 2001
# Directed and/or written + modified by J.Lauret, 2002 - 2013
#
# Methods of class FileCatalog:
#
#        ->new             : create new object FileCatalog
#        ->connect         : connect to the database FilaCatalog
#        ->destroy         : destroy object and disconnect database FileCatalog
#        ->set_context()   : set one of the context keywords to the given
#                            operator and value
#        ->set_optional_context()
#                          : set this context as well but only if one key
#                            is from the same table. Optional context are valid
#                            only for querry mode and NOT in record addition
#                            mode.
#        ->get_context()   : get a context value connected to a given keyword
#        ->clear_context() : clear/reset the context
#        ->get_keyword_list() : get the list of valid keywords
#        ->get_delimeter() : get the current delimiting string
#        ->set_delimeter() : set the current delimiting string
#
# the following methods require connect to dbtable and are meant to be used
# outside the module
#
#        -> check_ID_for_params() : returns the database row ID from the
#                          dictionary table connected to this keyword
#
#        -> insert_dictionary_value() : inserts the value from the context
#                          into the dictionary table
#        -> get_current_detector_configuration() : gets the ID of a detector
#                          configuration described by the current context. If
#                          does not exists, return an existing ID (or 0)
#        -> get_prodlib_version_ID (internal)
#
#        -> insert_run_param_info() : insert the run param record taking data
#                          from the current context
#        -> get_current_run_param() : get the ID of a run params corresponding
#                          to the current context
#        -> insert_file_data() : inserts file data record taking data from
#                          the current context
#        -> get_current_file_data() : gets the ID of a file data corresponding
#                          to the current context
#        -> insert_simulation_params() : insert the simulation parameters
#                          taking data from the current context
#        -> get_current_simulation_params : gets the ID of a simulation params
#                          corresponding to the current contex
#        -> insert_file_location() : insert the file location data taking
#                          data from the current context
#
#        -> get_file_location()
#                          returns the FileLocations info in an array context.
#                          The return value can be used as-is in a
#                          set_context() statement.
#        -> get_file_data()
#                          returns the FileData info in an array context.
#                          The return value can be used as-is in a
#                          set_context() statement.
#        -> clone_location()
#                          actually create an instance for FileData and a
#                          copy of FileLocations the latest to be modified
#                          with set_context() keywords.
#
#
#        -> run_query()   : get entries from dbtable FileCatalog according to
#                          query string defined by set_context you also give a
#                          list of fields to select form
#        -> run_query_cache() same but force caching if exists. This should not
#                          be used if the operations deletes records (as cache
#                          will not be updated until a cache lifetime expiration)
#
#        -> delete_records() : deletes the current file locations based on
#                          context. If it finds that the current file data has
#                          no file locations left, it deletes it too
#        -> update_record() : modifies the data in the database. The field
#                          corresponding to the given keyword changes it
#                          value from the one in the current context to the
#                          one specified as an argument
#
#        -> bootstrap() : database maintenance procedure. Looks at the dictionary table
#                          and find all the records that are not referenced by the child
#                          table. It offers an option of deleting this records.
#
#        -> set_delayed()  turn database operation in delay mode. A stack is built
#                          and execute later. This may be used in case of several
#                          non-correlated updates. Warning : no checks made on delayed
#                          commands.
#        -> unset_delayed() remove the delay flag
#        -> flush_delayed() flush out i.e. execute all delayed commands.
#        -> print_delayed() print out on screen all delayed commands.
#
#        -> set_thresholds() sets an upper limit for number of SELECT, INSERT, DELETE
#        -> warn_if_duplicates() disable warning of duplicate actions - to be used if a
#                            two database scheme is used (doing quesries on slave but
#                            updates on master) as slow propagation of updates may
#                            create this condition
#        -> was_file_cache_used()  return true/false depending on file cache usage
#                            This will help debugging
#
# NOT YET DOCUMENTED
#
#        ->add_trigger_composition()
#


package  FileCatalog;
require  5.008;


require  Exporter;
@ISA   = qw(Exporter);
@EXPORT= qw( connect destroy get_dbh
	     set_context set_optional_context get_context clear_context
	     get_keyword_list
	     set_delimeter get_delimeter
	     set_delayed unset_delayed flush_delayed print_delayed
	     add_trigger_composition

	     run_query	run_query_cache
	     was_file_cache_used
	     clone_location insert_file_data
	     delete_records update_location update_record

	     check_ID_for_params insert_dictionary_value
	     get_detectors

	     set_thresholds warn_if_duplicates

	     debug_on debug_off message_class
	     Require Version

	     );

# @EXPORT_OK = qw(%operset %valuset);


use vars qw($VERSION);
$VERSION   =   "V01.400";

# The hashes that hold a current context
my %optoperset;
my %optvaluset;
my %operset;
my %valuset;


use DBI;
use Digest::MD5;
use strict;
no strict "refs";

# define to print debug information
my $NCTRY     =  6;
my $NCSLP     =  5;
my $PCLASS    = "";
my $DELAY     =  0;
my $OPTIMLIMIT= 10;                      # purely empirical I am afraid (just run query with -limit)
my @DCMD;

# db information
my $DSITE     =   undef;
my $XMLREF    =   undef;
my $dbname    =   "FileCatalog_BNL";     # Defaults were
my $dbhost    =   "fc1.star.bnl.gov";    # "duvall.star.bnl.gov";
my $dbport    =   "3336";                # "3336";
my $dbuser    =   "FC_user";
my $dbpass    =   "FCatalog";
my $sth;

# Some other name-spaced globals
$FC::DEBUG        =  0;                  # enable debuging -debug
$FC::SILENT       =  0;                  # if SILENT is enabled, all messages are suppressed

$FC::DBH          = undef;
$FC::INTENT       = "User";              # Default intent / do not change

$FC::DBCONTIMEOUT = 5;                   # << NOT USED YET
$FC::TIMEOUT      = 2700;                # timeout of 45 mnts for query
$FC::USECACHE     = 1;                   # use query cache if exists
$FC::CACHELIFE    = 7200;                # query cache lifetime will be 2 hours
$FC::CACHE_USED   = 0;                   # was caching used or not?
@FC::LOADMANAGE   = (50,10,15);          # s,i,d - default values / no update count for now

$FC::WDUPS        = 1;                   # warn when duplicate update/delete happens


# hash of keywords
my %keywrds;
my %ksimilar;
my %ktransform;

# hash of obsolete keywords
my %obsolete;


# Arrays to treat triggers
$FC::IDX     = -1;
@FC::TRGNAME = undef;
@FC::TRGWORD = undef;
@FC::TRGVERS = undef;
@FC::TRGDEFS = undef;
@FC::TRGCNTS = undef;


# $keys{keyword} meaning of the parts of the field:
# k - parameter name as entered by the user
# 0 - field name in the database
# 1 - table name in the database for the given field
# 2 - critical for data insertion into the specified table
# 3 - type of the field (valid are text,num,date)
# 4 - (??) -- was used for dict or not dict but now in datastruct
# 5 - if 0, is not returned by the FileTableContent() routine (used in cloning)
# 6 - if 1, displays as a user usable keywords, skip otherwise (get_keyword_list)
#     This field cannot be a null string.
#
# only the keywords in this table are accepted in set_context sub

# Those are for private use only but require a keyword for access.
# DO NOT DOCUMENT THEM !!!
$keywrds{"flid"          }    =   "fileLocationID"            .",FileLocations"          .",0" .",num"  .",0" .",0" .",0";
$keywrds{"glid"          }    =   "fileLocationID"            .",FileLocationsID"        .",0" .",num"  .",0" .",0" .",0";

$keywrds{"fdid"          }    =   "fileDataID"                .",FileData"               .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rfdid"         }    =   "fileDataID"                .",FileLocations"          .",0" .",num"  .",0" .",1" .",0";
$keywrds{"pcid"          }    =   "productionConditionID"     .",ProductionConditions"   .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rpcid"         }    =   "productionConditionID"     .",FileData"               .",0" .",num"  .",0" .",1" .",0";
$keywrds{"rfpid"         }    =   "filePathID"                .",FileLocations"          .",0" .",num"  .",0" .",1" .",0";
$keywrds{"fpid"          }    =   "filePathID"                .",FilePaths"              .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rhid"          }    =   "hostID"                    .",FileLocations"          .",0" .",num"  .",0" .",1" .",0";
$keywrds{"hid"           }    =   "hostID"                    .",Hosts"                  .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rpid"          }    =   "runParamID"                .",RunParams"              .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rrpid"         }    =   "runParamID"                .",FileData"               .",0" .",num"  .",0" .",1" .",0";
$keywrds{"trgid"         }    =   "triggerSetupID"            .",TriggerSetups"          .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rtrgid"        }    =   "triggerSetupID"            .",Runparams"              .",0" .",num"  .",0" .",1" .",0";
$keywrds{"ftid"          }    =   "fileTypeID"                .",FileTypes"              .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rftid"         }    =   "fileTypeID"                .",FileData"               .",0" .",num"  .",0" .",1" .",0";
$keywrds{"stid"          }    =   "storageTypeID"             .",StorageTypes"           .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rstid"         }    =   "storageTypeID"             .",FileLocations"          .",0" .",num"  .",0" .",1" .",0";
$keywrds{"ssid"          }    =   "storageSiteID"             .",StorageSites"           .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rssid"         }    =   "storageSiteID"             .",FileLocations"          .",0" .",num"  .",0" .",1" .",0";
$keywrds{"dcid"          }    =   "detectorConfigurationID"   .",DetectorConfigurations" .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rdcid"         }    =   "detectorConfigurationID"   .",RunParams"              .",0" .",num"  .",0" .",1" .",0";
$keywrds{"dsid"          }    =   "detectorStateID"           .",DetectorStates"         .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rdsid"         }    =   "detectorStateID"           .",RunParams"              .",0" .",num"  .",0" .",1" .",0";
$keywrds{"spid"          }    =   "simulationParamsID"        .",SimulationParams"       .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rspid"         }    =   "simulationParamID"         .",EventGenerator"         .",0" .",num"  .",0" .",1" .",0";
$keywrds{"rtid"          }    =   "runTypeID"                 .",RunTypes"               .",0" .",num"  .",0" .",0" .",0";
$keywrds{"rrtid"         }    =   "runTypeID"                 .",RunParams"              .",0" .",num"  .",0" .",1" .",0";
$keywrds{"crid"          }    =   "creatorID"                 .",Creators"               .",0" .",num"  .",0" .",0" .",0";



# *** Those should be documented
$keywrds{"filetype"      }    =   "fileTypeName"              .",FileTypes"              .",1" .",text" .",0" .",1" .",1";
$keywrds{"extension"     }    =   "fileTypeExtension"         .",FileTypes"              .",1" .",text" .",0" .",1" .",1";
$keywrds{"storage"       }    =   "storageTypeName"           .",StorageTypes"           .",1" .",text" .",0" .",1" .",1";
$keywrds{"site"          }    =   "storageSiteName"           .",StorageSites"           .",1" .",text" .",0" .",1" .",1";
$keywrds{"siteloc"       }    =   "storageSiteLocation"       .",StorageSites"           .",1" .",text" .",0" .",1" .",1";
$keywrds{"sitecmt"       }    =   "storageComment"            .",StorageSites"           .",1" .",text" .",0" .",1" .",1";
$keywrds{"production"    }    =   "productionTag"             .",ProductionConditions"   .",1" .",text" .",0" .",1" .",1";
$keywrds{"prodcomment"   }    =   "productionComments"        .",ProductionConditions"   .",0" .",text" .",0" .",1" .",1";
$keywrds{"library"       }    =   "libraryVersion"            .",ProductionConditions"   .",1" .",text" .",0" .",1" .",1";


# Trigger related keywords. Reshaped and cleaned on Dec 1st 2002
$obsolete{"triggersetup"}= "trgsetupname";
$obsolete{"triggername"} = "trgsetupname";  # not a 1 to 1 mapping but this is what we initially meant
$obsolete{"triggerword"} = "trgword";

$keywrds{"trgsetupname"  }    =   "triggerSetupName"          .",TriggerSetups"          .",1" .",text" .",0" .",1" .",1";

# The count of individual triggers, the FileData index access in TriggerCompositions and
# the trigger word ID in the TriggerComposition table. Note that the filed 4 set for
# cloning will have no effect as the table association is "reversed" (it contains
# an association to FileData).
$keywrds{"tcfdid"        }    =   "fileDataID"                .",TriggerCompositions"    .",0" .",num"  .",0" .",0" .",0";
$keywrds{"tctwid"        }    =   "triggerWordID"             .",TriggerCompositions"    .",0" .",text" .",0" .",1" .",0";
$keywrds{"trgcount"      }    =   "triggerCount"              .",TriggerCompositions"    .",0" .",text" .",0" .",1" .",1";

$keywrds{"twid"          }    =   "triggerWordID"             .",TriggerWords"           .",0" .",text" .",0" .",0" .",0";
$keywrds{"trgname"       }    =   "triggerWordName"           .",TriggerWords"           .",0" .",text" .",0" .",1" .",1";
$keywrds{"trgversion"    }    =   "triggerWordVersion"        .",TriggerWords"           .",0" .",text" .",0" .",1" .",1";
$keywrds{"trgword"       }    =   "triggerWordBits"           .",TriggerWords"           .",0" .",text" .",0" .",1" .",1";
$keywrds{"trgdefinition" }    =   "triggerWordComment"        .",TriggerWords"           .",0" .",text" .",0" .",1" .",1";


# This keyword is a special keyword which will be used to enter
# a list of triggers/count in the database. It is an agregate
# keyword only used in INSERT mode.
#$keywrds{"triggerevents" }    =   ",,,,,,0";
$obsolete{"triggerevents" }    = "method add_trigger_composition()";




$keywrds{"runtype"       }    =   "runTypeName"               .",RunTypes"               .",1" .",text" .",0" .",1" .",1";
$keywrds{"configuration" }    =   "detectorConfigurationName" .",DetectorConfigurations" .",1" .",text" .",0" .",1" .",1";
$keywrds{"geometry"      }    =   "detectorConfigurationName" .",DetectorConfigurations" .",0" .",text" .",0" .",1" .",1";
$keywrds{"runnumber"     }    =   "runNumber"                 .",RunParams"              .",1" .",num"  .",0" .",1" .",1";
$keywrds{"runcomments"   }    =   "runParamComment"           .",RunParams"              .",0" .",text" .",0" .",0" .",1";
$keywrds{"collision"     }    =   "collisionEnergy"           .",CollisionTypes"         .",1" .",text" .",0" .",1" .",1";
$keywrds{"datastarts"    }    =   "dataTakingStart"           .",RunParams"              .",0" .",date" .",0" .",1" .",1";
$keywrds{"dataends"      }    =   "dataTakingEnd"             .",RunParams"              .",0" .",date" .",0" .",1" .",1";
$keywrds{"daynumber"     }    =   "dataTakingDay"             .",RunParams"              .",0" .",num"  .",0" .",1" .",1";
$keywrds{"year"          }    =   "dataTakingYear"            .",RunParams"              .",0" .",num"  .",0" .",1" .",1";
$keywrds{"magscale"      }    =   "magFieldScale"             .",RunParams"              .",1" .",text" .",0" .",1" .",1";
$keywrds{"magvalue"      }    =   "magFieldValue"             .",RunParams"              .",0" .",num"  .",0" .",1" .",1";
$keywrds{"filename"      }    =   "fileName"                  .",FileData"               .",1" .",text" .",0" .",1" .",1";
$keywrds{"basename"      }    =   "baseName"                  .",FileData"               .",0" .",text" .",0" .",1" .",1";
$keywrds{"sname1"        }    =   "sName1"                    .",FileData"               .",0" .",text" .",0" .",1" .",1";
$keywrds{"sname2"        }    =   "sName2"                    .",FileData"               .",0" .",text" .",0" .",1" .",1";
$keywrds{"fileseq"       }    =   "fileSeq"                   .",FileData"               .",1" .",num"  .",0" .",1" .",1";
$keywrds{"stream"        }    =   "fileStream"                .",FileData"               .",1" .",num"  .",0" .",1" .",1";
$keywrds{"filecomment"   }    =   "fileDataComment"           .",FileData"               .",0" .",text" .",0" .",0" .",1";
$keywrds{"events"        }    =   "numEntries"                .",FileData"               .",1" .",num"  .",0" .",1" .",1";
$keywrds{"md5sum"        }    =   "md5sum"                    .",FileData"               .",1" .",text" .",0" .",1" .",1";
#$keywrds{"fdcreator"     }    =   "fileDataCreator"           .",FileData"               .",1" .",num"  .",0" .",1" .",1";
$keywrds{"size"          }    =   "fsize"                     .",FileLocations"          .",1" .",num"  .",0" .",1" .",1";
$keywrds{"owner"         }    =   "owner"                     .",FileLocations"          .",0" .",text" .",0" .",1" .",1";
$keywrds{"protection"    }    =   "protection"                .",FileLocations"          .",0" .",text" .",0" .",1" .",1";
$keywrds{"available"     }    =   "availability"              .",FileLocations"          .",0" .",num"  .",0" .",1" .",1";
$keywrds{"persistent"    }    =   "persistent"                .",FileLocations"          .",0" .",num"  .",0" .",1" .",1";
$keywrds{"sanity"        }    =   "sanity"                    .",FileLocations"          .",0" .",num"  .",0" .",1" .",1";
$keywrds{"createtime"    }    =   "createTime"                .",FileLocations"          .",0" .",date" .",0" .",1" .",1";
$keywrds{"inserttime"    }    =   "insertTime"                .",FileLocations"          .",0" .",date" .",0" .",1" .",1";
$keywrds{"simcomment"    }    =   "simulationParamComment"    .",SimulationParams"       .",0" .",text" .",0" .",0" .",1";
$keywrds{"generator"     }    =   "eventGeneratorName"        .",EventGenerators"        .",1" .",text" .",0" .",1" .",1";
$keywrds{"genversion"    }    =   "eventGeneratorVersion"     .",EventGenerators"        .",1" .",text" .",0" .",1" .",1";
$keywrds{"gencomment"    }    =   "eventGeneratorComment"     .",EventGenerators"        .",0" .",text" .",0" .",0" .",1";
$keywrds{"genparams"     }    =   "eventGeneratorParams"      .",EventGenerators"        .",1" .",text" .",0" .",1" .",1";

# This is a dictionary with multiple association
$keywrds{"creator"       }    =   "creatorName"               .",Creators"               .",1" .",text" .",0" .",0" .",0";

# Path related keywords apart from index
$keywrds{"path"          }    =   "filePathName"              .",FilePaths"              .",1" .",text" .",0" .",0". ",1";
$keywrds{"pathcomment"   }    =   "filePathComment"           .",FilePaths"              .",0" .",text" .",0" .",0". ",1";

# Node related keywords
$keywrds{"node"         }     =   "hostName"                  .",Hosts"                  .",0" .",text" .",0" .",0". ",1";
$keywrds{"nodecomment"  }     =   "hostComment"               .",Hosts"                  .",0" .",text" .",0" .",0". ",1";

# old keyword made obsolete
$obsolete{"datetaken"} = "datastarts";


# The detector configuration can be extended as needed
# > alter table DetectorConfigurations ADD dEEMC TINYINT AFTER dEMC;
# > update DetectorConfigurations SET dEEMC=0;
#
# + definition here and insert_detector_configuration()
# and we are Ready to go for a new column
#
# DETECTORS array correspond the fields in DetectorConfigurations table.
# This will make extension easier. For a keyword 'xxx', a field 'dXXX'
# must exist in the db table.
# Auto-initialization of keywords will do the equivalent of the above
# for xxx=tpc. See _initialize() for how this is auto-set.
# $keywrds{"tpc"           }    =   "dTPC"                      .",DetectorConfigurations" .",1" .",num"  .",0" .",1" .",1";
my @DETECTORS=("tpc","svt","tof","emc","eemc","fpd","ftpc",
	       "pmd","rich","ssd","bbc","bsmd","esmd","zdc",
	       "ctb","tpx","fgt","mtd","pxl","ist","sst",
	       "gmt","l4", "fps", "pp2pp", "etof", "fcs",
               "rhicf");




# Special keywords
$keywrds{"simulation"    }    =   ",,,,,,1";
$keywrds{"nounique"      }    =   ",,,,,,1";
$keywrds{"noround"       }    =   ",,,,,,1";
$keywrds{"startrecord"   }    =   ",,,,,,1";
$keywrds{"limit"         }    =   ",,,,,,1";
$keywrds{"rlimit"        }    =   ",,,,,,1";
$keywrds{"all"           }    =   ",,,,,,1";
$keywrds{"cache"         }    =   ",,,,,,1";

# Keyword aliasing or keyword aggregate
$keywrds{"lgnm"          }    =   ",,,,,,1";
$keywrds{"lgpth"         }    =   ",,,,,,1";
$keywrds{"fulld"         }    =   ",,,,,,1";  # pseudo-full information agregate for real data
$keywrds{"fulls"         }    =   ",,,,,,1";  # pseudo-full information agregate for simulation data
#$keywrds{"md5n"          }    =   ",,,,,,1";
#$keywrds{"md5p"          }    =   ",,,,,,1";

$ksimilar{"lgnm"         }    =   "logical_name;production library runnumber runtype filename trgsetupname configuration";
$ksimilar{"lgpth"        }    =   "logical_path;site node storage path";
$ksimilar{"fulld"        }    =   "data_description;production library runnumber runtype site node storage path filename events trgsetupname";
$ksimilar{"fulls"        }    =   "simulation_description;production library runnumber runtype site node storage path filename events configuration generator genversion genparams";
#$ksimilar{"md5n"         }    =   "md5_name;production library trgsetupname runnumber filename";
#$ksimilar{"md5p"         }    =   "md5_path;site node storage path";


# Field transformation routine. This can be relatively powerfull as well as confusing
#$ktransform{"fdcreator"   }    =   "_CreatorID2Name;_CreatorName2ID";



# Fields that need to be rounded when selecting from the database
my $roundfields = "magFieldValue,2 collisionEnergy,0";

# The delimeter to sperate fields at output
my $delimeter = "::";


# The list of connections between tables in the database
# needed to build queries with joins
# fields:
# 1 - Table being linked to
# 2 - Table that links the given table
# 3 - Name of the linking field in both tables
# 4 - "Level" of the table in the DB structure
#     The table is at level 1, if it is not directly referenced by any other table
#     The table is at level 2 if it is directly referenced by a table at level 1
#     etc.
# 5 - 1 if table is a dictionary table, 0 otherwise
# 6 - unused
#
# FileLocations is considered as being at level 1
#
# CollisionTypes is NOT a dictionary because the usable returned value
# is subject to a merging combo of several field with truncation. This
# is a collection' table.
#
# Note : TriggerCompositions is NOT a dictionnary comparing to FileData
#        so is not a level 3 but 2.
#

my @datastruct;
$datastruct[0]  = ( "StorageTypes"           . ",FileLocations"       . ",storageTypeID"           . ",2" . ",1" .",0");
$datastruct[1]  = ( "StorageSites"           . ",FileLocations"       . ",storageSiteID"           . ",2" . ",1" .",0");
$datastruct[2]  = ( "FileData"               . ",FileLocations"       . ",fileDataID"              . ",2" . ",0" .",0");
$datastruct[3]  = ( "FilePaths"              . ",FileLocations"       . ",filePathID"              . ",2" . ",1" .",0");
$datastruct[4]  = ( "Hosts"                  . ",FileLocations"       . ",hostID"                  . ",2" . ",1" .",0");
$datastruct[5]  = ( "TriggerWords"           . ",TriggerCompositions" . ",triggerWordID"           . ",2" . ",1" .",0");
$datastruct[6]  = ( "FileData"               . ",TriggerCompositions" . ",fileDataID"              . ",2" . ",0" .",0");
$datastruct[7]  = ( "ProductionConditions"   . ",FileData"            . ",productionConditionID"   . ",3" . ",1" .",0");
$datastruct[8]  = ( "FileTypes"              . ",FileData"            . ",fileTypeID"              . ",3" . ",1" .",0");
$datastruct[9]  = ( "RunParams"              . ",FileData"            . ",runParamID"              . ",3" . ",0" .",0");
$datastruct[10] = ( "RunTypes"               . ",RunParams"           . ",runTypeID"               . ",4" . ",1" .",0");
$datastruct[11] = ( "DetectorConfigurations" . ",RunParams"           . ",detectorConfigurationID" . ",4" . ",0" .",0");
$datastruct[12] = ( "DetectorStates"         . ",RunParams"           . ",detectorStateID"         . ",4" . ",0" .",0");
$datastruct[13] = ( "CollisionTypes"         . ",RunParams"           . ",collisionTypeID"         . ",4" . ",0" .",0");
$datastruct[14] = ( "TriggerSetups"          . ",RunParams"           . ",triggerSetupID"          . ",4" . ",1" .",0");
$datastruct[15] = ( "SimulationParams"       . ",RunParams"           . ",simulationParamsID"      . ",4" . ",0" .",0");
$datastruct[16] = ( "EventGenerators"        . ",SimulationParams"    . ",eventGeneratorID"        . ",5" . ",1" .",0");
$datastruct[17] = ( "FileLocations"          . ","                    . ","                        . ",1" . ",0" .",0");
$datastruct[18] = ( "TriggerCompositions"    . ","                    . ","                        . ",1" . ",0" .",0");
$datastruct[19] = ( "Creators"               . ","                    . ","                        . ",1" . ",1" .",0");
$datastruct[20] = ( "FileLocationsID"        . ",FileLocations"       . ",fileLocationID"          . ",2" . ",1" .",0");



# Will build this by hand too and may automate later
$FC::SUPERIDX{"storage"} = "FileLocations";

# New internal relational table handling partitioning, a comma separated
# list of possible sub-indexes which will lead to Table_$index1{..._$indexN}
# Note also that partitioning by N-1 is forward comaptible with partitioning
# by N but re-ordering the index is not supported.
$FC::PARTITION{"FileLocations"}    = "site,storage";


#%FC::FLRELATED;
#%FC::FDRELATED;
#%FC::ISDICT;


# The operators allowed in set_context query - two-characters operators first
my @operators;
$operators[0] = "<=";
$operators[1] = ">=";
$operators[2] = "<>";
$operators[3] = "!="; # this operator mysteriously works in MySQL as well
$operators[4] = "=="; # this operator is fake and equivalent of "="
$operators[5] = "!~";
$operators[6] = "=";
$operators[7] = "][";
$operators[8] = "[]";
$operators[9] = ">";
$operators[10]= "<";
$operators[11]= "~";
$operators[12]= "%%";
$operators[13]= "%";




# The possible aggregate values
my @aggregates;
$aggregates[0] = "sum";
$aggregates[1] = "count";
$aggregates[2] = "avg";
$aggregates[3] = "min";
$aggregates[4] = "max";
$aggregates[5] = "grp";
$aggregates[6] = "orda";
$aggregates[7] = "ordd";

# A table holding the number of records in each table
#my %rowcounts;
#$rowcounts{"StorageTypes"} = 0;
#$rowcounts{"StorageSites"} = 0;
#$rowcounts{"FileData"} = 0;
#$rowcounts{"ProductionConditions"} = 0;
#$rowcounts{"FileTypes"} = 0;
#$rowcounts{"TriggerWords"} = 0;
#$rowcounts{"FileData"} = 0;
#$rowcounts{"RunParams"} = 0;
#$rowcounts{"RunTypes"} = 0;
#$rowcounts{"DetectorConfigurations"} = 0;
#$rowcounts{"CollisionTypes"} = 0;
#$rowcounts{"TriggerSetups"} = 0;
#$rowcounts{"SimulationParams"} = 0;
#$rowcounts{"EventGenerators"} = 0;
#$rowcounts{"FileLocations"} = 0;
#$rowcounts{"TriggerCompositions"} = 0;


# Those variables will be used internally
# - those are used to store main tables associated keywords
my @CTXMEM;
my @FDKWD;
my @FLKWD;
my %GUPDID;



# - those are for caching
my %KNOWNVP;                 # 2 arrays (the P=persistent and
my %KNOWNVT;                 #           T=Temporary)
my @KNOWNVC     =  (0,0);    # 2 counters
my $CACHESZ     = 500000;    # both are subject to cache flush but "T"
                             # holds large dictionnaries
my %TCACHED;
$TCACHED{"FilePaths"} = 1;   # declare who is in T (default is P)

my %TRIMMSG;


sub Require
{
    if ($_[0] =~ m/FileCatalog/) {
	shift @_;
    }
    my($vmin)=@_;

    if ( ! defined($vmin) ){ $vmin = "";}
    if ( $vmin gt $VERSION || $vmin eq ""){
	&die_message("Require",
		     "Required $vmin, current version $VERSION");
    }
}


sub Version
{
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}
    return $VERSION;
}


#============================================
# parse keywrds - get the field name for the given keyword
# Parameters:
# keyword to get the field name for
# Returns:
# field name for a given context keyword
sub _GetFieldName {
  my @params = @_;

  my ($fieldname, $tabname, $rest) = split(",",$keywrds{$params[0]});
  return $fieldname;
}

#============================================
# parse keywrds - get the table name for the given keyword
# Parameters:
# keyword to get the table name for
# Returns:
# table name for a given context keyword
sub _GetTableName {
  my ($mykey) = (@_);
  my ($tabname, $a, $b);

  if( ! defined($mykey) ){ return;}

  if (exists $keywrds{$mykey}){
      ($a,$tabname,$b) = split(",",$keywrds{$mykey});
  } else {
      &die_message("_GetTableName","Internal error ; Using non-existent key [$mykey]");
  }
  return $tabname;

}

#============================================
# get the list of valid keywords
# Returns:
# the list of valid keywords to use in FileCatalog queries
sub get_keyword_list {
    my($val,$kwd,$alldets);
    my(@items,@kwds,@dets);


    $alldets = ",".join(",",@DETECTORS).",";
    #print "[$alldets]";

    foreach $val (sort { $a cmp $b } keys %keywrds){
	@items = split(",",$keywrds{$val});
	if ($items[6] == 1){
	    if ( index($alldets,",$val,") != -1 ){
		# this is a detector presence flag - append at the end
		# print "Found $val\n";
		push(@dets,$val);
	    } else {
		push(@kwds,$val);
	    }
	} else {
	    &print_debug("get_keyword_list","Rejecting $items[0]");
	}
    }
    push(@kwds,@dets);
    return @kwds;
}

#============================================
# change the deleimiting string between output fields
# Parameters:
# new deliemiting string
sub set_delimeter {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  }
  my @params = @_;

  $delimeter = $params[0];
}

#============================================
# get the current delimiting string
# Returns:
# current delimiting string
sub get_delimeter {
  return $delimeter;
}

#============================================
# parse keywrds - get the field type
# Parameters:
# keyword to get the type for
# Returns:
# type of the field for a given keyword
sub get_field_type {
  my @params = @_;

  my ($fieldname, $tabname, $req, $type, $rest) = split(",",$keywrds{$params[0]});
  return $type;
}

#============================================
# parse keywrds - see if the field is required in insert statement
# Parameters:
# keyword to check
# Returns:
#  0 if field is not needed
#  1 if field is mandatory for inserts
sub is_critical {
  my @params = @_;

  my ($fieldname, $tabname, $req, $type, $rest) = split(",",$keywrds{$params[0]});
  # nolimt, all etc ... will lead to empty string, not numeric
  if($req eq ""){  $req = 0;}
  return $req;
}


#sub is_dictionary
#{
#    my();
#}

#============================================
sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self  = {};

  bless ($self , $class);
  $self->_initialize();

  return $self;
}

sub _initialize
{
    my ($self) = shift;

    # Only way to bless it is to declare them inside
    # new(). See also use Symbol; and usage of my $bla = gensym;
    $valuset{"all"} = 0;
    $delimeter = "::";

    #print "self is $self \n";


    #foreach my $el (sort keys %FileCatalog::){
    #	print "$el has for value $FileCatalog::{$el}\n";
    #}

    # Fill this associative arrays automatically
    my (@items);
    foreach (@datastruct){
	@items = split(",",$_);
	if    ( $items[1] eq "FileLocations"){  $FC::FLRELATED{$items[0]} = $items[2]; } # save relat table
	elsif ( $items[1] eq "FileData"){       $FC::FDRELATED{$items[0]} = $items[2]; } # save relat table
	if ( $items[4] eq "1"){                 $FC::ISDICT{$items[0]}    = $items[3];   # save level
						# BTW : we can only get this printed by outside-hack var() setup
						&print_debug("_initialize","Dictionary $items[0] $FC::ISDICT{$items[0]}");
					    }
    }
    #
    # Initialize the DETECTORS for handling the DetectorConfigurations
    # table. This is an automatic setup of keywords.
    #
    my($det);
    foreach $det (@DETECTORS){
	$keywrds{lc($det)}      = "d".uc($det)              .",DetectorConfigurations" .",1" .",num"  .",0" .",1" .",1";
	$keywrds{lc($det)."OK"} = "s".uc($det)              .",DetectorStates"         .",1" .",num"  .",0" .",1" .",0";
    }


}

#
# Read configuration file if any
# This routine is internal.
#
sub _ReadConfig
{
    my($intent,$flag)=@_;
    my($config,$line,$rest);
    my ($ok,$scope);
    my(%EL);                        # ($host,$db,$port,$user,$passwd,$site);


    $config = "";
    $ok     = 0;

    foreach $scope ( (".",
		      $ENV{HOME},
		      $ENV{SCATALOG},
	              defined($ENV{STAR_PATH})?$ENV{STAR_PATH}."/conf":"./conf",
		      defined($ENV{STAR})?$ENV{STAR}."/StDb/servers":"./StDb/servers",
		      ) ){
	if ( ! defined($scope) ){ next;}
	&print_debug("_ReadConfig","XML :: Checking $scope");
	if ( -e $scope."/Catalog.xml" ){
	    $config = $scope."/Catalog.xml";
	    &print_debug("_ReadConfig","XML :: Will use $config");
	    last;
	}
    }


    if ($config eq ""){
	&print_debug("_ReadConfig","XML :: could not find any config file");
    } else {
	eval "require XML::Simple";
	if (!$@){
	    if ( ! defined($XMLREF) ){
		# use Data::Dumper;
		require XML::Simple;

		my($xs) = new XML::Simple();
		$XMLREF = $xs->XMLin($config,
				     ForceArray => 1);
		# print Dumper($XMLREF);
	    }


	    my($site,$lintent);
	    my($ref,$server) ;
	    my($bref);
	    # intent can be BNL::Admin
	    if ($intent =~ /::/){
		($site,$lintent) = split("::",$intent);
	    } else {
		$site    = (defined($DSITE)?$DSITE:"");
		$lintent = $intent;
	    }
	    $FC::INTENT = $lintent;  # save it for later

	    #
	    # schema change in terms of object inheritance is not that
	    # hard but currently prevent us from looping over multiple
	    # the upper level.
	    #
	    my (@servers);
	    if ( ! defined($XMLREF->{VERSION}) ){
		&print_message("_ReadConfig","Old schema design found in $config (may fail). Please update ..");
	    }

	    &print_debug("_ReadConfig","XML :: Parsing schema version $XMLREF->{VERSION}");
	    if ( $XMLREF->{VERSION} eq "1.0.0" || ! defined($XMLREF->{SITE}) ){
		# Schema may have one level less i.e. SITE
		&print_debug("_ReadConfig","XML :: Dereferencing full schema");
		@servers = @{$XMLREF->{SERVER}};
	    } else {
		# Starting at 1.0.1 actually, SITE is is optional but
		# all defined and stabalized.
		$bref = $XMLREF->{SITE};
		foreach my $key (keys %{$XMLREF->{SITE}} ){
		    if ($key eq $site || $site eq ""){
			&print_debug("_ReadConfig","XML :: Parsing for SITE=$key (agree with intent=$FC::INTENT)");
			@servers = @{$XMLREF->{SITE}->{$key}->{SERVER}};
			last;
		    }
		}
	    }


	    # Several servers will appear in an array of hashes
	    if ($#servers != -1){
		foreach my $key2 ( @servers ){
		    if ( $key2->{SCOPE} eq $FC::INTENT ){
			&print_debug("_ReadConfig","XML :: Found entry for intent=$FC::INTENT (as requested)");
			my (@hosts) = @{$key2->{HOST}};

			if ($#hosts != -1){
			    my($ii,$dbref);
			    for($ii=0 ; $ii <= $#hosts ; $ii++){
				&print_debug("_ReadConfig","\t\tHost   = ".$hosts[$ii]->{NAME});
				&print_debug("_ReadConfig","\t\tDbname = ".$hosts[$ii]->{DBNAME});
				&print_debug("_ReadConfig","\t\tPort   = ".$hosts[$ii]->{PORT});
				if ( ! defined($EL{HOST}) ){
				    $EL{HOST}  =  $hosts[$#hosts]->{NAME};
				    $EL{DB}    =  $hosts[$#hosts]->{DBNAME};
				    $EL{PORT}  =  $hosts[$#hosts]->{PORT};
				    $EL{DBREF} =  "";
				}


				# And one authentication per host
				my (@auths) = @{$hosts[$ii]->{ACCESS}};
				if ($#auths != -1){
				    my($jj);
				    if ( ! defined($EL{PASS}) ){
					$EL{PASS}  =     $auths[$#auths]->{PASS};
					$EL{USER}  =     $auths[$#auths]->{USER};
				    }
				    for($jj=0 ; $jj <= $#auths ; $jj++){
					$EL{DBREF} .= join(":",
							   $hosts[$ii]->{DBNAME},
							   $hosts[$ii]->{NAME},
							   $hosts[$ii]->{PORT}).
							       ",".$auths[$jj]->{USER}.
							       ",".$auths[$jj]->{PASS}." ";

				    }
				} else {
				    # A simple format may be valid
				    $EL{DBREF} .= join(":",
						      $hosts[$ii]->{DBNAME},
						      $hosts[$ii]->{NAME},
						      $hosts[$ii]->{PORT}).",, ";
				}
			    }
			}
			last;
		    }
		}
	    } else {
		&print_message("_ReadConfig","XML $config seems invalid ... No SERVER entity found");
	    }


	} else {
	    #
	    # This home made parser may collapse at any point in time
	    # and will not be supported / extended.
	    # You MUST install XML::Simple
	    #
	    &print_debug("_ReadConfig","XML :: WARNING -- Parsing by hand");
	    $EL{DBREF} = "";
	    my ($site,$lintent);
	    if ($intent =~ /::/){
		($site,$lintent) = split("::",$intent);
	    } else {
		$site    = (defined($DSITE)?$DSITE:"");
		$lintent = $intent;
	    }
	    $FC::INTENT = $lintent;

	    if ( ! defined($flag) ){
		&print_debug("_ReadConfig","Searching for $FC::INTENT in $config");
	    }
	    open(FI,$config);

	    #
	    # This is low-key XML parsing. Already a better parsing
	    # would be to regexp =~ s/blablafound// to allow one
	    # line. Better even to use XML::Parser but this module
	    # is meant to be as indenpendant as possible from extraneous
	    # perl layers. We skip entireley the header ...
	    #
	    while( defined($line = <FI>) ){
		chomp($line);

		if ($line =~ /(\<SCATALOG)(.*\>)/i){
		    # does not parse SITE for now but should be done
		    $rest = $2;

		} elsif ($line =~ /(\<SERVER)(.*\>)/i ){
		    # SERVER was found, the syntax may be OK
		    $scope = $2;
		    $scope =~ m/(.*\")(.*)(\".*)/;
		    $scope = $2;

		    # the intent flag is not yet set
		    &print_debug("_ReadConfig","XML :: Comparing scopes found=[$scope] intent=[$FC::INTENT]");
		    if ($scope =~ m/$FC::INTENT/){
			&print_debug("_ReadConfig","XML :: scope matches intent (mask ON)");
			$ok = 1;
		    } else {
			&print_debug("_ReadConfig","XML :: scope does NOT match intent (masking out everything)");
			$ok = 0;
		    }
		} elsif ( $line =~ /(\<\/SERVER)(.*\>)/i){
		    $ok = 0;
		} elsif ( $line =~ /(\<\/SCATALOG)(.*\>)/i){
		    $ok = 0;
		} else {
		    # i.e. if syntax is correct and intent is allright

		    # Parsing of the block of interrest
		    # Host specific information. Note that we do not
		    # assemble things as a tree so, one value possible
		    # so far ... and the latest/
		    if ($line =~ /\<HOST/i && $ok){
			&print_debug("_ReadConfig","XML :: $line");
			if ( $line=~ m/(NAME=)(.*)(DBNAME=)(.* )(.*)/){
			    $EL{HOST} = $2;
			    $EL{DB}   = $4;
			    $EL{PORT} = $5;
			    if ($EL{PORT} =~ m/(PORT=)(.*)/){
				$EL{PORT} = $2;
			    }
			}
		    }
		    if ( $line =~ m/\<ACCESS/ && $ok){
			if ( $line =~ m/(USER=)(.*)(PASS=)(.*)/ ){
			    $EL{USER} = $2;
			    $EL{PASS} = $4;
			}
		    }
		}



	    }
	    close(FI);
	    foreach $ok (keys %EL){
		$EL{$ok} =~ s/[\"\/\>]//g;
		$EL{$ok} =~ s/^(.*?)\s*$/$1/;
		&print_debug("_ReadConfig","XML :: Got $ok [$EL{$ok}]\n");
		if ($EL{$ok} eq ""){ $EL{$ok} = undef;}
	    }
	    $EL{DBREF} = join(":",$EL{DB},$EL{HOST},$EL{PORT}).",".$EL{USER}.",".$EL{PASS}." ";
	}
    }
    # &print_debug("_ReadConfig","XML :: Host=$EL{HOST} Db=$EL{DB} Port=$EL{PORT}");
    # &print_debug("_ReadConfig","XML :: User=$EL{USER} Pass=$EL{PASS} Site=$EL{site}");
    # print "XML :: Host=$EL{HOST} Db=$EL{DB} Port=$EL{PORT}\n";
    # print "XML :: User=$EL{USER} Pass=$EL{PASS} Site=$EL{site}\n";
    chop($EL{DBREF}) if ( defined($EL{DBREF}));

    return ($EL{DBREF},$EL{HOST},$EL{DB},$EL{PORT},$EL{USER},$EL{PASS},$EL{site});
}



#=================================================
# This routine has been added later and interfaces
# with the XML description.

sub get_connection
{
    if ($_[0] =~ m/FileCatalog/) {   shift(@_);}

    my($intent)=@_;
    my($dbr,$host,$db,$port,$user,$passwd) = &_ReadConfig($intent,1);
    # print "Will return $user,$passwd,$port,$host,$db\n";
    return ($user,$passwd,$port,$host,$db);
}
sub get_Connection
{
    if ($_[0] =~ m/FileCatalog/) {   shift(@_);}

    my($intent)=@_;
    my($dbr) = &_ReadConfig($intent,1);
    return $dbr;
}

# Connect as 'intent' or specific incompletely specified
# reference.
sub connect_as
{
    if ($_[0] =~ m/FileCatalog/) {   shift(@_);}

    my($intent,$user,$passwd,$port,$host,$db)= @_;
    my($Ldbr,$Lhost,$Ldb,$Lport,$Luser,$Lpasswd,$Lsite);

    if ( ! defined($user)   &&
	 ! defined($passwd) &&
	 ! defined($port)   &&
	 ! defined($host)   &&
	 ! defined($db) ){
	# New style, intent is sufficient for returning a selection
	# of connections.
	($Ldbr) = &_ReadConfig($intent);
        # print "connect_as() $Ldbr\n";
	return &_Connect("FileCatalog",$Ldbr);
    } else {
	# Try again to read the missing stuff from XML if any
	($Ldbr,$Lhost,$Ldb,$Lport,$Luser,$Lpasswd) = &_ReadConfig($intent);
	# print "connect_as() we now got $Ldbr,$Lhost,$Ldb,$Lport,$Luser,$Lpasswd\n";
	if ( ! defined($user) ){    $user   = $Luser;}
	if ( ! defined($passwd) ){  $passwd = $Lpasswd;}
	if ( ! defined($port) ){    $port   = $Lport;}
	if ( ! defined($host) ){    $host   = $Lhost;}
	if ( ! defined($db) ){      $db     = $Ldb;}

	return &connect("FileCatalog",$user,$passwd,$port,$host,$db);
    }
}


# Maintained for backward compat or specific account/server
# connection. Beware that the order are different.
sub connect
{
    if ($_[0] =~ m/FileCatalog/) {   shift(@_);}
    my($user,$passwd,$port,$host,$db)=@_;
    if (!defined($user)){   $user   = $dbuser;}
    if (!defined($passwd)){ $passwd = $dbpass;}
    if (!defined($port)){   $port   = $dbport;}
    if (!defined($host)){   $host   = $dbhost;}
    if (!defined($db)){     $db     = $dbname;}

    # print "connect() called $db,$host,$port,$user,$passwd\n";
    return &_Connect("FileCatalog",join(":",$db,$host,$port).",$user,$passwd");
}


sub _Connect
{
    if ($_[0] =~ m/FileCatalog/) {   shift(@_);}

    # my ($user,$passwd,$port,$host,$db) = @_;
    my ($dbr)=@_;
    my ($user,$passwd,$host);
    my ($sth,$count);
    my ($tries,$rtries,$idx);

    # Build connect
    my(@Dbr)= split(" ",$dbr);
    my(@Dbrr);

    # Now randomize order
    #print "BEFORE $#Dbr [".join(" ",@Dbr)."]\n";
    for($tries=$#Dbr ; $tries >= 0 ; $tries--){
	$idx = splice(@Dbr,int(rand($#Dbr+1)),1);
	push(@Dbrr,$idx);
    }
    #print "AFTER  $#Dbrr [".join(" ",@Dbrr)."]\n";
    @Dbr = @Dbrr;


    # Make it more permissive. Simultaneous connections
    # may make this fail.
    $idx = $rtries = $tries = 0;
  CONNECT_TRY:
    $tries++; $rtries++;
    $idx++;
    if ($idx > $#Dbr){ $idx = 0;}

    &print_debug("_Connect","Trying connection $idx / $#Dbr");
    ($FC::DBRef,$user,$passwd)  =  split(",",$Dbr[$idx]);
    if ( $FC::DBRef =~ m/\.$/){ chop($FC::DBRef);}
    $host  = (split(":",$FC::DBRef))[1];
    $FC::DBRef = "DBI:mysql:$FC::DBRef";


    &print_debug("_Connect","$FC::DBRef,$user (+passwd)");
    $FC::DBH = DBI->connect($FC::DBRef,$user,$passwd,
	                     { PrintError => 0,
			       RaiseError => 0, AutoCommit => 1 }
                           );

    if (! $FC::DBH ){
	if ($DBI::err == 1045){
	    &print_message("_Connect","Incorrect password ".($passwd eq ""?"(NULL)":""));
	}
	if ($DBI::err == 2002){
	    &print_message("_Connect","Socket is invalid for [$FC::DBRef]",
		           ($host eq ""?"Host was unspecified (too old library version ??)":""));
	}
	if ( $tries < $NCTRY ){
	    &print_debug("_Connect","Connection failed $DBI::err $DBI::errstr . Retry in $NCSLP secondes");
	    sleep($NCSLP);
	    goto CONNECT_TRY;
	} else {
	    &print_message("_Connect","cannot connect to $dbname : $DBI::errstr");
	    return 0;
	}
    }

    # this feature will be enabled for MySQL
    #$FC::OPTIMIZE = ($FC::DBRef=~m/mysql/);
    $FC::OPTMIZE = 0;
    &print_debug("_Connect","Optimization is ".($FC::OPTIMIZE?"ON":"OFF"));

    # check if caching is enabled - unless query_cache_size is not null, there is
    # no hope
    $FC::FORCESQLCACHE= 1==0;
    $FC::HASSQLCACHE  = 1==0;
    {
    	my($sth) = $FC::DBH->prepare("SHOW VARIABLES LIKE '%query_cache%'");
    	my(@val);
    	if ( $sth->execute() ){
    	    while ( @val = $sth->fetchrow_array() ){
    		if ( $val[0] =~ /have_query_cache/ ){  $FC::HASSQLCACHE  = ($val[1] =~ /yes/i); }
    		if ( $val[0] =~ /query_cache_size/ ){  $FC::HASSQLCACHE &= ($val[1]!=0);        }
    	    }
    	}
    	$sth->finish();
    }
    &print_debug("_Connect","SQL Caching is ".($FC::HASSQLCACHE?"ON":"OFF"));


    # check number of SELECT but only if Admin
    if ($FC::INTENT =~ m/Admin/i){
    	my($sth) = $FC::DBH->prepare("SHOW PROCESSLIST");
	&print_debug("_Connect","Additional information");
 	my(@val,@pid);
	my($sel,$ins,$delr)=(0,0,0);
	my($cond)=0;
	my(@fr,$ii);

    	if ( $sth->execute() ){
	    while ( @val = $sth->fetchrow_array() ){
		if ( defined($val[7]) ){  # some commands are NULL when connections are established
		    &print_debug("_Connect","Running >> ".$val[7]);
		    if ($val[7] =~ /SELECT/){
			$sel++;
			push(@pid,$val[0]);
		    } elsif ($val[7] =~ /INSERT/){
			$ins++;
		    } elsif ($val[7] =~ /DELETE/){
			$delr++;
		    }
		}
	    }
	}
	$sth->finish();
	&print_debug("_Connect","SELECT=$sel INSERT=$ins DELETE=$delr");
	&print_debug("_Connect","\t".($#pid==-1?"Connections OK - cleare to proceed":"kill ".join("; kill ",@pid)));

	# be fair - the longer we wait, more likely we will go through
	for ($ii=0 ; $ii <= $#FC::LOADMANAGE ; $ii++){
	    if ( $FC::LOADMANAGE[$ii] != 0){
		$fr[$ii] = int( 0.5 + log($rtries)/($FC::LOADMANAGE[$ii]**0.1) );
	    } else {
		$fr[$ii] = 0;
	    }
	}

	if (      $cond = ($sel  > ($FC::LOADMANAGE[0]+$fr[0]) && $FC::LOADMANAGE[0] > 0) ){
	    &print_message("_Connect","SELECT=$sel INSERT=$ins DELETE=$delr - SELECT greater than threshold ".
		                      sprintf("%3.3d [%3.3d]",($FC::LOADMANAGE[0]+$fr[0]),$rtries)." ".localtime() );
	} elsif ( $cond = ($ins  > ($FC::LOADMANAGE[1]+$fr[1]) && $FC::LOADMANAGE[1] > 0) ){
	    &print_message("_Connect","SELECT=$sel INSERT=$ins DELETE=$delr - INSERT greater than threshold ".
		                      sprintf("%3.3d [%3.3d]",($FC::LOADMANAGE[1]+$fr[1]),$rtries)." ".localtime() );
	} elsif ( $cond = ($delr > ($FC::LOADMANAGE[2]+$fr[2]) && $FC::LOADMANAGE[2] > 0) ){
	    &print_message("_Connect","SELECT=$sel INSERT=$ins DELETE=$delr - DELETE greater than threshold ".
		                      sprintf("%3.3d [%3.3d]",($FC::LOADMANAGE[2]+$fr[2]),$rtries)." ".localtime() );
	}
	if ($cond){
	    &destroy();
	    # if ( $tries < $NCTRY-1){
	        $tries--;  # try infinitely
	        &print_debug("_Connect","Will sleep for ".($NCSLP*3)." seconds and retry");
		&destroy();
	        sleep($NCSLP*3);
		goto CONNECT_TRY;
	    # } else {
	    #	&print_message("_Connect","No luck - please try later");
	    #	return 0;
	    # }
	}
    }


    # Set/Unset global variables here
    $FC::IDX = -1;

    if ( ! defined($FC::DBH) ) {
	return 0;
    } else {
	# get a list of tables
	my($tab,$sth);
	foreach $tab (("FileLocations","FileData")){
	    $FC::SPLIT_MIN{$tab} = 0;
	    $FC::SPLIT_MAX{$tab} = 0;

	    $sth = $FC::DBH->prepare("SHOW TABLES LIKE '$tab%'");
	    if ( $sth->execute() ){
		my($val);
		while ( defined($val = $sth->fetchrow()) ){
		    if ( $val =~ m/($tab)(_)(\d+)/){
			if ( $FC::SPLIT_MAX{$tab} < $3){ $FC::SPLIT_MAX{$tab} = $3;}
			if ( $FC::SPLIT_MIN{$tab} > $3){ $FC::SPLIT_MIN{$tab} = $3;}
			&print_debug("_Connect","$tab is splitted -> $val");
		    }
		}
	    }
	    # this will make the test &_TypeSplitted() return FALSE for non-split
	    # tables i.e. $st > 0 && $st < 0 == FALSE.
	    if ($FC::SPLIT_MAX{$tab} != 0){
		$FC::SPLIT_MAX{$tab}++;
		&print_debug("_Connect","\tInterval $FC::SPLIT_MIN{$tab} to $FC::SPLIT_MAX{$tab}");
	    } else {
		&print_debug("_Connect","Analysis of $tab showed no split");
	    }
	    $sth->finish();
	}
	return 1;
    }
}


#
# Sets the duplicate warning flag
#
sub warn_if_duplicates
{
    my($val)=@_;

    if ( defined($val) ){
	$FC::WDUPS = ($val==1);
    } else {
	$FC::WDUPS = ! 	$FC::WDUPS;
    }
}

#
# Sets thresholds
#
sub set_thresholds
{
   if ($_[0] =~ m/FileCatalog/) {   shift(@_);}
   my($s,$i,$d)=@_;

   if ( defined($s) ){  $FC::LOADMANAGE[0] = $s;}
   if ( defined($i) ){  $FC::LOADMANAGE[1] = $i;}
   if ( defined($d) ){  $FC::LOADMANAGE[2] = $d;}
}



# dangerous external db handler manipulation perspectives
# for experts only
sub get_dbh
{
    if ($FC::DBH){ return $FC::DBH;}
    else     { return 0;}
}

#============================================
# disentangle keyowrd, operator and value from a context string
# Params:
# the context string
# Returns:
# keyword - the keyword used
# operator - the operator used
# value - the value assigned to the keyword
sub disentangle_param
{
    if ($_[0] =~ m/FileCatalog/) {   shift(@_);}

    my ($params) = @_;
    my $keyword;
    my $operator;
    my $value;

  OPS:
    foreach my $op (@operators ){
	#&print_debug("disentangle_param","Searching for operator $op");
	$op = '\]\[' if ($op eq "][");  # unfortunatly need
	$op = '\[\]' if ($op eq "[]");  # to be escaped

	if ($params =~ m/(.*)($op)(.*)/){
	    ($keyword, $operator, $value) = ($1,$2,$3);
	    last if (defined $keyword and defined $value);
	    $operator = "";
	}
    }

    if ($FC::DEBUG > 0 && defined($keyword) ) {
	&print_debug("disentangle_param",
		     " Keyword: |".$keyword."|",
		     " Value: |".$value."|");
    }
    return ($keyword, $operator, $value);
}

#============================================
# Set the context variable
# Params:
# context string in the form of:
# <context variable> <operator> <value>
sub set_optional_context { &_context(1,@_);}
sub set_context {          &_context(0,@_);}


sub _context {

  # Private routine now passing an extraneous argument
  my $mode = shift(@_);

  # ... but still need to shift (because it serves as a pass-through
  # interface for 2 routines.
  if ($_[0] =~ m/FileCatalog/) {   shift(@_);}



  my $params;
  my $keyw;
  my $oper;
  my $valu;

  foreach $params (@_){
       # print ("Setting context for: $params \n");
      push(@CTXMEM,$params);
      ($keyw, $oper, $valu) = &disentangle_param($params);

      if ( ! defined($keyw) ){
	  &print_message("_context","Sorry, but I don't understand ".
		       "[$params] in your query.");
	  &die_message("_context","May be a missing operator ?");
      }

      #&print_debug("_context","$keyw $oper $valu");

      # Chop spaces from the key name and value;
      $keyw =~ y/ //d;
      if ($valu =~ m/.*[\"\'].*[\"\'].*/) {
	  $valu =~ s/.*[\"\'](.*)[\"\'].*/$1/;
      } else {
	  $valu =~ s/ //g;
      }

      if ( exists $keywrds{$keyw}) {
	  if ($FC::DEBUG > 0) {
	      &print_debug("_context","Query accepted $FC::DEBUG: $keyw = $valu ($mode)");
	  }
	  if ($mode == 1){
	      $optoperset{$keyw} = $oper;
	      $optvaluset{$keyw} = $valu;
	  } else {
	      $operset{$keyw}    = $oper;
	      $valuset{$keyw}    = $valu;
	  }
      } else {
	  if ( defined($obsolete{$keyw}) ){
	      &die_message("_context",
			   "[$keyw] is obsolete. Use $obsolete{$keyw} instead\n");
	  } else {
	      my (@kwd);
	      @kwd = &get_keyword_list();
	      &die_message("_context",
			   "[$keyw] IS NOT a valid keyword. Choose one of\n".
			   join(" ",@kwd));
	  }
      }
  }
}

#============================================
# Clears the context deleting all the values
# form the context hashes
sub clear_context {
    my($key,@el);

    undef(@CTXMEM);
    foreach  $key (keys %optvaluset) { delete $optvaluset{$key};}
    foreach  $key (keys %optoperset) { delete $optoperset{$key};}

    foreach  $key (keys %valuset) {    delete $valuset{$key};}
    foreach  $key (keys %operset) {    delete $operset{$key};}

    # Only delete this global array if size is bigger than some
    # value
    @el = keys %GUPDID;
    if ( $#el > 1000){
	foreach  $key (keys %GUPDID)  {    delete $GUPDID{$key};}
    }
}

#============================================
# Get an ID for a record with a given name
# from a dictionary table
# NOTE: Field values in dictionary tables are CaSe insensitive
# Params:
#   table name
#   field name
#   field value
# Returns:
#   ID of record form a database or 0 if there is no such record
sub get_id_from_dictionary {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  }
  if( ! defined($FC::DBH)){
      &print_message("get_id_from_dictionary","Not connected/connecting");
      return 0;
  }

  my @params = @_;
  my $idname = $params[0];
  my $sth;
  my $id;
  my $idx;
  my $sqlquery;

  $idname = &_IDize("get_id_from_dictionary",$idname);
  $id     = 0;
  $idx    = uc($params[1])."_".uc($params[2]);

  ## use cache
  if ( ( $id = &_CachedValue($params[0],$idx) ) == 0){
      #
      # Dictionnary are intrinsically not case sensitive which may
      # be an issue for accessing Path information.
      #
      $sqlquery = &_SQLCACHED_SELECT()."$idname FROM $params[0] WHERE UPPER($params[1]) = UPPER(\"$params[2]\")";
      if ($FC::DEBUG > 0) {  &print_debug("get_id_from_dictionary","Executing: $sqlquery");}
      $sth = $FC::DBH->prepare($sqlquery);

      if( ! $sth){
	  &print_debug("get_id_from_dictionary","Failed to prepare [$sqlquery]");
      } else {
	  my( $val );

	  if ( $sth->execute() ){
	      $sth->bind_columns( \$val );

	      if ( $sth->fetch() ) {
		  $id = $val;
	      }
	  }
	  $sth->finish();
      }

      ## save in cache
      &_SaveValue($params[0],$idx,$id);
  }
  return $id;
}


# get val for consistency (both can be merged with a private routine)
sub get_value_from_dictionary {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  }
  if( ! defined($FC::DBH)){
      &print_message("get_id_from_dictionary","Not connected/connecting");
      return 0;
  }

  my @params  = @_;
  my $valname = $params[0];
  my $sth;
  my $id;
  my $idx;
  my $sqlquery;

  $valname  = &_Nameize("get_value_from_dictionary",$valname);
  $id       = 0;
  $idx      = uc($params[1])." ".$params[2];

  if ( ($id = &_CachedValue($params[0],$idx)) == 0 ){
      $sqlquery = &_SQLCACHED_SELECT()."$valname FROM $params[0] WHERE UPPER($params[1]) = $params[2]";
      if ($FC::DEBUG > 0) {  &print_debug("get_value_from_dictionary","Executing: $sqlquery");}
      $sth = $FC::DBH->prepare($sqlquery);

      if( ! $sth){
	  &print_debug("get_value_from_dictionary","Failed to prepare [$sqlquery]");
      } else {
	  my( $val );

	  if ( $sth->execute() ){
	      $sth->bind_columns( \$val );

	      if ( $sth->fetch() ) {
		  $id = $val;
	      }
	  }
	  $sth->finish();
      }
      ## save in cache
      &_SaveValue($params[0],$idx,$id);
  }
  return $id;
}


#
# Used several places so, made a few utility routine
#
sub _IDize {      return &_GlobIzer("ID",@_);   }
sub _Nameize {    return &_GlobIzer("Name",@_); }
sub _Countize {   return &_GlobIzer("Count",@_); }
sub _Creatorize { return &_GlobIzer("Creator",@_); }
sub _IDatize  {   return &_GlobIzer("IDate",@_); }

#
# Used several places so, made a utility routine
#
sub _GlobIzer
{
    my($postfix,$fac,$idname)=@_;
    my($tmp)=$idname;

    #chop($idname);
    #if ( substr($idname,length($idname),1) eq "s"){ chop($idname);}
    #$tmp =~ s/s$//; if ( $idname ne $tmp){ print "Got something different $idname $fac\n";}

    # if table are splitted, they will be post-fixed with _\d+
    if ($idname =~ m/(.*)(_\d+$)/ ){ $idname = $1;}

    # remove trailing "s" from table name, capitalize
    # postfix and we are done.
    $idname =~ s/s$//;

    $idname = lcfirst($idname);
    $idname.= $postfix;
    #print "$postfix,$fac,$idname - > $idname\n";
    $idname;
}


# This was added to handle backward compatible path
# scheme
#sub _Path_name_and_ID {  return &_name_and_ID("rfpid","path","Xpath");}
#sub _Host_name_and_ID {  return &_name_and_ID("rhid","node","Xnode");}

sub _name_and_ID
{
    my ($idx,$okwd,$nkwd)=@_;
    my ($val,$valID);

    if ( ! defined($valuset{$idx}) ){ $valuset{$idx} = 0;}

    if ( defined($valuset{$okwd}) && ! defined($valuset{$nkwd}) ){
	# transfer old keyword -> new keyword
	$val  = $valuset{$nkwd} = $valuset{$okwd};

    } elsif ( defined($valuset{$nkwd}) ){
	# transfer new -> old
	$val  = $valuset{$okwd} = $valuset{$nkwd};

    } elsif ( $valuset{$idx} != 0 ){
	# clone mode, possibly a non-zero rfpid, we need to recover the name
	my ($fieldname, $tabname, $rest) = split(",",$keywrds{$nkwd});
	$valID  = $valuset{$idx};
	$val    = &get_value_from_dictionary($tabname, $fieldname, $valID);

    } else {
	# no known method to recover the information
	&die_message("_Path_name_and_ID", "None of $nkwd / $okwd / $idx defined");
    }


    # any mode has set $nkwd
    $valID       = &check_ID_for_params($nkwd);

    #print "### --> $val $valID\n";

    return ('"'.$val.'"',$valID);
}



#============================================
# Check if there is a record with a given value
# in a corresponding dictionary table
# Parameters:
#    0 the keyword to check for
#    1 a flag indicating if the field has to be known - only used for printing
#    2 a forced value / bypass without having to set a valuset
#
# Returns:
#    The ID value for a given keyword value
#    or 0 if no such record exists
#
sub check_ID_for_params
{

    if ($_[0] =~ m/FileCatalog/) {  shift @_;}

    my ($params, $requested, $vval) = @_;
    my $retid;

    # forced a message to be displayed
    if ( ! defined($requested) ){ $requested = 1;}

    # allow forced value
    if ( ! defined($vval) ){  $vval =  $valuset{$params};}

    if ( defined($vval) ) {
	my $fieldname;
	my $tabname;
	my $rest;

	($fieldname, $tabname, $rest) = split(",",$keywrds{$params});
	$retid = &get_id_from_dictionary($tabname, $fieldname, $vval );
	if ($retid == 0) {
	    # *** THIS NEEDS TO BE LATER FIXED
	    if ( $FC::ISDICT{$tabname} ) {
		&print_debug("check_ID_for_params",
			     "Since $tabname is a dict, we will auto-insert");
		$retid = &insert_dictionary_value($params);

	    } else {
		&print_debug("check_ID_for_params",
			     "Returning 0 since ($tabname not a dict) there are no $params with value ".
			     $vval);
		$retid = 0;
	    }

	}
    } else {
	&print_debug("check_ID_for_params","No $params defined");
	&print_message("check_ID","$params is required but no value defined") if ($requested);
	$retid = 0;
    }
    &print_debug("check_ID_for_params","Returning $params --> $retid");

    return $retid;
}

#============================================
# inserts a value into a table corresponding to a given keyword
# Parameters:
#    A keyword to use
# Returns:
#    the ID of an inserted value
#    or 0 if such insertion was not possible
sub insert_dictionary_value {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  }
  if( ! defined($FC::DBH) ){
      &print_message("insert_dictionary_value","Not connected");
      return 0;
  }

  my ($keyname) = @_;
  my @additional;
  my $del_onexit=0;

  if (! defined $valuset{$keyname}) {
      if ( $keyname eq "creator"){
	  # This is a special keyword which may NOT be populated
	  # but need to be initialized"on demand"
	  $valuset{$keyname} = &_GetLogin();
	  $del_onexit = 1;
      } else {
	  if ($FC::DEBUG > 0) {
	      &print_debug("insert_dictionary_value",
			   "ERROR: No value for keyword $keyname.",
			   "Cannot add record to dictionary table.");
	  }
	  return 0;
      }
  }

  # Check if there are other fields from this table set
  foreach my $kwrd (keys(%keywrds)) {
    my ($fieldnameo, $tabnameo, $resto) = split(",",$keywrds{$keyname});
    my ($fieldnamet, $tabnamet, $restt) = split(",",$keywrds{$kwrd});

    if ($tabnameo eq $tabnamet && $keyname ne $kwrd) {
      if ($FC::DEBUG > 0) {
	  &print_debug("insert_dictionary_value",
		       "The field $fieldnamet $tabnamet is from the same table as $fieldnameo $tabnameo");
      }
      if (defined $valuset{$kwrd}) {
	  push @additional, ($kwrd);
      }
    }
  }

  my ($fieldname, $tabname, $rest) = split(",",$keywrds{$keyname});
  my $dtfields = "";
  my $dtvalues = "";
  my $dtinsert;

  foreach my $kwrd (@additional) {
    my ($fieldnamea, $tabnamea, $resta) = split(",",$keywrds{$kwrd});

    # Skip special keywords
    if ($fieldnamea !~ /Count/   &&
	$fieldnamea !~ /Creator/ &&
	$fieldnamea !~ /IDate/   ){
	$dtfields .= " , $fieldnamea";
	$dtvalues .= " , '".$valuset{$kwrd}."'";
    }
  }

  if ( $tabname eq "FileLocations"){
      $dtinsert   = "INSERT DELAYED IGNORE INTO $tabname ";
  } else {
      $dtinsert   = "INSERT IGNORE INTO $tabname ";
  }

  # If dictionary
  my ($dtxfields,$dtxvalues);
  if ( $FC::ISDICT{$tabname} ){
      &print_debug("insert_dictionary_value",
		   "Calling _IDatize($tabname) and  _Creatorize($tabname)");
      $dtxfields = " , ".&_IDatize("",$tabname);
      $dtxvalues = " , NOW()+0";

      # the addition of a bootstraping dictionary itself named Creator
      # requires an additional check
      if ($tabname !~ /Creator/){
	  $dtxfields .= ",  ".&_Creatorize("",$tabname);
	  $dtxvalues .= ", '".&_GetILogin()."'";
      }
  }

  $dtinsert  .= "($fieldname $dtfields $dtxfields)";
  $dtinsert  .= " VALUES('".$valuset{$keyname}."' $dtvalues $dtxvalues)";
  &print_debug("insert_dictionary_value","Execute $dtinsert");


  my $sth;
  my $retid=0;

  $sth = $FC::DBH->prepare( $dtinsert );
  if( ! $sth ){
      &print_debug("insert_dictionary_value","Failed to prepare [$dtinsert]");
  } else {
      if ( $sth->execute() ) {
	  $retid = &get_last_id();
	  if ($FC::DEBUG > 0) { &print_debug("insert_dictionary_value","Returning: $retid");}
      } elsif ( $FC::DBH->err == 1054) {
	  # wrong field name
	  &die_message("insert_dictionary_value","logic error for $tabname ".$FC::DBH->err." >> ".$FC::DBH->errstr);
      } else {
	  &print_debug("insert_dictionary_value","ERROR for $tabname ".$FC::DBH->err." >> ".$FC::DBH->errstr);
      }
      $sth->finish();
  }
  if ($del_onexit){ $valuset{$keyname} = undef;}

  return $retid;
}

sub get_prodlib_version_ID {

    if( ! $FC::DBH){
	&print_message("insert_detector_configuration","Not connected");
	return 0;
    }

    if ( defined($valuset{"library"}) &&   defined($valuset{"production"})){
	# Both are defined, we can check if the combo is in the table
	# and insert if necessary.
	my($id,$cmd1,$sth1);
	my($fldnm1, $fldnm2, $tabname, $rest);
	my($prod,$lib);

	($prod,$lib)               = ($valuset{"production"},$valuset{"library"});
	($fldnm1, $tabname, $rest) = split(",",$keywrds{"production"});
	($fldnm2, $tabname, $rest) = split(",",$keywrds{"library"});

	# trim leading/trailing
	$prod =~ s/^(.*?)\s*$/$1/;
	$lib  =~ s/^(.*?)\s*$/$1/;


	# fetch if exists
	$cmd1 = &_SQLCACHED_SELECT().&_IDize("get_prolib_version_ID",$tabname)." FROM $tabname WHERE $fldnm1=? AND $fldnm2=?";
	#print "$cmd1\n";
	$sth1 = $FC::DBH->prepare($cmd1);
	if ( ! $sth1 ){
	    &die_message("get_prodlib_version_ID","Prepare statements 1 failed");
	} else {
	    if ( $sth1->execute($prod,$lib) ){
		$sth1->bind_columns( \$id );
		if ($sth1->rows == 0) {
		    # then insert
		    my($cmd2,$sth2);

		    $cmd2  = "INSERT IGNORE INTO $tabname ($fldnm1, $fldnm2, ".&_IDatize("",$tabname).", ".&_Creatorize("",$tabname).") ";
		    $cmd2 .= "VALUES('".$prod."', '".$lib." ', NOW()+0, ".&_GetILogin().")";
		    #print "$cmd2\n";
		    $sth2  = $FC::DBH->prepare($cmd2);
		    if ( ! $sth2 ){
			&die_message("get_prodlib_version_ID","Cannot prepare [$cmd2] ".$FC::DBH->errstr);
		    } else {
			if ( $sth2->execute() ){
			    $id = &get_last_id();
			    if ( $id == 0){
				&print_message("get_prodlib_version_ID","Last inserted ID was 0, this is a problem");
			    } else {
				&print_debug("get_prodlib_version_ID","Inserted $prod,$lib as id=$id");
			    }
			} else {
			    &die_message("get_prodlib_version_ID","Failed to insert $prod,$lib ".$FC::DBH->errstr);
			}
			$sth2->finish();
		    }
		} elsif ($sth1->rows > 1) {
		    $sth1->finish();
		    &die_message("get_prodlib_version_ID","Self consistency check failed",
				 "More than one combination production/library found");
		} else {
		    # Fecth the unique value (it is bound to the $id)
		    $sth1->fetch();
		}
		$sth1->finish();
		return $id;
	    }
	    # can we reach here?
	    $sth1->finish();
	}

    } elsif ( defined($valuset{"library"})  ){
	# One parameter exists. Treat it as a dictionnary (may be null)
	return &check_ID_for_params("library");
    } elsif ( defined($valuset{"production"})  ){
	# One parameter exists. Treat it as a dictionnary (may be null)
	return &check_ID_for_params("production");
    } else {
	&die_message("get_prodlib_version_ID","None of production/library defined");
    }
}


#============================================
# get the ID for the current run number
# Returns:
#   the ID of a runParams record,
#   or 0 if no such record exists
sub get_current_detector_configuration {

  my ($detConfiguration,$cmd,$sth,$val);
  my ($tabname) = "DetectorConfigurations";
  my ($field)   = "detectorConfigurationName";
  my ($index)   = &_IDize("get_current_detector_configuration",$tabname);

  if( ! $FC::DBH){
      &print_message("insert_detector_configuration","Not connected");
      return 0;
  }

  # May be one or the other
  if ( defined($valuset{"geometry"}) ){
      $val = $valuset{"geometry"};
  } elsif ( defined($valuset{"configuration"}) ){
      $val = $valuset{"configuration"};
  } else {
      # Protection for caching. Similar protections have to
      # happen in the real routine insert_detector_configuration()
      &print_debug("insert_detector_configuration",
		   "ERROR: No detector configuration/geometry name given.");
      return 0;
  }

  # This routine introduces caching
  if ( ($detConfiguration = &_CachedValue($tabname,$val)) == 0){
      $cmd = &_SQLCACHED_SELECT()."$tabname.$index from $tabname WHERE $tabname.$field='$val'";

      $sth = $FC::DBH->prepare($cmd);
      if( ! $sth ){
	  &die_message("get_current_detector_configuration",
		       "Cannot prepare ddb sentence");
      } else {
	  if ( $sth->execute() ){
	      $sth->bind_columns( \$val );

	      if ( $sth->fetch() ) {
		  $detConfiguration = $val;
	      }
	  }
	  $sth->finish();
      }


      if ($detConfiguration == 0) {
	  # There is no detector configuration with this name
	  # we have to add it
	  $detConfiguration = &insert_detector_configuration();
      }
      ## save in cache
      &_SaveValue($tabname,$val,$detConfiguration);
  }
  return $detConfiguration;
}


# inserts a value into a table of Detector Configurations
# Returns:
#   The ID of an inserted value
#   or 0 if such insertion was not possible
# This routine is for INTERNAL USE ONLY AND MAY BE MERGED
# WITH THE PRECEEDING ONE.
#
sub insert_detector_configuration {

  if( ! $FC::DBH){
      &print_message("insert_detector_configuration","Not connected");
      return 0;
  }

  my ($config,$dtinsert,$dtvalues,$dtv);
  my ($sth);
  my ($el,$retid);

  if ( defined($valuset{"geometry"}) ){
      $config = $valuset{"geometry"};
  } else {
      if (! defined $valuset{"configuration"}) {
	  &print_debug("insert_detector_configuration",
		       "ERROR: No detector configuration/geometry name given.");
	  return 0;
      } else {
	  $config = $valuset{"configuration"};
      }
  }

  #
  # Try to guess the setup if unspecified
  #
  if ( defined($valuset{"configuration"}) ){
      #print "Trying to split ".$valuset{"configuration"}."\n";
      my @items = split(/\./,$valuset{"configuration"});
      foreach $el (@items){
	  # The keyword is defined in the 'configuration' but no value
	  # Prevent catastrophe and set default to 1.
	  if ( defined($keywrds{$el}) ){
	      if ( ! defined($valuset{$el}) ){
		  &print_debug("insert_detector_configuration",
			       "$el appears in configuration but not explicitly set. Auto-set");
		  $valuset{$el} = 1;
	      }
	  }
      }
  }

  # Auto-construct the full stuff based on content of @DETECTORS
  # Note that the order does not  matter, MySQL
  # does not care at all (as far as consistency dXXX and associated
  # value is consistent).
  $dtinsert = "INSERT IGNORE INTO DetectorConfigurations (detectorConfigurationName";
  $dtvalues = " VALUES('".$config."'";
  foreach $el (@DETECTORS){
      # the defined() test is NOT the same than above. It sets to 0 the
      # value of a un-specified keyword. Note also that only 1/0 are allowed
      if ( ! defined($valuset{$el}) ){
	  $dtv = 0;
      } else {
	  $dtv = $valuset{$el};
	  # prevent from being anything else than 1 or 0
	  if ( $dtv != 0){   $dtv = 1};
      }
      $dtinsert .= ", d".uc($el);
      $dtvalues .= ", ".$dtv;
  }
  $dtinsert .= ")";
  $dtvalues .= ")";

  if ($FC::DEBUG > 0) {  &print_debug("insert_detector_configuration","Execute $dtinsert$dtvalues");}

  $retid = 0;
  $sth   = $FC::DBH->prepare( $dtinsert.$dtvalues );
  if( ! $sth ){
      &print_debug("insert_detector_configuration","Failed to prepare [$dtinsert$dtvalues]");
  } else {
      if ( $sth->execute() ) {
	  $retid = &get_last_id();
      }
      $sth->finish();
  }
  return $retid;
}

sub get_detectors { return @DETECTORS;}

#
# Not equivalent to detectorConfigurations which do not
# repeat themselves.
#
#sub get_current_detector_state
#{
#    return 0;
#}


#============================================
# disentangle collision type parameters from the collsion type name
# Params:
# The collsion type
# Returns:
#   first particle name
#   second particle name
#   collision energy
sub disentangle_collision_type {
    if ($_[0] =~ m/FileCatalog/) {
	shift @_;
    }
    my ($colstring) = @_;
    my ($firstParticle,$secondParticle,$el);
    my (@particles) = ("proton", "gas", "au", "ga", "si", "p", "d", "s", "cu", "ca", "u","he3","al");

    $firstParticle = $secondParticle = "";
    $colstring     = lc($colstring);

    if (($colstring =~ m/cosmic/) > 0){
	# Special case for cosmic
	$firstParticle  = "cosmic";
	$secondParticle = "cosmic";
	$colstring = "0.0";
    } elsif (($colstring =~ m/unknown/) > 0){
	# Allow this as well
	$firstParticle  = "unknown";
	$secondParticle = "unknown";
	$colstring = "0.0";

    } else {
	# Otherwise, cut in first/second
	foreach $el (@particles){
	    if (($colstring =~ m/(^$el)(.*)/) > 0) {
		&print_debug("disentangle_collision_type","Found first particle  = $el in $colstring");
		$firstParticle = $el;
		$colstring =~ s/($el)(.*)/$2/;
		last;
	    }
	}
	foreach $el (@particles){
	    if (($colstring =~ m/(^.*)($el)/) > 0) {
		&print_debug("disentangle_collision_type","Found second particle = $el in $colstring");
		$secondParticle = $el;
		$colstring =~ s/(.*)($el)(.*)/$1$3/;
		# be sure to get the numerical value only
		$colstring =~ m/(\d+\.\d+|\d+)/;
		$colstring =  $1;
		last;
	    }
	}
    }
    return ($firstParticle, $secondParticle, $colstring);
}

#============================================
# disentangle collision type parameters from the collision type name
# Params:
# The collsion type to find
# Returns:
#   The id of a collision type in DB or 0 if there is no such collsion type
#
# METHOD IS FOR INTERNAL USE ONLY.
# A query may return several values since there is a truncation done.
#
sub get_collision_type
{
    my(@tab) = &get_collision_collection(@_);

    if( @tab ){
	return $tab[0];
    } else {
	return;
    }
}
sub get_collision_collection {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  };
  if( ! defined($FC::DBH) ){
      &print_message("get_collision_collection","Not connected/connecting");
      return 0;
  }


  my ($colstring) = @_;
  my $retid;
  my $firstParticle;
  my $secondParticle;
  my $energy;

  if( $colstring eq ""){
      &die_message("get_collision_collection","received empty argument");
  }

  $colstring = lc($colstring);

  ($firstParticle, $secondParticle, $energy) = &disentangle_collision_type($colstring);

  my $sqlquery =  &_SQLCACHED_SELECT()."collisionTypeID FROM CollisionTypes WHERE UPPER(firstParticle) = UPPER(\"$firstParticle\") AND UPPER(secondParticle) = UPPER(\"$secondParticle\")";
  if ( $energy ne "" ){
      $sqlquery .= " AND ROUND(collisionEnergy) = ROUND($energy)";
  }

  if ($FC::DEBUG > 0) {
      &print_debug("get_collision_collection",
		   "First particle : $firstParticle",
		   "Second particle: $secondParticle",
		   "Energy         : $colstring",
		   "Executing      : $sqlquery");
  }


  my @retv;
  my $id;

  my $sth = $FC::DBH->prepare($sqlquery);
  if( ! $sth){
      &print_debug("get_collision_collection","Failed to prepare [$sqlquery]");
  } else {
      if( ! $sth->execute() ){
	  &die_message("get_collision_collection","Could not execute [$sqlquery]");
      } else {
	  $sth->bind_columns( \$id );

	  while ( $sth->fetch() ) {
	      push(@retv,$id);
	  }
	  if($#retv == -1){
	      $id = &insert_collision_type();
	      if ( $id != 0){
		  &print_message("get_collision_collection","Inserting new CollisionTypes value [$colstring]");
		  push(@retv,$id);
	      }
	  }
      }
      $sth->finish();
  }

  return @retv;

}

#============================================
# insert a given collision tye into the database
# Returns:
# the id of a collision type in DB
# or 0 if the insertion was not possible
sub insert_collision_type {

  my $colstring = $valuset{"collision"};
  my $firstParticle;
  my $secondParticle;
  my $energy;

  if( ! defined($FC::DBH) ){
      &print_message("insert_collision_type","Not connected");
      return 0;
  }


  $colstring = lc($colstring);

  ($firstParticle, $secondParticle, $energy) = &disentangle_collision_type($colstring);

  if ( $firstParticle eq ""  ||
       $secondParticle eq "" ||
       $energy eq ""){

      # Don't waste time, it is wrong and not parsed
      &die_message("get_collision_type","Cannot parse [$colstring] - This is an invalid system");
      # cosmetics
      return 0;
  }


  my $ctinsert;
  $ctinsert  = "INSERT IGNORE INTO CollisionTypes ";
  $ctinsert .=
      "(firstParticle, secondParticle, collisionEnergy, ".
      " collisionTypeIDate, collisionTypeCreator)";
  $ctinsert .=
      " VALUES('$firstParticle' , '$secondParticle' , $energy, ".
      " NOW()+0, ".&_GetILogin().")";

  &print_debug("insert_collision_type","Execute $ctinsert");

  my $sth;
  my $retid=0;

  $sth = $FC::DBH->prepare( $ctinsert );
  if( ! $sth){
      &print_debug("insert_collision_type","Failed to prepare [$ctinsert]");
  } else {
      if ( $sth->execute() ) {
	  $retid = &get_last_id();
	  &print_debug("insert_collision_type","Returning: $retid");
      } else {
	  &print_debug("insert_collision_type","Failed to insert - returning $retid");
      }
      $sth->finish();
  }
  return $retid;
}

#============================================
# Get the ID of a last inserted record from the database
# Returns:
# The ID of a most recently successfully added record
sub get_last_id
{
    my $sqlquery = "SELECT LAST_INSERT_ID()";
    my $id;
    my $retv=0;

    if( ! defined($FC::DBH) ){
	&print_message("get_last_id","Not connected");
	return 0;
    }

    $sth = $FC::DBH->prepare($sqlquery);
    if( $sth ){
	if ( $sth->execute() ){
	    $sth->bind_columns( \$id );

	    if ( $sth->fetch() ) {
		$retv = $id;
	    } else {
		&print_message("get_last_id","ERROR: Cannot find the last inserted ID");
	    }
	}
	$sth->finish();
    } else {
	&print_debug("get_last_id","Arghhh !!! Cannot prepare [$sqlquery]");
    }
    return $retv;
}

#============================================
# Check for the availability of all parameters
# and execute run param info insert query
# Return:
# The id of the inserted run params
# 0 if there is insufficient data to insert run params
sub insert_run_param_info {

  my $triggerSetup;
  my $collisionType;
  my $runType;
  my $detConfiguration;
  my $detState;
  my $comment;
  my $start;
  my $end;
  my $year;
  my $day;
  my $collision;
  my $simulation;
  my $magvalue;

  if( ! defined($FC::DBH) ){
      &print_message("insert_run_param_info","Not connected");
      return 0;
  }


  $triggerSetup     = &check_ID_for_params("trgsetupname");
  $runType          = &check_ID_for_params("runtype");
  $detConfiguration = &get_current_detector_configuration();
  $detState         = 0;  # &get_current_detector_state();


  #
  # Those values may fix some side effect of closing
  #
  if (! defined $valuset{"runnumber"}) {
      if ( defined( $valuset{"rrpid"}) ){
	  # rpid is set in clone_location() mode
	  return $valuset{"rrpid"};
      } else {
	  &print_message("insert_run_param_info","runnumber not defined.");
	  return 0;
      }
  }

  #
  # Those values will be re-tested but may be auto-set at this stage
  #
  if (defined $valuset{"collision"}) {
      # only one value matters
      $collision = &get_collision_type($valuset{"collision"});
      if ($FC::DEBUG > 0) {
	  &print_debug("insert_run_param_info","insert_run_param_info","Collision      : $collision");
      }
  } else {
      &print_debug("insert_run_param_info","ERROR: collision not defined");
  }



  #
  # Now, there is nothing else we can do apart from rejecting insertion
  # if invalid. There are therefore last-resort mandatory values.
  #
  if (($triggerSetup == 0) || ($runType == 0) || ($detConfiguration == 0) || ($collision == 0)) {
      &print_message("insert_run_param_info","Missing trgsetupname, runtype or configuration",
		     "Aborting file insertion query");
      &die_message("insert_run_param_info","trgsetupname detected as missing") if ($triggerSetup == 0);
      &die_message("insert_run_param_info","runtype      detected as missing") if ($runType == 0);
      &die_message("insert_run_param_info","collision    detected as missing") if ($collision == 0);
  }
  if (! defined $valuset{"magscale"}) {
      &die_message("insert_run_param_info","magscale not defined.");
  }


  #
  # None of the above are mandatory and are subject to default values
  #
  if (! defined $valuset{"runcomments"}) {
    $comment = "NULL";
  } else {
    $comment = "\"".$comment."\"";
  }
  if (! defined $valuset{"magvalue"}) {
    $magvalue = "NULL";
  } else {
    $magvalue = $valuset{"magvalue"};
  }

  # all related to date and day
  if (! defined $valuset{"datastarts"} ) {
      $start = "NULL";
  } else {
      $start = "\"".$valuset{"datastarts"}."\"";
  }
  if (! defined $valuset{"dataends"} ) {
      $end   = "NULL";
  } else {
      $end   = "\"".$valuset{"dataends"}."\"";
  }

  # This hook would not quite work in update_record() mode
  # note that the above are autoset
  if (! defined $valuset{"daynumber"} ) {
      my($y,$d) = &get_date_numbers($valuset{"runnumber"});
      if ( defined($d) ){
	  $day = $d;
      } else {
	  $day = 0;
      }
  } else {
      $day   = $valuset{"daynumber"};
  }
  if (! defined $valuset{"year"} ) {
      # try to update anyhow based on runnumber which is
      # mandatory
      my($y,$d) = &get_date_numbers($valuset{"runnumber"});
      if ( defined($y) ){
	  $year = $y;
      } else {
	  $year = 0;
      }
  } else {
      $year   = $valuset{"year"};
  }


  if ((defined $valuset{"simulation"}) && (! ($valuset{"simulation"} eq '0') ) ) {
      &print_debug("insert_run_param_info","Adding simulation data.");
      $simulation = &get_current_simulation_params();
  } else {
      $simulation = "NULL";
  }


  my $rpinsert;

  $rpinsert   = "INSERT IGNORE INTO RunParams ";
  $rpinsert  .=
      "(runNumber, dataTakingStart, dataTakingEnd, dataTakingDay, dataTakingYear, ".
      " triggerSetupID, collisionTypeID, simulationParamsID, runTypeID, ".
      " detectorConfigurationID, detectorStateID, magFieldScale, magFieldValue, ".
      " runParamIDate, runParamCreator, runParamComment)";
  $rpinsert  .=
      " VALUES(".$valuset{"runnumber"}.", $start, $end, $day, $year, ".
      " $triggerSetup, $collision, $simulation, $runType, ".
      " $detConfiguration, $detState, '".$valuset{"magscale"}."', $magvalue, ".
      " NOW()+0, ".&_GetILogin().", $comment)";
  if ($FC::DEBUG > 0) {  &print_debug("insert_run_param_info","Execute $rpinsert");}


  my $sth;
  my $retid=0;

  $sth = $FC::DBH->prepare( $rpinsert );
  if( ! $sth ){
      &print_debug("insert_run_param_info","Failed to prepare [$rpinsert]");
  } else {
      # Insert the event counts for a given FileData
      if ( $sth->execute() ) {
	  $retid = &get_last_id();
	  if ($FC::DEBUG > 0) { &print_debug("insert_run_param_info","Returning: $retid");}
      }
      $sth->finish();
  }
  return $retid;
}


# This function is terribly STAR-specific
# Returns year, dayInYear
sub get_date_numbers
{
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}
    my($run)=@_;
    my($year,$day)=(0,0);

    if ( ! defined($run) ) { return undef;}  # undef
    if ( $run !~ m/^\d+$/) { return undef;}  # a string ?
    if ( $run =~ m/(\d+)(\d{3,})(\d{3,})/){
	$year= $1;
	$day = $2;
	$year+= 1999;
	return ($year,$day);
    } else {
	return undef;
    }
}


#============================================
# get the ID for the current run number
# Returns:
# the ID of a runParams record,
# or 0 if no such record exists
sub get_current_run_param {
  my $runNumber;

  $runNumber = &check_ID_for_params("runnumber");
  if ($runNumber == 0) {
    # There is no run with this run number
    # we have to add it
    $runNumber = &insert_run_param_info();
  }
  return $runNumber;
}

#============================================
# Execute the query to insert the file data
# Check if we have all the information
# Returns:
# The id of a lastly inserted file data
# or 0 is there is insufficient data to insert a record
#
# NOT EXPORTED until 2005/12
# EXPORTED aftreward AND TO USE WITH CARE
#
sub insert_file_data {
  my @params = @_;
  # Get the ID's of the dictionary tales
  my $production;
  my $library;
  my $fileType;
  my $runNumber;
  my $nevents;
  my $fileComment;
  my $fileSeq;
  my $filestream;
  my $md5sum;
  my @triggerWords;
  my @eventCounts;
  my @triggerIDs;
  my $count;

  if( ! defined($FC::DBH) ){
      &print_message("insert_file_data","Not connected");
      return 0;
  }

  #
  # production and library are dependent -- Logic had to change
  #
  # cloning has side effects. we must delete the content if replaced
  if ( defined($valuset{"rpcid"}) ){
      # library because later check assigns prod = lib
      if ( $library == 0 ){     $library = $valuset{"rpcid"};}
      delete($valuset{"rpcid"});
      if ($production == 0) {   $production = $library;}
  } else {
      $library    = &get_prodlib_version_ID();
      $production = $library;
  }

  # rftid for file type may be used in cloning
  if ( defined($valuset{"rftid"}) ){
      if ( $fileType == 0  ){   $fileType = $valuset{"rftid"};}
      delete($valuset{"rftid"});
  } else {
      $fileType   = &check_ID_for_params("filetype");
  }

  # We CANNOT proceed without those basic values
  if ((($production == 0 ) && ($library == 0)) || $fileType == 0){
      if ( ($production == 0 ) && ($library == 0)){
	  &print_message("insert_file_data","production/library are not defined, cannot add to FileData");
      } else {
	  &print_message("insert_file_data","filetype not defined, cannot add to FileData");
      }
      #&print_message("insert_file_data","production/library or filetype not defined, cannot add to FileData");
      return 0;
  }


  #
  # Now we can check for more mandatory parameters
  #
  # rrpid used in clone
  if ( defined($valuset{"rrpid"}) ){
      if ( $runNumber == 0  ){  $runNumber = $valuset{"rrpid"};}
      delete($valuset{"rrpid"});
  } else {
      $runNumber = &get_current_run_param();
  }
  if ($runNumber == 0) {
      &print_message("insert_file_data","runnumber not defined, cannot add to FileData");
      return 0;
  }

  if (! defined $valuset{"filename"}) {
      &print_message("insert_file_data","filename not defined, cannot add to FileData");
      return 0;
  }



  #
  # non mandatory keywords
  #
  if (! defined $valuset{"filecomment"}) {
      $fileComment = "NULL";
  } else {
      $fileComment = "\"".$valuset{"filecomment"}."\"";
  }
  if (! defined $valuset{"fileseq"}) {
      $fileSeq = "NULL";
  } else {
      $fileSeq = "\"".$valuset{"fileseq"}."\"";
  }
  if (! defined $valuset{"stream"}) {
      $filestream = 0;
  } else {
      $filestream = "\"".$valuset{"stream"}."\"";
  }
  if (! defined $valuset{"events"}) {
      $nevents = 0;
  } else {
      $nevents = $valuset{"events"};
  }
  if (! defined $valuset{"md5sum"}) {
      $md5sum = 0;
  } else {
      $md5sum = "\"".$valuset{"md5sum"}."\"";
  }

  # those fields are automatically set if unset
  #$name = "st_physics_10085114_raw_1020001.MuDst.root"; print $name."\n"; $name =~ s/\..*//;
  #print $name."\n"; $name =~ s/_\d+$//; print $name."\n"; $name =~ s/_\d+.*$//; print $name."\n";
  my($nm0,$nm1,$nm2) = &_GetONames($valuset{"filename"});
  if ( ! defined($valuset{"basename"}) ){  $valuset{"basename"} = $nm0;}
  if ( ! defined($valuset{"sname1"}) ){    $valuset{"sname1"}   = $nm1;}
  if ( ! defined($valuset{"sname2"}) ){    $valuset{"sname2"}   = $nm2;}



  # Prepare the SQL query and execute it
  my $fdinsert;
  $fdinsert  = "INSERT IGNORE INTO FileData ";
  $fdinsert .=
      "(runParamID, fileName, baseName, sName1, sName2, productionConditionID, numEntries, ".
      " md5sum, fileTypeID, fileDataComment, fileSeq, fileStream, ".
      " fileDataIDate, fileDataCreator)";
  $fdinsert .=
      " VALUES($runNumber, ".
      " \"".$valuset{"filename"}."\",\"".$valuset{"basename"}."\",\"".$valuset{"sname1"}."\",\"".$valuset{"sname2"}."\",".
      " $production, $nevents, $md5sum, $fileType,$fileComment,$fileSeq,$filestream, ".
      " NOW()+0, ".&_GetILogin().")";

  if ($FC::DEBUG > 0) { &print_debug("insert_file_data","Execute $fdinsert");}


  my $sth;
  my $retid;
  $sth = $FC::DBH->prepare( $fdinsert );
  if( $sth ){
      if ($DELAY){
	  push(@DCMD,
	       "# Delayed mode chosen but operation is not 'flush'-able. ".
	       "Use direct update for ".$fdinsert);
	  $retid = 0;
      } else {
	  if ( $sth->execute() ) {
	      $retid = &get_last_id();
	      &print_debug("insert_file_data","Returning: $retid");

	  } else {
	      &print_debug("insert_file_data",
			   "ERROR in insert_file_location() ".$FC::DBH->err." >> ".$FC::DBH->errstr);
	      $sth->finish();
	      return 0;
	  }
      }
      $sth->finish();
  } else {
      &print_debug("insert_file_data","Failed to prepare [$fdinsert]");
  }

  # TriggerComposition and TriggerWords are now handled by a single routine
  &set_trigger_composition($retid) if ($retid != 0);

  return $retid;
}







#============================================
#
# This internal routine was added to handle insertion
# of the trigger information.
# It is developped for handling updates as well.
#

#
# Add a new individual trigger to the collection
# The internal arrays are flushed by any calls to add_file_data() through
# a call to set_trigger_composition()
#
# Arguments are obvious ...
#
sub add_trigger_composition
{
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}
    my($triggerName,$triggerWord,$triggerVersion,$triggerDefinition,$triggerCount) = @_;

    # Store it in internal arrays
    $FC::IDX++;
    $FC::TRGNAME[$FC::IDX] = &get_value($triggerName,"unknown",0);
    $FC::TRGWORD[$FC::IDX] = &get_value($triggerWord,"000000",0);
    $FC::TRGVERS[$FC::IDX] = &get_value($triggerVersion,"V0.0",1);
    $FC::TRGDEFS[$FC::IDX] = &get_value($triggerDefinition,"unspecified",0);
    $FC::TRGCNTS[$FC::IDX] = &get_value($triggerCount,0,0);

}

#
# This really enters it (or updates) in the database
#
# Arg1 : a fdid
# Arg2 : an insert/update flag (0 insert, 1 update)
#
sub set_trigger_composition
{
    my($tcfdid,$update)=@_;

    if( ! defined($FC::DBH)){
	&print_message("set_trigger_compositio",
		       "Not connected/connecting");
	return;
    }
    if ( $FC::IDX == -1 ) {
	if ( defined($valuset{"simulation"}) ){
	    if ( $valuset{"simulation"} eq '0'){
		&print_message("set_trigger_composition",
			       "No trigger composition set");
	    }
	}
	return;
    }
    if( ! defined($tcfdid) ){
	&die_message("set_trigger_composition",
		     "Mandatory first argument undefined");
    }

    if( $tcfdid == 0){
	&print_message("set_trigger_composition",
		       "Preceeding operation prevents current execution");
	return;
    }


    my($i,$el,$cnt);
    my($cmd1,$sth1,$cmd2,$sth2);
    my($cmdd,$sthd);
    my(@all);
    my(@TrgID,@TrgCnt);



    #
    # Insert first all entries in TriggerWords in INSERT mode
    #
    $cmd1 =  &_SQLCACHED_SELECT()."triggerWordID, triggerWordComment FROM TriggerWords ".
	" WHERE triggerWordName=? AND triggerWordBits=?  AND triggerWordVersion=?";
    $cmd2 = "INSERT INTO TriggerWords VALUES(NULL, ?, ?, ?, NOW()+0, ".&_GetILogin().", 0, ?)";

    $sth1 = $FC::DBH->prepare($cmd1);
    $sth2 = $FC::DBH->prepare($cmd2);

    if ( ! $sth1 || ! $sth2 ){  &die_message("set_trigger_composition","Prepare statements 1 failed");}

    # Loop over and check/insert
    $cnt = -1;
    for ($i=0 ; $i <= $#FC::TRGNAME ; $i++){
	&print_debug("set_trigger_composition",
		     "Should insert $FC::TRGNAME[$i] $FC::TRGWORD[$i] $FC::TRGVERS[$i] $FC::TRGDEFS[$i] $FC::TRGCNTS[$i]");
	if ( $sth1->execute($FC::TRGNAME[$i],$FC::TRGWORD[$i],$FC::TRGVERS[$i]) ){
	    if ( @all = $sth1->fetchrow() ){
		# Already in, fetch
		&print_debug("set_trigger_composition","Fetched 1 triggerWordID $all[0]");
		$TrgID[++$cnt] = $all[0];
		$TrgCnt[$cnt]  = $FC::TRGCNTS[$i];
	    } else {
		# Not in, Insert
		if ( $sth2->execute($FC::TRGNAME[$i],$FC::TRGVERS[$i], $FC::TRGWORD[$i],$FC::TRGDEFS[$i]) ){
		    $TrgID[++$cnt] = &get_last_id();
		    $TrgCnt[$cnt]  = $FC::TRGCNTS[$i];
		    &print_debug("set_trigger_composition","Fecthed 2 triggerWordID $TrgID[$cnt]");
		} else {
		    &die_message("set_trigger_composition",
				 "Failed to insert $FC::TRGNAME[$i],$FC::TRGWORD[$i],$FC::TRGVERS[$i],$FC::TRGDEFS[$i] error=".
				 $FC::DBH->err." >> ".$FC::DBH->errstr);
		}
	    }
	} else {
	    &die_message("set_trigger_composition",
			 "Failed to execute for $FC::TRGNAME[$i],$FC::TRGWORD[$i],$FC::TRGVERS[$i] error=".
			 $FC::DBH->err." >> ".$FC::DBH->errstr);
	}
    }
    $sth1->finish();
    $sth2->finish();



    #
    # Enter entries in TriggerCompositions
    #
    $cmd1 =  &_SQLCACHED_SELECT()."triggerWordID FROM TriggerCompositions WHERE fileDataID=?";
    $cmd2 = "INSERT DELAYED INTO TriggerCompositions VALUES(?,?,?)";
    $sth1 = $FC::DBH->prepare($cmd1);
    $sth2 = $FC::DBH->prepare($cmd2);

    if ( ! $sth1 || ! $sth2 ){  &die_message("set_trigger_composition","Prepare statements 2 failed");}

    if ( ! $sth1->execute($tcfdid) ){
	&print_message("set_trigger_composition","Could not execute [$cmd1 , $tcfdid]");
    } else {
	@all = $sth1->fetchrow();
	if ($#all != -1){
	    # There are entries for this tcfdid already
	    if ($update){
		# We can drop them all from TriggerCompositions. We cannot delay this ...
		# The entries in the TriggerWords table may be used in other records
		# (will do a bootstrap routine).
		#
		# Here, we deleted only the fully-specified records because we envision
		# this routine to be used in update of perticular triggerWord (leaving
		# the rest of the list unmodified).
		#
		$cmdd = "DELETE LOW_PRIORITY FROM TriggerCompositions WHERE fileDataID=? AND triggerWord=?";
		$sthd = $FC::DBH->prepare($cmdd);
		foreach $el (@all){
		    &print_debug("set_trigger_composition","$cmdd , $tcfdid, $el");
		    $sthd->execute($tcfdid,$el);
		}
		$sthd->finish();

	    } else {
		&print_message("set_trigger_composition","Update not yet implemented");
		return;
	    }
	} else {
	    # The table is empty. We can now insert the triggerWordIDs recovered
	    # preceedingly. Note that we MUST die() here if it fails since we
	    # have already checked the existence of $tcfdid entries ... and there
	    # none.
	    for ( $i=0 ; $i <= $#TrgID ; $i++){
		if ( ! $sth2->execute($tcfdid,$TrgID[$i],$TrgCnt[$i]) ){
		    &die_message("set_trigger_composition",
				 "Insertion of ($tcfdid,$TrgID[$i],$TrgCnt[$i]) failed");
		}
	    }

	}
    }
    $sth1->finish();
    $sth2->finish();


    # Entries are NOT re-usable (would be too dangerous)
    $FC::IDX=-1;
    undef(@FC::TRGNAME);
    undef(@FC::TRGWORD);
    undef(@FC::TRGVERS);
    undef(@FC::TRGDEFS);
}



# This is an internal routine
sub del_trigger_composition
{
    my($tcfdid,$doit)=@_;
    my($cmd,$sth);

    if ($doit){
	if ( $DELAY ){
	    push(@DCMD,
		 "DELETE LOW_PRIORITY FROM TriggerCompositions WHERE fileDataID=$tcfdid");
	} else {
	    # a complete different story
	    $cmd = "DELETE LOW_PRIORITY FROM TriggerCompositions WHERE fileDataID=?";
	    $sth = $FC::DBH->prepare($cmd);

	    if ( ! $sth ){
		&print_message("del_trigger_composition",
			       "Prepare failed. Bootstrap TRGC needed.");
		return;
	    }
	    &print_debug("del_trigger_composition","Execute $cmd , $tcfdid");

	    if ( ! $sth->execute($tcfdid) ){
		&print_message("del_trigger_composition",
			       "Execute failed. Bootstrap TRGC needed.");
	    }
	    $sth->finish();
	}
    } else {
	&print_message("del_trigger_composition",
		       "[$tcfdid] from TriggerCompositions would be deleted");
    }
}




#============================================
# get the ID for the current file data, or create it
# if not found.
# Returns:
#  the ID of a fileData record
#  or 0 if no such fielData, cannot create it, or more than one record exists for a given context.
sub get_current_file_data {
  my $runParam;
  my $fileName;
  my $production;
  my $library;
  my $fileType;
  my $fileSeq;
  my $filestream;
  my $sqlquery;

  if( ! defined($FC::DBH) ){
      &print_message("get_current_file_data","Not connected");
      return 0;
  }

  # cloned
  if ( defined( $valuset{"rfdid"}) ){ return $valuset{"rfdid"};}

  # Otherwise, must search for the ID
  $runParam   = &get_current_run_param();
  $fileType   = &check_ID_for_params("filetype");

  #$production = &check_ID_for_params("production");
  #$library    = &check_ID_for_params("library");
  $library    = &get_prodlib_version_ID();
  $production = $library;


  #print "In get_current_file_data $runParam $production $library $fileType\n";

  if ($production == 0) {
      $production = $library;
  }
  if ( $runParam != 0) {
      $sqlquery .= " runParamID = $runParam AND ";
  }
  if (defined $valuset{"filename"}) {
      $sqlquery .= " fileName = '".$valuset{"filename"}."' AND ";
  }
  if (defined $valuset{"fileseq"}) {
      $sqlquery .= " fileSeq = '".$valuset{"fileseq"}."' AND ";
  }
  if (defined $valuset{"stream"}) {
      $sqlquery .= " fileStream = '".$valuset{"stream"}."' AND ";
  }
  if ( $production != 0) {
      $sqlquery .= " productionConditionID = $production AND ";
  }
  if ( $fileType != 0) {
      $sqlquery .= " fileTypeID = $fileType AND ";
  }
  if ( defined($sqlquery) ){
      $sqlquery =~ s/(.*)AND $/$1/g;
  } else {
      &print_message("get_current_file_data","No parameters set to identify File Data");
      #print "Zobi1 ...\n";
      return 0;
  }
  $sqlquery = "SELECT fileDataID FROM FileData WHERE $sqlquery";
  if ($FC::DEBUG > 0) {
      &print_debug("get_current_file_data","Executing query: $sqlquery");
  }

  my($sth,$id);

  $sth = $FC::DBH->prepare($sqlquery);

  if( ! $sth ){
      &print_message("get_current_file_data","Failed to prepare [$sqlquery]");
      #print "Zobi2 [$sqlquery] ...\n";
      return 0;
  }

  #print "Zobi 3 (execute)\n";
  my $retv=0;
  if ( $sth->execute() ){
      $sth->bind_columns( \$id );

      if ($sth->rows == 0) {
	  $retv = &insert_file_data();

      } elsif ($sth->rows > 1) {
	  &print_message("get_current_file_data","More than one file data matches the query criteria",
			 "Add more data to unambiguously identify file data");

      } elsif ( $sth->fetch() ) {
	  if ($FC::DEBUG > 0) { &print_debug("get_current_file_data","Returning: $id");}

	  $retv = $id;
      }
  }
  $sth->finish();

  #print "Return $retv\n";
  return $retv;
}


#============================================
# inserts the file location data and the file and run data
# if neccessary.
# Returns: The ID of a newly created File Location
# or 0 if the insert fails
sub insert_simulation_params {
  my $simComments;
  my $evgenComments;
  my $eventGenerator;

  if( ! defined($FC::DBH) ){
      &print_message("insert_simulation_params","Not connected");
      return 0;
  }

  if (! (defined $valuset{"generator"}  &&
	 defined $valuset{"genversion"} &&
	 defined $valuset{"genparams"})) {
      &die_message("insert_simulation_params",
		   "Not enough parameters to insert event generator data",
		   "Define generator, genversion and genparams");
      return 0;
  }
  if (! defined $valuset{"gencomment"}) {
      &print_debug("insert_simulation_params","WARNING: gencomment not defined. Using NULL");
      $evgenComments = "NULL";
  } else {
      $evgenComments = '"'.$valuset{"gencomment"}.'"';
  }
  if (! defined $valuset{"simcomment"}) {
      &print_debug("insert_simulation_params","WARNING: simcomment not defined. Using NULL");
      $simComments = "NULL";
  } else {
      $simComments = '"'.$valuset{"simcomment"}.'"';
  }

  my $sqlquery =  &_SQLCACHED_SELECT()."eventGeneratorID FROM EventGenerators WHERE eventGeneratorName = '".$valuset{"generator"}."' AND eventGeneratorVersion = '".$valuset{"genversion"}."' AND eventGeneratorParams = '".$valuset{"genparams"}."'";
  if ($FC::DEBUG > 0) {
      &print_debug("insert_simulation_params","Executing query: $sqlquery");
  }


  my $sth;
  $sth = $FC::DBH->prepare($sqlquery);
  if( ! $sth ){
      &print_debug("insert_simulation_params","Failed to prepare [$sqlquery]");
      return 0;
  }

  my( $id );
  if ( ! $sth->execute() ){
      $sth->finish();
      return 0;
  }

  $sth->bind_columns( \$id );

  if ($sth->rows == 0) {
      my $eginsert   = "INSERT IGNORE INTO EventGenerators ";
      $eginsert  .= "(eventGeneratorName, eventGeneratorVersion, eventGeneratorComment, eventGeneratorParams)";
      $eginsert  .= " VALUES('".$valuset{"generator"}."', '".$valuset{"genversion"}."', $evgenComments, '".$valuset{"genparams"}."')";
      if ($FC::DEBUG > 0) {
	  &print_debug("insert_simulation_params","Execute $eginsert");
      }
      my $sthe = $FC::DBH->prepare( $eginsert );

      if ( $sthe ){
	  if ( $sthe->execute() ) {
	      $eventGenerator = &get_last_id();
	      if ($FC::DEBUG > 0) {
		  &print_debug("insert_simulation_params","Returning: $eventGenerator");
	      }
	  } else {
	      &print_debug("insert_simulation_params",
			   "Could not add event generator data.",
			   "Aborting simulation data insertion query");
	  }
	  $sthe->finish();
      } else {
	  &print_debug("insert_simulation_params","Failed to preapre [$eginsert]");
	  $sth->finish();
	  return 0;
      }
  } else {
      $sth->fetch();
      if ($FC::DEBUG > 0) {
	  &print_debug("insert_simulation_params","Got eventGenerator: $id");
      }
      $eventGenerator = $id;
  }
  $sth->finish();

  my $spinsert   = "INSERT IGNORE INTO SimulationParams ";
  $spinsert  .= "(eventGeneratorID, simulationParamIDate, simulationParamCreator, simulationParamComment)";
  $spinsert  .= " VALUES($eventGenerator,  NOW()+0, ".&_GetILogin().", $simComments)";
  if ($FC::DEBUG > 0) {
      &print_debug("insert_simulation_params","Execute $spinsert");
  }

  $sth = $FC::DBH->prepare( $spinsert );
  if( ! $sth){
      &print_debug("FileCatalog::insert_simulation_params : Failed to prepare [$spinsert]");
      return 0;
  }
  if ( $sth->execute() ) {
      my $retid = &get_last_id();
      if ($FC::DEBUG > 0) {
	  &print_debug("insert_simulation_params","Returning: $retid");
      }
      $sth->finish();
  } else {
      &print_debug("insert_simulation_params",
		   "Could not add simulation data.",
		   "Aborting simulation data insertion query.");
      $sth->finish();
      return 0;
  }
}

#============================================
# get the ID for the current simulation parameters, or create them
# if not found.
# Returns:
# the ID of a SimulationParams record
# or 0 if no such record and cannot create it
sub get_current_simulation_params {
  my $simComment;
  my $generator;
  my $generatorVer;
  my $generatorParams;
  my $generatorComment;
  my $sqlquery;

  if( ! defined($FC::DBH) ){
      &print_message("get_current_simulation_params","Not connected");
      return 0;
  }

  if (! (defined $valuset{"generator"}  &&
	 defined $valuset{"genversion"} &&
	 defined $valuset{"genparams"})) {
      &die_message("get_current_simulation_params",
		   "Not enough parameters to find event generator data",
		   "Define generator, genversion and genparams");
      return 0;
  }
  $sqlquery =  &_SQLCACHED_SELECT()."simulationParamsID FROM SimulationParams, EventGenerators WHERE eventGeneratorName = '".$valuset{"generator"}."' AND eventGeneratorVersion = '".$valuset{"genversion"}."' AND eventGeneratorParams = '".$valuset{"genparams"}."' AND SimulationParams.eventGeneratorID = EventGenerators.eventGeneratorID";
  if ($FC::DEBUG > 0) {
      &print_debug("get_current_simulation_params","Executing query: $sqlquery");
  }

  my ($sth);
  $sth = $FC::DBH->prepare($sqlquery);
  if ( ! $sth){
      &print_debug("get_current_simulation_params","Failed to prepare [$sqlquery]");
      return 0;
  }

  my ($id);

  if ( $sth->execute() ){
      $sth->bind_columns( \$id );

      if ($sth->rows == 0) {
	  my $newid;
	  $newid = &insert_simulation_params();
	  $sth->finish();
	  return $newid;
      } else {
	  if ( $sth->fetch() ) {
	      if ($FC::DEBUG > 0) { &print_debug("get_current_simulation_params","Returning: $id");}
	      $sth->finish();
	      return $id;
	  }
	  $sth->finish();
      }
  }
  return 0;

}

#
# Gets the entry associated to a context and reset
# the context with the exact full value list required
# for a new entry.
#
sub clone_location {
    my(@allfd,@allfl,$tmp);

    #&print_debug("clone_location","Checking FileData");
    @allfd = &get_file_data();
    #&print_debug("clone_location","Checking FileLocation");
    @allfl = &get_file_location();

    if ($#allfd != -1 && $#allfl != -1){
	&print_debug("clone_location","Clearing context now");
	# we clear the context, so the real keyword are flushed, leaving only
	# the reference to the keyword. For example, rpcid is kept but not
	# production. The external use can then reset the production keyword
	# which will be considered (otherwise, rpcid will be used).
	&clear_context();
	&print_debug("clone_location","Setting  context now");
	&set_context(@allfd);
	&set_context(@allfl);

	&print_debug("clone_location",
		     "What was queried\n",
		     "\t".join(",",@allfd),
		     "\t".join(",",@allfl));
	1;
    } else {
	&print_message("clone_location","FileData/FileLocation cloning failed $#allfd $#allfl");
	0;

    }
}

sub get_file_location(){   return &_FileTableContent("FileLocation","FLKWD");}
sub get_file_data(){       return &_FileTableContent("FileData","FDKWD");}


#
# Routine will populate the $TABREF arrays and return those fields
# with values as key=value list. This is used for cloning records.
#
sub _FileTableContent {

    my($table,$TABREF)=@_;

    my($i,@itab,$iref,@query,@items);

    #print "Checking for $table $TABREF\n";
    eval("@itab = @$TABREF");
    #if($@){ &die_message("FileTableContent()","eval() has failed with $@");}

    #print "Evaluating for $table\n";
    foreach ( keys %keywrds ){
	@items = split(",",$keywrds{$_});
	if ( $items[1] =~ m/$table/ && $items[5] == 1 ){
	    push(@itab,$_);
	    #} else {
	    #print "Rejecting $_ $keywrds{$_}\n";
	}
    }
    #print "-->".join(",",@itab)."\n";


    my (@all,$delim,$dsts);


    $delim = &get_delimeter();
    $dsts  = &get_debug();
    &set_debug(0) if ($table eq "FileLocation");
    &set_delimeter("::");                          # set to known one
    @all = &run_query("FileCatalog",@itab);
    &set_delimeter($delim);                        # restore delim
    &set_debug($dsts);

    #print "Run with ".join("/",@itab)."\n";
    &print_debug("_FileTableContent","+ Run with ".join("/",@itab));


    undef(@query);
    if ($#all != -1){
	#print "Will loop over $all[0]\n";
	@all = split("::",$all[0]);                # Only one instance

	for ( $i=0 ; $i <= $#itab ; $i++){
	    #&print_debug("Return value for $itab[$i] is $all[$i]");
	    if ( ! defined($all[$i]) ){
		&print_debug("_FileTableContent","/!\\  $itab[$i] has no value - setting to null_st\n");
		$all[$i] = "";
	    }
	    &print_debug("_FileTableContent","--> Return value for $itab[$i] is $all[$i]");
	    if( $all[$i] ne ""){
		push(@query,"$itab[$i] = $all[$i]");
	    }
	}
    } else {
	&print_debug("_FileTableContent","/!\\ Querry leaded to nothing");
    }
    return @query;
}




#============================================
# inserts the file location data and the file and run data
# if neccessary.
# Returns: The ID of a newly created File Location
#          or 0 if the insert fails
sub insert_file_location {
  my $fileData;
  my $storageType;
  my $storageSite;
  my $filePathID;
  my $createTime;
  my $owner;
  my $fsize;
  my $protection;
  my $nodeID;
  my $availability;
  my $persistent;
  my $sanity;

  if( ! defined($FC::DBH) ){
      &print_message("insert_file_location","Not connected");
      return 0;
  }

  $fileData = &get_current_file_data();
  if ($fileData == 0) {
      if ($DELAY){
	  &print_message("insert_file_location","Proceeding for debugging purposes only");

      } else {
	  &print_message("insert_file_location","No file data available",
			 "Aborting file insertion query");
	  return 0;
      }
  }


  $storageType = &check_ID_for_params("storage");
  $storageSite = &check_ID_for_params("site");

  if ( $storageType == 0 ) {
      # take default value from 'clone'
      if( defined($valuset{"rstid"}) ){  $storageType = $valuset{"rstid"}; delete($valuset{"rstid"});}
      if ( $storageType == 0){
	  # if still NULL, abort because it is mandatory
	  &print_message("insert_file_location","Aborting file location insertion query. [storage] mandatory");
	  return 0;
      }
  }
  if ( $storageSite == 0){
      if( defined($valuset{"rssid"}) ){  $storageSite = $valuset{"rssid"}; delete($valuset{"rssid"});}
      if ($storageSite == 0){
	  &print_message("insert_file_location","Aborting file location insertion query. [site] mandatory");
	  return 0;
      }
  }


  #
  # there should be NO defaults for path ...
  #
  $filePathID = &check_ID_for_params("path");
  if ( $filePathID == 0 ) {
      # take default value from 'clone'
      if( defined($valuset{"rfpid"}) ){  $filePathID  = $valuset{"rfpid"}; delete($valuset{"rfpid"});}
      if ( $filePathID == 0){
	  # if still NULL, abort because it is mandatory
	  &print_message("insert_file_location","Aborting file location insertion query. [path] mandatory");
	  return 0;
      }
  }

  #
  # not mandatory
  #
  if (! defined $valuset{"createtime"}) {
      &print_debug("insert_file_location","WARNING: createtime not defined. Using a default value");
      $createTime = "NULL+0";
  } else {
      $createTime = '"'.$valuset{"createtime"}.'"';
      $createTime =~ s/[:-]//g;
      &print_debug("insert_file_location","Taking valuset $createTime");
  }

  if (! defined $valuset{"owner"}) {
      &print_debug("insert_file_location","WARNING: owner not defined. Using a default value");
      $owner = "''";
  } else {
      $owner = '"'.$valuset{"owner"}.'"';
  }
  if (! defined $valuset{"protection"}) {
      &print_debug("insert_file_location","WARNING: protection not defined. Using a default value");
      $protection = '" "';
  } else {
      $protection = '"'.$valuset{"protection"}.'"';
  }

  #
  # node (really host entry in Hosts dictionary) would revert to a
  # default value if not specified.
  #
  $nodeID = &check_ID_for_params("node",0);
  if ( $nodeID == 0 ) {
      # as usual, use clone default if exists
      if( defined($valuset{"rhid"}) ){
	  $nodeID = $valuset{"rhid"}; delete($valuset{"rhid"});
      }
      if ( $nodeID == 0){
	  &print_debug("insert_file_location","WARNING: node not defined. Using a default value");
	  $valuset{"node"}  = "localhost";        # Cannot be NULL because of check
	  $nodeID = &check_ID_for_params("node"); # recheck index with default value
      }
  }
  if ($nodeID == 0){
      &print_debug("insert_file_location","NodeID is NULL (failed)");
      return 0;
      # not because it is mandatory, but because it
      # it has failed
  }


  # now for those ...
  if (! defined $valuset{"available"}) {
      &print_debug("insert_file_location","WARNING: available not defined. Using a default value");
      $availability = 1 ;
  } else {
      $availability = $valuset{"available"};
  }
  if (! defined $valuset{"persistent"}) {
      &print_debug("insert_file_location","WARNING: persistent not defined. Using a default value");
      $persistent = 0 ;
  } else {
      $persistent = $valuset{"persistent"};
  }
  if (! defined $valuset{"sanity"}) {
      &print_debug("insert_file_location","WARNING: sanity not defined. Using a default value");
      $sanity = 0;
  } else {
      $sanity = $valuset{"sanity"};
  }

  if (! defined $valuset{"size"}) {
      $fsize = 0;
  } else {
      $fsize = $valuset{"size"};
  }


  # Handle check if recor exists & insert new record (but ignore if exists still)
  my ($flinchk,$flinsert);
  my ($UFID);

  $UFID = 0;
  if ( &_CanHandleSplitted() ){
      # started in 2008, I splitted FileLocations by storageTypeID
      if ( &_TypeSplitted("FileLocations",$storageType) ){
	  $flinchk    = "SELECT fileLocationID from FileLocations_$storageType WHERE ";
	  $flinsert   = "INSERT IGNORE INTO FileLocations_$storageType ";
      } else {
	  # use index _0 as default
	  $flinchk    = "SELECT fileLocationID from FileLocations_0 WHERE ";
	  $flinsert   = "INSERT IGNORE INTO FileLocations_0 ";
      }

      # note that in this case, we must handle the ID separately as
      # indexes should be centrally managed if inserts ought to happen
      # in the underlined tables - note that having this separate table
      # may become handy to later find empty/usable indexes
      #
      # TODO Bootstrap global with
      #   SELECT  FileLocationsID.fileLocationID FROM  FileLocationsID LEFT OUTER JOIN FileLocations
      #       ON FileLocations.fileLocationID =  FileLocationsID.fileLocationID
      #       WHERE FileLocationsID.fileLocationID IS NULL;
      #
      #   and delete the indexes returned
      #
      my $sthid = $FC::DBH->prepare("INSERT INTO FileLocationsID (fileLocationID) VALUES (NULL)");
      if ( $sthid->execute() ){
	  $UFID = &get_last_id();
	  &print_debug("insert_file_location","Selected unique FLID $UFID");
	  $sthid->finish();
      } else {
	  &print_message("insert_file_location","Failed to reserve a new index ".$FC::DBH->errstr);
	  $sthid->finish();
	  return 0;
      }
  } else {
      $flinchk    = "SELECT fileLocationID from FileLocations WHERE ";
      $flinsert   = "INSERT IGNORE INTO FileLocations ";
  }

  $flinsert  .=
      "(fileLocationID, fileDataID, storageTypeID, filePathID, ".
      " createTime, insertTime, owner, fsize, storageSiteID, protection, ".
      " hostID, availability, persistent, sanity)";

  $flinsert  .=
      " VALUES(".($UFID==0?"NULL":$UFID).", $fileData, $storageType, $filePathID, ".
      " $createTime, NULL, $owner, $fsize, $storageSite, $protection, ".
      " $nodeID, $availability, $persistent, $sanity)";

  # NONE of the NULL value should appear below otherwise, one keeps adding
  # entry over and over ... protection and woner are irrelevant here and
  # requires an UPDATE instead of a new insert.
  $flinchk   .= " fileDataID=$fileData AND storageTypeID=$storageType AND filePathID=$filePathID AND storageSiteID=$storageSite AND hostID=$nodeID";

  # AND filePath=$filePathnodeName=$nodeName";




  my $sth;
  my $retid=0;

  &print_debug("insert_file_location","Execute $flinchk");
  #print "Executing $flinchk\n";
  $sth = $FC::DBH->prepare( $flinchk );
  if ( ! $sth ){
      &print_debug("insert_file_location","Failed to prepare [$flinchk]");
  } else {
      if ( $sth->execute() ){
	  my ($val);
	  if ( defined($val = $sth->fetchrow()) ){
	      &print_message("insert","Record already in as $val (may want to update)\n");
	      $retid = $val;
	  }
      }
  }
  $sth->finish();

  if( $retid == 0){
      &print_debug("insert_file_location","Execute $flinsert");
      $sth = $FC::DBH->prepare( $flinsert );
      #print "++ $flinsert ++";
      if( ! $sth ){
	  &print_debug("insert_file_location","Failed to prepare [$flinsert]");
      } else {
	  if ($DELAY ){
	      push(@DCMD,$flinsert);
	      $retid = 0;
	  } else {
	      # Just for fun, time this operation
	      my($ts,$td);
	      $ts = time();

	      # $FC::DBH->trace(9);
	      if ( $sth->execute() ) {
		  if ( $UFID == 0){
		      $retid = &get_last_id();
		      if ( &_CanHandleSplitted() ){
			  # we MUST re-synchronized
			  $FC::DBH->do("INSERT IGNORE INTO FileLocationsID (fileLocationID) VALUES ($retid)");
		      }
		  } else {
		      $retid = $UFID;
		  }
		  &print_debug("insert_file_location","Returning: $retid");
	      } else {
		  &print_debug("insert_file_location",
			       "ERROR in insert_file_location() ".$FC::DBH->err." >> ".$FC::DBH->errstr);
	      }

	      $td = time()-$ts;
	      &print_debug("insert_file_location","Execute took $td secondes");
	  }
	  $sth->finish();
      }
  }
  return $retid;

}


# small routine to return caching statement
# this is based on MySQL caching and detected at _Connect()
sub _SQLCACHED_SELECT
{
    $FC::HASSQLCACHE?"SELECT SQL_CACHE ":"SELECT ";
}


#====================================================
# Helper subs for handling split tables and super-indexes
#====================================================

#
# The main idea of this routine is that while we are building
# a condition statement, we can check if $keyw with $val is
# a super index. Note that this DOES not check for val1 OR val2
# or fancy queries - this check should be external.
#   $keyw    the key to check by name (i.e. -cond etc ...)
#   $val     the value of that key so the split range could be checked
#            It will be assumed that the merged table was built with
#            a _0 default table and INSERT_METHOD=FIRST
#
sub _IsSuperIndex
{
    my($keyw,$val)=@_;
    my($tab);

    if ( &_CanHandleSplitted() ){
	if ( defined($tab = $FC::SUPERIDX{$keyw}) ){
	    &print_debug("_IsSuperIndex","$keyw -> ".$FC::SUPERIDX{$keyw});
	    if( &_TypeSplitted($tab,$val) ){
		return "$tab:$tab"."_$val";
	    } else {
		# return default which will activate anyhow the _0
		# by construct
		return "$tab:$tab";
	    }
	}
    }
    return "";
}

#
# Returns true
#
sub _CanHandleSplitted()
{
    my($yn)=(0==1);

    # First, our version MUST be greater than the below
    # Intermediate versions did not have splitting
    if ($VERSION gt "V01.340"){
	$yn=1==1;
    }
    $yn;
}

#
# Only returns true/false depending if the index checked is
# in the range of the split.
#
sub _TypeSplitted
{
    my($tab,$st)=@_;
    if ( defined($FC::SPLIT_MAX{$tab}) ){
	return ($st > $FC::SPLIT_MIN{$tab} && $st < $FC::SPLIT_MAX{$tab});
    } else {
	return 1==0;
    }
}



#====================================================
# Helper subroutines needed for the generic query building
#====================================================
# Gets the 'level' of a database table
# The table is at level 1, if it not directly referenced by any other table
# The table is at level 2 if it is directly referenced by a table at level 1 etc.
sub _GetStructLevel {
  my $count;
  my ($paramtable) = @_;

  if( ! defined($paramtable) ){  return 0;}

  for ($count = 0; $count<($#datastruct+1); $count++) {
    my ($mtable, $stable, $cfield, $level) = split(",",$datastruct[$count]);
    if ($mtable eq $paramtable) {
      return $level;
    }
  }
  return 0;
}

#====================================================
# Get all the tables at the lower level connnected to this table
sub get_lower_level {
  my $count;
  my ($paramtable) = @_;

  if( ! defined($paramtable) ){  return 0;}

  for ($count = 0; $count<($#datastruct+1); $count++) {
    my ($mtable, $stable, $cfield, $level) = split(",",$datastruct[$count]);
    if (($mtable eq $paramtable) && ($stable ne "")) {
      return ($stable, $level-1, $count);
    }
  }
  return 0;
}

#===================================================
# Get all the tables at the upper level connected to this table
sub get_all_upper {
  my $count;
  my ($paramtable) = @_;
  my @lower;

  if( ! defined($paramtable) ){  return 0;}

  for ($count = 0; $count<($#datastruct+1); $count++) {
    my ($mtable, $stable, $cfield, $level) = split(",",$datastruct[$count]);
    if ($stable eq $paramtable) {
      push(@lower, $mtable);
    }
  }
  return (@lower);
}

#===================================================
# Return the connection description index from a @datastruct table
# describing the connection between two tables
sub _get_connection {
  my ($amtable, $astable) = (@_);

  for (my $count = 0; $count<($#datastruct+1); $count++) {
    my ($mtable, $stable, $cfield, $level) = split(",",$datastruct[$count]);
    if (($stable eq $astable) && ($mtable eq $amtable)) {
      return $count;
    }
  }
  return -1;
}

#==================================================
# check if thwo sets of numbers have any common part
# Params:
# number of elements in the first collection
# first collection (a list)
# second collection
sub get_intersect {
  my @params = @_;
  my ($count, $cf, $cs);
  my (@first, @second);

  for ($count = 1; $count<$params[0]+2; $count++) {
    $first[$count-1] = $params[$count];
  }
  for ($count=$params[0]+2; $count < $#params+1; $count++) {
    $second[$count-$params[0]-2] = $params[$count];
  }
  if ($FC::DEBUG > 0) {
      &print_debug("get_intersect",
		   "First set: [".join(" ", (@first)."]"),
		   "Second set: [".join(" ", (@second)."]"));
  }
  for ($cf=0; $cf<$#first+1; $cf++) {
    for ($cs=0; $cs<$#second+1; $cs++) {
      if ($first[$cf] eq $second[$cs]) {
	if ($FC::DEBUG > 0) {
	    &print_debug("get_intersect","Got intersect: $cf, $cs");
	}
	return ($cf, $cs);
      }
    }
  }
  return -1;
}

#============================================
# Get the DB table connection path from a field to field
# as a list of connection indexes from @datastruct table
# Params:
# keywords for fields to connect
# Returns:
# list of the numbers of the connections
sub connect_fields {
  if ($_[0] =~ m/FileCatalog/) {  shift @_;}

  my ($begkeyword, $endkeyword) = (@_);
  my ($begtable, $begfield, $endtable, $endfield, $blevel, $elevel);
  my ($ftable, $stable, $flevel, $slevel);
  my (@connections, $connum);


  &print_debug("connect_fields",
	       "Looking for connection between fields: $begkeyword, $endkeyword");


  $begtable = &_GetTableName($begkeyword);
  $begfield = &_GetFieldName($begkeyword);
  $endtable = &_GetTableName($endkeyword);
  $endfield = &_GetFieldName($endkeyword);
  $blevel   = &_GetStructLevel($begtable);
  $elevel   = &_GetStructLevel($endtable);
  if ($blevel > $elevel) {
    ($ftable, $stable, $flevel, $slevel) =
      ($begtable, $endtable, $blevel, $elevel)
    } else {
      ($stable, $ftable, $slevel, $flevel) =
	($begtable, $endtable, $blevel, $elevel)
      }
  if ($FC::DEBUG > 0) {
      &print_debug("connect_fields",
		   "\tFirst: $ftable , $flevel",
		   "\tSecond: $stable , $slevel");
  }
  #if (! defined($slevel) ){
  #    &print_debug("connect_fields","Level in undefined (??)");
  #    return undef;
  #}
  #if ( $slevel eq "" ){
  #    &print_debug("connect_fields","Level is empty");
  #    return undef;
  #}

  # Get to the fields on the same level in tree hierarchy
  while ($slevel < $flevel) {
    my ($ttable, $tlevel, $connum) = &get_lower_level($ftable);
    push(@connections, $connum);
    $flevel = $tlevel;
    $ftable = $ttable;
  }

  # If not the same table - look again
  if ( $stable ne $ftable) {
    &print_debug("connect_fields","\tLooking for downward connections [$stable] [$ftable]");
    my @upconnections;
    # look upward in table structure
    while (($stable ne $ftable) && ($slevel != 1)) {
      my ($ttable, $tlevel, $connum) = &get_lower_level($stable);
      push(@upconnections, $connum);
      $slevel = $tlevel;
      $stable = $ttable;
      ($ttable, $tlevel, $connum) = &get_lower_level($ftable);
      push(@upconnections, $connum);
      $flevel = $tlevel;
      $ftable = $ttable;
    }
    push (@connections, @upconnections);
    &print_debug("connect_fields","\tNow adding ".join("/",@upconnections));

    if ( $stable ne $ftable) {
      &print_debug("connect_fields","Looking for upward connections");

      # Go up from the tree root searching for the link between tables
      my (%froads, %sroads);
      my (@flevelfields, @slevelfields);
      my ($findex, $sindex);

      @flevelfields = $ftable;
      @slevelfields = $stable;
      ($findex, $sindex) = &get_intersect($#flevelfields, @flevelfields, @slevelfields);
      while ($findex == -1) {
	if ($FC::DEBUG > 0) {
	    &print_debug("connect_fields",
			 "Index        : ".$findex,
			 "First fields : [".join(" ",(@flevelfields)."]"),
			 "Second fields: [".join(" ",(@slevelfields)."]"));
	}
	my ($fcount, $scount);
	my (@flower, @slower);
	for ($fcount=0; $fcount<$#flevelfields+1; $fcount++) {
	  # Get all the fields that are connected to this one
	  # and one level up
	  (@flower) = &get_all_upper($flevelfields[$fcount]);
	  if ($FC::DEBUG > 0) {
	      &print_debug("connect_fields","All first descendants: ".join(" ",(@flower)));
	  }
	  for (my $cflow = 0; $cflow <= $#flower; $cflow++) {
	      # Add a road going from the tree root to this field
	      if( defined($froads{$flower[$cflow]}) ){
		  $froads{$flower[$cflow]} = $froads{$flevelfields[$fcount]}." ".
		      &_get_connection($flower[$cflow], $flevelfields[$fcount]);
	      } else {
		  $froads{$flower[$cflow]} = " ".
		      &_get_connection($flower[$cflow], $flevelfields[$fcount]);
	      }
	      if ($FC::DEBUG > 0) {
		  &print_debug("connect_fields",
			       "Added road $froads{$flower[$cflow]}");
	      }
	  }
	}
	for ($scount=0; $scount <= $#slevelfields ; $scount++) {
	  # Get all the fields that are connected to this one
	  # and one level up
	  (@slower) = &get_all_upper($slevelfields[$scount]);
	  if ($FC::DEBUG > 0) {
	      &print_debug("connect_fields",
			   "All second descendants: ".join(" ",(@slower)));
	  }
	  for (my $cslow = 0; $cslow < $#slower+1; $cslow++) {
	    # Add a road going from the tree root to this field
	      if( defined($sroads{$slower[$cslow]}) ){
		  $sroads{$slower[$cslow]} = $sroads{$slevelfields[$scount]}." ".
		      &_get_connection($slower[$cslow], $slevelfields[$scount]);
	      } else {
		  $sroads{$slower[$cslow]} = " ".
		      &_get_connection($slower[$cslow], $slevelfields[$scount]);
	      }

	    if ($FC::DEBUG > 0) {
		&print_debug("connect_fields",
			     "Added road $sroads{$slower[$cslow]}");
	    }
	  }
	}
	@flevelfields = @flower;
	@slevelfields = @slower;
	($findex, $sindex) = &get_intersect($#flevelfields, @flevelfields, @slevelfields);
      }
      push (@connections, $froads{$flevelfields[$findex]});
      push (@connections, $sroads{$slevelfields[$sindex]});
    }
  }
  return join(" ",@connections);
}



#============================================
# Runs the query to get the data from the database
# Params: list of keyowrds determining the data
# fields to get:
# Returns:
# list of rows matching the query build on the current context
# in each row the fileds are separated by ::
sub run_query_st {
    my (@tab)=&run_query(@_);
    if ( @tab ){
	return join("\n",@tab);
    } else {
	return undef;
    }
}

sub run_query_cache
{
    my (@tab);

    $FC::FORCESQLCACHE = 1==1;
    @tab = &run_query(@_);
    $FC::FORCESQLCACHE = 1==0;
    return @tab;
}

sub run_query {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  };

  # Do not run if not DB
  if( ! defined($FC::DBH) ){
      &print_message("run_query","Not connected");
      return;
  }
  # Protect against bogus empty or undefined query
  if( ! defined($_[0]) ){
      &print_message("run_query()","method called without arguments");
      return;
  }
  # Treatment for ugly hack to get FileLocation id number from
  # within the module
  my $flkey;
  if ($_[0] eq "id"){
      $flkey = 1;
      shift @_;
  }

  my (@keywords)  = (@_);
  my (%functions);
  my ($dele,$i);
  my ($keyw,$count);
  my ($rlimit,$idpushed,$fdidpos);
  my (%keyset,%xkeys);
  my (%TableUSED,%XTableUSED);
  my (@super_index);
  my $grouping = "";
  my $using_global_func = 0;



  # Transfer into associative array for easier handling
  # Detect position of special keyword fdid if any - will
  # be used for rlimit logic
  $fdidpos  = 0;
  $i        = 0;
  foreach $keyw (@keywords){
      $keyw =~ s/ //g;
      $keyset{$keyw} =$keyw ;
      if ( $keyw eq "fdid"){  $fdidpos = $i;}
      $i++;
  }



  # Get rlimit
  $idpushed = 0;
  if (defined $valuset{"rlimit"}){
      $rlimit = $valuset{"rlimit"};
      &print_debug("run_query","We have rlimit=$rlimit");
      if($rlimit <= 0){
	  $rlimit    = 0;
      } else {
	  # push the unique key
	  if ( ! defined($keyset{"fdid"}) ){
	      &print_debug("run_query","rlimit algo requires to push in fdid, not initially requested");
	      $idpushed = 1;
	      unshift(@keywords,"fdid");
	      $keyset{"fdid"} = "fdid";
	  }
	  # verify limit keyword
	  my($l);
	  if ( defined($l = $valuset{"limit"}) ){
	      # really should be much more
	      &print_debug("run_query","limit=$l cannot be < rlimit=$rlimit, levelling to $rlimit");
	      if($l < $rlimit){  $l = $rlimit;}
	      $valuset{"limit"} = $l;
	  }
	  $using_global_func = 1;
      }
  } else {
      $rlimit = 0;
  }


  # Enventually treat ENV variables - needs to be documented
  #if ( defined($ENV{FC_DSITE}) && ! defined($valuset{"site"}) ){
  #    $valuset{"site"} = $ENV{FC_DSITE};
  #    &print_debug("run_query","Pushing site=".$valuset{"site"}." from ENV");
  #}




  # Little debugging of the table size. This information was
  # taken during the call to connect(). This information may
  # be used later to improve queries.
  #if($FC::DEBUG > 0){
  #    &print_debug("run_query", "By the way ...");
  #    foreach (keys(%rowcounts)){
  #        &print_debug("run_query","\t$_ count is ".$rowcounts{$_}."\n");
  #    }
  #}
  # coming from context
  #$operset $valuset $optoperset $optvaluset

  &print_debug("run_query","BEGIN Will treat keywords  [".join(",",@keywords)."]");
  &print_debug("run_query","BEGIN Receiving condition  [".join(",",keys %valuset)."]");
  &print_debug("run_query","BEGIN Receiving opt cond   [".join(",",keys %optvaluset)."]");

  #&print_debug("run_query","BEGIN FLRELATED ".join(",",%FC::FLRELATED));

  #+
  # Check the validity of the keywords
  #-
  $i = 0;
  for ($i = 0 ; $i <= $#keywords ; $i++){
      #First check if it is a request for an agregate value
      my ($aggr,$afun);

      $_ = $keywords[$i]; # too lazzy to change it all but to be cleaned
      $_ =~ y/ //d;

      #&print_debug("run_query","\tChecking $_");

      foreach $afun (@aggregates){
	  #&print_debug("run_query","\tSearching for aggregate $afun");
	  ($aggr, $keyw) = $_ =~ m/($afun)\((.*)\)/;
	  last if ( defined($aggr) and defined($keyw) );
      }
      &print_debug("run_query","\tAggregate operation '$aggr' found for '$keyw'\n") if (defined($aggr));

      if ( defined($keyw) ){
	  #&print_debug("run_query","Found aggregate function |$aggr| on keyword |$keyw|");

	  # If it is - save the function, and store only the bare keyword
	  $functions{$keyw} = $aggr;
	  $_ = $keywords[$i]= $keyw;
	  $keyset{$keyw}    = 1;

	  if ( defined($ksimilar{$keyw}) ){
	      &print_message("run_query",
			     "Nested agrregate function '$aggr' on ".
			     "aggregate keyword '$keyw' not supported and ignored.");
	  }
      }

      if ( ! defined ($keywrds{$_})){
	  if ( defined($obsolete{$_}) ){
	      &print_message("run_query","Keyword $_ is obsolete ; use $obsolete{$_} instead");
	  } else {
	      &print_message("run_query","Wrong keyword: $_");
	  }
	  return;
      } else {
	  $keyw = $_;
	  if ( defined($afun = $ksimilar{$keyw}) ){
	      &print_debug("run_query","Found aggregate option $keyw = $afun");
	      my ($func,$list) = split(";",$afun);

	      foreach my $el ( (split(" ",$list)) ){
		  if ( defined($keyset{$el}) ){
		      &print_message("run_query()","$el specified with $keyw containing it ...");
		      delete($keyset{$el});
		  }
		  #print "Adding  $el\n";
	      }
	      $xkeys{$keyw}  = $list;
	      $keyset{$keyw} = "_$func";
	  } else {
	      $afun = &_GetTableName($keyw);
	      if ( &_GetTableName($keyw) eq ""){
		  &die_message("run_query",
			       "[$keyw] is a special condition or input keyword and does not return a value ");
	      }
	  }
      }
  }

  # Re-transfer clean list of keys. An associated array is NOT sorted
  # so we have to revert to a rather ugly (but fast) trick. We will
  # use the %keyset later
  my ($j,$tl,@temp,@items,@setkeys);

  for ($j=$i=0 ; $i <= $#keywords ; $i++){
      $keyw = $keywords[$i];
      &print_debug("run_query","... Checking [$keyw]");
      if ( defined($keyset{$keyw}) ){
	  if ( defined($xkeys{$keyw}) ){
	      push(@setkeys,$keyw);   # keep ordered track for later use
	      &print_debug("run_query","\tPushing in >> ".$xkeys{$keyw}." <<");
	      @items = split(" ",$xkeys{$keyw});
	      push(@temp,@items);
	      $keyset{$keyw} .= "($j,$#items";
	      &print_debug("run_query","\tDefined as xkeys with function ".$keyset{$keyw});
	      #$j += ($#items+1);
	  } else {
	      delete($keyset{$keyw});
	      push(@temp,$keyw);

	      # logic is for optional context
	      $tl = &_GetTableName($keyw);
	      &print_debug("run_query","\tSelected as a valid key $tl");

	      $TableUSED{$tl} = 1;
	      if ( $FC::FLRELATED{$tl} ){ $XTableUSED{"FileLocations"} = 1; &print_debug("run_query","\tRelated to FL");}
	      if ( $FC::FDRELATED{$tl} ){ $XTableUSED{"FileData"}      = 1; &print_debug("run_query","\tRelated to FD");}
	      #$j++;
	  }
	  $j++; # <-- not a bug :-)
      }
  }
  @keywords = @temp;
  undef(%xkeys);
  undef(@items);
  undef(@temp);
  undef($j);
  undef($tl);

  &print_debug("run_query","Ordered list is [".join(" ",@keywords)."]");

  # Get all keys for later scan in another form. Those are
  # really all the tables we will ever need to access ...
  my @USEDTables = keys %TableUSED;


  # Optional condition should be checked now and added to the stack if applies
  # Note that they are enabled only if one of the returned
  # keys belong to the same table than the condition.
  &print_debug("run_query","Will inspect optional context now");
  foreach $keyw (keys %optvaluset){
      my ($tabname) = &_GetTableName($keyw);
      &print_debug("run_query","\tOptional Key $keyw, table=$tabname");
      if( $tabname ne ""){
	  &print_debug("run_query","\tConditional check to promote to major cond ".
		       join("-",((defined($TableUSED{$tabname}))?"1":"0",
				 (defined($XTableUSED{$tabname}))?"1":"0",
				 (defined($TableUSED{FileData}) && defined($XTableUSED{FileLocations}))?"1":"0",
				 (defined($TableUSED{FileLocations}) && defined($XTableUSED{FileData}))?"1":"0"))
		       );
	  # The conditions below are tricky to understand in one glance.
	  # - The first one is simple, the table is in use, optional context is set
	  # - The second/third implies that if there is a cross dependence FileData/FileLocations
	  #   in the keyword list, there WILL be a JOIN on FileData/FileLocations and therefore,
	  #   it is also a candidate for adding the optionally context on the query stack.
	  if ( ( defined($TableUSED{$tabname}) )                                         ||
	       ( defined($XTableUSED{$tabname}) )                                        ||
	       ( defined($TableUSED{FileData}) && defined($XTableUSED{FileLocations}) )  ||
	       ( defined($TableUSED{FileLocations}) && defined($XTableUSED{FileData}) )  ){
	      if ( ! defined($valuset{$keyw}) ){
		  &print_debug("run_query",
			       "\t** Activating optional context $keyw ".
			       "$optoperset{$keyw} $optvaluset{$keyw}");
		  $valuset{$keyw} = $optvaluset{$keyw};
		  $operset{$keyw} = $optoperset{$keyw};
	      } else {
		  &print_debug("run_query",
			       "\t** Optional context $keyw will NOT be activated");
	      }
	  } else {
	      &print_debug("run_query","\t** Dropping $keyw as the association conditions with selected keys does not validate");
	  }
      } else {
	  &print_debug("run_query","\tBogus $keyw leads to tablename NULL");
      }
  }

  # make a rapid scan of the condition table now and check for the
  # tables we would be adding. This is mainly later to check the
  # case of querry values from table X with condition with table X
  # only.
  foreach $keyw (keys %valuset){
      my ($tabname) = &_GetTableName($keyw);
      if ($tabname eq ""){ next;}
      $TableUSED{$tabname} = 1;
  }
  my @XUSEDTables= keys %TableUSED;



  # TODO: not used yet, can save and use for stats
  # This could be a hash or a monitoring of user's queries
  my $user_usage="keys [".join(",",@keywords)."] cond [";
  foreach my $k (sort keys %valuset){
      $user_usage .= lc($k).($operset{$k}?$operset{$k}:"=").$valuset{$k}.",";
  }
  chop($user_usage); $user_usage .= "]";
  #&print_debug("run_query","User requested ".$user_usage);




  # destructor (don't need them anymore)
  undef(%XTableUSED);
  undef(%TableUSED);



  #
  # THIS NEXT BLOCK IS FLAKY AND SHOULD BE HANDLED WITH CARE. IT
  # WOULD PREVENT SINGLE TABLE QUERY WITH DEPENDENCE CONDITION ON
  # SIMPLE CODING ERROR.
  #
  # Introduced at version 1.14 . Need to be revisited.
  # Idea of this block was to eliminate parts of
  # where X.Id=Y.Id AND X.String=' '  and just use
  # where X.Id=value
  #
  # Was restored to a working version at version 1.31
  #
  # Do the constraint pre-check (for query optimization)
  # check if a given constraint produces a single record ID
  # If so remove the constraint and use this ID directly instead
  #
  my @constraint;
  my @from;
  my @connections;
  my %threaded;

  if(1==1){
      &print_debug("run_query","Scanning valuset ".join(",",keys %valuset));
      foreach $keyw (keys(%valuset)) {
	  my ($tabname) = &_GetTableName($keyw);

	  # Check if the table name is one of the dictionary ones (or
	  # check that it is not a dictionary to be more precise)
	  if ( defined($FC::ISDICT{$tabname}) ){
	      my ($fieldname,$idname,$addedconstr) =
		  (&_GetFieldName($keyw),$tabname,"");

	      &print_debug("run_query","Table $tabname is a dictionary $#USEDTables");
	      $idname = &_IDize("run_query",$idname);

	      # Find which table this one is connecting to
	      my $parent_tabname;
	      #if ( $#USEDTables > 0){
		  # There are more than one table involve so we need
		  # to do a complicated dependency threading ...
		  foreach my $el (@datastruct){
		      if (($el =~ m/$idname/) > 0){
			  # We found the right row - get the table name
			  my ($stab,$fld);
			  ($stab,$parent_tabname,$fld) = split(",",$el);
			  &print_debug("run_query","\t$idname relates to $parent_tabname");
		      }
		  }
	      #} else {
	          if ( $#XUSEDTables == 0){
		      # Only one table is used (table info within same table cond)
		      &print_debug("run_query",
				   "\t++TBC $idname relates to [$parent_tabname] but one table only $#XUSEDTables");
		      $parent_tabname = $XUSEDTables[0];
		  }
	      #}

	      my $sqlquery = &_SQLCACHED_SELECT()."$idname FROM $tabname WHERE ";

	      if ((($roundfields =~ m/$fieldname/) > 0) && (! defined $valuset{"noround"})){
		  #&print_debug("run_query","1 Inspecting [$roundfields] [$fieldname]");
		  my ($nround) = $roundfields =~ m/$fieldname,([0-9]*)/;
		  #&print_debug("run_query","1 Rounding to [$roundfields] [$fieldname] [$nround]");


		  $sqlquery .= "ROUND($fieldname, $nround) ".$operset{$keyw}." ";
		  if( $valuset{$keyw} =~ m/^\d+/){
		      $sqlquery .= $valuset{$keyw};
		  } else {
		      $sqlquery .= "'$valuset{$keyw}'";
		  }

		  #&print_debug("run_query","1 Rounding Query will be [$sqlquery]");

	      } elsif ($operset{$keyw} eq "~"){
		  $sqlquery .= &_TreatLOps("$fieldname",
					   "LIKE",
					   $valuset{$keyw},
					   3);
		  #$sqlquery .= "$fieldname LIKE '%".$valuset{$keyw}."%'";

	      } elsif ($operset{$keyw} eq "!~"){
		  $sqlquery .= &_TreatLOps("$fieldname",
					   "NOT LIKE",
					   $valuset{$keyw},
					   3);
		  #$sqlquery .= "$fieldname NOT LIKE '%".$valuset{$keyw}."%'";

	      } elsif ($operset{$keyw} eq "[]"){
		  $sqlquery .= &_TreatLOps("$fieldname",
					   "BETWEEN",
					   $valuset{$keyw},
					   4);

	      } elsif ($operset{$keyw} eq "]["){
		  $sqlquery .= &_TreatLOps("$fieldname",
					   "NOT BETWEEN",
					   $valuset{$keyw},
					   4);

	      } elsif ($operset{$keyw} eq "%"){
		  $sqlquery .= &_TreatLOps("$fieldname",
					   "MOD",
					   $valuset{$keyw},
					   4);

	      } elsif ($operset{$keyw} eq "%%"){
		  $sqlquery .= &_TreatLOps("$fieldname",
					   "NOT MOD",
					   $valuset{$keyw},
					   4);


	      } else {
		  $sqlquery .= &_TreatLOps($fieldname,
					   $operset{$keyw},
					   $valuset{$keyw},
					   (&get_field_type($keyw) eq "text")?2:undef);

		  #if (&get_field_type($keyw) eq "text"){
		  #$sqlquery .= "$fieldname ".$operset{$keyw}." '".$valuset{$keyw}."'";
		  #} else {
		  #$sqlquery .= "$fieldname ".$operset{$keyw}." ".$valuset{$keyw};
		  #}
	      }
	      if ($FC::DEBUG > 0) {  &print_debug("run_query","\tExecuting special: $sqlquery");}
	      $sth = $FC::DBH->prepare($sqlquery);

	      if( ! $sth){
		  &print_debug("run_query","\tget id's : Failed to prepare [$sqlquery]");

	      } else {
		  my( $id );
		  #&print_debug("run_query","\tI am here");
		  if ( $sth->execute() ){
		      $sth->bind_columns( \$id );
		      &print_debug("run_query","\tWe executed fine and got ".$sth->rows. " values / limit 5");
		      if (( $sth->rows < 5) && ($sth->rows>0)) {
			  # Create a new constraint
			  $addedconstr = "";

			  # loop below replaced by a IN() logic 2010
			  # This will actually bypass the later use of _NormalizeAND_OR()
			  # Watch for side effects

			  #while ( $sth->fetch() ) {
			  #    if ($addedconstr ne " "){
			  #	  $addedconstr .= " OR $parent_tabname.$idname = $id ";
			  #    } else {
			  #	  $addedconstr .= " $parent_tabname.$idname = $id ";
			  #    }
			  #    &print_debug("run_query","\tAdded constraints case-1 now $addedconstr");
			  #}
			  while ( $sth->fetch() ) {
			      if ( $addedconstr eq ""){
			  	  $addedconstr  = $id;
			      } else {
				  $addedconstr .= ",$id";
			      }
			  }
			  if ( index($addedconstr,",") == -1){
			      $addedconstr = " $parent_tabname.$idname = $addedconstr ";
			  } else {
			      $addedconstr = " $parent_tabname.$idname IN ($addedconstr) ";
			  }
			  &print_debug("run_query","\tAdded constraints case-1 now $addedconstr");


			  # we can switch to super-index logic only if only
			  # one of them is used
			  #&_storagetypeSplitted($storageType)
			  if ( ($id = &_IsSuperIndex($keyw,$id)) ne "" && $addedconstr !~ m/OR / && $addedconstr !~ m/IN /){
			      &print_debug("run_query","\t$keyw is a super index");
			      # we shall drop this condition all together
			      push(@super_index,$id);

			  } else {
			      # Add a newly constructed keyword
			      #if ( index(join(" ",@constraint),$addedconstr) == -1){
			      $addedconstr = &_NormalizeAND_OR($addedconstr);
			      push (@constraint, $addedconstr);
			      #}
			  }


			  # Missing backward constraint for more-than-one table
			  # relation keyword. Adding it by hand for now (dirty)
			  # **** NEED TO BE CHANGED AND MADE AUTOMATIC AND USE LEVELS ***
			  # This does not happen if the field is specified
			  # as a returned keyword.
			  if ($parent_tabname eq "TriggerCompositions" ){
			      $addedconstr = " $parent_tabname.fileDataID = FileData.fileDataID";
			      push(@constraint,$addedconstr);
			  }



			  # Remove the condition - we already take care of it
			  &print_debug("run_query","\tDeleting $keyw=$valuset{$keyw}");
			  #delete $valuset{$keyw};
			  $threaded{$keyw}=1;

			  # But remember to add the parent table
			  # push (@connections, (connect_fields($keywords[0], $keyw)));
			  &print_debug("run_query","\tPushing $parent_tabname in the case-1 \@from list");
			  push (@from, $parent_tabname);
		      }
		  } else {
		      &print_debug("run_query","\tExecuted failed [".$FC::DBH->errstr."]");
		  }
		  $sth->finish();
	      }

	  } else {
	      # careful, $tabname may be null
	      &print_debug("run_query","Table for [$keyw]=$tabname  is NOT a dictionary ...");

	      if ($tabname eq "CollisionTypes"){
		  # A special case - the collision type
		  my $fieldname   = &_GetFieldName($keyw);
		  my $idname      = $tabname;
		  my $addedconstr = "";

		  chop($idname);
		  $idname = lcfirst($idname);
		  $idname.="ID";

		  # Find which table this one is connecting to
		  my $parent_tabname;
		  my @retcollisions;
		  foreach my $el (@datastruct){
		      if (($el =~ m/$idname/) > 0){
			  # We found the right row - get the table name
			  my ($stab,$fld);
			  ($stab,$parent_tabname,$fld) = split(",",$el);
		      }
		  }

		  (@retcollisions) = &get_collision_collection($valuset{$keyw});
		  &print_debug("run_query","Returned ".join(" ",(@retcollisions))." $#retcollisions\n");
		  if (( $#retcollisions+1 < 5) && ($#retcollisions+1 > 0)) {
		      # Create a new constraint
		      $addedconstr = " ( ";
		      foreach my $collisionid (@retcollisions){
			  if ($addedconstr ne " ( "){
			      $addedconstr .= " OR $parent_tabname.$idname = $collisionid ";
			  } else {
			      $addedconstr .= " $parent_tabname.$idname = $collisionid ";
			  }
			  &print_debug("run_query","Added constraints case-2 now $addedconstr");
		      }
		      $addedconstr .= " ) ";
		      # Add a newly constructed keyword
		      push (@constraint, $addedconstr);
		      #
		      # Remove the condition - we already take care of it
		      #delete $valuset{$keyw};
		      $threaded{$keyw}=1;

		      # But remember to add the the parent table
		      #	    push (@connections, (connect_fields($keywords[0], $keyw)));
		      &print_debug("run_query","\tPushing $parent_tabname in the case-2 \@from list");
		      push (@from, $parent_tabname);
		  }
	      }
	  }
      }
  }

  &print_debug("run_query","Pushing in FROM ".
	       join("/",@from)." ".&_GetTableName($keywords[0])." $#keywords ".
	       join("/",@keywords));
  push (@from, &_GetTableName($keywords[0]));

  for ($count=1; $count<$#keywords+1; $count++) {
      &print_debug("run_query","\t. Connecting $keywords[0] $keywords[$count] ".
		   &connect_fields($keywords[0], $keywords[$count]));

      push (@connections, (&connect_fields($keywords[0], $keywords[$count])));
      push (@from, &_GetTableName($keywords[$count]));
  }

  # Also add to the FROM array the tables for each set keyword
  foreach my $key (keys %valuset){
      if ( &_GetTableName($key) ne "" ){
	  &print_debug("run_query","Initiate resolve for $keywords[0] $key");

	  my(@l)= &connect_fields($keywords[0], $key);
	  my($t)= &_GetTableName($key);

	  &print_debug("run_query","\t. Connect ".join(" ",@l).
		       " From < $t for kwrd=$keywords[0] and key=$key");
	  push (@connections, @l);
	  push (@from, &_GetTableName($key));
      }
  }
  &print_debug("run_query","Connections to build the query (1): ".join(" ",@connections));


  if (defined $valuset{"simulation"}){
      push (@connections, (&connect_fields($keywords[0], "runnumber")));
      push (@from, "RunParams");
  }

  # Fill the table of connections
  my $connections = join(" ",(@connections));
  my @toquery;
  foreach my $el (sort {$a <=> $b} (split(" ",$connections))) {
    if ((not $toquery[$#toquery]) || ($toquery[$#toquery] != $el)) {
      push (@toquery, $el);
    }
  }
  &print_debug("run_query","Connections to build the query (2): ".join(" ",@toquery));



  # Get the select fields
  my @select;
  my @kstack;
  foreach $keyw (@keywords) {
      if (defined $functions{$keyw}){
	  &print_debug("run_query",">> Adding keyword: [$keyw] associated with operation [$functions{$keyw}] ");
	  if ($functions{$keyw} eq "grp"){
	      if (($grouping =~ m/GROUP BY/) == 0){
		  $grouping .= " GROUP BY ".&_GetTableName($keyw).".".&_GetFieldName($keyw)." ";
		  push (@select, &_GetTableName($keyw).".".&_GetFieldName($keyw));
	      } else {
		  &print_debug("run_query","\tSecond grouping ignored. Already $grouping");
	      }

	  } elsif ($functions{$keyw} eq "orda"){
	      $grouping .= " ORDER BY ".&_GetTableName($keyw).".".&_GetFieldName($keyw)." ASC ";
	      push (@select, &_GetTableName($keyw).".".&_GetFieldName($keyw));

	  } elsif ($functions{$keyw} eq "ordd"){
	      $grouping .= " ORDER BY ".&_GetTableName($keyw).".".&_GetFieldName($keyw)." DESC ";
	      push (@select, &_GetTableName($keyw).".".&_GetFieldName($keyw));

	  } else {
	      push (@select, uc($functions{$keyw})."(".&_GetTableName($keyw).".".&_GetFieldName($keyw).")");
	  }


      } elsif ($keyw eq "collision") {
	  &print_debug("run_query",">> Adding keyword: [$keyw] (special treatment) ");
	  my $tab = &_GetTableName($keyw);
	  push (@select, "CONCAT( $tab.firstParticle, $tab.secondParticle, $tab.collisionEnergy )");
	  push(@kstack,$keyw);


      } else {
	  &print_debug("run_query",">> Adding keyword: [$keyw] (nothing special) ");
	  push (@select, &_GetTableName($keyw).".".&_GetFieldName($keyw));
	  push (@kstack,$keyw);

      }
  }


  # Build the FROM and WHERE parts of the query
  # using thew connection list
  &print_debug("run_query","Summary at this stage");
  &print_debug("run_query","\tTables are so far          ".join("/",@from));
  &print_debug("run_query","\tToquery table contains idx ".join("/",@toquery));
  &print_debug("run_query","\tSelect  table contains val ".join("/",@select));
  &print_debug("run_query","\tGrouping is now            ".$grouping);


  my $where="";
  &print_debug("run_query","Calculating where");
  foreach my $el (@toquery) {
      my ($mtable, $stable, $field, $level) = split(",",$datastruct[$el]);
      &print_debug("run_query","\tFor $el Got $mtable/$stable/$field/$level from $datastruct[$el]");
      if (($mtable eq "FileData") && ($stable eq "FileLocations")){
	  next;
      }
      &print_debug("run_query","\tTable $mtable is not one of FileData/FileLocations");

      push (@from, $mtable);
      push (@from, $stable);
      if (not $where) {
	  $where .= " $mtable.$field = $stable.$field ";
      } else {
	  # user may request fields multiple times - do not
	  # allow repeats
	  if ( $where !~ /$mtable.$field = $stable.$field/){
	      $where .= " AND $mtable.$field = $stable.$field ";
	  }
      }
  }
  my $toquery = join(" ",(@from));
  &print_debug("run_query","<Table list 1> $toquery ; [$where]");



  # Get only the unique table names
  my @fromunique;
  foreach my $el (sort {$a cmp $b} split(" ",$toquery)) {
      #&print_debug("run_query","Adding $el");
      if ((not $fromunique[$#fromunique]) || ($fromunique[$#fromunique] ne $el)) {
	  push (@fromunique, $el);
      }
  }
  &print_debug("run_query","<Table list 2> $toquery ; [$where]");


  # Extra debug line
  #if($FC::DEBUG){
  #    &print_debug("run_query","--> order is --> ".join(" ",@select));
  #}


  # Get only the unique field names
  my(@selectunique,@kstackunique);

  for (my $ii=0 ; $ii <= $#select ; $ii++){
      my $el = $select[$ii];
      my $kel= $kstack[$ii];
      if ($FC::DEBUG > 0) {
	  &print_debug("run_query","Adding $el");
      }
      if ((not $selectunique[$#selectunique]) || ($selectunique[$#selectunique] ne $el)) {
	  push(@selectunique, $el);
	  push(@kstackunique, $kel)
      }
  }



  # See if we have any constaint parameters
  foreach $keyw (keys(%valuset)) {
      if ( defined($threaded{$keyw})) { next;}
      my $fromlist = join(" " , (@fromunique));
      my $tabname = &_GetTableName($keyw);



      if ((($fromlist =~ m/$tabname/) > 0) && ($tabname ne "")  ) {
	  my $fieldname = &_GetFieldName($keyw);
	  if ((($roundfields =~ m/$fieldname/) > 0) && (! defined $valuset{"noround"})){
	      my ($nround) = $roundfields =~ m/$fieldname,([0-9]*)/;
	      my ($roundv) = "ROUND($tabname.$fieldname, $nround) ".$operset{$keyw}." ";

	      if( $valuset{$keyw} =~ m/^\d+/){
		  $roundv .= $valuset{$keyw};
	      } else {
		  $roundv  .= "'$valuset{$keyw}'";
	      }
	      push(@constraint,$roundv);

	  }  elsif ($operset{$keyw} eq "~"){
	      push( @constraint, &_TreatLOps("$tabname.$fieldname",
					     "LIKE",
					     $valuset{$keyw},
					     3));
	      #push( @constraint, "$tabname.$fieldname LIKE '%".$valuset{$keyw}."%'" );

	  }  elsif ($operset{$keyw} eq "!~"){
	      push( @constraint, &_TreatLOps("$tabname.$fieldname",
					     "NOT LIKE",
					     $valuset{$keyw},
					     3));
	      #push( @constraint, "$tabname.$fieldname NOT LIKE '%".$valuset{$keyw}."%'" );

	  }  elsif ($operset{$keyw} eq "[]"){
	      push( @constraint, &_TreatLOps("$tabname.$fieldname",
					     "BETWEEN",
					     $valuset{$keyw},
					     4));

	  }  elsif ($operset{$keyw} eq "]["){
	      push( @constraint, &_TreatLOps("$tabname.$fieldname",
					     "NOT BETWEEN",
					     $valuset{$keyw},
					     4));
	  } elsif ($operset{$keyw} eq "%"){
	      push( @constraint, &_TreatLOps("$tabname.$fieldname",
					     "MOD",
					     $valuset{$keyw},
					     4));

	  } elsif ($operset{$keyw} eq "%%"){
	      push( @constraint, &_TreatLOps("$tabname.$fieldname",
					     "NOT MOD",
					     $valuset{$keyw},
					     4));

	  } else {
	      push(@constraint,&_TreatLOps("$tabname.$fieldname",
					   $operset{$keyw},
					   $valuset{$keyw},
					   (&get_field_type($keyw) eq "text")?2:undef));
	  }
      }
  }


  #
  # This drastic change between simulation and real data prevents
  # the definition of a consistent logical name. Logical name must
  # depend on  $valuset{"simulation"}
  #
  if (defined $valuset{"simulation"}){
      if ($valuset{"simulation"} eq "1"){
	  push ( @constraint, "RunParams.simulationParamsID IS NOT NULL");
      } else {
	  push ( @constraint, "RunParams.simulationParamsID IS NULL");
      }
  }


  # Check to see if we are getting info from the FileLocations table
  # if so, and "all" keyword is not set - get only the records
  # with non-zero availability
  my $floc = join(" ",(@fromunique)) =~ m/FileLocations/;


  &print_debug("run_query","Checking for FileLocations ".(join(" ",@fromunique))." $floc");


  if ( $floc > 0 && defined($valuset{"all"}) ){
      if ( $valuset{"all"} == 0 && ! defined($valuset{"available"}) ){
	  push ( @constraint, "FileLocations.availability > 0");
	  &print_debug("run_query","Detecting we do NOT have 'availability' specified and 'all' is null -> adding availability > 0");
      }
  }


  my $constraint = join(" AND ",@constraint);

  # Build the actual query string
  my $sqlquery;

  if ( $FC::FORCESQLCACHE ){
      $sqlquery  = &_SQLCACHED_SELECT();
  } else {
      $sqlquery  = "SELECT ";
      $sqlquery .= "SQL_BUFFER_RESULT " if ($FC::OPTIMIZE);
  }

  if (! defined($valuset{"nounique"}) ){  $valuset{"nounique"} = 0;}
  if (! $valuset{"nounique"} ){           $sqlquery .= " DISTINCT ";}


  # An ugly hack to return FileLocationID from within the module
  if (((join(" ",(@fromunique)) =~ m/FileLocations/) > 0) && defined($flkey) ){
      $sqlquery .= " FileLocations.FileLocationID , ";
  }



  # Ugly hack to test the natural join
  # (but it's the only way to treat this special case)
  my $fdat = join(" ",(@fromunique)) =~ m/FileData/;
  &print_debug("run_query","Before the natural: ".join(" ",@fromunique));
  if (($floc > 0) && ($fdat >0)){
      my $i;
      #Find the FileLocations and FileData and delete it
      for ($i=0; $i <= $#fromunique; ){
	  #&print_debug("run_query","Considering splicing of $i $#fromunique [$fromunique[$i]]");
	  if ($fromunique[$i] eq "FileLocations"){
	      #&print_debug("run_query","Splicing FileLocations [$fromunique[$i]]");
	      splice(@fromunique, $i, 1);
	  } elsif ($fromunique[$i] eq "FileData"){
	      #&print_debug("run_query","Splicing FileData $#fromunique [$fromunique[$i]]");
	      splice(@fromunique, $i, 1);
	  } else {
	      $i++;
	  }
      }
      # Add a natural join instead
      push (@fromunique, "FileData NATURAL JOIN FileLocations");
      #push (@fromunique, "FileData JOIN FileLocations");
  }
  &print_debug("run_query","After the natural: ".join(" ",@fromunique));




  &print_debug("run_query","Selector ".join(" , ",@selectunique)." FROM ".join(" , ",(@fromunique)));




  $sqlquery .= join(" , ",(@selectunique))." FROM ".join(" , ",(@fromunique));

  # Small user stupidity check + a useful check on unsetting the limit
  if ( $#selectunique >= 1             &&
       ($sqlquery =~ /SUM\(.*\)/ ||
	$sqlquery =~ /MIN\(.*\)/ ||
	$sqlquery =~ /MAX\(.*\)/ ||
	$sqlquery =~ /COUNT\(.*\)/ ) ){
      if ( $grouping !~ /GROUP BY/){
	  $sqlquery =~ m/( .*\()(.*)/;
	  &print_message("run_query()","$1) without GRP() will not lead to any result");
	  return;
      } else {
	  $using_global_func = 1;
      }
  }



  if ( $where ne "" ) {
      &print_debug("run_query","where clause [$where] constraint [$constraint]");
      $sqlquery .=" WHERE $where";
      if ($constraint ne "") {
	  $sqlquery .= " AND $constraint";
      }
  } elsif ($constraint ne "") {
      &print_debug("run_query","no where clause but constrained");
      $sqlquery .= " WHERE $constraint";
  }
  $sqlquery .= $grouping;




    #
    # Sort out if a limiting number of records or range
    # has been asked
    #
    my ($offset, $limit)=(undef,undef);
    if (defined $valuset{"startrecord"}){
	$offset = $valuset{"startrecord"};
    } else {
	$offset = 0;
    }
    if (defined $valuset{"limit"}){
	$limit = $valuset{"limit"};
	if($limit <= 0){
	    $limit    = undef;
	}
    } else {
	if ( ! $using_global_func){ $limit = 100;}
    }

    # Verify if cache querry is disabled/enabled
    # there is no need to test for the "cache" option since
    # the default is 1
    if ( defined($valuset{"cache"}) ){
	$FC::USECACHE = $valuset{"cache"};
    }



    # Since we compare the id, do we need to order??
    # DB should come with ordered IDs but some evidence this is
    # not the case ...
    if ( $rlimit != 0){
	$sqlquery .= " ORDER BY FileData.fileDataID";
    }
    # limit and rlimit are exclusive if we supress the second
    # condition
    if ( defined($limit) && $rlimit == 0 ){
	if ( $limit < $OPTIMLIMIT && $FC::OPTIMIZE){
	    # remove optimization if a small number of rows is requested
	    # limit is arbitrary
	    $sqlquery =~ s/SQL_BUFFER_RESULT //;
	}
	$sqlquery .= " LIMIT $offset, $limit";
    }



    # We can replace FileLocations here if needed
    if ( $#super_index != -1 ){
	my($el,$tab,$rep);
	foreach $el (@super_index){
	    ($tab,$rep) = split(":",$el);
	    &print_debug("run_query","Super index was previously detected for $tab -> $rep");
	    $sqlquery =~ s/$tab/$rep/g;
	}
    }



    &print_debug("run_query","Using query: $sqlquery");

    #+
    # Cache handling
    #-
    # TODO: limit 0 could always be used for any other limit
    #
    # in rlimit, we benefit from caching
    my $qhash=Digest::MD5->new();
    my $md5=$qhash->add($sqlquery.$delimeter)->hexdigest();
    my $FHDL=undef;
    &print_debug("run_query","Query digest is [$md5] rlimit=$rlimit");

    use FileHandle;

    umask(0000);
    my($cachedir) = "/tmp/FC_cache_".(getpwuid($<))[3];
    if ( ! -d $cachedir ){
	&print_debug("run_query","Creating $cachedir");
	if ( ! mkdir($cachedir,0775) ){  $cachedir = "tmp";}
    }

    my($f)="$cachedir/$md5.dat";
    my($docache)=$FC::USECACHE;
    my($age)=0;

    &print_debug("run_query","Use of cache is ".
		 ($FC::USECACHE?"enabled":"disabled"));

    # check age of file if exists
    if ( -e $f){
	my(@stat);
	@stat=stat($f);
	if ($#stat != -1){
	    $age=time()-$stat[10];

	    # possibly handle expiration
	    if ( $age > $FC::CACHELIFE || -z $f ){
		# be sure that regardless of whether the cache file can be
		# deflted or not, we disable cache after expiration
		$docache = 0;
		unlink($f);
	    }
	}
    }

    &print_debug("run_query","Cache age $age (lifetime=$FC::CACHELIFE, docache=$docache)");


    if ( -e $f && $docache){
	#+
	# WILL RETURN FROM CACHE
	#-
	my($line);
	my($previd,$curid,$idcnt);
	my(%result,@cols);
	$idcnt = 0;
	$count = 0;

	&print_debug("run_query","Cache file exists - reading from it");
	if ( defined($FHDL = FileHandle->new("$f")) ){
	    &print_debug("run_query","File is opened, reading line rlimit=$rlimit idpushed=$idpushed");
	    while ( defined($line = $FHDL->getline() ) ){
		chomp($line);

		next if ( $line =~ m/^\#/);       # skip comment lines

		# now split by separator
		@cols = split($delimeter,$line);

		#&print_debug("run_query","Read one line");

		if ( $rlimit != 0){
		    # COMMON CODE LOGIC AAB
		    if ( $idpushed ){
			$curid = shift(@cols);
		    } else {
			$curid = $cols[$fdidpos];
		    }
		    if ( $curid ne $previd){
			$previd = $curid;
			$idcnt++;
			# in this case, we do not need to continue
			# because it is cached
			last if ( $idcnt > $rlimit+$offset);
		    }
		    if ( $offset < $idcnt ){
			#$result[$count++] = join($delimeter,@cols);
			$result{join($delimeter,@cols)}=$count++;
		    }
		} else {
		    # since the delimeter is part of the hash, we can read line as-is
		    #push(@result,$line);
		    $result{$line}=$count++;
		}
	    }
	    &print_debug("run_query","Will return values from cache right away and skip query");
	    $FC::CACHE_USED=1;
	    return sort { $result{$a} <=> $result{$b} } keys %result;
	}
	#
	# note -> if we fail opening cache, it will revert to do a query below
	#

    } else {
	# no cache, prepare a cache file
	$FC::CACHE_USED=0;
	if ( defined($FHDL = FileHandle->new(">$f")) ){
	    chmod(0775,$f);
	}
    }

    #+
    # WILL DO A SQL QUERY
    #-
    my ($sth)= $FC::DBH->prepare($sqlquery);
    if ( ! $sth ){ # we failed to prepare, there is a no-go
	&print_debug("run_query","Failed to prepare [$sqlquery]");
	return;

    } else {        # sth prepare was fine
	my (%result,$res,$rescount);
	my (@cols);
	my ($success)=0;

	# start timer
	my($ts,$tf);
	$ts = time(); &print_debug("run_query","START Time DBRef:$FC::DBRef ".localtime($ts));

	# try this
	&print_debug("run_query","Raising ALRM - timer expires in ".($FC::TIMEOUT/60)." mnts");
	eval {
	    # RAISE ALARM
	    local $SIG{ALRM} = sub { die "ALARM\n"};
	    alarm($FC::TIMEOUT);
	    # EXECUTE
	    $success = $sth->execute();
	    # CANCEL ALARM
	    alarm(0);
	};
	if ($@){
	    $sth->finish();
	    &print_message("run_query","ALRM signal received, we timed out after ".($FC::TIMEOUT/60)." mnts");
	    return 0;
	} else {
	    alarm(0);
	    &print_debug("run_query","ALRM cancelled - success");
	}

	$count = 0;
	if ( $success ){
	    my($previd,$curid,$idcnt);
	    my($rkey);

	    $idcnt = 0;

	    &print_debug("run_query","rlimit=$rlimit limit=$limit - fetching");

	    while ( @cols = $sth->fetchrow_array() ) {
		# if field is empty, fetchrow_array() returns undef()
		# fix it by empty string instead.
		for ($i=0 ; $i <= $#cols ; $i++){
		    # do not return undefined field, set to null-string
		    if( ! defined($cols[$i]) ){ $cols[$i] = "";}
		    # transformation in read here
		    next if ( ! defined($rkey = $kstackunique[$i]) );
		    if ( defined($ktransform{$rkey}) ){
			&print_debug("run_query","We will transform $kstackunique[$i] = $cols[$i]\n");
			eval("\$cols[$i] = ".(split(";",$ktransform{$kstackunique[$i]}))[0]."($cols[$i]);");
		    }
		}


		# We are not done ...
		foreach $flkey (@setkeys){
		    $res = "\@cols = $keyset{$flkey}";
		    foreach my $el (@cols){  $res .= ",\"$el\"";}
		    $res .= ");";
		    &print_debug("run_query","eval() $res");
		    eval("\@cols = $res;");
		}

		# need to print prior <-- true only for previous for old logic
		# print $FHDL join($delimeter,@cols)."\n" if ( defined($FHDL) );

		if ( $rlimit > 0 ){
		    # rlimit mode

		    # COMMON CODE LOGIC AAB
		    if ( $idpushed ){
			$curid = shift(@cols);
		    } else {
			$curid = $cols[$fdidpos];
		    }
		    # &print_debug("run_query","[$curid] [$previd]");
		    if ( $curid ne $previd){
			$previd = $curid;
			$idcnt++;
			# last if ( $idcnt > $rlimit+$offset);
		    }
		    # if ( $offset < $idcnt ){
		    if ( ($offset < $idcnt) && ( $idcnt <= $rlimit+$offset) ){
			#$result[$count++] = join($delimeter,@cols);
			$rkey = join($delimeter,@cols);
			if ( ! defined($result{$rkey}) ){
			    $result{$rkey}=$count;
			    print $FHDL "$rkey\n" if ( defined($FHDL) );
			}
			$count++;
		    }
		} else {
		    # Normal mode
		    #$result[$count++] = join($delimeter,@cols);
		    $rkey = join($delimeter,@cols);
		    if ( ! defined($result{$rkey}) ){
			$result{$rkey}=$count;
			print $FHDL "$rkey\n" if ( defined($FHDL) );
		    }
		    $count++;
		}
	    }

	} else {
	    &print_debug("run_query","ERROR ".$FC::DBH->err." >> ".$FC::DBH->errstr);
	}
	$sth->finish();

	# disable alarm
	alarm(0);

	# end-timer
	$tf  = time(); &print_debug("run_query","END Time DBRef:$FC::DBRef ".localtime($tf));
	$tf -= $ts;    &print_debug("run_query","DELTA Time DBRef:$FC::DBRef $tf nfiles=$count");

	# close file
	if ( defined($FHDL) ){
	    print $FHDL "# count=$count delta=$tf usage=$user_usage\n";
	    close($FHDL) ;
	    # If too small or not expensive, delete to keep number of files under control
	    # Note: spiders checks nearly all files individually ...
	    if ($count <= 1 || $tf <= 2){
		&print_debug("run_query","(count=$count,delta=$tf) <= (1,2) - delete cache $f");
		unlink($f);
	    }
	}

	# now return results sorted
	return (sort { $result{$a} <=> $result{$b} } keys %result);

    }
}

sub was_file_cache_used
{
    if ($_[0] =~ m/FileCatalog/) {
	shift @_;
    }
    return $FC::CACHE_USED;
}

# 2 examples of eval() routines
sub _logical_name
{
    my($start,$num,@cols)=@_;
    my($lstr);

    $lstr    = $cols[$start]."&&".join("&&",splice(@cols,$start+1,$num));
    $cols[$start] = $lstr;
    @cols;
}

#sub _md5_name
#{
#    my($start,$num,@cols)=@_;
#    my($lstr,$md5);
#
#    $md5     =  Digest::MD5->new;
#    $md5->add($cols[$start]."&&".join("&&",splice(@cols,$start+1,$num)));
#    $lstr    = $md5->hexdigest();
#
#    $cols[$start] = $lstr;
#    @cols;
#}

sub _logical_path { return &_logical_name(@_);}
#sub _md5_path     { return &_md5_name(@_);}



#
# Routine to treat Logical, Bitwise, string vs integer
# values in one does it all.
# Flag is used as follow
#   1  get the $val 'as-is' i.e. do not check for string/number
#   2  explicit string value 'as-is'
#   3  explicit APPROXIMATE string matching
#
# Note thate && makes no sens whatsoever. Left for
# documentary / test purposes.
#
sub _TreatLOps
{
    my($fldnam,$op,$ival,$flag)=@_;
    my(@Val,$val,$qq,$connect);

    $flag = 0 if ( ! defined($flag) );


    if ( index($ival,"||") != -1 ){
	@Val = split(/\|\|/,$ival);
	$connect = "OR";

    } elsif ( index($ival,"&&") != -1 ){
	@Val = split("&&",$ival);
	$connect = "AND";

    } else {
	# Yet another special treatment
	if ( $op =~ /BETWEEN/){
	    @Val = split("-",$ival);
	    if ($#Val > 1){
		&die_message("_TreatLOps","Syntax for a range is min-max");
	    }
	    $ival = "'".$Val[0]."' and '".$Val[1]."'";
	    undef(@Val);

	} elsif ( $op =~ /NOT MOD/){
	    # Don't worry about the logic inversion
	    # which is a small detail of how MySQL deals
	    # with it vs how our logic is.
	    $ival  = "MOD($fldnam,$ival)";
	    $fldnam= "";
	    $op    = "";

	} elsif ( $op =~ /MOD/){
	    $ival  = "NOT MOD($fldnam,$ival)";
	    $fldnam= "";
	    $op    = "";
	}

	push(@Val,$ival);
	$connect = "";
    }

    #print "$ival -> $#Val\n";

    # initialize
    foreach $val (@Val){
	#print "Start $val\n";
	if($flag == 0){
	    if ($val !~ m/^\d+/){
		$val = "'$val'";
	    }
	} elsif ($flag == 2){
	    if ($val eq "NULL"){
		$connect = "";
		$op      = "IS";
		$val     = "NULL";
		$qq      = "";
	    } else {
		$val = "'$val'";
	    }
	} elsif ($flag == 3){
	    $val = "'%$val%'";
	} elsif ($flag == 4){
	    # By pass for special cases.
	    # Do nothing
	} else {
	    &die_message("_TreatLOps","Internal error ; unknown flag $flag");
	}
	#print "Now   $val\n";

	if ( defined($qq) ){
	    $qq .= " $connect $fldnam $op $val";
	} else {
	    $qq = "$fldnam $op $val";
	}
    }

    # OR and AND should be re-grouped by ()
    if ($#Val > 0 || $flag == 4){
	&print_debug("_TreatLOps","re-group");
	$qq = &_NormalizeAND_OR($qq,1);
	#$qq = "( $qq )";
    }
    #&print_debug("_TreatLOps","Will return $qq");
    $qq;
}



#============================================
# deletes the record that matches the current
# context. Actually calls run_query() so it is
# easier to see what we select
#
# First it deletes it from the file
# locations. If this makes the current file
# data have no location, it deletes the file
# data too.
#
# Returns:
#  1 if delete was successfull
#  0 if delete failed
#
# Argument : 1/0 doit or not, default doit=1
#
sub delete_records {
  # first delete the file location
  my @deletes;

  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  }
  if( ! defined($FC::DBH) ){
      &print_message("delete_record","Not connected");
      return 0;
  }


  my $doit = shift @_;
  if( ! defined($doit) ){  $doit = 0;}

  # Since the context is defined, we will rely on run_query()
  # to return the list of records to be deleted. This will be
  # less programming support and easier check of what we will
  # be deleting btw (one can run a regular query first and delete
  # after ensuring the same things will be removed) ...
  my $delim = &get_delimeter();
  my @all   = &run_query("FileCatalog","flid","rfdid","path","filename");

  &set_delimeter("::");


  my($count,$cmd);
  my($sth,$sth2,$stq);
  my(@ids,$status,$rc);
  my($rows);

  $status = 0;
  foreach (@all){
      # We now have a pair flid/rfdid. Only flid can be deleted
      # rfdid is the logical grouping and may be associated with
      # more than one location.
      @ids = split("::",$_);

      $cmd = "DELETE LOW_PRIORITY FROM FileLocations WHERE fileLocationID=$ids[0]";
      #&print_debug("run_query",$cmd);
      $sth = $FC::DBH->prepare( $cmd );

      if( $doit ){
	  if( $DELAY ){
	      $rc = 1;
	      push(@DCMD,$cmd);
	  } else {
	      $rc = $sth->execute();
	  }
      } else {
	  &print_message("delete_record","id=$ids[0] from FileLocation would be deleted");
	  $rc = 1;
      }

      if ( $rc ){
	  &print_debug("run_query","FileLocation ID=$ids[0] operation done. Checking FileData");

	  $cmd  = "SELECT FileLocations.fileLocationID from FileLocations, FileData ".
		  " WHERE FileLocations.fileDataID = FileData.fileDataID AND FileData.fileDataID = $ids[1] ";
	  $stq     = $FC::DBH->prepare( $cmd );


	  if ( ! $stq->execute() ){
	      &print_debug("run_query","Execution failed [$cmd]");
	  }

	  $rows = $stq->rows;
	  $stq->finish();

	  if ($rows == 0 || ($DELAY && $rows == 1) ){
	      # This file data has no file "other" locations
	      $cmd  = "DELETE LOW_PRIORITY FROM FileData WHERE fileDataID = $ids[1]";
	      $sth2 = $FC::DBH->prepare($cmd);

	      if ($doit){
		  if ( $DELAY ){
		      push(@DCMD,$cmd);
		  } else {
		      $sth2->execute();
		  }
		  &del_trigger_composition($ids[1],$doit);
	      } else {
		  &print_message("delete_record","id=$ids[1] from FileData would be deleted");
	      }
	      $sth2->finish();
	  }

      } else {
	  &print_debug("run_query","Delete failed ".$FC::DBH->err." ".$FC::DBH->errstr);
      }
      $sth->finish();
  }
  &set_delimeter($delim);

  return @all;
}



#============================================
# Bootstraps a table - meaning it checks if all
# the records in this table are connected to some
# child table
#
# Params:
#   keyword - keword from the table, which is to be checked
#   dodelete - set to 1 to automaticaly delete the offending records
#
# Returns
#  List of records that are not connected or 0 if there were errors
#  or no unconnected records
#
# This routine is the top routine
#

# Backward compatible function will be soon removed.
#
sub bootstrap_trgc { &print_message("bootstrap_trgc","Obsolete routine called. Use bootstrap() instead");}
sub bootstrap_sim  { &print_message("bootstrap_sim", "Obsolete routine called. Use bootstrap() instead");}
sub bootstrap_data { &print_message("bootstrap_data","Obsolete routine called. Use bootstrap() instead");}


sub bootstrap {
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}

    if( ! defined($FC::DBH) ){
	&print_message("bootstrap","Not connected");
	return 0;
    }

    my($keyword, $delete) = (@_);

    my($table);


    $table = &_GetTableName($keyword);
    if ($table eq ""){ return 0; }
    &print_debug("bootstrap","$keyword in table $table");

    # Now, pipe it to other routines
    if ($table eq "FileData" || $table eq "FileLocations"){
	return &_bootstrap_data($keyword, $delete);

    } elsif ($table eq "TriggerWords" || $table eq "TriggerCompositions"){
	return &_bootstrap_2levels("TriggerCompositions","TriggerWords","FileData",
				   "fileDataID",         "triggerWordID",
				   $delete);


    } elsif ($table eq "SimulationParams" || $table eq "EventGenerators"){
	# return &bootstrap_sim($delete);
	return &_bootstrap_2levels("SimulationParams","EventGenerators","RunParams",
				   "simulationParamsID","eventGeneratorID",
				   $delete);

    } else {
	return &_bootstrap_general($keyword, $table, $delete);
    }
}


#
# Bootstrap all no-special tables (dictionaries, FileData and
# FileLocations).
#
sub _bootstrap_general
{
    my($keyword, $table, $delete) = @_;

    my($refcount);
    my($childtable, $linkfield);
    my($mtable, $ctable, $lfield);
    my($dcquery,$stq);
    my(@rows, $id, $el);
    my($dcdelete,$stfdd);

    # Check if this really is a dictionary table
    $refcount = 0;
    if ($table eq "RunParams"){
	$childtable = "FileData";
	$linkfield  = "RunParamID";
    } else {
	foreach $el (@datastruct){
	    #print "$el\n";
	    ($mtable, $ctable, $lfield) = split(",",$el);
	    if ($ctable eq $table){
		# This table is referencing another one - it is not a dictionary!
		&print_message("bootstrap","$table is not a dictionary table !");
		return 0;
	    }
	    if ($mtable eq $table){
		$childtable = $ctable;
		$linkfield = $lfield;
		$refcount++;
	    }
	}
	if ($refcount != 1){
	    # This table is not referenced by any other table or referenced
	    # by more than one - it is not a proper dictionary
	    &print_message("bootstrap","$table is not a dictionary table !");
	    return 0;
	}
    }


    $dcquery = "SELECT ".($FC::OPTIMIZE?"SQL_BUFFER_RESULT ":"")."$table.$linkfield FROM $table LEFT OUTER JOIN $childtable ON $table.$linkfield = $childtable.$linkfield WHERE $childtable.$linkfield IS NULL";

    $stq = $FC::DBH->prepare( $dcquery );
    if( ! $stq ){
	&print_debug("_bootstrap_general","Failed to prepare [$dcquery]");
	return 0;
    }

    &print_debug("_bootstrap_general","Running [$dcquery]");
    if ( $stq->execute() ){
	$stq->bind_columns( \$id );

	while ( $stq->fetch() ) { push ( @rows, $id );}
	if ($delete == 1 && $#rows != -1){
	    # We do a bootstapping with delete
	    $dcdelete = "DELETE LOW_PRIORITY FROM $table WHERE $linkfield IN (".join(" , ",(@rows)).")";
	    if ( $DELAY ){
		push(@DCMD,$dcdelete);
	    } else {
		&print_debug("_bootstrap_general","Executing $dcdelete");

		$stfdd = $FC::DBH->prepare($dcdelete);
		if ($stfdd){
		    $stfdd->execute();
		} else {
		    &print_debug("_bootstrap_general",
				 "Failed to prepare [$dcdelete]",
				 " Record in $table will not be deleted");
		}
		$stfdd->finish();
	    }
	}
	$stq->finish();
	return (@rows);
    } else {
	$stq->finish();
	return 0;
    }
}

#
# Bootstraps 2 level tables for example
#  Bootstraps TrigerCompositions and TriggerWords
#  Bootstraps SimulationParams and EventGenerators
#

sub _bootstrap_2levels {
    my($tab1,$tab2,$tab3,$field1,$field2,$delete) = @_;

    my($cmd1,$cmd2,$sth1,$sth2);
    my(@rows,@rows1,@rows2,$id);
    my($p1,$p2);
    my($cmdd,$sthd);

    $cmd1  = "SELECT $tab1.$field1 FROM $tab1 LEFT OUTER JOIN $tab3 ON $tab1.$field1 = $tab3.$field1 WHERE $tab3.$field1 IS NULL";
    $cmd2  = "SELECT $tab2.$field2 FROM $tab2 LEFT OUTER JOIN $tab1 ON $tab2.$field2 = $tab1.$field2 WHERE $tab1.$field2 IS NULL";
    $sth1 = $FC::DBH->prepare( $cmd1 );
    $sth2 = $FC::DBH->prepare( $cmd2 );

    if( ! $sth1 || ! $sth2 ){ &die_message("_bootstrap_2levels"," Failed to prepare statements");}

    $p1 = $tab1;  $p1 =~ s/[a-z]*//g;
    $p2 = $tab2;  $p2 =~ s/[a-z]*//g;


    #
    # Run the first sth on $tab1 since it may leave further
    # holes sth2 would pick up.
    #
    &print_debug("_bootstrap_2levels","Running [$cmd1]");
    if ( ! $sth1->execute() ){  &die_message("bootstrap_2levels","Execute 1 failed");}

    $sth1->bind_columns( \$id );

    while ( $sth1->fetch() ) {  push ( @rows1, $id );}

    if ($delete == 1 && $#rows1 != -1){
	$cmdd = "DELETE LOW_PRIORITY FROM $tab1 WHERE $field1 IN (".join(" , ",(@rows1)).")";
	if ( $DELAY ){
	    push(@DCMD,$cmdd);
	} else {
	    &print_debug("_bootstrap_2levels","Executing $cmdd");
	    $sthd = $FC::DBH->prepare($cmdd);
	    if ($sthd){
		$sthd->execute();
	    } else {
		&print_debug("_bootstrap_2levels",
			     "Failed to prepare [$cmdd]",
			     " Records in $tab1 will not be deleted");
	    }
	    $sthd->finish();
	}
    }
    $sth1->finish();


    &print_debug("_bootstrap_2levels","Running [$cmd2]");
    if ( ! $sth2->execute() ){  &die_message("bootstrap_2levels","Execute 2 failed");}

    $sth2->bind_columns( \$id );

    while ( $sth2->fetch() ) {  push ( @rows2, $id );}

    if ($delete == 1 && $#rows2 != -1){
	$cmdd = "DELETE LOW_PRIORITY FROM $tab2 WHERE $field2 IN (".join(" , ",(@rows2)).")";
	if ( $DELAY ){
	    push(@DCMD,$cmdd);
	} else {
	    &print_debug("_bootstrap_2levels","Executing [$cmdd]");
	    $sthd = $FC::DBH->prepare($cmdd);
	    if ($sthd){
		$sthd->execute();
	    } else {
		&print_debug("_bootstrap_2levels",
			     "Failed to prepare [$cmdd]",
			     " Records in $tab2 will not be deleted");
	    }
	    $sthd->finish();
	}
    }
    $sth2->finish();

    # Return value
    if ( $#rows1 != -1){ foreach $id (@rows1){ push(@rows,"$p1-$id");}}
    if ( $#rows2 != -1){ foreach $id (@rows2){ push(@rows,"$p2-$id");}}

    if ( $#rows != -1){ return @rows; }
    else {              return 0;}

}


sub _bootstrap_data
{
    if ($_[0] =~ m/FileCatalog/) { shift @_;}

    if( ! defined($FC::DBH) ){
	&print_message("bootstrap_data","Not connected");
	return 0;
    }

    my ($keyword, $delete) = (@_);
    my $table = &_GetTableName($keyword);

    if (($table ne "FileData") && ($table ne "FileLocations")){
	&print_message("bootstrap_data","Wrong usage of routine. Use bootstrap()");
	return 0;
    }

  my $dcquery;
  if ($table eq "FileData")
    {
      $dcquery = "SELECT FileData.fileDataID FROM FileData LEFT OUTER JOIN FileLocations ON FileData.fileDataID = FileLocations.fileDataID WHERE FileLocations.fileLocationID IS NULL";
    }
  elsif ($table eq "FileLocations")
    {
      $dcquery = "SELECT FileLocations.fileLocationID FROM FileLocations LEFT OUTER JOIN FileData ON FileData.fileDataID = FileLocations.fileDataID WHERE FileData.fileDataID IS NULL";
    }

  my $stq;
  $stq = $FC::DBH->prepare( $dcquery );
  if( ! $stq ){
      &print_debug("_bootstrap_data","Failed to prepare [$dcquery]");
      return 0;
  }
  &print_debug("_bootstrap_data","Running [$dcquery]");

  if ( $stq->execute() ){
      my @rows;
      my( $id );
      $stq->bind_columns( \$id );

      while ( $stq->fetch() ) {
	push ( @rows, $id );
      }
      if ($delete == 1 && $#rows != -1)
      {
	  # We do a bootstapping with delete
	  my $dcdelete;
	  if ($table eq "FileData")
	  {
	      $dcdelete = "DELETE LOW_PRIORITY FROM $table WHERE $table.fileDataID IN (".join(" , ",(@rows)).")";
	  }
	  elsif ($table eq "FileLocations")
	  {
	      $dcdelete = "DELETE LOW_PRIORITY FROM $table WHERE $table.fileLocationID IN (".join(" , ",(@rows)).")";
	  }
	  if ( $DELAY ){
	      push(@DCMD,$dcdelete);
	  } else {
	      if ($FC::DEBUG > 0) { &print_debug("_bootstrap_data","Executing $dcdelete"); }
	      my $stfdd = $FC::DBH->prepare($dcdelete);
	      if ($stfdd){
		  $stfdd->execute();
	      } else {
		  &print_debug("_bootstrap_data",
			       "Failed to prepare [$dcdelete]",
			       " Records in $table will not be deleted");
	      }
	      $stfdd->finish();
	  }
      }
      $stq->finish();
      return (@rows);
  }
  $stq->finish();
  return 0;
}



#============================================
# Updates the field coresponding to a given keyword
# with a new value, replaces the value in the current
# context.The value of the keyword to be modified,
# MUST appear in a previous set_context() statement.
# This is a limitation which has been chosen in
# order to also treat changing values in dictionaries.
#
# Params:
# keyword - the keyword which data is to be updated
# value   - new value that should be put into the database
#           instead of the current one
# doit    - an extra non-mandatory value 0/1
#
# Returns:
#  1 if update was successfull
#  0 if delete failed
sub update_record {
  if ($_[0] =~ m/FileCatalog/) {
    my $self = shift;
  }
  if( ! defined($FC::DBH) ){
      &print_message("update_record","Not connected");
      return 0;
  }

  my ($ukeyword, $newvalue, $doit) = (@_);
  my @updates;
  my $xcond;

  my $utable = &_GetTableName($ukeyword);
  my $ufield = &_GetFieldName($ukeyword);

  # There is bunch of exclusion preventing catastrophe
  # ALL Id's associated to their main tables should be
  # eliminated
  if ( $ufield =~ m/(.*)(ID)/ ){
      my $idnm=$1;
      if ( index(lc($utable),lc($idnm)) != -1 ){
  	  &print_message("update_record","Changing $ufield in $utable is not allowed\n");
  	  # btw, if you do this, you are screwed big time ... because you
  	  # lose the cross table associations.
  	  return 0;
      }
  }


  if( ! defined($doit) ){  $doit = 1;}

  foreach my $key (keys %keywrds){
      my $field = &_GetFieldName($key);
      my $table = &_GetTableName($key);

      # grab keywords which belongs to the same table
      # This will be used as the selection WHERE clause.
      # The ufield is excluded because we use it by default
      # in the SET xxx= WHERE xxx= as an extra MANDATORY
      # clause.
      if (($table eq $utable) && ($field ne $ufield)){
	  if (defined($valuset{$key})){
	      if (&get_field_type($key) eq "text" ){
		  if ( $valuset{$key} eq "NULL"){
		      push (@updates, "$table.$field IS NULL");
		  } else {
		      push (@updates, "$table.$field = '".$valuset{$key}."'");
		  }
	      } else {
		  push (@updates, "$table.$field = ".$valuset{$key});
	      }
	  }
      } else {
	  # Otherkeywords may make the context more specific if there is
	  # a relation. The only relation we will support if through
	  # the fdid and tables containing this field.
	  # **** NEED TO WRITE A ROUTINE SORTING OUT THE RELATION ****
	  if ( $key eq "fdid" &&
	      ( $utable eq "TriggerCompositions" ||
		$utable eq "FileLocations") ){
	      if ( defined($valuset{$key}) ){
		  #print "Found fdid = $valuset{$key}\n";
		  $xcond = " $utable.fileDataID = $valuset{$key}" if ( ! defined($xcond) );
	      }
	  }

      }
  }
  if ( defined($xcond) ){ push(@updates,$xcond);}
  my $whereclause="";
  $whereclause = join(" AND ",(@updates)) if ( $#updates != -1);



  # Some extra debug here to be sure
  #&print_debug("update_record",
  #"$ukeyword,$utable.$ufield xcond=$xcond whereclause=$whereclause");


  if ($utable eq ""){
      &print_debug("update_record",
		   "ERROR: $ukeyword does not have an associated table",
		   "Cannot update");
      return 0;
  }

  # Prevent disaster by checking this out
  my ($qupdate,$qselect);
  if (&get_field_type($ukeyword) eq "text"){
      $qselect = "SELECT $utable.$ufield FROM $utable WHERE $utable.$ufield = '$newvalue'";
      $qupdate = "UPDATE $utable SET $utable.$ufield = '$newvalue' ";
      if( defined($valuset{$ukeyword}) ){
	  if ( $valuset{$ukeyword} eq $newvalue){
	      &print_message("update_record",
			     "Attempting to set keyword [$ukeyword] to identical initial value $newvalue");
	      return 0;
	  }
	  if ( $valuset{$ukeyword} eq "NULL"){
	      $qupdate .= " WHERE $utable.$ufield IS NULL";
	  } else {
	      $qupdate .= " WHERE $utable.$ufield = '$valuset{$ukeyword}'";
	  }
      } else {
	  # In string mode, we can append.

	  &print_message("update_record",
			 "$ukeyword not set with an initial value (giving up)");
	  return 0;
      }

  } else {
      $qselect = "SELECT $utable.$ufield FROM $utable WHERE $utable.$ufield = $newvalue";
      $qupdate = "UPDATE $utable SET $utable.$ufield = $newvalue ";
      if( defined($valuset{$ukeyword}) ){
	  if ( $valuset{$ukeyword} == $newvalue){
	      &print_message("update_record",
			     "Attempting to set keyword [$ukeyword] to identical initial value $newvalue");
	      return 0;
	  }
	  $qupdate .= " WHERE $utable.$ufield = $valuset{$ukeyword}";
      } else {
	  &print_message("update_record",
			 "$ukeyword not set with an initial value (giving up)");
	  return 0;
      }

  }


  if ($whereclause ne ""){
      $qselect .= " AND $whereclause";
      $qupdate .= " AND $whereclause";
  }

  &print_debug("update_record","Executing update: $qupdate\n");


  # We may be missing a 'WHERE'
  # This is a provision in case we decide to allow
  # updating records without having the keyword in.
  # In principle, we can't since this routine also
  # updates dictionaries.
  if ( $qupdate !~ /WHERE/){
      $qupdate =~ s/AND//;  # strip one AND
      $qupdate = "WHERE ".$qupdate;
  }


  if( ! $doit ){
      &print_message("update_record","$qupdate");
      return 0;
  } else {
      my ($sth,$retv,$val);

      $retv=0;

      # The warning is displayed for information only and mainly
      # for dictionaries where changing values may be a real
      # disaster.
      if ($utable ne "FileLocations"){
	  $sth = $FC::DBH->prepare($qselect);
	  if ($sth){
	      if ( $sth->execute() ){
		  $sth->bind_columns(\$val);
		  if ( $sth->fetch() ){
		      # this does not need to be displayed if $utable
		      # id is selected in whereclause as this would
		      # select a unique record
		      #print "--\n";
		      my $tmp= $utable.".".&_IDize("update_record",$utable);
		      #print "$tmp $whereclause\n";
		      if ( index($whereclause,$tmp) == - 1){
			  my($msg)="WARNING - $ukeyword=%s exists in table $utable";

			  if ( !defined($TRIMMSG{$msg}) ){
			      &print_message("update_record",sprintf($msg,$newvalue));
			      &print_message("update_record",
					     "        - if [$whereclause] is not unique, this ".
					     "may lead to disaster");
			  }
		      }
		  }
	      }
	      $sth->finish();
	  }
      }

      if($DELAY){
	  # Delayed mode
	  push(@DCMD,$qupdate );
	  return 1;
      } else {
	  $sth = $FC::DBH->prepare( $qupdate );
	  if (!$sth){
	      &print_debug("update_record","Failed to prepare [$qupdate]");
	  } else {
	      if ( $sth->execute() ){
		  $retv = $sth->rows;
	      } else {
		  &print_message("update_record","update failed with error=".
				 $FC::DBH->err." >> ".$FC::DBH->errstr);
	      }
	      $sth->finish();
	  }

      }
      return $retv;
  }
}



#============================================
#
# The 3 following method are argument-less.
#
# Set operation in delay mode.
#
sub set_delayed
{
    if ($_[0] =~ m/FileCatalog/) {
	my $self = shift;
    }
    $DELAY = 1;
}

sub unset_delayed
{
    if ($_[0] =~ m/FileCatalog/) {
	my $self = shift;
    }
    $DELAY = 0;
}



#
# Quick and dirty stack command execution
# Dirty because a do() statement has only little
# handle on what can be done ... whatever is in the
# stack may succeed or not without error bootstraping.
# However, this will be fine/adequate in any major record
# update.
# Possible argument {0|1} (default 0)
#   1 means it will display a message  time/#of updates
#
sub flush_delayed
{
    if ($_[0] =~ m/FileCatalog/) {
	my $self = shift;
    }
    my($flag)=@_;
    my($cmd,$sth);
    my($sts)=1;

    if( ! defined($flag) ){ $flag = 0;}
    if( ! $FC::DBH){  return 0;}

    if( $flag){
	&print_message("flush_delayed","Flushing ".($#DCMD+1)." commands on ".localtime());
    }

    foreach $cmd (@DCMD){
	&print_debug("update_record","Executing $cmd");
	if ( ! $FC::DBH->do($cmd) ){
	    &print_message("flush_delayed","Failed $cmd [".$FC::DBH->errstr."]");
	    $sts = 0;
	}
    }
    undef(@DCMD);
    $DELAY = 0;
    return $sts;
}


sub print_delayed
{
    if ($_[0] =~ m/FileCatalog/) { my $self = shift;}

    my($flag)=@_;
    my($cmd);

    if( $flag){
	&print_message("print_delayed","Printing ".($#DCMD+1)." commands on ".localtime());
    }

    foreach $cmd (@DCMD){
	# ready for a piping to cmdline mysql
	print "$cmd;\n";
    }
    undef(@DCMD);
    $DELAY = 0;
}


#============================================
# Updates the fields in FileLocations table - mainly
# used for updating availability and persistency
#
# Params:
# keyword - the keyword which data is to be updated
# value   - new value that should be put into the database
#           instead of the current one
# doit    - do it or not, default is 1
# delete  - delete duplicate it would otherwise create (default 0 and ignore ops)
#
sub update_location {
  if ($_[0] =~ m/FileCatalog/) { my $self = shift;}

  if( ! defined($FC::DBH) ){
      &print_message("update_location","Not connected");
      return 0;
  }


  my @updates;

  my ($ukeyword, $newvalue, $doit, $delete) = (@_);
  my ($mtable);

  my $fltblnam = "FileLocations";
  my $utable   = &_GetTableName($ukeyword);
  my $ufield   = &_GetFieldName($ukeyword);


  &print_debug("update_location","The keyword is [$ukeyword] from table=[$utable]");

  # Change this out to dictionary search and revert keyword
  # application to the location table for tables immediatly
  # related to the FileLocations table.
  # The order IS important
  if ( defined($FC::FDRELATED{$utable}) ){
      $mtable = "FileData";
  } elsif ( defined($FC::FLRELATED{$utable}) ){
      # *** this name need to change ***
      $mtable = $fltblnam;
  } else {
      $mtable = $utable;
  }

  # this is left for protection
  if ( $mtable ne $fltblnam  &&
       $mtable ne "FileData"      ){
      &print_message("update_location","Improper method called for keyword $ukeyword ($mtable)");
      return 0;
  }


  &print_debug("update_location","Main table related to [$utable] is indentified as [$mtable]");


  my @REFid;

  my $delim;

  if( ! defined($doit) ){    $doit    = 1;}
  if( ! defined($delete) ){  $delete  = 0;}

  $delim  = &get_delimeter();

  # Get the list of the files to be updated
  # Note that this is additional to what has been
  # required in a call to set_context() so we
  # can restrict to any field in addition of the id.
  # 'id' is for internal use only and is NOT an
  # external keyword.
  &set_delimeter("::");
  &set_context("all=1");                  # all availability
  &set_context("nounique=1");             # do not run with DISTINCT

  my ($id);
  if ($mtable eq $fltblnam ){
      $id = "flid";
  } else {
      $id = "fdid";
  }
  @REFid = &run_query($id);
  &print_debug("update_location","REFid ($id) = ".join(",",@REFid));

  # Bring back the previous delimeter
  &set_delimeter($delim);

  # delete($valuset{"path"});


  if ($#REFid == -1){
      my($info);
      $info = &get_context("storage")."::".&get_context("path")."::".&get_context("filename");
      if ( $FC::WDUPS ){
	  &print_message("update_location","The context did not return any candidate for [$info]");
      } else {
	  &print_debug("update_location","Context not found, record may have been updated [$info]");
      }
      return 0;
  }



  foreach my $key (keys %keywrds){
      my $field = &_GetFieldName($key);
      my $table = &_GetTableName($key);

      # grab keywords which belongs to the same table
      # This will be used as the selection WHERE clause.
      # The ufield is excluded because we use it by default
      # in the SET xxx= WHERE xxx= as an extra MANDATORY
      # clause.
      if (defined($valuset{$key})){
	  &print_debug("update_location","+ key=[$key] for table=$table");
	  if ( (($table eq $mtable) && ($field ne $ufield))  ||
	       (($table eq $utable))
	       ){
	      if ( $operset{$key} =~ m/^=/ ){
		  &print_debug("update_location","Accepting operator = for [$key]");
		  if (&get_field_type($key) eq "text" ){
		      if ( $valuset{$key} eq "NULL"){
			  push (@updates, "$table.$field IS NULL");
		      } else {
			  push (@updates, "$table.$field = '".$valuset{$key}."'");
		      }
		  } else {
		      push (@updates, "$table.$field = ".$valuset{$key});
		  }
	      } else {
		  # note that run_query() takes care of it. This block only treats extraneous
		  # '=' operators in a constraint and is meant to constraint value=newval
		  # where value=oldvalue cases.
		  &print_debug("update_location","Found operator $operset{$key} for key [$key] (untreated here)");
	      }
	  }
      }
  }
  my $whereclause = join(" AND ",(@updates));

  if ($utable eq ""){
      &print_debug("update_location",
		   "ERROR: $ukeyword does not have an associated table",
		   "Cannot update");
      return 0;
  }
  &print_debug("update_location","found whereclause = $whereclause");


  #
  # This routine actually handles FileData or FileLocations as logic
  # change table name upon detecting proper arguments (LFN / PFN
  # transparency).
  # But we can improve if the table is splittable espeically if only
  # FileLocations is used. Until now, $fltblnam <=> "FileLocations"
  #
  if ($utable eq $fltblnam && &_CanHandleSplitted() ){
      # we can use this logic only if storage was specified
      my ($storageType) = &check_ID_for_params("storage");
      if ( &_TypeSplitted($fltblnam,$storageType) && $storageType != 0){
	  $utable = "$fltblnam"."_$storageType";
	  if ($mtable eq $fltblnam ){  $mtable = $utable;}
	  # now reset the $fltblnam
	  $fltblnam = $utable;
      }
  }


  #
  # Sort out sth
  #
  my($qupdate,$qselect,$qdelete);
  my($sth1,$sth2,$sth3);

  if ( defined($FC::FLRELATED{$utable}) && ($utable ne "FileData") ){
      #+
      # THIS IS ONLY FOR TABLES WITH A RELATION TO FileLocations BUT FileData
      #-
      # Patch ... The above logic is true only for
      # tables like FileData/FileLocation but not true for others
      # Note that we should not test FDRELATED in this case ...
      &print_debug("update_location","Case FLRELATED");
      my $uid    = &get_id_from_dictionary($utable,$ufield,$newvalue);
      $ukeyword  = &_IDize("update_location",$utable);
      #$qselect = "SELECT $ukeyword FROM $mtable WHERE $ukeyword=$uid";
      $qdelete = "DELETE LOW_PRIORITY FROM $mtable " ;
      $qupdate = "UPDATE $mtable SET $mtable.$ukeyword=$uid ";

      #if( defined($valuset{$ukeyword}) ){
      #$qupdate .= " WHERE $mtable.$ukeyword=?";
      #}

      # overwite the value of utable in this case as we have
      # all indices now and can work ONLY on mtable (the one related
      # to ukeyword)
      $utable = $mtable;

  } elsif (&get_field_type($ukeyword) eq "text" ){
      #+
      # THOSE ONLY UPDATES VALUES WHICH ARE TEXT
      #-
      &print_debug("update_location","Case field_type is text");
      #$qselect = "SELECT $ukeyword FROM $mtable WHERE $ufield='$newvalue'";
      $qdelete = "DELETE LOW_PRIORITY FROM $mtable" ;
      $qupdate = "UPDATE $utable SET $utable.$ufield = '$newvalue' ";
      if( defined($valuset{$ukeyword}) ){
	  if ( $valuset{$ukeyword} eq "NULL"){
	      $qupdate .= " WHERE $utable.$ufield IS NULL";
	  } else {
	      $qupdate .= " WHERE $utable.$ufield = '$valuset{$ukeyword}'";
	  }
      } else {
	  &print_debug("update_location"," (text)  $ukeyword ($ufield) not set with an initial value");
	  #return 0;
      }

  } else {
      #+
      # THIS BLOCK CAN BE REACHED BY SELECT ON FileLocations, FileData OR TABLES
      # UNRELATED To FileLocations
      #-
      &print_debug("update_location","Case any-other-case");
      #$qselect = "SELECT $ufield FROM $mtable WHERE $ufield=$newvalue" ;
      $qdelete = "DELETE LOW_PRIORITY FROM $mtable" ;
      $qupdate = "UPDATE $utable SET $utable.$ufield = $newvalue ";
      if( defined($valuset{$ukeyword}) ){
	  $qupdate .= " WHERE $utable.$ufield = $valuset{$ukeyword}";
      } else {
	  &print_debug("update_location"," (other) $ukeyword ($ufield) not set with an initial value");
	  #return 0;
      }
  }


  #
  # mtable was used for building the REFid array.
  # utable is the table containg the field to update
  #
  # We are missing the relation between utable and mtable if necessary
  #
  my($xrel,$xjoin);
  if  ( $utable ne $mtable){
      # But since we are only speaking about FileData or FileLocations
      # we only need to put back in the relation between those tables.
      $xrel = " FileData.fileDataID = $fltblnam.fileDataID ";
      $xjoin= "$utable,$mtable";
      # however, we do not know if cond is related to FileLocations
      # or FileData so we need to re-check all table (kind of screwed
      # logic and relationship between tables)
      if ($xjoin !~ /FileLocations/){ $xjoin .= ",$fltblnam";}
      if ($xjoin !~ /FileData/){      $xjoin .= ",FileData";}
      &print_debug("update_location","will use $utable --> $utable,$mtable");
  } else {
      $xrel = "";
      $xjoin= "";
  }

  if ($qupdate =~ /WHERE/){
      $qupdate .= " AND ".($xrel ne ""?"$xrel AND ":"")."$mtable.".&_IDize("update_location",$mtable)." = ?";
  } else {
      $qupdate .= " WHERE ".($xrel ne ""?"$xrel AND ":"")."$mtable.".&_IDize("update_location",$mtable)." = ?";
  }
  #$qselect .= " AND fileLocationID = ?";
  $qdelete .= " WHERE $mtable.".&_IDize("update_location",$mtable)." = ?";

  if ( $xjoin ne ""){
      $qupdate =~ s/$utable/$xjoin/;
  }



  &print_debug("update_location"," update >> $qupdate");
  #&print_debug("update_location"," select >> $qselect");
  &print_debug("update_location"," delete >> $qdelete");


  #$sth1 = $FC::DBH->prepare( $qselect );
  $sth2 = $FC::DBH->prepare( $qdelete );
  $sth3 = $FC::DBH->prepare( $qupdate );
  #if ( ! $sth1 || ! $sth2 || ! $sth3){
  if (  ! $sth3){
      #$sth1->finish() if ($sth1);
      $sth2->finish() if ($sth2);
      $sth3->finish() if ($sth3);
      #&print_debug("update_location","Failed to prepare [$qupdate] [$qselect] [$qdelete]");
      &print_debug("update_location","Failed to prepare [$qupdate] [$qdelete]");
      return 0;
  }

  #
  # Now, loop over records with an already prepared sth
  #
  my($tmp,$failed,$count);

  $failed = $count = 0;
  &print_debug("update_location","Ready to scan filelist now ".($#REFid+1)."\n");

  foreach my $line (@REFid) {
      &print_debug("update_location","Returned line (id/$ukeyword): $line");

      my($fldid) = $line;

      #&print_debug("update_location","Executing update: $qupdate");

      if (! $doit){
	  #$tmp = $qselect; $tmp =~ s/\?/$fldid/;  &print_message("update_location","$tmp");
	  $tmp = $qupdate; $tmp =~ s/\?/$fldid/;  &print_message("update_location","would try      $tmp");
	  $tmp = $qdelete; $tmp =~ s/\?/$fldid/;  &print_message("update_location","if duplicate  ($tmp)");

      } else {
	  if( $DELAY){
	      # Delay mode
	      $tmp = $qupdate; $tmp =~ s/\?/$fldid/;  push(@DCMD,"$tmp");
	      #$tmp = $qdelete; $tmp =~ s/\?/$fldid/;  push(@DCMD,"$tmp");
	      $count++;
	  } else {
	      # if ( $sth1->execute($fldid) ){
	      #	  my(@all);
	      #	  if ( @all = $sth1->fetchrow() ){
	      #	      &print_message("update_location","Deleting similar records for $fldid");
	      #	      $sth2->execute($fldid);
	      #	  }
	      # }
	      if ( defined($GUPDID{$qupdate.$fldid}) ){
		  if ( $FC::WDUPS ){
		      &print_message("update_location",
			             "Loop detected for $mtable (record exists, update failed, corrupt db or slow propagation of updates)",
				     "\tat $fldid [$qupdate] with ? = $fldid",
				     "\tContext = ".join(",",@CTXMEM));
		  } else {
		      &print_debug(  "update_location",
			             "Loop detected for $mtable (record exists for fldid=$fldid)",
				     "\tContext = ".join(",",@CTXMEM));
		  }
	      } else {
		  $GUPDID{$qupdate.$fldid} = $qdelete;
		  # &print_debug("update_location","Working on flid: $fldid");
		  if ( $sth3->execute($fldid) ){
		      #&print_debug("update_location","Update of $mtable at $fldid succeeded [$qupdate]");
		      $count++;
		  } else {
		      $failed++;
		      if ($FC::DBH->err == 1062){
			  # Duplicate entry being replaced
			  #&print_debug("update_location","Duplicate entry is being replaced");
			  if ( $delete){
			      if ( $sth2->execute($fldid) ){
				  &print_message("update_location",
						 "selected fldid=$fldid deleted as update would ".
						 "lead to duplicate key)");
				  # This counts as a success because it moves records
				  # as well.
				  $count++;
			      }
			  } else {
			      &print_message("update_location",
					     "selected flid=$fldid cannot be updated ".
					     "(would lead to duplicate)");
			  }
		      } else {
			  &print_message("update_location","Update of $utable failed error=".
					 $FC::DBH->err." >> ".$FC::DBH->errstr);
		      }
		  }
	      }
	  }
      }

  }



  #$sth1->finish();
  $sth2->finish();
  $sth3->finish();

  return ($count);
}

#============================================
# Returns the context value assigned to the current field
# Params:
# keyowrd - which context keyword to get
# Return:
# context value for keyword
sub get_context {
  if ($_[0] =~ m/FileCatalog/) {
    shift @_;
  }

  my ($param) = @_;
  if ( defined($param) ){
      return $valuset{$param};
  } else {
      # return the full thing as a string
      my($tmp)="";
      foreach (keys %valuset){
	  if ( defined($operset{$_}) ){
	      $tmp .= "$_$operset{$_}$valuset{$_},";
	  } else {
	      $tmp .= "$_=$valuset{$_},";
	  }
      }
      foreach (keys %optvaluset){
	  if ( defined($optoperset{$_}) ){
	      $tmp .= "$_$optoperset{$_}$optvaluset{$_},";
	  } else {
	      $tmp .= "$_=$optvaluset{$_},";
	  }
      }
      chop($tmp);
      return $tmp;
  }
}

#============================================
#
# Set default value if necessary. Returns it
#  Arg1   the variable to set (passed by value)
#  Arg2   the default value
#  Arg3   a flag : 1  upperCase
#                  2  lowercase
#
sub get_value
{
    my($var,$dval,$flag)=@_;

    # If undef, use default value
    if ( ! defined($var) ){ $var = $dval;}

    # If null, use default as well. Do not allow \s+ or "" in ddb (sorry)
    if ( $var =~ m/^\s*$/){ $var = $dval;}

    # Now treat special cases
    if($flag == 1){      $var = uc($var);}
    elsif($flag == 2){   $var = lc($var);}

    # Return that value
    $var;
}


#============================================
sub message_class {
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}
    my($cl)=@_;
    $PCLASS = $cl;
}


sub debug_on
{
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}

    my($mode)=@_;

    #print "Debug is $mode\n";
    $FC::DEBUG  = 1;
    if( defined($mode) ){
	if(    $mode =~ m/html/i){ $FC::DEBUG = 2;}  # html, display as text
	elsif( $mode =~ m/cmth/i){ $FC::DEBUG = 3;}  # comments in html
	else {                     $FC::DEBUG = 1;}  # revert to default, plain text
    }
}

sub debug_off {    $FC::DEBUG = 0;}
sub get_debug {    $FC::DEBUG;}
sub set_debug {
    if ($_[0] =~ m/FileCatalog/) {  shift @_;}

    my ($d)=@_;
    $FC::DEBUG = defined($d)?$d:1;
    $FC::DEBUG;
}

sub print_debug
{
    my($head,@lines)=@_;
    my($line);

    return if ($FC::DEBUG==0);
    if ($PCLASS ne ""){
	if ( lc($head) !~ m/$PCLASS/i){
	    #print "$head do not match class $PCLASS\n";
	    return;
	}
    }

    foreach $line (@lines){
	chomp($line);
	if($FC::DEBUG==2){
	    print "<b>$head</b> <tt>$line<tt><br>\n";
	} elsif($FC::DEBUG==3) {
	    print "<!-- $head $line -->\n";
	} else {
	    printf "FC-DBG :: %20.20s %s\n",$head,$line;
	}
    }
}

#============================================

sub set_silent
{
    if ($_[0] =~ m/FileCatalog/) {
	shift @_;
    }
    my($mode)=@_;

    $FC::SILENT = $mode & 1;
    $FC::SILENT;
}

sub die_message
{
    &print_message(@_);
    die "\n";
}

sub print_message
{
    my($routine,@lines)=@_;
    my($line);

    if ( $FC::SILENT ){ return;}
    if ( $PCLASS ne ""){ if ( lc($routine) !~ m/$PCLASS/i){  return;}}

    foreach $line (@lines){
	chomp($line);
	printf STDERR "%20.20s :: %15.15s : %s\n","FileCatalog($$)",$routine,$line;
    }
    return;
}


#============================================

sub destroy {
  my $self = shift;
  &clear_context();
  if ( ! defined($FC::DBH) ) { return 0;}

  if ( $FC::DBH){
      if ( $FC::DBH->disconnect ) {
	  return 1;
      } else {
	  return 0;
      }
  } else {
      return 0;
  }
}


sub _GetONames
{
    if ($_[0] =~ m/FileCatalog/) {
	shift @_;
    }
    my($nm)=@_;
    my($name0,$name1,$name2);

    $nm =~ s/\..*//;    $name0 = $nm;
    $nm =~ s/_\d+$//;   $name1 = $nm;
    $nm =~ s/_\d+.*$//;
    if ($nm eq $name1){
	# remove one more _.*
	if ( $name1  =~ m/(.*)(_)(.*)/){
	     $nm = $1;
	 }
    }
    $name2 = $nm;
    &print_debug("_GetONames","Returning $name0,$name1,$name2");
    return ($name0,$name1,$name2);
}

sub _NormalizeAND_OR
{
    my($cond,$p)=@_;
    my(@test,$case);

    &print_debug("_NormalizeOR","OK, checking");
    if( index($cond,"OR") != -1){
	$case = 1;
	@test = split("OR",$cond);
    } elsif ( index($cond,"AND") != -1){
	$case = 2;
	@test = split("AND",$cond);
    }

    if ($#test != -1){
	&print_debug("_NormalizeAND_OR","Found $#test items $test[0]");
    }
    if ($#test > 5){
	my($base)=$test[0];
	my($el,$list,$newop);

	#
	# OR and != will lead to TRUE  always
	# AND and = will lead to FALSE always
	#
	if ($case == 1 && $base =~ m/!=/){  return " (1=1) ";}
	if ($case == 2 && $base =~ m/ = /){
	    &print_message("_NormalizeAND_OR","One keyword chain of ANDs of equality will lead to FALSE");
	    return " (1=0) ";
	}

	#
	# OR  has sens only with  = and translates into IN
	# AND has sens only with != and translates into NOT IN
	#
	$newop = ($case==1?"IN":"NOT IN");
	if ( $base =~ m/!=/){
	    $base =~ s/!=.*//;
	} else {
	    $base =~ s/=.*//;
	}
	$list = "";
	foreach $el (@test){ $el =~ s/.*=//; $list .= "$el,";}
	chop($list);
	$list =~ s/\s+//g;
	return "$base $newop ($list)";
    } else {
	return " ($cond) ";
    }
}


#============================================
#
# Just as a side note, if caching is enabled (which it is)
# it is unsafe to run bootstraps too often (other processes
# may be caching values you are deleting). The window of
# oportunity is as small as the time for one insert in MySQL
# but nonetheless, not 0.
#
sub _SaveValue
{
    my($table,$idx,$val)=@_;
    my($type);

    if ( $KNOWNVC[0] > $CACHESZ){
	&print_debug("SaveValue","Flushing the P cache (all OK)");
	$KNOWNVC[0] = 0;
	undef(%KNOWNVP);
    }
    if ( $KNOWNVC[1] > $CACHESZ){
	&print_message("SaveValue","Flushing the T cache (all OK)");
	$KNOWNVC[1] = 0;
	undef(%KNOWNVT);
    }

    if ( defined($TCACHED{$table}) ){
	$KNOWNVC[1]++;
	$KNOWNVT{$table." ".$idx} = $val;
	$type = "T";
    } else {
	$KNOWNVC[0]++;
	$KNOWNVP{$table." ".$idx} = $val;
	$type = "P";
    }
    &print_debug("<<SaveValue>>".
		 "Keeping track of $type value [$val] for table=$table for idx=$idx");
}

sub _CachedValue
{
    my($tab,$idx)=@_;
    my($rv,$type);

    if ( defined($TCACHED{$tab}) ){
	$rv   = $KNOWNVT{$tab." ".$idx};
	$type = "T";
    } else {
	$rv   = $KNOWNVP{$tab." ".$idx};
	$type = "P";
    }

    if ( defined($rv) ){
	&print_debug("<<CachedValue>>".
		     "Returning in $type memory $rv for table=$tab idx=$idx");
	return $rv;
    } else {
	return 0;
    }
}

#============================================
# Transformation routine
sub _CreatorID2Name
{
    my($arg)=@_;
    return &get_value_from_dictionary("Creators", "creatorID", $arg);
}

sub _CreatorName2ID
{
    my($arg)=@_;
    return &get_id_from_dictionary("Creators", "creatorName", $arg);
}

#============================================
# getlogin() appeared to be unreliable
sub _GetILogin
{
    return &check_ID_for_params("creator",0,&_GetLogin());
}


sub _GetLogin
{
    if( ! defined($FC::USER) ){
	#my($user)=getlogin(); bad !! bad !! bad !! Gets the account prior
	# to su.
	my($user) = getpwuid($<);

	if ( ! defined($user) ){
	    if ( defined($ENV{USER}) ){
		$user = $ENV{USER};
	    } else {
		chomp($user = `id`);
		$user =~ m/(uid=\d+\()(.*)\)(\s+gid=)/;
		$user = $2;
	    }
	}
	$FC::USER = $user;
    }
    if( $FC::USER eq ""){  $FC::USER = "getpwuid() failed";}
    $FC::USER;
}

#============================================
1;

