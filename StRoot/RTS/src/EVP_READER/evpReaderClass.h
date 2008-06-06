#ifndef _EVP_READERCLASS_HH_
#define _EVP_READERCLASS_HH_
#include <sys/stat.h> 

struct DATAP;

typedef unsigned int UINT32;

#include "evp.h"
#include "cfgutil.h"

class sfs_index;
class rts_reader;
class evpReader {
public:
	evpReader(char *fname) ;
	~evpReader(void) ;

	char *get(int which, int type=EVP_TYPE_ANY) ;	
        char *getSFSEventNumber();

	int setMmap(int flag) ;		// flag=1 - enable memory mapping
	int setOpen(int flag) ;		// flag=1 - enable file open
	int setLog(int flag) ;		// flag=1 - enable logging...
	char *setEvpDisk(char *fs) ;	// sets the local directory name where the evp.star disks
                                        // are mounted

	int status ;		// 0 OK, all others should disregard the event!

	// all the variables below are valid ONLY if the status is 0!!!

  //  These parameters are used for the evp reads with event number non-zero
  //  readall_lastevt --
        u_int readall_rundone;
        u_int readall_lastevt;
        u_int readall_run;
        void readall_reset() { readall_rundone = 0; readall_lastevt = 0; readall_run=0; };


	u_int event_number ;	// current event in the evp or file 
	u_int total_events ;	// total number of events seen by this object so far

	u_int bytes ;		// size of the current event
	

	char file_name[256] ;	// fully qualified file name (on evp.star)
	u_int file_size ;	// size of the file in bytes


	u_int run ;		// current run number

	u_int evb_type ;	// event type	(only from ACTIVE run)
	u_int evb_cou ;		// total events in this run (only from ACTIVE run)
	u_int evb_type_cou ;	// total events of the above type in this run (only from ACTIVE run)

	u_int token ;		// current token
	u_int trgcmd ;		// current trigger command
	u_int daqcmd ;		// current DAQ command
	u_int trgword ;		// the Trigger Word
	u_int phyword ;		// the Physics Word
	u_int daqbits ;		// "offline" bits aka L3 summary...
	u_int daqbits_l1;       // triggers satisfying l1 
	u_int daqbits_l2;       // triggers satisfying l2
        u_int evpgroups ;       // evp groups aka L3 summary[2]     

	u_int evt_time ;	// time in UNIX seconds
	u_int seq ;		// event sequence from EVB
	u_int detectors ;	// detectors present bit mask according to DAQ!

        u_int detsinrun ;
        u_int evpgroupsinrun;

	int isdir ;		// are we running through a directory?
	int isfile ;		// ... or a file?
	int isevp ;		// ... or the active run from EVP?

	char *mem ;	
	DATAP *sumdatap;         // pointer to a datap with valid summaries...
        sfs_index *sfs;         // the sfs reader object...
        int sfs_lastevt;

	// new rts reader
	rts_reader *rts_rr ;

        int evt_offset_in_file; // where is the start of the event
        int evt_datap_offset_in_file;  // start of datap in file
        int evt_mapping_offset;        // where the event is mapped from

	int bytes_mapped ;
	u_int tot_bytes ;
	char *mem_mapped ;
	int page_size ;

	int fixDatapSummary(DATAP *datap);

private:	// one shouldn't care...
	int reconnect(void) ;

	int do_mmap ;
	int do_open ;
	int do_log ;
	char evp_disk[256]; 

	int issued ;
	int last_issued ;	// time

	int task ;
	int desc ;		// file descriptor

	int evpDesc ;		// message queue desc.
	// file variables
	char fname[256] ;
	struct stat stat_buf ;

        rccnf runconfig;

	// mmap variables

} ;


#endif
