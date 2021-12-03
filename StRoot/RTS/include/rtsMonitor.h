#ifndef _RTS_MONITOR_H_
#define _RTS_MONITOR_H_


#include <RC_Config.h>	/* for TRIGGERS_MAX of the day... */

/* LINUX issues */
#ifndef SHM_SHARE_MMU
#define SHM_SHARE_MMU  0
#endif



/* unused ! */
#define RTS_MON_SHM_KEY	0xffab0000 




/* 
	Default monitoring host.
	Default monitoring UDP port.
*/

#define RTS_MON_PORT		8100


#ifdef RTS_PROJECT_PP
	#define RTS_MON_HOST		"130.199.91.18"	/* old pp2pp host, deprecated */
#else
	#ifdef RTS_DAQMAN
		#define RTS_MON_HOST	RTS_DAQMAN
	#else
		#define RTS_MON_HOST	"boothost"	/* only really good for vxworks but I leave it in */
	#endif
#endif


#define RTS_MON_FILE		"/RTS/log/monServer.dta"		/* always! */


/*
	All the items are unsigned ints because I don't want to deal
	with allignment problems on var. compilers/platforms.
	All MUST be BigEndian!
	Correction...actually, the endianess is determined through the "task" i.e.
	task should be less than 255 and if it isn't, the we need to swap...

*/



#define RTS_MON_VERSION_OLD	0x000000003	/* in yyyy.xxxx BCD format */
#define RTS_MON_VERSION		0x000000003	/* in yyyy.xxxx BCD format */

#define RTS_MON_VERSION_X_FLAG	0x10000000 
#define RTS_MON_VERSION_X	(RTS_MON_VERSION_X_FLAG | RTS_MON_VERSION)	/* FY04 (Oct03) extended version */

/* total number of entities (i.e. node-task pairs we support) */
#define RTS_MON_MAX_NODES	512

#define RTS_MON_USER_WORDS	100	/* NX4bytes of storage for user defined entries */ 
#define RTS_MON_SYS_WORDS	17	/* DO NOT MODIFY UNLESS ONE CHANGES THE rtsMonStruct ! */

struct rtsMonRequired           /* this represents the minimum packet necessary */
{
  unsigned int size ;           /* size in bytes of this message */
  unsigned int node ;           /* daq Node Id of this sender */
  unsigned int task ;           /* DAQ TaskId of the sender */
  unsigned int version ;        /* version of this struct - see DAQ_MON_VERSION */
  unsigned int tim ;            /* current time in UNIX seconds */
  unsigned int state ;          /* 0 OFF, 1 ON, 2 AUX */
};

// Added by jml:   I need the bare header which I use to construct
// local monitor structures.
//
// ie:   struct jmlRtsMonL1Full {
//          rtsMonHeader head;
//          rtsMonL1 l1;
//       };
//
// Then it is easy to send, I can use sizeof, I only need one variable
// etc...   The user words in rtsMonStruct always screws me up...

struct rtsMonHeader {
	unsigned int size ;		/* size in bytes of this message */
	unsigned int node ;		/* RTS Node Id of this sender */
	unsigned int task ;		/* RTS TaskId of the sender */
	unsigned int version ;		/* version of this struct - see RTS_MON_VERSION */
	unsigned int tim ;		/* current time in UNIX seconds */
	unsigned int state ;		/* RC States */
	unsigned int tknIn ;		/* last announced token */
	unsigned int tknOut ;		/* last released token */
	unsigned int tknBad ;		/* last erroneus token */
	unsigned int couEvtsIn ;	/* tokens currently in the system */
	unsigned int couEvtsRun ;	/* events in this run */
	unsigned int couEvtsAll ;	/* events since boot */
	unsigned int couEvtsBad ;	/* rejected events because of errors in this run */
	unsigned int busy ;		/* % of time the system was BUSY i.e. incapable of new events */
	unsigned int evtsSec ;		/* input rate in events/second */
	unsigned int kbSecEvb ;		/* rate in kB/sec to the EVB */
	unsigned int kbSecAux ;		/* rate in kB/sec to the L3 system */
};

struct rtsMonStruct {
/* these MUST be present! */
	unsigned int size ;		/* size in bytes of this message */
	unsigned int node ;		/* RTS Node Id of this sender */
	unsigned int task ;		/* RTS TaskId of the sender */
	unsigned int version ;		/* version of this struct - see RTS_MON_VERSION */
	unsigned int tim ;		/* current time in UNIX seconds */
	unsigned int state ;		/* RC States */
/* these should be present although one can use 0xffffffff to signify Not Applicable */
	unsigned int tknIn ;		/* last announced token */
	unsigned int tknOut ;		/* last released token */
	unsigned int tknBad ;		/* last erroneus token */
	unsigned int couEvtsIn ;	/* tokens currently in the system */
	unsigned int couEvtsRun ;	/* events in this run */
	unsigned int couEvtsAll ;	/* events since boot */
	unsigned int couEvtsBad ;	/* rejected events because of errors in this run */
	unsigned int busy ;		/* % of time the system was BUSY i.e. incapable of new events */
	unsigned int evtsSec ;		/* input rate in events/second */
	unsigned int kbSecEvb ;		/* rate in kB/sec to the EVB */
	unsigned int kbSecAux ;		/* rate in kB/sec to the L3 system */
/* used for user defined entries --> see comment below */
	unsigned int user[RTS_MON_USER_WORDS] ;
} ;


// old! total size in shared memory 
#define RTS_MON_DATA_SIZE_V3	(sizeof(struct rtsMonStruct)*RTS_MON_MAX_NODES)

// new X-tended version...
#define RTS_MON_PACKET_SIZE	(10*1024)
#define RTS_MON_DATA_SIZE	(RTS_MON_PACKET_SIZE*RTS_MON_MAX_NODES)


/* 
	Maximum values of <name,value> strings in user space after which 
	the display program will chop the string off.
	Used to prevent crashes and to ease the display formatting.
*/
#define RTS_MON_NAME_MAX	12
#define RTS_MON_VAL_MAX		24


/* value which means "Not Applicable" and will be shown as "N.A." in the display */
#define RTS_MON_NA		0xFFFFFFFF
#define RTS_MON_NA_STRING	"N.A."

/*
	based upon the size ("size") the displaying program will determine the amount of
	user-added variables (if any) at the end of the structure.
	The "user" field is treated as a sequence of zero-terminated C strings which will
	be displayed in pairs <name,value> i.e.
	"File Name :","Myfile","Really bad tokens","123" etc.

*/


struct rtsMonGB {
	struct {
	  char name[32] ;	// if strlen() == NULL, unused..
	  uint32_t off_id ;		// offline id number i.e. 5001
	  uint32_t fired ;		// received from trigger
	  uint32_t rate ;		// rate of above
	  uint32_t l3_sent ;		// sent to L3
	  uint32_t aborted_gb ;	// released by GB
	  uint32_t aborted_l3 ;	// released by L3
	  uint32_t aborted_l25 ;	// released as L2.5
	  uint32_t err ;		// events with error
	  uint32_t built ;		// events built _successfully_
	  uint32_t xpress ;		// events sent to xpress stream
	} trgs[TRIGGERS_MAX+1] ;	// TRIGGERS_MAX are the usual, last is the sum
} ;


struct rtsMonTCD_new {
/* these MUST be present! */
	unsigned int size ;		/* size in bytes of this message */
	unsigned int node ;		/* RTS Node Id of this sender */
	unsigned int task ;		/* RTS TaskId of the sender */
	unsigned int version ;		/* version of this struct - see RTS_MON_VERSION */
	unsigned int tim ;		/* current time in UNIX seconds */
	unsigned int state ;		/* RC States */
/* these should be present although one can use 0xffffffff to signify Not Applicable */
	unsigned int tknIn ;		/* last announced token */
	unsigned int tknOut ;		/* last released token */
	unsigned int tknBad ;		/* last erroneus token */
	unsigned int couEvtsIn ;	/* tokens currently in the system */
	unsigned int couEvtsRun ;	/* events in this run */
	unsigned int couEvtsAll ;	/* events since boot */
	unsigned int couEvtsBad ;	/* rejected events because of errors in this run */
	unsigned int busy ;		/* % of time the system was BUSY i.e. incapable of new events */
	unsigned int evtsSec ;		/* input rate in events/second */
	unsigned int kbSecEvb ;		/* rate in kB/sec to the EVB */
	unsigned int kbSecAux ;		/* rate in kB/sec to the L3 system */

	int deadtime[32] ;
	int clock[32] ;	// the clock of the TCD; or TCU for "Trigger"
	unsigned int cpu_busy ;

	struct {
		int sca_hz ;
		int sca_dead ;
		int l0_evts ;
		int l0_hz ;
		int abt ;
	} l0[65] ;

} ;

struct rtsMonTCD {
	int deadtime[32] ;
	int clock[32] ;	// the clock of the TCD; or TCU for "Trigger"
} ;


struct rtsMonSCA {
	struct {
		char name[32] ;
		uint32_t rate ;
	} trgs[TRIGGERS_MAX] ;	// 0-32 are normal, the rest is reserved

} ;

struct rtsMonL1Counters {
  struct {
    //  char name[32];
    uint32_t enabled;
    uint32_t rate;
    uint32_t deadtime;
  } trgs[TRIGGERS_MAX+1];
  uint32_t detector_deadtime[16];
};

struct rtsMonEVB {
	uint32_t mb_run ;	// MB stored in this run 
	uint32_t gb_free ;	// still free on all disks
	uint32_t gb_all ;	// total capacity on all discs (for %-free calc...)
	// stuff pertaining to RCF goes here..

	uint32_t run_number ;	// hm, this is old, why do I have this here? *shrug*
	struct rtsMonGB gb ;	// new in FY09 -- EVB sends GB stuff as well!
} ;

struct rtsMonEVB_supertask {
  uint32_t mon[RTS_MON_SYS_WORDS];
  // General
  uint32_t mb_run;     // total data size for this run
  uint32_t gb_free;    // free disk space
  uint32_t gb_all;     // total disk space

  // RCF
  uint32_t files_sent;     // sent within last resets on the hour
  uint32_t files_waiting;  // waiting to be sent

  // By trigger Information
  struct {
    uint32_t off_id ;	   // offline id number i.e. 5001
  
    uint32_t fired ;	   // received from trigger
    uint32_t rate ;	   // rate of above
    uint32_t built ;	   // events built _successfully_

    uint32_t aborted_l25 ;	   // released as L2.5
    uint32_t err ;		   // events with error (det timeout, evb resources)
    uint32_t spurious;        // events with spurious (trg timeout, evt overrun)

    uint32_t stream;	   // which stream evts sent to

  } trgs[TRIGGERS_MAX+1] ;	           // 0-31 are the usual, 32 is the sum
};



// RCF writers structure
// the usual state is:
//	RC_PRESENT or RC_NONE	dormant
//	RC_READY	attempting to connect to RCF
//	RC_RUNNING	currently writing to RCF
//	RC_ERROR	an RCF error encountered
// Usual monioring words are:
//	kbSecEvb	is the last known rate
struct rtsMonRCF {
        char type[20];
        int curr_run;
        int curr_idx;
	struct {
		uint32_t run ;		// run number ; 0 means invalid/empty
		uint32_t mb_done ;		// MB already stored for this run
		uint32_t mb_all ;		// ...of MB total for this run
		uint32_t seq_done ;	// part number already stored
		uint32_t seq_all ;		//... of parts remaining
	} runs[11] ;

	// runs[0] is the _current_ run _if_ the state is either RC_RUNNING or RC_ERROR
	// runs[10] is the summary of remaining runs:
	//		run is the _number_ of runs remaining
	//		mb_all	is the MB remaining
	//		seq_all is the sequences remaining
};


// xtended structure used by RC/handler
// The "state" from the standard header is used as the general state
// The couEvtsRun from the standard header is the number of "scheduled"
// events.
// If not running the data should reflect the state of the last known
// run...
struct rtsMonRC {
	uint32_t run ;		// run number
	uint32_t type ;		// PEDESTAL etc.
	char config[128] ;	// Configuration name
	char dest[64] ;		// EVB destination name
	uint32_t t_start ;		// Start run time
	uint32_t t_end ;		// Stop run time (0 if not yet finished)
	uint32_t accepted ;	// bits: 1 operator rejected, 2 force stop by sombody, 4 unsuccessfull end
	uint32_t node_count ;	// the count of nodes following (all of which are in the run...)
        uint16_t err_node ;        // jml... the node that caused the last stop run
        uint16_t n_err_node;
	struct {
		uint16_t node ;
		uint16_t task ;
		uint32_t state ;
	} nodes[256] ;
	struct {
		char name[32] ;	// if strlen() == NULL, unused..
		uint32_t off_id ;		// offline id number i.e. 5001
	} trgs[TRIGGERS_MAX] ;
} ;


struct rtsMonL1 {		// used by GL3 as well!
	uint32_t late_events ;	// number of events above the time cutoff (i.e. 7 ms)
	uint32_t max_us ;		// the current maximum turaround time (in us)
	struct {
		uint32_t off_id ;		// offline id number
		uint32_t accepted ;	// fired in the run so far
		uint32_t rate ;		// rate/sec of "accepted"
		uint32_t aborted ;		// number of evebts aborted in this run
	} trgs[TRIGGERS_MAX+1] ;			// 0-31 are per trigger, 32 is the total sum...
} ;

struct rtsMonDET {
	uint32_t val_rbs ;	// valid RBs
	uint32_t cfg_rbs ;	// configured
	uint32_t buffs ;	// EVB/SL3 buffers free
	uint32_t evtsSpur ;	// spurious events; evtsBad are only the bad ones _with_ the Trigger
	uint8_t rb_status[12] ;	// USED in DDL dets!
	uint32_t dbg_ctrs[10] ;	// 10 debug counters
} ;



//////////////////////////
//  2011 L0/L1 updates
//////////////////////////

// This is the only packet coming from L0
//
// Standard packet + TCU fired by trigger.
//
struct rts2011MonL0 {
  rtsMonHeader head;

  struct {
    uint32_t off_id;
    uint32_t fired;
    uint32_t rate;
  } trg[TRIGGERS_MAX + 1];
};

// This is the only packet coming from L1
//
// Standard packet + L1 counters by trigger.
//
struct rts2011MonL1 {
  rtsMonHeader head;

  struct {
    uint32_t rate;
    uint32_t deadtime;
  } scaler[TRIGGERS_MAX + 1];
  
  uint32_t detector_dead[16];
};


// This is the update to the evbSuperMon struct
//
struct rts2011EvbxSuperMon {
  rtsMonHeader head;

  // General
  uint32_t mb_run;     // total data size for this run
  uint32_t gb_free;    // free disk space
  uint32_t gb_all;     // total disk space

  // RCF
  uint32_t files_sent;     // sent within last resets on the hour
  uint32_t files_waiting;  // waiting to be sent

  // By trigger Information
  struct {
    uint32_t off_id ;	   // offline id number i.e. 5001
  
    uint32_t fired ;	   // received from trigger
    uint32_t fired_rate;

    uint32_t built ;	   // events built _successfully_
    uint32_t built_rate;

    uint32_t l1_abort;
    uint32_t l1_abort_rate;

    uint32_t l2_abort;
    uint32_t l2_abort_rate;

    uint32_t l3_abort;
    uint32_t l3_abort_rate;

    uint32_t err;
    uint32_t err_rate;

    uint32_t stream;	   // which stream evts sent to
  } trgs[TRIGGERS_MAX+1] ;

  uint32_t detectorErrors[32];
};

// new, FY13, slow controls monitoring (via scDeamon)
struct rtsMonSC {
	rtsMonHeader head ;

	int rich_scalers[32] ;
	float mag_field ;

	short blu_ions ;
	short blu_energy ;
	char blu_status[32] ;

	short yel_ions ;
	short yel_energy ;
	char yel_status[32] ;

	char mcr_vote[32] ;

	uint32_t phys_on ;
	uint32_t phys_off ;

	uint32_t tcu_clock ;
	uint32_t zdc_corrected ;
	uint32_t reserved[9] ;
};

struct rts2013_L4Mon {
    rtsMonHeader head;

    // By trigger Information
    struct {
	uint32_t off_id ;	   // offline id number i.e. 5001
	
	uint32_t daq_cnt ;	   // received from EVB
	uint32_t daq_rate;
	
	uint32_t l4_accept;
	uint32_t l4_accept_rate;
    } trgs[TRIGGERS_MAX+1] ;
};

struct rts2013_L4EvbMon {
    rtsMonHeader head;
};

struct rts2013_L4RcfMon {    
    rtsMonHeader head;

    // General
    uint32_t gb_free;    // free disk space
    uint32_t gb_all;     // total disk space

    // RCF
    uint32_t files_sent;     // sent within last resets on the hour
    uint32_t files_waiting;  // waiting to be sent
};

struct rts2013_TmMon {
    rtsMonHeader head; // all values according to token...
    
    uint32_t evtsManaged;   // eventsIn by tmtoken rather than token
    uint32_t evtsEvb[10];   // eventsIn by tmtoken by evb
};


// TPC Gating grid monitoring
struct ggMonStruct {
        struct rtsMonHeader m ;

	uint16_t mode ;
	uint16_t hv ;

        uint32_t status[12] ;
        uint16_t adc[12][4][3] ;
        uint16_t dac[12][4][3] ;
} ;


#endif
