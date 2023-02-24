#ifndef _DDB_H_
#define _DDB_H_

#include <rtsMonitor.h>
#include <rtsSystems.h>

//#include <TEF_RDO/rb_c.h>
#include "rb_c.h"

#include "l_atomic.h"

class rb_c ;
class ipcQClass ;
class sfs_index ;
class EvbChooser ;
class SimpleXmlDoc ;


#define DET_MAX_MOVERS	EVBX_NODE_COUNT
// statics
#define RB_MAX_COUNT		8
#define WRK_THREAD_COUNT	32	// at least the same as RDOs normally 16 in STAR

#define EVB_T_PAYLOAD	5

struct evtSender_struct {
        sfs_index *sfs ; // pointer to my flavor of Jeff's sender class (SFS?) or NULL for unused
        u_char is_eth ; // is ethernet?
        u_char is_used ;        // in this run?
        u_int evts ;    // event counter for local evbuilding...
        u_int dstNode ;
        u_int dstTask ;
	unsigned long word_counter ;	// per evt_sender
} ;




struct ic_evb_t {
	u_short token ;
	u_char to_disk ;	// write it to local disk
	u_char to_evb ;

	u_char trg_cmd ;
	u_char daq_cmd ;


	u_int bytes[EVB_T_PAYLOAD] ;
	char *mem[EVB_T_PAYLOAD] ;
	
} ;

struct msgbuf_t {
//	long msg_sig ;	// always >0!

	struct {
		u_short t ;
		u_char trg_cmd ;
		u_char daq_cmd ;

		u_short rb_mask ;
		u_short error ;

		unsigned long time_in ;	

		struct {
			u_int words ;
			u_short pad_s ;	// padding
			u_char pad_c ;	// padding
			u_char fifo_ix ;
		} rb[8] ;
	} ;

} ;

struct dep_mon_t {
	u_char sector ;
	u_char rdo ;


	u_char det ;
	u_char ns ;
	u_char dep ;

	u_char crate ;
	u_char slot ;
	u_char bp ;

	u_char fee_cou ;
	u_char fee_state ;
//	u_char gtp_stat ;

	float temp_c ;
	u_int ht_rate ;

	float deadtime ;
	float rx_throttle ;

	char clock[16] ;
	u_int wire1_id ;
	char firmware[64] ;

	u_int fw_version ;	// the VERSION in rdo_top.vhd

	time_t alive ;
	time_t fee_alive ;
} ;

struct ddb_errs_t {	// per RDO
	// these get cleared upon auto recovery
	atomic_t tmout ;	// RDO stopped sending data
	atomic_t overrun ;	//  
	atomic_t evt ;		// event checker claims error
	atomic_t algo ;		// algo checker claims error

	u_int run_tmout ;	// per run
	u_int run_overrun ;
	u_int run_evt ;
	u_int run_algo ;
} ;

struct ddb_t {
	u_short rts_id ;
	u_short node_id ;
	u_char sector ;

	int rb_mask ;
	int phy_mask ;
	int rb_max_count ;
	int rb_fifo_cou ;
	int rb_kb ;

	u_int events ;

	int wrk_cou ;
	
	volatile char checking_link ;

	const char *rts_name ;

	struct {
		u_char sector ;
		u_char rdo ;
		u_char dev ;	//RORC: board (or devide)
		u_char ch ;	//RORC: sub-board (or channel)
	} to_real[RB_MAX_COUNT] ;

	time_t rc_cmd_tm ;
	time_t run_started ;
	time_t first_evt ;

	int run_type ;
	int run ;
	int raw_write ;
	char ped_incorrect ;	// beam in RHIC, not a tcd_only run, etc
	char standalone ;
	char sim_on ;		// data is simulated!

	char config[128] ;
	int trg_rate ;		// from Run Control


	int evb_test_bytes_rdo ;

	int asic_seq_lo ;
	int asic_thr_lo ;
	int ped_mode ;
	int gain_mode ;

	u_char n_sigma ;	// used in STGC

	u_char busy_on ;

	u_int det_res[5] ;	// from daq_det_res

	char auto_recovery ;	// 1:ongoing; 0:normal
	u_int recoveries[8] ;

	FILE *f_thresh ;

	char use_local_clock ;

	u_int first_event ;
	int fee_count ;

	//
	u_char rc_emulated ;
	SimpleXmlDoc *xml ;

	// evb
	class EvbChooser *evb_chooser ;

	u_int evts_to_disk_req ;
	atomic_t a_evts_to_disk ;
	u_int evts_to_disk_ix ;

	// monitoring
	volatile rtsMonStruct mon ;
	//atomic_t a_words_evb ;
	double kbAux ;	// from DEPs
	atomic_t a_evts_in ;


	time_t uptime ;
	volatile char gs_state ;

	FILE *sc_f ;	// for misc per-run data

	// Readout Board
	rb_c *crb[RB_MAX_COUNT] ;
	//rb_main_c *crb[RB_MAX_COUNT] ;
	volatile u_char gtp_status[RB_MAX_COUNT] ;
	volatile u_char led_status[RB_MAX_COUNT] ;

	// queues
	ipcQClass *detQ ;
	ipcQClass *ethQ ;
	ipcQClass *mqttQ ;


	ipcQClass *evtSendQ[DET_MAX_MOVERS] ;


	// errors 
	ddb_errs_t errs[RB_MAX_COUNT] ;

	// statuses
	dep_mon_t dep_mon[RB_MAX_COUNT] ;
} ;

extern struct ddb_t ddb ;


extern int det_sc_remote(int rts_id,char cmd, int sec, int rdo, int on, u_int run) ;

//extern int cpu_usage_get() ;


#endif
