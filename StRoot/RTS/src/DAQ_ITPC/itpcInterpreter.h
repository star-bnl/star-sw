#ifndef _ITPC_INTERPRETER_
#define _ITPC_INTERPRETER_

#include <sys/types.h>
#include <stdio.h>
#include <I386/atomic.h>

class itpcData ;

#define ERR_UNKNOWN		0
#define ERR_DIFFERENT_FEE_IDS	1

class itpcInterpreter
{
public:

	itpcInterpreter() ;

	~itpcInterpreter() {;} ;

	FILE *fout ;	// as stdout

	void run_start(u_int run) ;
	void run_stop() ;
	void start_event(u_int bytes) ;
	void stop_event() ;

	void fee_dbase(const char *fname=0) ;


	int rdo_scan_top(u_int *ddata, int words) ;	// new!

	int ana_send_config(u_int *data, u_int *data_end) ;
	int ana_pedestal(u_int *data, u_int *data_end) ;
	int ana_triggered(u_int *data, u_int *data_end) ;

	int rdo_scan(u_int *ddata, int words) ;


	u_int *fee_scan(u_int *start, u_int *end) ;
	u_int *sampa_lane_scan(u_int *start, u_int *end) ;
	u_int *sampa_ch_hunt(u_int *start, u_int *end) ;
	int sampa_ch_scan() ;

	void run_err_add(int rdo1, int type) ;

	// filled by client
	int id ;
	int dbg_level ;
	int realtime ;	// running in realtime
	u_int run_number ;
	int sector_id ;
	int rdo_id ;

	u_int status ;
	u_int evt_bytes ;
	u_int word_ix ;
	u_int evt_ix ;

	u_int fee_evt_cou ;	// events with FEEs

	enum s_type {S_IDLE, S_FEE_ASCII, S_FEE_ASCII_END,S_FEE_PORT, S_TRIGGER, S_FEE_END_A, S_FEE_END_B, S_FEE_END_HDR} ;
	s_type state ;

	itpcData *ped_c ;

	u_int evt_err[8] ;	// zapped before every event, logged after

	//run errors, for all RDOs, for all workers!
	static atomic_t run_errors[4][16] ;

	// various variables filled in as we go
	u_int rdo_wire1_id ;
	struct fee_t {
		u_int padplane_id ;
		u_int wire1_id ;
		u_int send_config_errs ;
		u_int event_errs ;
		u_int pedestal_errs ;
		u_int ch_errs ;
	} fee[17] ;	// index is RDO port starting from 1!

	// FPGA versions
	u_int fpga_fee_v_all ;

	// electronics format versions
	int rdo_version ;
	int expected_rdo_version ;
	int fee_version ;
	int expected_fee_version ;

	// filled in fee_scan
	int fee_port ;	// from 1..16
	u_int fee_id ;	// padplane id: 0..63 (although 55 is the last valid value)
	u_int fee_bx ;

	// filled in sampa_lane_scan
	u_int found_ch_mask ;
	int sampa_bx ;	// for cross checks
	int sampa_id ;
	int sampa_ch ;	//0..31
	int fee_ch ;	//0..63

	// used by sampa_ch_scan
	int tb_cou ;
	u_short tb_buff[1024] ;

	int d_cou ;

	
	// for various ASCII packets from RDO or FEE
	int ascii_cou ;
	char ascii_dta[1024] ;


	static struct itpc_config_t {
		struct {
			int rdo_id ;
			u_int wire1 ;
			u_int fee_mask ;	// if 0: whole RDO is masked!
			int phase ;
			int fee_count ;

			struct {
				u_int wire1 ;
				u_char padplane_id ;
				u_char sampa_version ;	// 2, 3 or 4
				u_char lane_dead_mask ;	// 4 bits; normally 0
				u_char sampa_dead_mask ;	// 2 bits; normally 0
			} fee[17] ;	// index is port (from 1)


		} rdo[4] ;	// index is rb/gtp (from 0)

	} itpc_config[25] ;	// index is sector (from 1)

	static int itpc_fee_map[24][4][16] ;
	static u_int ifee_mask(int sec1, int rdo1) ; 
	static int fee_map_check() ;

	static int parse_config(const char *fname) ;
	static int parse_default() ;

} ;


#endif

