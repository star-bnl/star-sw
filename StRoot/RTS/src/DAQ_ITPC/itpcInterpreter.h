#ifndef _ITPC_INTERPRETER_
#define _ITPC_INTERPRETER_

#include <stdint.h>
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

	void run_start(uint32_t run) ;
	void run_stop() ;
	void start_event(uint32_t bytes) ;
	void stop_event() ;

	void fee_dbase(const char *fname=0) ;


	int rdo_scan_top(uint32_t *ddata, int words) ;	// new!

	int ana_send_config(uint32_t *data, uint32_t *data_end) ;
	int ana_pedestal(uint32_t *data, uint32_t *data_end) ;
	int ana_triggered(uint32_t *data, uint32_t *data_end) ;

	int rdo_scan(uint32_t *ddata, int words) ;


	uint32_t *fee_scan(uint32_t *start, uint32_t *end) ;
	uint32_t *sampa_lane_scan(uint32_t *start, uint32_t *end) ;
	uint32_t *sampa_ch_hunt(uint32_t *start, uint32_t *end) ;
	int sampa_ch_scan() ;

	void run_err_add(int rdo1, int type) ;

	// filled by client
	int id ;
	int dbg_level ;
	int realtime ;	// running in realtime
	uint32_t run_number ;
	int sector_id ;
	int rdo_id ;

	uint32_t status ;
	uint32_t evt_bytes ;
	uint32_t word_ix ;
	uint32_t evt_ix ;

	uint32_t fee_evt_cou ;	// events with FEEs

	enum s_type {S_IDLE, S_FEE_ASCII, S_FEE_ASCII_END,S_FEE_PORT, S_TRIGGER, S_FEE_END_A, S_FEE_END_B, S_FEE_END_HDR} ;
	s_type state ;

	itpcData *ped_c ;

	uint32_t evt_err[8] ;	// zapped before every event, logged after

	//run errors, for all RDOs, for all workers!
	static atomic_t run_errors[4][16] ;

	// various variables filled in as we go
	uint32_t rdo_wire1_id ;
	struct fee_t {
		uint32_t padplane_id ;
		uint32_t wire1_id ;
		uint32_t send_config_errs ;
		uint32_t event_errs ;
		uint32_t pedestal_errs ;
		uint32_t ch_errs ;
	} fee[17] ;	// index is RDO port starting from 1!

	// FPGA versions
	uint32_t fpga_fee_v_all ;

	// electronics format versions
	int rdo_version ;
	int expected_rdo_version ;
	int fee_version ;
	int expected_fee_version ;

	// filled in fee_scan
	int fee_port ;	// from 1..16
	uint32_t fee_id ;	// padplane id: 0..63 (although 55 is the last valid value)
	uint32_t fee_bx ;

	// filled in sampa_lane_scan
	uint32_t found_ch_mask ;
	int sampa_bx ;	// for cross checks
	int sampa_id ;
	int sampa_ch ;	//0..31
	int fee_ch ;	//0..63

	// used by sampa_ch_scan
	int tb_cou ;
	uint16_t tb_buff[1024] ;

	int d_cou ;

	
	// for various ASCII packets from RDO or FEE
	int ascii_cou ;
	char ascii_dta[1024] ;


	static struct itpc_config_t {
		struct {
			int rdo_id ;
			uint32_t wire1 ;
			uint32_t fee_mask ;	// if 0: whole RDO is masked!
			int phase ;
			int fee_count ;

			struct {
				uint32_t wire1 ;
				uint8_t padplane_id ;
				uint8_t sampa_version ;	// 2, 3 or 4
				uint8_t lane_dead_mask ;	// 4 bits; normally 0
				uint8_t sampa_dead_mask ;	// 2 bits; normally 0
			} fee[17] ;	// index is port (from 1)


		} rdo[4] ;	// index is rb/gtp (from 0)

	} itpc_config[25] ;	// index is sector (from 1)

	static int itpc_fee_map[24][4][16] ;
	static uint32_t ifee_mask(int sec1, int rdo1) ; 
	static int fee_map_check() ;

	static int parse_config(const char *fname) ;
	static int parse_default() ;

} ;


#endif

