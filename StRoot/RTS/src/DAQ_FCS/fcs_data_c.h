#ifndef _FCS_DATA_C_H_
#define _FCS_DATA_C_H_

#include <pthread.h>


#define FCS_SECTOR_COU	12



#include <sys/types.h>
typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;


// Helper class for various things (data extraction, pedestal calculation etc)
class fcs_data_c {
public:


	fcs_data_c() {
		sector = 0 ;
		rdo = 0 ;
		id = 0 ;
		board_id = 0 ;

		version = 0 ;


		realtime = 0 ;
		log_level = 0 ;

		// these are statics but stil...
		run_type = 0 ;
		run_number = 0 ;

		n_sigma = 4.0 ;
		n_pre = 8 ;
		n_post = 8 ;
		n_cou = 4 ;

		rhic_freq = 0 ;
		fee_state = 0 ;

		bad_error = 0 ;
		err_count = 0 ;
	} ;

	~fcs_data_c() {
//		LOG(WARN,"fcs_data_c: destructor?") ;
	} ;



	// per run
	void run_start(u_int run_number, int type) ;
	void run_stop(int bad_ped) ;

	void ped_start() ;
	void ped_stop(int bad_ped) ;



	// per event
	int start(u_short *d16, int shorts) ;
	int hdr_event() ;
	int event() ;
	int accum(u_int ch, u_int tb, u_short adc) ;
	int ana_ch() ;
	int event_end(int how) ;

	int event_stream() ;
	int event_pre_fy19() ;
	int accum_pre_fy19(u_int ch, u_int tb, u_short adc) ;


	u_short set_rdo(int rdo1) ;




	u_char log_level ;
	u_int events ;		// for this instance
	u_char realtime ;	// to disable some checks
	char want_saved ;

	u_char id ;		// when I have an array of fcs_data_c classes; id=0 is special

	u_short *dta_p ;
	u_short *dta_stop ;
	u_short *dta_start ;
	int dta_shorts ;

	u_int bad_error ;
	u_int err_count ;

	// streaming specific

	// count of xings per run entered in the worker thread
	// and later summed and presented/saved at the end of the run

	// occupancy
	u_int adc_xings[FCS_SECTOR_COU][8][32] ;	// sector,rdo,channel

	// for ZS
	int zs_start(u_short *results) ;
	u_char mark[8*1024] ;


	// for a specific instance
	u_char sector ;
	u_char rdo ;
	u_short board_id ;

	u_int rhic_freq ;
	u_char fee_state ;

	// temporary storage for the current board and event
	u_int version ;
	u_int hdr_trg_word ;
	u_int hdr_rhic_counter ;
	u_short hdr_board_id ;
	int hdr_sector ;
	int hdr_rdo ;
	u_int hdr_det ;
	u_int hdr_ns ;
	u_int hdr_dep ;
	short token ;
	u_char daq_cmd ;
	u_char trg_cmd ;

	u_char trgd_event ;
//	u_char has_ascii ;
	char *ascii_p ;
	int ascii_words ;
	static u_char ascii_no ;

	// temprorary storage for the current board, event and for a single channel
	int ch ;			// channel [0..32]
	int tb_cou ;			
	u_short adc[8*1024] ;		// storage for 1 channel
	int ch_count ;			// debugging
	u_int rhic_start ;		// debugging
	unsigned long long ch_mask_seen ;	// debugging
	int first_tb_cou;

	// I don't remember what the stuff is? Pre FY19.
//	int first_rhic_strobe_tick ;
//	int trigger_tick ;

	struct fcs_ped_inline_t {
		u_char fmt_version ;
		u_char det ;
		u_char ns ;
		u_char dep ;

		u_short params[16] ;	// stage params

		union {
			struct {	// DEP/ADC
				u_short ped ;
				u_short gain ;
			} ped[32] ;

			struct {	// STAGE2 & 3
				unsigned long long ch_mask ;
				u_char dsm_delay ;
				u_char dsm_mode ;
				u_char dsm_pattern ;
			} ;
		} ;

		u_char s1_delay ;	// delay of the strobe_in
	} ;

	struct fcs_ped_t {
		double mean[37] ;
		double rms[37] ;
		u_int cou[37] ;

		u_int bad_4[37] ;

		double mean_8[37] ;
		double rms_8[37] ;
		u_int cou_8[37] ;

		double tmp_val_8[37] ;
		u_int tmp_cou_8[37] ;

		float el_gain[37] ;	// electronics gain
		float et_gain[37] ;	// the Et adjustment due to position, from e.g. Akio


		// loaded into DEP for trigger
		u_short i_ped[37] ;	// integerized, multipled by 8
		u_short i_gain[37] ;	// integerized in the 4.8 (12 bit) form
	} ;

	struct rdo_map_t {
		u_char det ;	//0=ECAL,1=HCAL,2=PRE,3=Main
		u_char ns ;	//0=North,1=South
		u_char dep ;	// from 0 ;

		u_char crate ;	// 0..4
		u_char slot ;	// 0..19

		u_char crt ;	// from Akio's file
		u_char slt ;	// from Akio's file

		struct {	// from Akio's file
			u_short id ;
			u_char row ;
			u_char col ;
			u_char sc_dep ;
			u_char sc_bra ;
			u_char sc_add ;
			u_char sc_sipm ;
		} ch[32] ;

		unsigned long long ch_mask ;
	} ;

	struct det_map_t {
		u_char sector ;
		u_char rdo ;
	} ;


	// statics, common to all instalnces
	static u_int run_number ;
	static u_int run_type ;

	// for ZS
	static float n_sigma ;
	static short n_pre ;
	static short n_post ;
	static short n_cou ;
	static char n_mode ;	// 0: use rms, 1: fixed threshold in n_sigma

	// set in send_config, for shared access during data-checking
	static u_short ht_threshold ;
	static u_short tb_pre ;
	static u_short tb_all ;


	static struct fcs_ped_t ped[FCS_SECTOR_COU][8] ;	// sector,rdo
	static struct rdo_map_t rdo_map[FCS_SECTOR_COU][8] ;	// sector,rdo
	static struct det_map_t det_map[4][2][24] ;	// reverse map: det,ns,dep
	static u_char rdo_map_loaded ;			// boolean

	static int ped_from_cache(const char *fname) ;
        static int gain_from_cache(const char *fname=0) ;
	static int load_rdo_map(const char *fname=0) ;
	static int load_readout_map(const char *fname=0) ;
	static int load_sc_map(const char *fname=0) ;

	static u_char fcs_bad_ch_all[FCS_SECTOR_COU][8][34] ;
	static u_char fcs_bad_ch[8][34] ;

	static int load_bad_ch(const char *fname=0, int sector=0) ;

	static const char *stage_labels[32] ;
	static int stage_params_txt[32] ;	// from the params_xx file below
	static int load_stage_params(int sec1, const char *fname=0) ;

	// mutex for pedestals but also for statistics
	static struct statistics_t {
		float temperature ;
		float deadtime ;
		float rx_throttle ;
		int ht_rate ;
//		u_int odd_ped[32] ;
	} statistics[8] ;





#ifndef __CINT__
	static pthread_mutex_t ped_mutex ;
	static void ped_lock() {
		pthread_mutex_lock(&ped_mutex) ;
	} ;
	static void ped_unlock() {
		pthread_mutex_unlock(&ped_mutex) ;
	} ;
	static void ped_mutex_init() {
		pthread_mutex_init(&ped_mutex,0) ;
	} ;
#endif

	static long dep_to_char(int det, int ns, int dep) ;

private:
	u_short set_board_id() ;

} ;



#endif

