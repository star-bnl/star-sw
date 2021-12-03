#ifndef _FCS_DATA_C_H_
#define _FCS_DATA_C_H_

#include <pthread.h>


#define FCS_SECTOR_COU	12



#include <stdint.h>


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
	void run_start(uint32_t run_number, int type) ;
	void run_stop(int bad_ped) ;

	void ped_start() ;
	void ped_stop(int bad_ped) ;



	// per event
	int start(uint16_t *d16, int shorts) ;
	int hdr_event() ;
	int event() ;
	int accum(uint32_t ch, uint32_t tb, uint16_t adc) ;
	int ana_ch() ;
	int event_end(int how) ;

	int event_stream() ;
	int event_pre_fy19() ;
	int accum_pre_fy19(uint32_t ch, uint32_t tb, uint16_t adc) ;


	uint16_t set_rdo(int rdo1) ;




	uint8_t log_level ;
	uint32_t events ;		// for this instance
	uint8_t realtime ;	// to disable some checks
	char want_saved ;

	uint8_t id ;		// when I have an array of fcs_data_c classes; id=0 is special

	uint16_t *dta_p ;
	uint16_t *dta_stop ;
	uint16_t *dta_start ;
	int dta_shorts ;

	uint32_t bad_error ;
	uint32_t err_count ;

	// streaming specific

	// count of xings per run entered in the worker thread
	// and later summed and presented/saved at the end of the run

	// occupancy
	uint32_t adc_xings[FCS_SECTOR_COU][8][32] ;	// sector,rdo,channel

	// for ZS
	int zs_start(uint16_t *results) ;
	uint8_t mark[8*1024] ;


	// for a specific instance
	uint8_t sector ;
	uint8_t rdo ;
	uint16_t board_id ;

	uint32_t rhic_freq ;
	uint8_t fee_state ;

	// temporary storage for the current board and event
	uint32_t version ;
	uint32_t hdr_trg_word ;
	uint32_t hdr_rhic_counter ;
	uint16_t hdr_board_id ;
	int hdr_sector ;
	int hdr_rdo ;
	uint32_t hdr_det ;
	uint32_t hdr_ns ;
	uint32_t hdr_dep ;
	short token ;
	uint8_t daq_cmd ;
	uint8_t trg_cmd ;

	uint8_t trgd_event ;
//	uint8_t has_ascii ;
	char *ascii_p ;
	int ascii_words ;
	static uint8_t ascii_no ;

	// temprorary storage for the current board, event and for a single channel
	int ch ;			// channel [0..32]
	int tb_cou ;			
	uint16_t adc[8*1024] ;		// storage for 1 channel
	int ch_count ;			// debugging
	uint32_t rhic_start ;		// debugging
	unsigned long long ch_mask_seen ;	// debugging
	int first_tb_cou;

	// I don't remember what the stuff is? Pre FY19.
//	int first_rhic_strobe_tick ;
//	int trigger_tick ;

	struct fcs_ped_inline_t {
		uint8_t fmt_version ;
		uint8_t det ;
		uint8_t ns ;
		uint8_t dep ;

		uint16_t params[16] ;	// stage params

		union {
			struct {	// DEP/ADC
				uint16_t ped ;
				uint16_t gain ;
			} ped[32] ;

			struct {	// STAGE2 & 3
				unsigned long long ch_mask ;
				uint8_t dsm_delay ;
				uint8_t dsm_mode ;
				uint8_t dsm_pattern ;
			} ;
		} ;

		uint8_t s1_delay ;	// delay of the strobe_in
	} ;

	struct fcs_ped_t {
		double mean[37] ;
		double rms[37] ;
		uint32_t cou[37] ;

		uint32_t bad_4[37] ;

		double mean_8[37] ;
		double rms_8[37] ;
		uint32_t cou_8[37] ;

		double tmp_val_8[37] ;
		uint32_t tmp_cou_8[37] ;

		float el_gain[37] ;	// electronics gain
		float et_gain[37] ;	// the Et adjustment due to position, from e.g. Akio


		// loaded into DEP for trigger
		uint16_t i_ped[37] ;	// integerized, multipled by 8
		uint16_t i_gain[37] ;	// integerized in the 4.8 (12 bit) form
	} ;

	struct rdo_map_t {
		uint8_t det ;	//0=ECAL,1=HCAL,2=PRE,3=Main
		uint8_t ns ;	//0=North,1=South
		uint8_t dep ;	// from 0 ;

		uint8_t crate ;	// 0..4
		uint8_t slot ;	// 0..19

		uint8_t crt ;	// from Akio's file
		uint8_t slt ;	// from Akio's file

		struct {	// from Akio's file
			uint16_t id ;
			uint8_t row ;
			uint8_t col ;
			uint8_t sc_dep ;
			uint8_t sc_bra ;
			uint8_t sc_add ;
			uint8_t sc_sipm ;
		} ch[32] ;

		unsigned long long ch_mask ;
	} ;

	struct det_map_t {
		uint8_t sector ;
		uint8_t rdo ;
	} ;


	// statics, common to all instalnces
	static uint32_t run_number ;
	static uint32_t run_type ;

	// for ZS
	static float n_sigma ;
	static short n_pre ;
	static short n_post ;
	static short n_cou ;
	static char n_mode ;	// 0: use rms, 1: fixed threshold in n_sigma

	// set in send_config, for shared access during data-checking
	static uint16_t ht_threshold ;
	static uint16_t tb_pre ;
	static uint16_t tb_all ;


	static struct fcs_ped_t ped[FCS_SECTOR_COU][8] ;	// sector,rdo
	static struct rdo_map_t rdo_map[FCS_SECTOR_COU][8] ;	// sector,rdo
	static struct det_map_t det_map[4][2][24] ;	// reverse map: det,ns,dep
	static uint8_t rdo_map_loaded ;			// boolean

	static int ped_from_cache(const char *fname) ;
        static int gain_from_cache(const char *fname=0) ;
	static int load_rdo_map(const char *fname=0) ;
	static int load_readout_map(const char *fname=0) ;
	static int load_sc_map(const char *fname=0) ;

	static uint8_t fcs_bad_ch_all[FCS_SECTOR_COU][8][34] ;
	static uint8_t fcs_bad_ch[8][34] ;

	static int load_bad_ch(const char *fname=0, int sector=0) ;

	// mutex for pedestals but also for statistics
	static struct statistics_t {
		float temperature ;
		float deadtime ;
		float rx_throttle ;
		int ht_rate ;
//		uint32_t odd_ped[32] ;
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
	uint16_t set_board_id() ;

} ;



#endif

