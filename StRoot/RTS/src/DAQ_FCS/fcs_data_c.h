#ifndef _FCS_DATA_C_H_
#define _FCS_DATA_C_H_


// Helper class for various things (data extraction, pedestal calculation etc)
class fcs_data_c {
public:
	fcs_data_c() {
		sector = 0 ;
		rdo = 0 ;
		id = 0 ;

		version = 0 ;
		board_id = 0 ;
		run_type = 0 ;
		run_number = 0 ;

		realtime = 0 ;
		log_level = 0 ;

		n_sigma = 4.0 ;
		n_pre = 8 ;
		n_post = 8 ;
		n_cou = 4 ;
	} ;

	~fcs_data_c() {;} ;


	struct fcs_ped_t {
		double mean[32] ;
		double rms[32] ;
		u_int cou[32] ;


		float gain[32] ;

		// loaded into DEP
		u_short i_ped[32] ;	// integerized, multipled by 8
		u_char i_gain[32] ;
	} ;

	// per run
	void run_start(u_int run_number, int type) ;
	void run_stop() ;

	void ped_start() ;
	void ped_stop() ;
	int ped_from_cache(const char *fname=0) ;

	int gain_from_cache(const char *fname=0) ;

	// per event
	int start(u_short *d16, int shorts) ;
	int hdr_event() ;
	int event() ;
	int accum(u_int ch, u_int tb, u_short adc) ;
	int event_end(int how) ;

	int event_pre_fy19() ;

	u_char log_level ;
	u_int events ;
	u_char realtime ;	// to disable some checks

	u_char id ;	// when I have an array of fcs_data_c classes; id=0 is special

	u_short *dta_p ;
	u_short *dta_stop ;
	u_short *dta_start ;
	int dta_shorts ;

	// for ZS
	int zs_start(u_short *results) ;
	u_char mark[8*1024] ;

	float n_sigma ;
	short n_pre ;
	short n_post ;
	short n_cou ;


	// for a specific instance
	u_char sector ;
	u_char rdo ;
	u_short board_id ;

	// temporary storage for the current board and event
	u_int version ;
	u_int hdr_trg_word ;
	u_int hdr_rhic_counter ;
	u_char trgd_event ;

	int ch ;
	int tb_cou ;
	u_short adc[8*1024] ;
	int ch_count ;
	u_int rhic_start ;
	unsigned long long ch_mask_seen ;


	// I don't remember what the stuff is? Pre FY19.
	int first_rhic_strobe_tick ;
	int trigger_tick ;



	// statics, common to all instalnces
	static double fee_currents[8][32][3] ;	// 8 RDOs, 32 channels
	static struct fcs_ped_t ped[8] ;	// 8 RDOs
	static u_int run_number ;
	static u_int run_type ;

	
} ;



#endif

