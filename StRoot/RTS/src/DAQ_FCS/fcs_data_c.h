#ifndef _FCS_DATA_C_H_
#define _FCS_DATA_C_H_


// Helper class for various things (data extraction, pedestal calculation etc)
class fcs_data_c {
public:
	fcs_data_c() {
		sector = 1 ;
		rdo = 1 ;

		version = 0 ;
		board_id = 0 ;
		run_type = 0 ;
		run_number = 0 ;

		realtime = 0 ;
		log_level = 0 ;
	} ;

	~fcs_data_c() {;} ;

	
	int start(u_short *d16, int shorts) ;
	int event() ;
	int event_pre_fy19() ;
	int hdr_event() ;
	int accum(int ch, int tb, u_short adc) ;

	u_short *dta_p ;
	u_short *dta_stop ;
	u_short *dta_start ;
	int dta_shorts ;

	struct fcs_ped_t {
		double mean[32] ;
		double rms[32] ;
		u_int cou[32] ;
	} ;



	void run_start(u_int run_number, int type) ;
	void run_stop() ;
	void ped_start() ;
	void ped_stop() ;

	//globals
	u_char log_level ;
	u_int run_number ;
	int run_type ;
	u_int events ;

	u_char realtime ;	// to disable some checks

	// for a specific board
	int sector ;
	int rdo ;
	u_short board_id ;
	double fee_currents[32][3] ;
	struct fcs_ped_t ped ;

	// temporary storage for the current board and event
	u_int version ;
	u_int hdr_trg_word ;
	u_int hdr_rhic_counter ;
	u_char trgd_event ;



	int ch ;
	int tb_cou ;
	u_short adc[48*1024] ;
	int ch_count ;

	// I don't remember what the stuff is? Pre FY19.
	int first_rhic_strobe_tick ;
	int trigger_tick ;


	u_int rhic_start ;

} ;



#endif

