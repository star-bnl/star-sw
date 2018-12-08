#ifndef _DAQ_FCS_H_
#define _DAQ_FCS_H_

#include <DAQ_READER/daq_det.h>





class daq_fcs : public daq_det {
private:
	class daq_dta *handle_raw() ;
	class daq_dta *handle_adc() ;

	class daq_dta *raw ;
	class daq_dta *adc ;

	static const char *help_string ;
protected:


public:
	daq_fcs(daqReader *rts_caller=0) ;
	~daq_fcs() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;


	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	int get_token(char *buff, int buff_bytes) ;


	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

	u_int version ;
} ;


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


	// I don't remember what the stuff is? Pre FY19.
	int first_rhic_strobe_tick ;
	int trigger_tick ;


	u_int rhic_start ;

	
	



} ;



#endif

