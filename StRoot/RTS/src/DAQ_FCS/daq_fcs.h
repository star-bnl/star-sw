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

} ;


// Helper class for various things (data extraction, pedestal calculation etc)
class fcs_data_c {
public:
	fcs_data_c() {
		sector = 1 ;
		rdo = 1 ;
		ch = -1 ;
		tb_cou = 0 ;
	} ;

	~fcs_data_c() {;} ;

	
	int start(u_short *d16, int shorts) ;
	int event() ;
	int accum(int ch, int tb, u_short adc) ;

	u_short *dta_p ;
	u_short *dta_stop ;

	struct fcs_ped_t {
		double mean[16] ;
		double rms[16] ;
		u_int cou[16] ;
	} ;

	struct fcs_ped_t ped ;
	int ped_run ;
	void ped_start() ;
	void ped_stop() ;

	//globals
	u_int run_number ;
	double fee_currents[16][3] ;

	// temporary storage for 1 channel
	int sector ;
	int rdo ;
	int ch ;
	int tb_cou ;
	u_short adc[16*1024] ;
} ;



#endif

