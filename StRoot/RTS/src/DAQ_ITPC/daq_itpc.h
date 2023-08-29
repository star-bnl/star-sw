#ifndef _DAQ_ITPC_H_
#define _DAQ_ITPC_H_

#include <DAQ_READER/daq_det.h>



class itpcInterpreter ;
class itpc_fcf_c ;

class daq_itpc : public daq_det {
private:

	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_sampa(int sec, int rdo, int in_adc) ;
	class daq_dta *handle_ped(int sec, int rdo) ;
	class daq_dta *handle_cld(int sec) ;
	class daq_dta *handle_cld_sim(int sec) ;

	class daq_dta *raw ;
	class daq_dta *sampa ;
	class daq_dta *ped ;
	class daq_dta *cld ;

	// for simulation
	class daq_dta *adc_sim ;
	class daq_dta *cld_sim ;
	class daq_dta *gain ;

	//OLD FY17 2-FEE data
	class daq_dta *handle_ifee_fy17_raw() ;
	class daq_dta *handle_ifee_fy17_sampa() ;

	class daq_dta *ifee_fy17_raw ;
	class daq_dta *ifee_fy17_sampa ;

	// main data interpreter, unpackeer and checker
	itpcInterpreter *it ;

	// for Offline cluster finder and gain loaded...
	itpc_fcf_c *fcf[25] ;	// one per sector
	int fcf_gains_loaded ;	// 
	int fcf_det_type ;
	int fcf_det_orient ;
	



	static const char *help_string ;
protected:


public:
	daq_itpc(daqReader *rts_caller=0) ;
	~daq_itpc() ;


	daq_dta *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;
	daq_dta *put(const char *in_bank="*", int sec=-1, int row=-1, int pad=-1, void *p1=0, void *p2=0) ;


	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	int get_token(char *buff, int buff_bytes) ;


	void run_stop() ;
	void setup_fcf(int det, int orient) ;

	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

	static int no_sw16 ;
} ;


#endif

