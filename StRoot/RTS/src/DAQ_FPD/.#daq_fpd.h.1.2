#ifndef _DAQ_FPD_H_
#define _DAQ_FPD_H_

#include <stdio.h>
#include <DAQ_READER/daq_det.h>



struct bbc_t {
	u_short pulse[32] ;
	u_short time[32] ;
	u_short proof[2] ;
	u_short spare[6] ;
	u_short ped[32] ;
	u_short rms[32] ;
	u_short peaks[64] ;
	u_int scl[32] ;
} ;

struct fpd_t {
	int mode ;
	int channels ;
	int max_channels ;

	u_short adc[256] ;
	u_short tdc[8] ;
	u_short reg[3] ;
	u_short ped[256] ;
	u_short rms[256] ;

	struct bbc_t bbc ;
		
} ;	





class daq_fpd : public daq_det {
private:
	class daq_dta *handle_legacy() ;

	class daq_dta *legacy ;	// "legacy" bank

	static const char *help_string ;
protected:


public:
	daq_fpd(daqReader *rts_caller=0) ;
	~daq_fpd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_FPD_H_
