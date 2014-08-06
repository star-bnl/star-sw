#ifndef _DAQ_RIC_H_
#define _DAQ_RIC_H_

#include <stdio.h>

#include <DAQ_READER/daq_det.h>

struct ric_t {
	int mode ;
	int channels ;
	int max_channels ;	// 16*960

	u_short adc[16][960] ;
} ;



class daq_ric : public daq_det {
private:
	class daq_dta *handle_legacy() ;

	class daq_dta *legacy ;	// "legacy" bank

	static const char *help_string ;
protected:


public:
	daq_ric(daqReader *rts_caller=0) ;
	~daq_ric() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_RIC_H_
