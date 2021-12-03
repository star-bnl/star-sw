#ifndef _DAQ_FPD_H_
#define _DAQ_FPD_H_

#include <stdio.h>
#include <DAQ_READER/daq_det.h>



struct bbc_t {
	uint16_t pulse[32] ;
	uint16_t time[32] ;
	uint16_t proof[2] ;
	uint16_t spare[6] ;
	uint16_t ped[32] ;
	uint16_t rms[32] ;
	uint16_t peaks[64] ;
	uint32_t scl[32] ;
} ;

struct fpd_t {
	int mode ;
	int channels ;
	int max_channels ;

	uint16_t adc[256] ;
	uint16_t tdc[8] ;
	uint16_t reg[3] ;
	uint16_t ped[256] ;
	uint16_t rms[256] ;

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
