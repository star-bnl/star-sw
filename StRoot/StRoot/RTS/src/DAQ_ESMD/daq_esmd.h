#ifndef _DAQ_ESMD_H_
#define _DAQ_ESMD_H_


#include <DAQ_READER/daq_det.h>

#include <DAQ_EMC/daq_emc.h>


struct esmd_t {
	short adc[ESMD_MAXFEE][ESMD_DATSIZE] ;
	short preamble[ESMD_MAXFEE][ESMD_PRESIZE] ;	// this is the preamble...
} ;

extern int esmd_crate_map[] ;

class daq_esmd : public daq_det {
private:
	class daq_dta *handle_raw() ;
	class daq_dta *handle_adc() ;

	class daq_dta *raw ;	// "raw"
	class daq_dta *adc ;	// "adc"


	static const char *help_string ;

protected:


public:
	daq_esmd(daqReader *rts_caller=0) ;
	~daq_esmd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_ESMD_H_
