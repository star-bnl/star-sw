#ifndef _DAQ_ESMD_H_
#define _DAQ_ESMD_H_


#include <RTS_READER/daq_det.h>


class daq_esmd : public daq_det {
private:
	class daq_dta *handle_raw() ;
	class daq_dta *handle_adc() ;
	class daq_dta *handle_preamble() ;

	class daq_dta *raw ;	// "raw"
	class daq_dta *adc ;	// "adc"
	class daq_dta *preamble ;	// "preamble"

	// esmd has 48 somethings (FEEs) -- will call that "sectors" here!
	static const int MAX_SEC = 48 ;	// was ESMD_MAXFEE; used to be 30 before FY05...
	static const int MAX_RDO = 1 ;	// not used


protected:


public:
	daq_esmd(const char *dname="esmd", rts_reader *rts_caller=0) ;
	~daq_esmd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_ESMD_H_
