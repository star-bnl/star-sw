#ifndef _DAQ_HLT_H_
#define _DAQ_HLT_H_

struct hlt_gl3_t {
	char *buff ;
	int bytes ;
	char name[32] ;
	char data[] ;
} ;

#ifndef DAQ_HLT_DATA_STRUCTURE
#include <DAQ_READER/daq_det.h>

class daq_hlt : public daq_det {
private:
	class daq_dta *handle_tpx(int sector) ;
	class daq_dta *handle_tof() ;
	class daq_dta *handle_trg() ;
	class daq_dta *handle_gl3(int sector, const char *bank) ;

	class daq_dta *tpx ;	
	class daq_dta *tof ;	
	class daq_dta *trg ;	
	class daq_dta *gl3 ;	

	static const char *help_string ;

protected:


public:
	daq_hlt(daqReader *rts_caller=0) ;
	~daq_hlt() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;



	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;

#endif   //  DAQ_HLT_DATA_STRUCTURE
#endif	// _DAQ_HLT_H_
