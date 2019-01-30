#ifndef _DAQ_L4_H_
#define _DAQ_L4_H_

#pragma pack(1)
struct l4_gl3_t {
	char *buff ;
	int bytes ;
	char name[32] ;
	char data[] ;
} ;
#pragma pack()

#ifndef DAQ_L4_DATA_STRUCTURE
#include <DAQ_READER/daq_det.h>

class daq_l4 : public daq_det {
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
	daq_l4(daqReader *rts_caller=0) ;
	~daq_l4() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;



	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;

#endif   //  DAQ_L4_DATA_STRUCTURE
#endif	// _DAQ_L4_H_
