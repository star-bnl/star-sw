#ifndef _DAQ_GMT_H_
#define _DAQ_GMT_H_

#include <DAQ_READER/daq_det.h>


class daq_gmt : public daq_det {
private:
	class daq_dta *handle_raw(int rdo) ;

	class daq_dta *raw ;

	static const char *help_string ;

public:
	daq_gmt(daqReader *rts_caller=0) ;
	~daq_gmt() ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) {
		return -1 ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif
