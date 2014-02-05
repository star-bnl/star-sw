#ifndef _DAQ_SST_H_
#define _DAQ_SST_H_

#include <DAQ_READER/daq_det.h>


struct daq_sst_data_t {
	u_short strip ;
	u_short adc ;

	u_char hybrid ;
} ;

class daq_sst : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_adc(int sec, int rdo, char *buff=0, int words=0) ;

	class daq_dta *raw ;
	class daq_dta *adc ;

	static const char *help_string ;

public:
	daq_sst(daqReader *rts_caller=0) ;
	~daq_sst() ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif
