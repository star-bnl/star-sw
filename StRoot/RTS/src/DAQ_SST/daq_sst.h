#ifndef _DAQ_SST_H_
#define _DAQ_SST_H_

#include <DAQ_READER/daq_det.h>

#define SST_RDO_COU	3	//per sector maximum
#define SST_FIBER_COU	8
#define SST_HYBRID_COU	16
#define SST_STRIP_COU	768


struct daq_sst_data_t {
	u_short strip ;
	u_short adc ;

	u_char hybrid ;
} ;

class daq_sst : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;


	class daq_dta *raw ;
	class daq_dta *adc ;

	static const char *help_string ;

public:
	daq_sst(daqReader *rts_caller=0) ;
	~daq_sst() ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;


	class daq_dta *handle_adc(int sec, int rdo, char *buff=0, int words=0) ;


	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif
