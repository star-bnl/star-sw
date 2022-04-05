#ifndef _DAQ_BTOW_H_
#define _DAQ_BTOW_H_


#include <DAQ_READER/daq_det.h>

#include <DAQ_EMC/daq_emc.h>

struct btow_t {
	short adc[BTOW_MAXFEE][BTOW_DATSIZE] ;
	short preamble[BTOW_MAXFEE][BTOW_PRESIZE] ;
} ;

extern const int btow_crate_map[] ;

class daq_btow : public daq_det {
private:
	class daq_dta *handle_raw() ;
	class daq_dta *handle_adc() ;

	class daq_dta *raw ;
	class daq_dta *adc ;
	
	static const char *help_string ;


protected:
	int Make() ;	// BTOW needs a special make...

public:
	daq_btow(daqReader *rts_caller=0) ;
	~daq_btow() ;


	virtual daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;


	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;


	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_BTOW_H_
