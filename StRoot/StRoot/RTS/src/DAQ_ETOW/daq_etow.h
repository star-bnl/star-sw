#ifndef _DAQ_ETOW_H_
#define _DAQ_ETOW_H_


#include <DAQ_READER/daq_det.h>

#include <DAQ_EMC/daq_emc.h>

struct etow_t {
	short adc[ETOW_MAXFEE][ETOW_DATSIZE] ;
	short preamble[ETOW_MAXFEE][ETOW_PRESIZE] ;
} ;

extern const int etow_crate_map[] ;


class daq_etow : public daq_det {
private:
	class daq_dta *handle_raw() ;
	class daq_dta *handle_adc() ;

	class daq_dta *raw ;
	class daq_dta *adc ;

	
	static const char *help_string ;


protected:
	int Make() ;	// ETOW needs a special make...

public:
	daq_etow(daqReader *rts_caller=0) ;
	~daq_etow() ;


	virtual daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_ETOW_H_
