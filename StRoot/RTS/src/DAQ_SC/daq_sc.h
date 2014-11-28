#ifndef _DAQ_SC_H_
#define _DAQ_SC_H_

#include <sys/types.h>  // for u_int

struct sc_t {
  u_int valid;       
  u_int time;
  int timelag;  // lag between read time and event time (to 1 sec)
  float mag_field;
  u_int rich_scalers[16];
};

#ifndef DAQ_SC_DATA_STRUCTURE
#include <stdio.h>
#include <DAQ_READER/daq_det.h>



class daq_sc : public daq_det {
private:
	class daq_dta *handle_legacy() ;
	class daq_dta *handle_raw() ;

	class daq_dta *legacy ;	// "legacy" bank
	class daq_dta *raw ;

	static const char *help_string ;
protected:


public:
	daq_sc(daqReader *rts_caller=0) ;
	~daq_sc() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;

#endif   //  DAQ_SC_DATA_STRUCTURE
#endif	// _DAQ_SC_H_
