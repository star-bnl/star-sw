#ifndef _DAQ_PP2PP_H_
#define _DAQ_PP2PP_H_


#include <RTS_READER/daq_det.h>


class daq_pp2pp : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;

	class daq_dta *raw ;

	static const int MAX_SEC = 2 ;
	static const int MAX_RDO = 11 ;	// can be 0 for all RDOs 

	static const char *help_string ;
protected:


public:
	daq_pp2pp(const char *dname="PP2PP", rts_reader *rts_caller=0) ;
	~daq_pp2pp() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_PP2PP_H_
