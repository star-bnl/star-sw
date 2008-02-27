#ifndef _DAQ_TOF_H_
#define _DAQ_TOF_H_


#include <RTS_READER/rts_reader.h>


class daq_tof : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;

	class daq_dta *raw ;

	static const int MAX_SEC = 1 ;
	static const int MAX_RDO = 4 ;

	static const char *help_string ;
protected:


public:
	daq_tof(const char *dname="TOF", rts_reader *rts_caller=0, detHandler *det_h=0) ;
	~daq_tof() ;



	void help() const { printf("%s",help_string) ; } ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

#ifdef __ROOT__
ClassDef(daq_tof,0)
#endif

} ;


#endif	// _DAQ_TOF_H_
