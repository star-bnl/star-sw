#ifndef _DAQ_ITPC_H_
#define _DAQ_ITPC_H_

#include <DAQ_READER/daq_det.h>





class daq_itpc : public daq_det {
private:
	class daq_dta *handle_ifee_raw() ;
	class daq_dta *handle_ifee_sampa() ;

	class daq_dta *ifee_raw ;
	class daq_dta *ifee_sampa ;

	static const char *help_string ;
protected:


public:
	daq_itpc(daqReader *rts_caller=0) ;
	~daq_itpc() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;


	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	int get_token(char *buff, int buff_bytes) ;


	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif

