#ifndef _DAQ_TPC_H_
#define _DAQ_TPC_H_


#include <RTS_READER/daq_det.h>


class daq_tpc : public daq_det {
private:
	class daq_dta *handle_adc(int sec, int rdo) ;
	class daq_dta *handle_cld(int sec, int rdo) ;

	class daq_dta *adc ;	// "adc"
	class daq_dta *cld ;	// "cld"

	static const int MAX_SEC = 24 ;	// was TPC_MAXFEE; used to be 30 before FY05...
	static const int MAX_RDO = 6 ;	// not used


protected:


public:
	daq_tpc(const char *dname="tpc", rts_reader *rts_caller=0) ;
	~daq_tpc() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_TPC_H_
