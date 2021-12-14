#ifndef _DAQ_STGC_H_
#define _DAQ_STGC_H_


#include <DAQ_READER/daq_det.h>

struct stgc_vmm_t {
	u_char feb_vmm ;	
	u_char ch ;
	u_short adc ;
	u_short bcid ;
	short bcid_delta ;
	short tb ;	// from the trigger
} ;

class daqReader;

class daq_stgc : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_vmmraw(int sec) ;
	class daq_dta *handle_altro(int sec, int rdo) ;
	class daq_dta *handle_ped(int sec) ;
	class daq_dta *handle_vmm(int sec) ;

	// direct maps to file content:
	class daq_dta *raw ;	
	class daq_dta *altro ;
	class daq_dta *vmm ;
	class daq_dta *vmmraw ;

	static const int MAX_SEC = 7 ;
	static const int MAX_RDO = 6 ;

	static const char *help_string ;

protected:

public:
	daq_stgc(daqReader *rts_caller=0) ;
	~daq_stgc() ;


	class daq_dta  *get(const char *bank="*", int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	// these functions perform on the raw DDL buffer data i.e. per RDO
	int get_token(char *buff, int buff_bytes) ;
	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt=0) ;

	// trivial stuff below...
	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

	void help() const ;

	int xing_min ;
	int xing_max ;

	class stgc_data_c *stgc_d ;

private:
	int get_l2_vmm(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt=0) ;
} ;


#endif	// _DAQ_STGC_H_
