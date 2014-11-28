#ifndef _DAQ_MTD_H_
#define _DAQ_MTD_H_


#include <DAQ_READER/daq_det.h>

// modelled after TOF...

struct mtd_t {
	int mode ;
	int channels ;
	int max_channels ;


	u_int ddl[2][12000] ;	// content of up to 4 fibers; was 10000 before FY09 but
				// Jo Schambach claims the maximum can be 11745
	u_int ddl_words[2] ;	// the count of words (32bit) for above
} ;





class daq_mtd : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_legacy() ;	// in the sense of TOF

	class daq_dta *raw ;
	class daq_dta *legacy ;


	static const char *help_string ;
protected:


public:
	daq_mtd(daqReader *rts_caller=0) ;
	~daq_mtd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;


	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	int get_token(char *buff, int buff_bytes) ;

	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

	static const int MAX_SEC = 1 ;
	static const int MAX_RDO = 2 ;	// 1 in FY11


} ;


#endif	// _DAQ_MTD_H_
