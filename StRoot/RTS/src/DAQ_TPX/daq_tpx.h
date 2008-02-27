#ifndef _DAQ_TPX_H_
#define _DAQ_TPX_H_


#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_det.h>


class daq_algo ;

class daq_tpx : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_adc(int sec, int rdo) ;
	class daq_dta *handle_cld(int sec, int rdo) ;
	class daq_dta *handle_cld_raw(int sec, int rdo) ;

	// direct maps to file content
	class daq_dta *raw ;
	class daq_dta *cld_raw ;
	class daq_dta *gain ;	// token 0
	class daq_dta *ped ;	// token 0

	// calculated from raw
	class daq_dta *cld_c ;
	class daq_dta *adc ;
	class daq_dta *statistics ;

	// calculated from cld_raw
	class daq_dta *cld ;

	// at finish run via "raw"
	class daq_dta *gain_c ;
	class daq_dta *ped_c ;

	// algorithms
	class daq_algo *a_ped ;
	class daq_algo *a_stat ;
	class daq_algo *a_gain ;
	class daq_algo *a_fcf ;
	class daq_algo *a_raw ;

	static const int MAX_SEC = 24 ;
	static const int MAX_RDO = 6 ;

	static const char *help_string ;

protected:

public:
	daq_tpx(const char *dname="TPX", rts_reader *rts_caller=0) ;
	~daq_tpx() ;



	int Make() ;			// equivalent to analyze
	int InitRun(int run_num) ;	// used in send_config
	int FinishRun(int old_run) ;	// used in inject_token0


	class daq_dta  *get(const char *bank="*", int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	// these functions perform on the raw DDL buffer data i.e. per RDO
	int get_token(char *buff, int buff_bytes) ;
	int get_l2(char *buff, int buff_bytes, daq_trg_word *trg, int prompt=0) ;


	

	// trivial stuff below...
	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

	void help() const { printf("%s\n%s\n",GetCVS(),help_string) ; } ;

#ifdef __RTS_ROOT__
Class_Def(daq_tpx,0)
#endif
} ;


#endif	// _DAQ_TPX_H_
