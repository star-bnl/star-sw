#ifndef _DAQ_TPX_H_
#define _DAQ_TPX_H_


#include <DAQ_READER/daq_det.h>


// forward decls
class tpxPed ;
class tpxGain ;
class tpxFCF ;
class tpxStat ;
class daqReader;
class tpxFCF_2D ;

class daq_tpx : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_legacy(int sec, int rdo) ;
	class daq_dta *handle_adc(int sec, int rdo) ;
	class daq_dta *handle_altro(int sec, int rdo) ;
	class daq_dta *handle_cld(int sec, int rdo) ;
	class daq_dta *handle_cld_raw(int sec, int rdo) ;
	class daq_dta *handle_cld_sim(int sec, int row) ;
	class daq_dta *handle_cld_2d_sim(int sec, int row) ;
	class daq_dta *handle_ped(int sec) ;

	// direct maps to file content:
	class daq_dta *raw ;	
	class daq_dta *cld_raw ;
	class daq_dta *ped_raw ;	// token 0

	// calculated from "raw" (sfs "adc")
	class daq_dta *adc ;

	// calculated from "cld_raw"	(sfs adc)
	class daq_dta *cld ;

	// made to look like TPC!
	class daq_dta *legacy ;

	// input classes by the user
	class daq_dta *adc_sim ;
	class daq_dta *gain ;	// ACTIVE: from file or database
	class daq_dta *ped ;	// ACTIVE: from file

	// calculated from "adc_sim"
	class daq_dta *cld_sim ;
	class daq_dta *cld_2d_sim ;

	// calculated via algorithm at end-run
	class daq_dta *ped_c ;	// in ped runs 
	class daq_dta *gain_c ;	// in pulser runs

	class daq_dta *altro ;


	// algorithms
	class tpxPed *ped_algo ;

	class tpxStat *stat_algo ;



	u_int *fcf_tmp_storage ;
	
	static const int FCF_TMP_BYTES = (1024*1024) ;	// for local FCF

	static const int MAX_SEC = 24 ;
	static const int MAX_RDO = 6 ;

	static const char *help_string ;

protected:

public:
	daq_tpx(daqReader *rts_caller=0) ;
	~daq_tpx() ;

	class tpxGain *gain_algo ;

	char fcf_afterburner_disable ;
	char fcf_run_compatibility ;
	char fcf_do_cuts ;

	// for use by simulation
	int sim_row_count ;
	unsigned char *sim_tpx_rowlen ;


	class tpxFCF *fcf_algo[25] ;
	class tpxFCF_2D *fcf_2d_algo[25] ;

	int InitRun(int run_num) ;	// used in send_config
	int FinishRun(int old_run) ;	// used in inject_token0


	class daq_dta  *get(const char *bank="*", int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	class daq_dta  *put(const char *bank="*", int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	// these functions perform on the raw DDL buffer data i.e. per RDO
	int get_token(char *buff, int buff_bytes) ;
	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt=0) ;



	// trivial stuff below...
	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

	void help() const ;

} ;


#endif	// _DAQ_TPX_H_
