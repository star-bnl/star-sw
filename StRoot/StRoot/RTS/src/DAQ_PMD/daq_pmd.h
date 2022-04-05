#ifndef _DAQ_PMD_H_
#define _DAQ_PMD_H_

#include <sys/types.h>
#include <daqFormats.h>

struct pmd_t {
	int mode ;	// 0 normal, 1 ped
	int channels ;
	int max_channels ;	// 2*10*2*2016

	u_int status[2] ;

	// 2 sectors, 10 CRAMS, 2 CRAM channels, 2016 values max
	u_short adc[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;

	u_short ped[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	u_short rms[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	u_short thr[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;

} ;

#ifndef DAQ_PMD_DATA_STRUCTURE
#include <stdio.h>
#include <DAQ_READER/daq_det.h>

class daq_pmd : public daq_det {
private:
	class daq_dta *handle_legacy() ;
	class daq_dta *handle_raw(int sec) ;

	class daq_dta *legacy ;	// "legacy" bank
	class daq_dta *raw ;	// raw bank

	static const char *help_string ;
protected:


public:
	daq_pmd(daqReader *rts_caller=0) ;
	~daq_pmd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif   //  DAQ_PMD_DATA_STRUCTURE
#endif	// _DAQ_PMD_H_
