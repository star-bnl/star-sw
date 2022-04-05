#ifndef _DAQ_SVT_H_
#define _DAQ_SVT_H_

#include <stdio.h>
#include <DAQ_READER/daq_det.h>



struct svt_t {
	int channels ;
	int mode ;	// 0 normal, 1 pedestals/RMSs
	int max_channels ;
	int pre, post, pedoffset ;

	// how many valid timebins in this hybrid
	u_char counts[24][3][6][240] ;
	// up to 128 valid timebins (count is in counts)
	// timebin is overloaded for pedestal RMS data!
	u_char timebin[24][3][6][240][128] ;

	// up to 128 valid adcs (same count as above...)
	u_char adc[24][3][6][240][128] ;

	// helpers for the remap
	u_char B[24][3][6] ;	// from RB,MZ,ASIC (all start from 1!) to Barrel
	u_char L[24][3][6] ;	// ... to Ladder
	u_char W[24][3][6] ;	// ... to Wafer
	u_char H[24][3][6] ;	// ... to Hybrid
} ;





class daq_svt : public daq_det {
private:
	class daq_dta *handle_legacy() ;

	class daq_dta *legacy ;	// "legacy" bank

	static const char *help_string ;
protected:


public:
	daq_svt(daqReader *rts_caller=0) ;
	~daq_svt() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_SVT_H_
