#ifndef _DAQ_L3_H_
#define _DAQ_L3_H_

#include <stdio.h>
#include <DAQ_READER/daq_det.h>
#include <daqFormats.h>

#define L3_MAX_NR_TRACKS 10000

struct l3_t {
    int mode;	// for FY09 HLT this contains the "decision"!
    int channels;	// for FY09 HLT this contains the unique event sequence!
    int max_channels;
    
    u_int tracks_num;
    u_int cluster_num;
    float xVertex;
    float yVertex;
    float zVertex;

    global_track track[L3_MAX_NR_TRACKS];
};



class daq_l3 : public daq_det {
private:
	class daq_dta *handle_legacy() ;

	class daq_dta *legacy ;	// "legacy" bank

	static const char *help_string ;
protected:


public:
	daq_l3(daqReader *rts_caller=0) ;
	~daq_l3() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_L3_H_
