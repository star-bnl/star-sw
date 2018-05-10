#ifndef _DAQ_DTA_STRUCTS_H_
#define _DAQ_DTA_STRUCTS_H_

#include <sys/types.h>

// Trigger basics
struct daq_trg_word {
	short t ;		// token
	unsigned char daq ;	// daq command
	unsigned char trg ;	// trigger command
	
	unsigned int rhic ;	// rhic clock timestamp
	int rhic_delta ;		// delta from previous

	unsigned int reserved[5] ;
} ;


// I.e. raw TPC/TPX data
struct daq_adc_tb {
	unsigned short adc ;
	unsigned short tb ;
} ;


struct daq_sim_adc_tb {
	unsigned short adc ;
	unsigned short tb ;

	// embeding info:
	unsigned short track_id ;
	short reserved ;
} ;


// TPX/TPC clusters
struct daq_cld {
	float tb ;
	float pad ;

	unsigned short charge ;
	unsigned short flags ;

	unsigned short t1, t2 ;
	unsigned short p1, p2 ;
} ;


struct daq_sim_cld {
	// same as from file
	struct daq_cld cld ;

	// embedding info
	unsigned short track_id ;
	short quality ;
} ;

struct daq_sim_cld_x {
	// same as from file
	struct daq_cld cld ;

	// embedding info
	unsigned short track_id ;
	short quality ;

	// other data
	unsigned short pixels ;
	unsigned short max_adc ;

	unsigned int reserved[4] ;
} ;


struct daq_det_gain {
	float gain ;
	float t0 ;
} ;

struct daq_det_pedrms {
	float rms ;
	u_short ped ;
} ;

//size_t daq_dta_dict(const char *det, const char *bank) ;

#endif

