#ifndef _DAQ_DTA_STRUCTS_H_
#define _DAQ_DTA_STRUCTS_H_

// Trigger basics
struct daq_trg_word {
	unsigned short t ;
	unsigned char daq ;
	unsigned char trg ;

	unsigned int clock ;
	unsigned int misc ;
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

	// extended info
	unsigned short pix_count ;
	unsigned short max_adc ;
	unsigned int cl_id ;	// global cluster id

	// embedding info
	unsigned short track_id ;
	short quality ;
} ;


struct daq_det_gain {
	float gain ;
	float t0 ;
} ;

#endif

