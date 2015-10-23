#ifndef _DAQ_SST_H_
#define _DAQ_SST_H_

#include <DAQ_READER/daq_det.h>

#define SST_RDO_COU	3	//per sector maximum
#define SST_FIBER_COU	8
#define SST_HYBRID_COU	16
#define SST_STRIP_COU	768


struct daq_sst_data_t {
	u_short strip ;
	u_short adc ;

	u_char hybrid ;
} ;

struct daq_sst_pedrms_t {
	short ped[SST_HYBRID_COU][SST_STRIP_COU] ;
	short rms[SST_HYBRID_COU][SST_STRIP_COU] ;	//double d_rms = (double)rms/16.0
} ;

/*
	According to Jim on Feb 2014 the pedestals are calculated in the
	following way:

	ADCprime = (ADC + PED_ADC_OFFSET) % 1024 ;
	if(ADCprime > PED_ADC_HI) skip ;
	if(ADCprime < PED_ADC_LO) skip ;
	...
	ped = mean(ADCprime) - PED_ADC_OFFSET
	rms = sigma(ADCprime)
*/

#define SST_PED_ADC_OFFSET	375
#define SST_OUTLIERBAND		256

//#define SST_PED_ADC_HI		750
//#define SST_PED_ADC_LO		250

struct daq_sst_ped_t {
	double ped[SST_FIBER_COU][SST_HYBRID_COU][SST_STRIP_COU];
	double rms[SST_FIBER_COU][SST_HYBRID_COU][SST_STRIP_COU];
	u_int cou[SST_FIBER_COU][SST_HYBRID_COU][SST_STRIP_COU];
} ;


class daq_sst : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_ped(int sec) ;
	class daq_dta *handle_adc(int sec, int rdo, char *buff=0, int words=0) ;

	class daq_dta *raw ;
	class daq_dta *adc ;
	class daq_dta *ped ;

	static const char *help_string ;



public:
	daq_sst(daqReader *rts_caller=0) ;
	~daq_sst() ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;


	int raw_to_adc_utility(int s, int r, char *rdobuff, int words, daq_sst_ped_t *ped, int mode) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}


	u_int events[SST_RDO_COU] ;
	u_int fiber_events[SST_RDO_COU][SST_FIBER_COU] ;

} ;


#endif
