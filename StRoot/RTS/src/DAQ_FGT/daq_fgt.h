#ifndef _DAQ_FGT_H_
#define _DAQ_FGT_H_


#include <DAQ_READER/daq_det.h>


#define FGT_STRIP_TYPE_R	0
#define FGT_STRIP_TYPE_PHI	1

#define FGT_STRIP_R_COU		326
#define FGT_STRIP_PHI_COU	1138

#define FGT_POINT_MAX_COU	10	//?

// when asking for the "phys" bank
struct fgt_phys_t {
	unsigned short strip ;	// 0..1137 for phi-strips; 0..325 for r-strips
	unsigned short adc ;
	// grumble... does not align nicely... grumble...
	unsigned char point ;	// 0..9?
} ;

// when asking for the "adc" bank
struct fgt_adc_t {
	unsigned char ch ;	// 0..127
	unsigned char point ;	// 0..9 (?)
	unsigned short adc ;
} ;

	
struct fgt_pedrms_t {
	unsigned char ch ;
	unsigned char point ;
	float ped ;
	float rms ;
} ;

class daq_fgt : public daq_det {
private:
	class daq_dta *handle_raw(int rdo) ;
	class daq_dta *handle_adc(int rdo) ;
	class daq_dta *handle_phys(int disk, int quadrant, int strip_type) ;
	class daq_dta *handle_ped(int rdo) ;

	class daq_dta *raw ;	// "raw"
	class daq_dta *adc ;	// "adc"
	class daq_dta *phys ;	// "phys"
	class daq_dta *ped ;	// "pedrms" 

	static const char *help_string ;

protected:


public:
	daq_fgt(daqReader *rts_caller=0) ;
	~daq_fgt() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_FGT_H_
