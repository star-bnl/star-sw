#ifndef _DAQ_FGT_H_
#define _DAQ_FGT_H_


#include <DAQ_READER/daq_det.h>

// logical maps
#define FGT_RDO_COU		2
#define FGT_ARM_COU		6
#define FGT_APV_COU		24
#define FGT_CH_COU		128
#define FGT_TIMEBIN_COU		192

// physical mapsa
#define FGT_DISK_COU		6
#define FGT_QUADRANT_COU	4
#define FGT_STRIP_R_COU		326
#define FGT_STRIP_PHI_COU	1138

#define FGT_STRIP_TYPE_R	0
#define FGT_STRIP_TYPE_PHI	1




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

	u_int rdo_warns[3] ;	// number of warnings issued, per rdo
	u_char rdo_id[3] ;	// for RDO checks in get_l2
	u_int t_data ;		// for trg_cmd==1 pattern tests...
} ;


#endif	// _DAQ_FGT_H_
