#ifndef _DAQ_FGT_H_
#define _DAQ_FGT_H_


#include <DAQ_READER/daq_det.h>

// logical maps
#define FGT_RDO_COU		2
#define FGT_ARM_COU		6
#define FGT_APV_COU		24
#define FGT_CH_COU		128

#define FGT_TB_COU		31	//???



// physical maps
#define FGT_DISK_COU		6	// 0..5
#define FGT_QUADRANT_COU	4	// A-D
#define FGT_STRIP_TYPE_COU	2	// 2 types: see below
#define FGT_STRIP_TYPE_R	0
#define FGT_STRIP_TYPE_PHI	1

#define FGT_STRIP_R_COU		326
#define FGT_STRIP_PHI_COU	1138



// when asking for the "adc" or "phys"bank
struct fgt_adc_t {
	unsigned short ch ;	// 0..127 for adc ch, 0..1137 for phys strip
	unsigned char tb ;	// 0..9 (?)
	unsigned short adc ;
} ;

	
struct fgt_pedrms_t {
	unsigned short ch ;
	unsigned char tb ;
	float ped ;
	float rms ;
} ;

class daq_fgt : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;

	class daq_dta *handle_phys(int disk, int quadrant, int strip_type) ;
	class daq_dta *handle_ped(int sec, int rdo) ;

	class daq_dta *raw ;	// "raw"
	class daq_dta *adc ;	// "adc"
	class daq_dta *phys ;	// "phys"
	class daq_dta *ped ;	// "pedrms" 

	static const char *help_string ;

protected:


public:
	daq_fgt(daqReader *rts_caller=0) ;
	~daq_fgt() ;


	void set_flavor(int id) ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
	}


	class daq_dta *handle_adc(int sec, int rdo, char *rdobuff = 0 ) ;


	u_int rdo_warns[7] ;	// number of warnings issued, per rdo; count from 1
	u_char rdo_id[7] ;	// for RDO checks in get_l2; count from 1
	u_int t_data ;		// for trg_cmd==1 pattern tests...
} ;


#endif	// _DAQ_FGT_H_
