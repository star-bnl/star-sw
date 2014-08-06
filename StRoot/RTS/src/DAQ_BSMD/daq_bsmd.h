#ifndef _DAQ_BSMD_H_
#define _DAQ_BSMD_H_


#include <DAQ_READER/daq_det.h>

#include <DAQ_EMC/daq_emc.h>


struct bsmd_t {
	short adc[BSMD_DATSIZE] ;
	u_char  cap ;	// capacitor
} ;


class daq_bsmd : public daq_det {
private:
	class daq_dta *handle_adc(int rdo) ;
	class daq_dta *handle_raw(int rdo) ;
	class daq_dta *handle_adc_non_zs(int rdo) ;
	class daq_dta *handle_ped_rms(int rdo, int is_ped) ;


	class daq_dta *adc ;	// "adc"
	class daq_dta *adc_non_zs ;
	class daq_dta *ped ;	
	class daq_dta *rms ;	
	class daq_dta *raw ;

	static const char *help_string ;




protected:


public:
	daq_bsmd(daqReader *rts_caller=0) ;
	~daq_bsmd() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

	u_char rdo_id[7] ;	// for RDO checks in get_l2
	u_int rdo_warns[7] ;	// number of warning issued...

	unsigned int t_data ;	// for trg_cmd pattern tests!


} ;

// internal support
struct bsmd_desc {
	char *dta[BSMD_FIBERS][3] ;	//0 is non-ZS, 1 is ZS, 2 is PED
	int   bytes[BSMD_FIBERS][3] ;
	int   endian[BSMD_FIBERS][3] ;	// 1-BIG, 0-LITTLE
} ;

extern char *bsmd_reader(char *e, struct bsmd_desc *bsmd_d) ;

#endif	// _DAQ_BSMD_H_
