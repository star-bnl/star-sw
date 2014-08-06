#ifndef _DAQ_FGT_H_
#define _DAQ_FGT_H_


#include <DAQ_READER/daq_det.h>

// logical maps
// represent the _maximums_ over any APV-type detector: FGT, GMT, IST...

#define FGT_RDO_COU		6	// aka ARC; but counts from 1..6
#define FGT_ARM_COU		6	// from 0..5
#define FGT_APV_COU		24	// from 0..23
#define FGT_CH_COU		128	// from 0..127
#define FGT_TB_COU		31	// from 0..30

/*
#define FGT_CH_STAT_SHOULD		0x01	// exists in hardware
#define FGT_CH_STAT_NO_CONFIG		0x02	// killed in RC or config file
#define FGT_CH_STAT_NO_RESPONSE		0x04	// killed in ARS_configure, non responding CH
#define FGT_CH_STAT_BAD			0x08	// killed in bad_channel
*/

// when asking for the "adc" or "phys"bank
struct fgt_adc_t {
	unsigned short ch ;
	unsigned char tb ;
	short adc ;
} ;

	
struct fgt_pedrms_t {
	unsigned short ch ;
	unsigned char tb ;
	float ped ;
	float rms ;
} ;

struct apv_meta_t {
	u_int version ;	

	struct {
		char present ;
		char error ;

		int format_code ;
		int arm_mask ;

		struct {
			char present ;
			char error ;

			int arm_id ;
			int arm_seq ;
			int arm_err ;
			int apv_mask ;

			struct {
				char present ;
				char error ;

				int apv_id ;
				int fmt ;
				int length ;
				int seq ;
				int capid ;
				int nhits ;
				int is_error ;
				int refadc ;
				int ntim ;

			} apv[FGT_APV_COU] ; // from 0..23

		} arm[FGT_ARM_COU] ;	// from 0..23...

	} arc[FGT_RDO_COU+1] ;	// or RDO; from 1..6
	
} ;

const short META_ZS_VERSION = 0x0001 ;
const short META_PED_ZS_VERSION = 0x0101 ;

struct apv_meta_zs_t {	// used in the header of the ZS bank
	u_char tb_cou ;

	u_char status[FGT_ARM_COU][FGT_APV_COU] ;	// bits: 1 present; 2 error
} ;

	
class daq_fgt : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_ped(int sec, int rdo) ;

	class daq_dta *raw ;	// "raw"
	class daq_dta *adc ;	// "adc"
	class daq_dta *ped ;	// "ped"
	class daq_dta *zs ;	// "zs" 

	static const char *help_string ;

protected:


public:
	daq_fgt(daqReader *rts_caller=0) ;
	~daq_fgt() ;

	struct apv_meta_t apv_meta ;

	void set_flavor(int id) ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}


	class daq_dta *handle_adc(int sec, int rdo, char *rdobuff = 0 ) ;
	class daq_dta *handle_zs(int sec, int rdo, char *rdobuff = 0, int inbytes = 0 ) ;

	u_int rdo_warns[7] ;	// number of warnings issued, per rdo; count from 1
	u_char rdo_id[7] ;	// for RDO checks in get_l2; count from 1
	u_int t_data ;		// for trg_cmd==1 pattern tests...


} ;


#endif	// _DAQ_FGT_H_
