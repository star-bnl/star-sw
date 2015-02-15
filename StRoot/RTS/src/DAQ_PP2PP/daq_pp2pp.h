#ifndef _DAQ_PP2PP_H_
#define _DAQ_PP2PP_H_

#include <sys/types.h>

#define PP2PP_SVX_CH	128


//used for:
//   non zero-suppressed data
//   zero-suppressed data
//   zero-suppressed data and pedestal subtracted too
struct pp2pp_t {
	u_char seq_id ;		// 1..whatever is coded with sequencer jumpers
	u_char chain_id ;	// 0..3 ;
	u_char svx_id ;		// 0..7 ;
	u_char error ;		// error occured!
	
	u_char bunch_xing ;
	u_char not_sparse ;	
	u_char res2 ;
	u_char res3 ;

	u_char adc[PP2PP_SVX_CH] ;	
	u_char trace[PP2PP_SVX_CH] ;	// 0-ch not found;1 - ch found OK; 2 - duplicate ch
} ;


#define PP2PP_PED_SUB_VERSION	0x01
struct pp2pp_ped_sub_t {
	u_char seq_id ;		// 1..whatever is coded with sequencer jumpers
	u_char chain_id ;	// 0..3 ;
	u_char svx_id ;		// 0..7 ;
	u_char error ;		// error occured!
	
	u_char bunch_xing ;
	u_char not_sparse ;	
	u_char ch_cou ;
	u_char version ;

	struct {
		u_char adc ;	
		u_char ch ;
	} dta[0] ;
} ;


#define PP2PP_PED_VERSION 0 
struct pp2pp_pedrms_t {
	u_char version ;
	u_char seq_id ;		// 1..whatever is coded with sequencer jumpers
	u_char chain_id ;	// 0..3 ;
	u_char svx_id ;		// 0..7 ;

	float svx_ped ;
	float svx_rms ;

	float ped[PP2PP_SVX_CH] ;	
	float rms[PP2PP_SVX_CH] ;	// 0-ch not found;1 - ch found OK; 2 - duplicate ch
} ;

#ifndef DAQ_PP2PP_DATA_STRUCTURE
#include <stdio.h>
#include <DAQ_READER/daq_det.h>


class daq_pp2pp : public daq_det {
private:
	class daq_dta *handle_raw(int sec, int rdo) ;
	class daq_dta *handle_adc(int sec, int rdo) ;
	class daq_dta *handle_pedrms(int sec) ;
	class daq_dta *handle_adc_ped_sub(int sec, int rdo) ;

	class daq_dta *raw ;
	class daq_dta *pedrms ;
	class daq_dta *adc_ped_sub ;

	static const int MAX_SEC = 2 ;
	static const int MAX_RDO = 4 ;	// can be 0 for all RDOs; sequencers, typically 4

	static const char *help_string ;


protected:


public:
	daq_pp2pp(daqReader *rts_caller=0) ;
	~daq_pp2pp() ;

	class daq_dta *adc ;


	int decode(int sec_id, char *raw, int bytes) ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;
	int get_token(char *buff, int buff_bytes) ;

	virtual const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif   //  DAQ_PP2PP_DATA_STRUCTURE
#endif   // _DAQ_PP2PP_H_
