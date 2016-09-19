#ifndef _DAQ_FPS_H_
#define _DAQ_FPS_H_

#include <DAQ_READER/daq_det.h>

#define FPS_FORMAT_VERSION	0x000001

//#define FPS_VME_PORT		3000


//raw data header
struct fps_evt_hdr_t {
	u_int ver ;	// version<<8 | (header words)
	u_int words ; // of the event, including this header

	u_int ev ;	// event counter
	u_int token ;	// trg/daq/token, 20bits


	u_int xing ;

	u_char qt_cou ;
	u_char pre_post_cou ;
	char status ;		// 1 OK; something else otherwise
	u_char trg_type ;

	u_int tick ;		//at event start, in us
	int delta ;		//in us

	u_int stp_data[3] ;
	u_int reserved[2] ;
} ;

#if 0

#define FPS_QT_COU			8
#define FPS_QT_MAX_TIMEBINS		41

// raw data
struct fps_dta_t {
	char timebin ;	// relative to time of trigger!
	struct {
		char ix ;
		char ch_cou ;
		u_short adc[32] ;
		u_char  tdc[32] ;
		u_char  ch[32] ;
	} qt[FPS_QT_COU] ;
} ;

//raw data event
struct fps_evt_t {
	fps_evt_hdr_t hdr ;
	fps_dta_t xing[FPS_QT_MAX_TIMEBINS] ;
};
#endif


// transformed to ADC data
struct fps_adc_t {
	u_char ch ;
	u_char tdc ;
	u_short adc ;
} ;

struct fps_config_t {
	u_int run_type ;	//3:physics, 1:pedestal
	u_int ped_mode ;	//1:do zs, 0:no zs
	u_int qt_mask ;
	u_int pre ;
	u_int post ;
	u_int log_level ;	// tonkoLogLevel thing
	u_int run_number ;

	u_int trg_type ;	//2:pulser, 4:coinc, 0x1000:STP
	u_int rcc_required ;
	u_int events_required ;	
	u_int qt_cou ;

	u_int reserved[27] ;

};

#define FPS_PED_VERSION	0x0

struct fps_pedrms_t {	
	u_char version ;
	u_char qt_ix ;		//0..7
	u_char ch_cou ;	//32
	
	struct {
		float ped ;
		float rms ;
		u_char flag ;	//1=error
	} ped[32] ;
} ;


class daq_fps : public daq_det {
private:
	class daq_dta *handle_raw(int sec) ;
	class daq_dta *handle_pedrms(int sec) ;
	class daq_dta *handle_adc(int sec) ;

	class daq_dta *raw ;
	class daq_dta *adc ;
	class daq_dta *pedrms ;

	static const char *help_string ;

public:
	daq_fps(daqReader *rts_caller=0) ;
	~daq_fps() ;

	daq_dta *get(const char *bank="*",int c1=-1,int c2=-1,int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo) ;


	// this is the meta data, overwritten per sector!
	struct fps_evt_hdr_t meta_hdr ;

	const char *GetCVS() const {
		static const char cvs[] = "Tag $Name:  $Id built " __DATE__ " " __TIME__ ; return cvs ;
	}
} ;

#endif
