#ifndef _DAQ_FPS_H_
#define _DAQ_FPS_H_

#include <DAQ_READER/daq_det.h>

#define FPS_FORMAT_VERSION	0x000001

//#define FPS_VME_PORT		3000


//raw data header
struct fps_evt_hdr_t {
	uint32_t ver ;	// version<<8 | (header words)
	uint32_t words ; // of the event, including this header

	uint32_t ev ;	// event counter
	uint32_t token ;	// trg/daq/token, 20bits


	uint32_t xing ;

	uint8_t qt_cou ;
	uint8_t pre_post_cou ;
	char status ;		// 1 OK; something else otherwise
	uint8_t trg_type ;

	uint32_t tick ;		//at event start, in us
	int delta ;		//in us

	uint32_t stp_data[3] ;
	uint32_t reserved[2] ;
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
		uint16_t adc[32] ;
		uint8_t  tdc[32] ;
		uint8_t  ch[32] ;
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
	uint8_t ch ;
	uint8_t tdc ;
	uint16_t adc ;
} ;

struct fps_config_t {
	uint32_t run_type ;	//3:physics, 1:pedestal
	uint32_t ped_mode ;	//1:do zs, 0:no zs
	uint32_t qt_mask ;
	uint32_t pre ;
	uint32_t post ;
	uint32_t log_level ;	// tonkoLogLevel thing
	uint32_t run_number ;

	uint32_t trg_type ;	//2:pulser, 4:coinc, 0x1000:STP
	uint32_t rcc_required ;
	uint32_t events_required ;	
	uint32_t qt_cou ;

	uint32_t reserved[27] ;

};

#define FPS_PED_VERSION	0x0

struct fps_pedrms_t {	
	uint8_t version ;
	uint8_t qt_ix ;		//0..7
	uint8_t ch_cou ;	//32
	
	struct {
		float ped ;
		float rms ;
		uint8_t flag ;	//1=error
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
