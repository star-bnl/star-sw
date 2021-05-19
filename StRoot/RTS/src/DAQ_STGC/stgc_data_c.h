#ifndef _STGC_DATA_C_H_
#define _STGC_DATA_C_H_


#include <sys/types.h>

#define STGC_SECTOR_COU	4
#define STGC_RDO_COU	4
#define STGC_FEE_COU	6
#define STGC_VMM_COU	4

// I will have 1 per worker thread
class stgc_data_c {
public:
	stgc_data_c() ;
	~stgc_data_c() {;} ;

	static u_int run_number ;
	static u_int run_type ;

	u_char id ;	// 0 is special

	u_char sector1 ;
	u_char rdo1 ;

	u_char bad_error ;
	u_char want_saved ;
	u_char realtime ;

	//per event; per RDO
	int hdr_check(u_short *d16, int shorts) ;

	int start_0001(u_short *d16, int shorts) ;
	int event_0001() ;

	int start(u_short *d16, int shorts) ;
	int event() ;
	int event_end(int flag) ;

	static const char *type_c(u_short type) ;

	void set_rdo(int r1) { rdo1 = r1 ; } ;


	static struct feb_t {
		u_char present ;
		struct {
			u_short threshold ;
			struct {
				u_char threshold ;
				u_char mask ;
			} ch[64] ;
		} vmm[STGC_VMM_COU] ;
	} feb[STGC_SECTOR_COU][STGC_RDO_COU][STGC_FEE_COU] ;


	static struct errs_t {
		u_int fifo ;
	} errs[STGC_RDO_COU] ;

	int adc_cou ;
	int trg_cou ;

	int xing_min ;
	int xing_max ;

	int event_any ;
	int event_data ;

	u_short version ;	// 0x0001 is the version in May 2021
	u_short evt_type ;

	u_short *d16_start ;
	u_short *d16_data ;
	u_short *d16_last ;

	u_int mhz_start_evt_marker ;
	u_int mhz_stop_evt_marker ;
	u_int mhz_trg_marker ;

	struct stgc_vmm_t vmm ;

	u_short event_type ;
	int ch_count ;

	u_short token ;
	u_char daq_cmd ;
	u_char trg_cmd ;

private:

} ;

#endif
