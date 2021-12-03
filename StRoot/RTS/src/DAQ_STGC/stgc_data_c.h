#ifndef _STGC_DATA_C_H_
#define _STGC_DATA_C_H_


#include <stdint.h>

#define STGC_SECTOR_COU	4
#define STGC_RDO_COU	4
#define STGC_FEE_COU	6
#define STGC_VMM_COU	4

// I will have 1 per worker thread
class stgc_data_c {
public:
	stgc_data_c() ;
	~stgc_data_c() {;} ;

	static uint32_t run_number ;
	static uint32_t run_type ;

	uint8_t id ;	// 0 is special

	uint8_t sector1 ;
	uint8_t rdo1 ;

	uint32_t bad_error ;	// flags
	uint8_t want_saved ;
	uint8_t realtime ;

	//per event; per RDO
	int hdr_check(uint16_t *d16, int shorts) ;

	int start_0001(uint16_t *d16, int shorts) ;
	int event_0001() ;

	int run_start() ;

	int start(uint16_t *d16, int shorts) ;
	int event() ;
	int event_end(int flag) ;

	static const char *type_c(uint16_t type) ;

//	void set_rdo(int r1) { rdo1 = r1 ; } ;


	static struct feb_t {
		uint8_t present ;
		struct {
			uint16_t threshold ;
			struct {
				uint8_t threshold ;
				uint8_t mask ;
			} ch[64] ;
		} vmm[STGC_VMM_COU] ;
	} feb[STGC_SECTOR_COU][STGC_RDO_COU][STGC_FEE_COU] ;


	static struct errs_t {
		uint32_t fifo ;
	} errs[STGC_RDO_COU] ;

	int adc_cou ;
	int trg_cou ;

	int xing_min ;
	int xing_max ;

	uint32_t event_any ;
	uint32_t event_data ;

	uint16_t version ;	// 0x0001 is the version in May 2021
	uint16_t evt_type ;
	uint16_t status ;	// from d[3]
	uint16_t echo ;		// if echo command
	unsigned long response ;	// from FEE response packet
	unsigned long fee_status ;

	uint16_t *d16_start ;
	uint16_t *d16_data ;
	uint16_t *d16_last ;
	

	

	uint32_t mhz_start_evt_marker ;
	uint32_t mhz_stop_evt_marker ;
	uint32_t mhz_trg_marker ;

	struct stgc_vmm_t vmm ;

	uint16_t event_type ;
	int ch_count ;

	uint16_t token ;
	uint8_t daq_cmd ;
	uint8_t trg_cmd ;

private:

} ;

#endif
