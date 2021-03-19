#ifndef _STGC_DATA_C_H_
#define _STGC_DATA_C_H_


#include <sys/types.h>

#define STGC_SECTOR_COU	4



// I will have 1 per worker thread
class stgc_data_c {
public:
	stgc_data_c() ;
	~stgc_data_c() {;} ;


	u_char id ;	// 0 is special


	//per event
	int start(u_short *d16, int shorts) ;
	int event() ;
	int event_end() ;

	static struct feb_t {
		u_char present ;
	} feb[STGC_SECTOR_COU][4][6] ;

	// temporary storage
	u_char sector1 ;
	u_char rdo1 ;


	int event_any ;
	int event_data ;

	u_short *d16_start ;
	u_short *d16_data ;

	struct stgc_vmm_t vmm ;

	u_short event_type ;
	int ch_count ;

	u_short token ;
	u_char daq_cmd ;
	u_char trg_cmd ;

private:

} ;

#endif
