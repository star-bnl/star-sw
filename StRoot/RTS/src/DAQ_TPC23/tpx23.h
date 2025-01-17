#ifndef _TPX23_H_
#define _TPX23_H_

//#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "tpc23_base.h"

class tpxPed ;
struct daq_dta ;
struct tpx_altro_struct ;

class tpx23Data {
public:
	tpx23Data() {;} ;
	~tpx23Data() {;} ;

	int sector ;
	int rdo ;
	int row ;
	int pad ;
	int altro ;
	int ch ;
	int tb_cou ;

	daq_dta *dta ;
	daq_adc_tb *at ;

	void ch_start(int c) {
		ch = c ;
		tb_cou = 0 ;

		at = (daq_adc_tb *)dta->request(512) ;
	}

	void accum(int tb, int adc) {
		at[tb_cou].adc = adc;
		at[tb_cou].tb = tb ;
		tb_cou++ ;
	}

	void ch_done() {
		dta->finalize(tb_cou,sector,row,pad) ;
	}
} ;


class tpx23 : public tpc23_base {
public:
	tpx23() ;
	~tpx23() { return ; } ;

//	int run_start() ;

	int rdo_scan(char *c_addr, int words) ;
	int from22to23(char *c_addr, int words) ;

	u_int get_token_s(char *c_addr, int words) ;
	u_int set_rdo(int s, int r) ;

	static class tpxPed *peds ;


	u_int get_token(char *c_addr, int words) ;

	int init(daq_dta *gain) ;

	static struct row_pad_t (*rp_gain_tpx)[ROW_MAX+1][PAD_MAX+1] ;     // max for both dets; all sectors

	u_int tdbg[10] ;	// debugging counters, etc

	u_char hdr_version ;

	u_int fpga_usercode[5] ;
	u_char log_is_error ;
	u_char rhic_clock ;

	class tpx23Data *tpx_d ;

	tpx_altro_struct *altro ;
	int altro_cou ;

private:

	int fee_scan() ;

	int log_dump(char *c_addr, int wds) ;
	int msc_dump(char *c_addr, int wds) ;

	u_char flags_row_pad(int asic, int channel, int &row, int &pad) ;

	u_char type ;	
	u_char subtype ;

	u_int *d_start ;	// original FIFO c_addr
	u_int *d_end ;		// very last ALTRO datum
	u_int *trl ;		// start of trailer aka end-of-event

	int words ;		// original FIFO words




} ;


#endif

