#ifndef _TPX23_H_
#define _TPX23_H_

#include "tpc23_base.h"

class tpxPed ;
struct daq_dta ;

class tpx23 : public tpc23_base {
public:
	tpx23() ;
	~tpx23() { return ; } ;


	int rdo_scan(char *c_addr, int words) ;
	int from22to23(char *c_addr, int words) ;

	u_int get_token_s(char *c_addr, int words) ;
	inline void set_rdo(int s, int r) ;

	static class tpxPed *peds ;


	u_int get_token(char *c_addr, int words) ;

	int init(daq_dta *gain) ;

	static struct row_pad_t (*rp_gain_tpx)[ROW_MAX+1][PAD_MAX+1] ;     // max for both dets; all sectors

private:

	u_int *fee_scan() ;

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

