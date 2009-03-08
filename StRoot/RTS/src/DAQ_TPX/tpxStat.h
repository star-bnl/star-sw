#ifndef _TPX_STAT_HH_
#define _TPX_STAT_HH_

#include <sys/types.h>


class tpxStat {
public:
	tpxStat() {;} ;
	~tpxStat() {;} ;


	// used in FEE_CHECK runs...
	int fee_check_on ;
	int fee_check_altros[256] ;
	struct fee_check_data {
		double ped, ped_sigma ;
		double rms, rms_sigma ;
		double charge, charge_sigma ;
		double t0, t0_sigma ;
		double count ;
		int aid ;
	} fee_check_data[72][16];	// max 72 altros, 16 ch each...

	int sector ;	// from 1

	u_int stripes ;	// count of bad pads with all pizels lit...

	// run in the thread!
	void run_start(u_int rb_mask, int run_type) ;

	// run in the thread, per RB
	void accum(char *rdobuff, int bytes) ;


	// run globally
	int run_stop(FILE *f, u_int rb_mask, int run_type, char *fname) ;

	// singleton....
	static struct tpx_stat_struct {
		u_char should ;
		u_int count ;
		u_int errs ;
		struct {
			u_char should ;
			struct {
				u_int count ;
				u_short max_adc ;
				u_short min_adc ;
			} c[16] ;
		} a[256] ;
	} r[6] ;



private:
} ;

#endif
