#ifndef _TPX_STAT_HH_
#define _TPX_STAT_HH_

#include <sys/types.h>


class tpxStat {
public:
	tpxStat() {;} ;
	~tpxStat() {;} ;



	// run in the thread!
	void run_start(u_int rb_mask) ;

	// run in the thread, per RB
	void accum(char *rdobuff, int bytes) ;


	// run globally
	static int run_stop(FILE *f, u_int rb_mask, int run_type) ;

	// singleton....
	static struct tpx_stat_struct {
		u_char should ;
		u_int count ;
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
