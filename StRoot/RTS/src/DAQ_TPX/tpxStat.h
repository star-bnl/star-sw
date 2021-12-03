#ifndef _TPX_STAT_HH_
#define _TPX_STAT_HH_

#include <stdint.h>


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

	int sector ;	// from 1..36!!!

	uint32_t stripes ;	// count of bad pads with all pixels lit...

	// runs globally
	void run_start(uint32_t rb_mask, int run_type) ;

	// run in the thread, per RB
	void accum(char *rdobuff, int bytes) ;


	// runs globally
	int run_stop(FILE *f, uint32_t rb_mask, int run_type, char *fname) ;

	struct tpx_stat_struct {
		uint8_t should ;	// should the RDO be present
		uint32_t count ;
		uint32_t errs ;
		struct {
			uint8_t should ;	// should the ALTRO be present
			struct {
				uint32_t count ;
				uint16_t max_adc ;
				uint16_t min_adc ;
				uint32_t stripes ;
			} c[16] ;	// ALTRO channel (aka pad)
		} a[256] ;	// ALTRO
	} r[6] ;	// RDO; as logical!!!



private:
	int run_type ;	// saved from run_start...
} ;

#endif
