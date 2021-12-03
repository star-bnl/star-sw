#ifndef _ITPC_PED_H_
#define _ITPC_PED_H_

#include <stdint.h>

class itpcData
{
public:
	itpcData() { want_data = 0 ;} ;
	virtual ~itpcData() {;} ;

	virtual void ch_start(int c) { ch = c ; tb_cou = 0 ;} ;
	virtual void accum(int sec0, int rdo0, int port0, int fee_id, int ch, int tb, int adc) { tb_cou++ ;} ;
	virtual void ch_done(int err) {;} ;
	virtual int do_ch(int fee_id, int fee_ch, uint32_t *data, int words) { return 0 ; } ;

	int run_type ;
	uint32_t run_number ;
	int sector_id ;	// actual PC

	int want_data ;
	uint32_t data ;
	int words ;

	int sector ;
	int rdo ;
	int port ;
	int fee_id ;
	int ch ;

	int tb_cou ;
} ;

	
class itpcPed : public itpcData
{
public:
	itpcPed() ;
	~itpcPed() ;

	void init(int sector, int rdo, uint32_t fee_mask) ;

	void set_fee_mask(int sector, int rdo, uint32_t f_mask) {
		fee_mask[sector-1][rdo-1] = f_mask ;
	}

	void set_padplane_id(int sector, int rdo, int port, int id) ;
	void clear() ;

	void accum(int sec0, int rdo0, int port0, int fee_id, int ch, int tb, int adc) ;

	void calc() ;

	int from_cache(const char *fname, int sec1, int rdo1) ;
	int to_cache(const char *fname, int sec1, int rdo1) ;

	int sanity(int mode) ;

	int kill_non_phys() ;

	struct ped_t {
		double ped[512] ;
		double rms[512] ;
		uint16_t cou[512] ;

		double c_ped ;
		double c_rms ;
		uint32_t c_cou ;
	} *ped_p[24][4][16][64] ;


	uint8_t evts[24][4] ;

	uint8_t padplane_id[24][4][16] ;

	uint8_t fee_err[24][4][16][64] ;

	uint16_t pulser_peak_timebin ;	// because of differences in lab vs STAR
	uint8_t pulser_in_star ;	// because it is noisy

private:

	uint16_t fee_mask[24][4] ;



	
} ;


#endif

