#ifndef _ITPC_PED_H_
#define _ITPC_PED_H_

#include <sys/types.h>

class itpcData
{
public:
	itpcData() {;} ;
	virtual ~itpcData() {;} ;

	virtual void ch_start(int c) { ch = c ; tb_cou = 0 ;} ;
	virtual void accum(int tb, int adc) { tb_cou++ ;} ;
	virtual void ch_done(int err) {;} ;

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

	void init(int sector, int rdo, u_int fee_mask) ;
	void set_padplane_id(int sector, int rdo, int port, int id) ;
	void clear() ;


	virtual void accum(int tb, int adc_val) ;

	void calc() ;

	int from_cache() ;
	int to_cache(const char *fname=0) ;



private:

	u_short fee_mask[24][4] ;

	u_char padplane_id[24][4][16] ;

	struct ped_t {
		double ped[512] ;
		double rms[512] ;
		u_short cou[512] ;

		double c_ped ;
		double c_rms ;
		u_int c_cou ;
	} *ped_p[24][4][16][64] ;

	
} ;


#endif

