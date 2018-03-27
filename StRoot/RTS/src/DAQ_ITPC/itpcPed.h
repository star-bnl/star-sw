#ifndef _ITPC_PED_H_
#define _ITPC_PED_H_

#include <sys/types.h>

class itpcData
{
public:
	itpcData() { want_data = 0 ;} ;
	virtual ~itpcData() {;} ;

	virtual void ch_start(int c) { ch = c ; tb_cou = 0 ;} ;
	virtual void accum(int sec0, int rdo0, int port0, int fee_id, int ch, int tb, int adc) { tb_cou++ ;} ;
	virtual void ch_done(int err) {;} ;
	virtual int do_ch(int fee_id, int fee_ch, u_int *data, int words) { return 0 ; } ;

	int want_data ;
	u_int data ;
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

	void init(int sector, int rdo, u_int fee_mask) ;
	void set_padplane_id(int sector, int rdo, int port, int id) ;
	void clear() ;

	void accum(int sec0, int rdo0, int port0, int fee_id, int ch, int tb, int adc) ;

	void calc() ;

	int from_cache(const char *fname) ;
	int to_cache(const char *fname=0) ;

	struct ped_t {
		double ped[512] ;
		double rms[512] ;
		u_short cou[512] ;

		double c_ped ;
		double c_rms ;
		u_int c_cou ;
	} *ped_p[24][4][16][64] ;


	u_char padplane_id[24][4][16] ;

private:

	u_short fee_mask[24][4] ;



	
} ;


#endif

