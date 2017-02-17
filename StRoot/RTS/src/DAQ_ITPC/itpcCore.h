#ifndef _ITPC_CORE_H_
#define _ITPC_CORE_H_

#include <DAQ_READER/daq_dta_structs.h>

struct itpc_ped_t {
	double mean[64][64][512] ;
	double rms[64][64][512] ;
	int cou[64][64][512] ;
} ;

class itpc_data_c {
public:
	itpc_data_c() { rdo_p = 0 ; next_word = 0 ;} ;
	~itpc_data_c() {;} ;

	struct fee_ch_t {
		u_char fee ;	//0..63
		u_char ch ;	//0..63
		u_char err ;

		u_short words ;
	} ;

	
	struct rdo_t {
		u_short token ;
		u_char trg_cmd ;
		u_char daq_cmd ;
		u_char rdo ;

		fee_ch_t *fee_ch[64*64] ;	//64 FEEs with 64ch
		int fee_ch_cou ;
	} ;

	rdo_t *rdo_p ;



	void rdo_start(int irdo) {
		next_word = 0 ;

		rdo_p = (rdo_t *)malloc(sizeof(rdo_t)) ;
		rdo_p->rdo = irdo ;
		
		rdo_p->fee_ch_cou = 0 ;

	}

	static void rdo_zap(void *rdo_p) ;

	void data_accum(int fee, int ch, int tb, int adc) ;

	int fee_scan(u_short *d16, int shorts) ;


	static itpc_ped_t *ped_p ;
	static int ped_run ;
	static void ped_start() ;
	static void ped_stop() ;

	int sector ;

	int fee_id ;
	int fee_ch ;
	int fee_err ;

	int sampa_id ;
	int sampa_ch ;
	int words ;

	int sampa_bx ;
	int sampa_type ;
	
	int tb_cou ;
	daq_adc_tb at[512] ;
	
private:
	int next_word ;
	int hdr_cou[8] ;

	void start(u_short *d16) ;	
};


extern void itpc_sampa_to_rowpad(int id, int sampa, int ch, int &row, int &pad) ;
extern void itpc_rowpad_to_id(int row, int pad, int &id, int &pin)  ;
extern  int itpc_altro_to_ifee(int altro) ;
extern void itpc_altro_to_rowpad(int altro, int ch, int odd, int &row, int &pad) ;

#endif
