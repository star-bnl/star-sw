#ifndef _ITPC_CORE_H_
#define _ITPC_CORE_H_

#include <DAQ_READER/daq_dta_structs.h>


//per sector
struct itpc_ped_t {
	double mean[64][64][512] ;	//fee,ch,timebin
	double rms[64][64][512] ;
	int cou[64][64][512] ;

	double g_mean[64][64] ;		//fee,ch
	double g_rms[64][64] ;
	int g_cou[64][64] ;
} ;

class itpc_data_c {
public:
	itpc_data_c() { rdo_p = 0 ; next_word = 0 ;} ;
	~itpc_data_c() { 
		if(rdo_p) free(rdo_p) ;
		rdo_p = 0 ;
	}

	//per FEE
	struct fee_ch_t {
		u_char port ;	//0..15
		u_char fee ;	//0..63; padplane id
		u_char ch ;	//0..63
		u_char err ;

		u_short words ;
	} ;

	//per RDO, per event
	struct rdo_t {
		u_char sector ;
		u_char rdo ;
		u_char rb ;
		u_char reserved ;

		u_short token ;

		u_char trg_cmd ;
		u_char daq_cmd ;



		fee_ch_t *fee_ch[64*64] ;	//64 FEEs with 64ch
		int fee_ch_cou ;
	} ;

	rdo_t *rdo_p ;


	//start of RDO event
	void rdo_start(int sector, int irdo, int rb) {
		next_word = 0 ;

		if(rdo_p) {
			//LOG(ERR,"rdo_p not free!") ;
		}
		else {
			rdo_p = (rdo_t *)malloc(sizeof(rdo_t)) ;
		}

		rdo_p->sector = sector ;
		rdo_p->rdo = irdo ;
		rdo_p->rb = rb ;
		rdo_p->reserved = 0 ;	//just in case

		rdo_p->fee_ch_cou = 0 ;

	}

	static void rdo_zap(void *rdo_p) ;

	void data_accum(fee_ch_t *fee_p, int tb, int adc) ;

	int fee_scan(u_short *d16, int shorts) ;


	static itpc_ped_t *ped_p ;
	static int ped_run ;
	static void ped_start() ;
	static void ped_stop() ;

	u_int format_version ;

	int sector ;

	int port_id ;	//via RDO

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

	int start(u_short *d16) ;	
};


extern void itpc_sampa_to_rowpad(int id, int sampa, int ch, int &row, int &pad) ;

extern void itpc_ifee_to_rowpad(int fee_id, int ch, int &row, int &pad) ;
void itpc_rowpad_to_ifee(int row, int pad, int &fee_id, int &fee_ch);

extern void itpc_rowpad_to_id(int row, int pad, int &id, int &pin)  ;
extern  int itpc_altro_to_ifee(int altro) ;
extern void itpc_altro_to_rowpad(int altro, int ch, int odd, int &row, int &pad) ;

#endif
