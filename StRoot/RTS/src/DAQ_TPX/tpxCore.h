#ifndef _TPX_CORE_H_
#define _TPX_CORE_H_

#include <sys/types.h>
#include <rtsLog.h>

#define TPX_GAIN_MASTER_FILE        "/RTS/conf/tpx/tpx_gains.txt"   // global -- read only AFAIK this code is concerned!
#define TPX_CONFIG_FILE             "/RTS/conf/tpx/tpx_config_%03d"
#define TPX_REMAP_FILE              "/RTS/conf/tpx/tpx_remap.txt"

#define TPX_ALTRO_DO_CHECK	(1<<0)	// check altro data
#define TPX_ALTRO_DO_ADC	(1<<1)	// dump the ADCs into the altro_struct
#define TPX_ALTRO_DO_FCF	(1<<2)	// run the CLUSTERfinder as well...

#define TPX_MAX_PAD	182

/*
        In pedestal mode I always include the 15 pre-triggers.
        Due to FIFO sizes in the RDO & FEE I can't have more
        than 502 entries thus pedestal runs will have 502 timebins,
        from 0 to 511 where timebins 0..14 are before the trigger.
*/



#define TPX_MAX_TB	420
#define TPX_DEF_TB	400

//#define TPX_MAX_TB	480
//#define TPX_DEF_TB	460

#include <TPX/tpx_rdo.h>

struct tpx_odd_fee_t {
	u_char tpc_fee_padplane ;	// fee id from padplane
	u_char status ;	// 1=overriden; 2=marked bad
	u_char sector ;	// from 1
	u_char rdo ;	// from 1

	u_char altro_id_padplane ;
} ;

struct tpx_rdo_event {
	u_int *data_start ;
	u_int *data_end ;
	u_int trg_cou ;
	struct trg_data *trg ;

	short token ;	// can be negative!
	short sector ;	// can be negative, if uknown...
	short rdo ;	// can be negative, if unknown; starts from 1!

	u_char l2_cmd ;	// abort,accept,unknown yet...
	u_char type ;
	u_char subtype ;
	u_char data_err ;
} ;


struct tpx_altro_struct {
	u_char id ;		// original altro id; even if overriden
	u_char ch ;	// original altro channel
	u_char rdo ;	// from 0; this is an _incoming_ datum to the scanner!
	u_char what ;	// bitfield: do what, see defines above...

	u_char sector ;	// input: from 1
	u_char fee ;	// TPC physical absolute FEE; will be correct even for overriden ALTROs!
	u_char row ;
	u_char pad ;

	u_short wc_dummy ;	// full altro word count, used in FCF
	u_char log_err ;	// input!
	u_char err ;	// output

//	u_int *where ;	// pointer to this altro's data...

	u_short t ;	// token; incoming, for debugging
	u_short count ;		// length of data

	u_short adc[512] ;
	u_short tb[512] ;

} ;


struct tpx_rdo_dbg
{
	u_int delta ;
	u_int old_rhic ;
} ;

struct tpx_rdo_heartbeat_t {
			u_char pll ;
			u_char status ;
			u_short retries ;
			
			u_int trgs ;
			u_int trg_csr ;	// shows i.e. BUSY
			u_int a_ticks ;
			u_int sta_reg ;	// status
			u_int rhic_ticks ; // rhic clocks..
} ;

inline int tpx36_from_real(int s36, int s_real, int r_real)
{
	int r0_logical ;

	if(s36 <= 24) return r_real ;

	if((s_real % 2)==0) {		// i.e. 24
		r0_logical = (r_real-4)+2 ;  // 5->3, 6->4
	}
	else {
		r0_logical = (r_real - 4) ;	// 5->1, 6->2 ;
	}

	if((r0_logical < 1) || (r0_logical > 4)) {
		LOG(ERR,"Mismap: s36 %d, Shw %02d:%d = %d",s36,s_real,r_real,r0_logical) ;
		r0_logical = 1 ;
	}

	return r0_logical ;
}


inline void tpx36_to_real(int s36, int r1, int &s_real, int &r_real)
{
	s_real = r_real = 1 ;

	// this handles both the S35 case when RDOs are 1..4
	// but _also_ the old, pre-36 case for all 6 RDOs
	if(s36 <= 24) {
		s_real = s36 ;
		r_real = r1 ;

		return ;
	}
	

	switch(r1) {
	case 1 :
		r_real = 5 ;
		s_real = (s36-24)*2 - 1 ;
		break ;
	case 2 :
		r_real = 6 ;
		s_real = (s36-24)*2 - 1  ;
		break ;
	case 3 :
		r_real = 5 ;
		s_real = (s36-24)*2 ;
		break ;
	case 4 :
		r_real = 6 ;
		s_real = (s36-24)*2 ;
		break ;
	default:
		LOG(ERR,"Mismap: S%02d:%d",s36,r1) ;

		r_real = 1 ;
		s_real = 1 ;
		break ;
	}

	return ;
}




extern int tpx_get_start(char *buff, u_int words, struct tpx_rdo_event *rdo, int do_log)  ;
extern u_int *tpx_scan_to_next(u_int *now, u_int *data_start, struct tpx_altro_struct *a) ;

extern void tpx_from_altro(int rdo, int a, int ch, int &row, int &pad) ;
extern void tpx_to_altro(int row, int pad, int &rdo, int &a, int &ch) ;
extern int tpx_altro_to_fee(int rdo, int a) ;
extern u_char tpx_rdo_fees(int rdo, int cou) ;
//extern u_char tpx_altro_ch_to_fee(int a, int ch) ;

extern void tpx_analyze_log(int sector, int rdo, char *buff) ;
extern int  tpx_analyze_msc(int sector, int rdo, char *buff, int *altro_list=0) ;
extern int tpx_show_status(int sector, int rb_mask, int *altro_list=0) ;

extern struct tpx_rdo tpx_rdo[24][6] ;
extern struct tpx_rdo_dbg tpx_rdo_dbg[24][6] ;
extern int tpx_fee_check ;

extern struct tpx_odd_fee_t tpx_odd_fee[256] ;
extern int tpx_odd_fee_count ;

extern int *tpx_altro_to_row_override ;
extern int tpx_fy16_map ;
extern int tpx_is_stgc ;
extern int tpx_rdo_override ;

#endif
