#ifndef _TPX_CORE_H_
#define _TPX_CORE_H_

#include <sys/types.h>


#define TPX_GAIN_MASTER_FILE        "/RTS/conf/tpx/tpx_gains.txt"   // global -- read only AFAIK this code is concerned!
#define TPX_CONFIG_FILE             "/RTS/conf/tpx/tpx_config_%03d"
#define TPX_REMAP_FILE              "/RTS/conf/tpx/tpx_remap.txt"

#define TPX_ALTRO_DO_CHECK	(1<<0)	// check altro data
#define TPX_ALTRO_DO_ADC	(1<<1)	// dump the ADCs into the altro_struct
#define TPX_ALTRO_DO_FCF	(1<<2)	// run the CLUSTERfinder as well...

#include <TPX/tpx_rdo.h>

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
	u_char id ;
	u_char ch ;
	u_char rdo ;	// from 0; this is an _incoming_ datum to the scanner!
	u_char what ;	// bitfield: do what, see defines above...

	u_char row ;
	u_char pad ;

	u_int wc ;	// full altro word count, used in FCF
	u_int *where ;	// pointer to this altro's data...

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

extern int tpx_get_start(char *buff, u_int words, struct tpx_rdo_event *rdo, int do_log)  ;
extern u_int *tpx_scan_to_next(u_int *now, u_int *data_start, struct tpx_altro_struct *a) ;

extern void tpx_from_altro(int rdo, int a, int ch, int &row, int &pad) ;
extern void tpx_to_altro(int row, int pad, int &rdo, int &a, int &ch) ;
extern int tpx_altro_to_fee(int rdo, int a) ;
extern u_char tpx_rdo_fees(int rdo, int cou) ;
extern u_char tpx_altro_ch_to_fee(int a, int ch) ;

extern void tpx_analyze_log(int sector, int rdo, char *buff) ;
extern int  tpx_analyze_msc(int sector, int rdo, char *buff, int *altro_list=0) ;
extern int tpx_show_status(int sector, int rb_mask, int *altro_list=0) ;

extern struct tpx_rdo tpx_rdo[6] ;
extern struct tpx_rdo_dbg tpx_rdo_dbg[6] ;
extern int tpx_fee_check ;

#endif
