#ifndef _RB_C_H_
#define _RB_C_H_

#include <pthread.h>

//uncomment under ESB, comment for ddb

//#define _DDB_

//#include "rb_main_c.h"
#include "tef_c.h"

#ifdef _DDB_
//#warning DDB_ON

#else

#warning DDB_OFF

#include <DAQ1000/rb.hh>

#endif

class tef_c ;



//class rb_c : public rb_main_c // -- I'm not sure about this
//class rb_c : // is this for ddb???
//class rb_c
// when running in ESB?
#ifdef _DDB_
class rb_c 
#else
class rb_c : public rb 
#endif
{
public:
	rb_c(int board, int ch, int fifo_cou, u_int buff_bytes) ;	// fifo_cou & buff_bytes are ignored
//	rb_c() { return ; } ;
	~rb_c() ;


	int get_priority(char **addr, u_int *status, int *fifo_ix) ;
	int get_priority_fast(char **addr, u_int *status, int *fifo_ix) ;
	int free_fast(char *addr) ;
	int free(char *addr) ;

//	u_int get_fifo_status(int fifo_ix) ;
//	void  set_fifo_status(int fifo_ix, u_int status) ;
//	int get_fifo_event(int fifo_ix, char **addr, u_int *status) ;
//	char *get_fifo_addr(int fifo_ix) ;

	int get_free_fifos() ;
	int link_check() ;
	int busy() ;
	int r_open(char *vbuff, u_int pbuff) ;
	void r_close() ;
	int start(u_int what) ;
	int stop() ;
	int config(int emul) ;
	int fifo_scan() ;

	int log_status() ;

	//dummies
	int free_ix(int ix) { return 0 ;} ;
	int mark(char *addr, u_int how) { return 0 ;} ;
	int get(char **addr, u_int *status) {
		return get_priority(addr,status,0) ;
	}

	//run control
	int upload(int a, const char *fname) ;
	int prom_checksum(int a, const char *fname) ;
	int want_checksum ;

	int send_config(int run) ;
	int ped_upload() ;
	int run_start(int run) ;
	int run_stop() ;

	int wr_comma(u_char comma) ;

	int wr(u_int *ptr, int words) ;
	int rd(u_int *ptr, int words) ;

	int wr_bytes(void *ptr, int bytes) ;
	int wr_byte(u_char byte) ;
	void wr_int(u_int v) ;
	void wr_short(u_short v) ;
	u_int wr_string(const char *s) ;
	u_int wr_var(const char *s, ...) ;

	int rd_bytes(void *ptr, int bytes) ;
	int rd_string(char *str) ;
	int rd_log() ;

	void cpu_write(u_short addr, u_short val);
	u_int cpu_read(u_short addr) ;
	u_int cpu_set_bit(u_short addr, int b) ;
	u_int cpu_clr_bit(u_short addr, int b)  ;
	u_int cpu_pulse_bit(u_short addr, int b) ;
	u_int cpu_get_bit(u_short addr, int b) ;

	void cpu_pulse_bit_n(u_short addr, int b) ;

	void gtp_reset_full(int loopback=0) ;
	void gtp_clear_link() ;
	int gtp_status(int quiet=0) ;
	u_int gtp_st ;	// status after gtp_status() call
	
	int gtp_start(int mode) ;
	u_int gtp_spin() ;
	void gtp_reset_read_ix() ;

	tef_c *tef ;
	static tef_c *tef_glo[2] ;	// I can have only 1!!!

	volatile u_int *gtp_resmem ;

	int fifo_on_tef ;
	int fifo_cou ;
	int fifo_bytes ;
	int fifo_ix[512] ;	// up to 512 FIFOs, max

	// run-time parameters for this RDO
	int disable ;	// not used, skip all rd/wr commands
	int rdo_id ;	// 1..4
	int sector_id ;	// 1..24
	u_int fee_mask ;
//	int fee_40_phase ;
	int tef_ix ;	//in case of more TEFs: 0..1
	int gtp_ix ;	//GTP link on TEF: 0..3
//	int rb_id ;	// gtp_ix+1 -- used in logging
	int sim_on ;

	u_char log ;
	u_char version ;	// for IRDO and iRDO 2023

	// helpers
	int run_number ;
	int realtime ;	// running in real-time

	// temporary
	u_int gtp_read_fifo_word(int *st) ;
	volatile u_int *gtp ;	// points to my GTP AXI slave

	u_int gtp_physmem ;
	u_int gtp_physmem_rorc ;
	volatile u_int *gtp_start_mem ;


	void set_ix(int ix, int max_fifos) ;

	int gtp_write_fifo_word(u_int d) ;
	void gtp_reset_tx() ;

	void gtp_restart(int fifo_ix, int no_lock=0) ;
	void gtp_restart_sched(int fifo_ix, int no_lock=0) ;

	virtual u_int get_fifo_status(int fifo_ix)	// used
	{
		u_int shorts ;
		volatile u_int *start = gtp_resmem + fifo_ix * fifo_bytes / 4 ;

		shorts = start[0] ;

		if(sim_on && shorts==0xFFFFFFFF) {
			int words ;
			if(sim_on==1) {
				words = 1024 ;
			}
			else {
				words = sim_dta[fifo_ix].bytes/4 ;
			}

			set_fifo_status(fifo_ix,words) ;
			return words ; 
		}

		if(shorts && (shorts<0x10000000)) {
			shorts += 4*2 ;	// add header
			shorts /= 2 ;
			return shorts ;	// in words
		}

		return shorts ;	// actually status
		
	}

	virtual void set_fifo_status(int fifo_ix, u_int status)	// used
	{
		volatile u_int *start = gtp_resmem + fifo_ix * fifo_bytes / 4 ;

		start[0] = status ;
	}

	void set_fifo_sim(int fifo_ix, char *mem, int bytes) {	// used
		sim_dta[fifo_ix].mem = mem ;
		sim_dta[fifo_ix].bytes = bytes ;
	}


	virtual int ret_fifo(int fifo_ix) {	// used
		if(sim_on) {
			set_fifo_status(fifo_ix,0xFFFFFFFF) ;
			return 1 ;
		}

		if(fifo_ret_check()) {
			gtp_restart_sched(fifo_ix,1) ;
			return 1 ;
		}
		return 0 ;
	}

	virtual void clr_link() {
		LOG(ERR,"BAD") ;
	}

	virtual char *get_fifo_addr(int fifo_ix)	// used
	{
		if(sim_on) {
			return sim_dta[fifo_ix].mem ;
		}
		return (char *)(gtp_resmem + fifo_ix * fifo_bytes / 4) ;
	}

	virtual int get_fifo_event(int fifo_ix, char **addr, u_int *status)
	{	
		volatile u_int *start = gtp_resmem + fifo_ix * fifo_bytes / 4 ;

		int shorts = start[0] ;
		//add the header
		shorts += 4*2 ;

		*status = start[2] ;	// extract status
		*addr = (char *)start ;

		return shorts/2 ;
	
	}

	int get_fifo_event_raw(int fifo_ix, char **addr, u_int *status)
	{	

		volatile u_int *start = gtp_resmem + fifo_ix * fifo_bytes / 4 ;

		int shorts = start[0] ;

		*status = start[2] ;	// extract status
		*addr = (char *)start ;

		return shorts ;
	
	}

	int fifo_ret_check() {
		return tef->fifo_ret_check() ;
	}

	struct sim_t {
		char *mem ;
		int bytes ;
	} sim_dta[64] ;

private:
	u_int evts ;

	static int rb_c_cou[2] ;	// count of RBs per TEF

	int gtp_mode ;	//1 single shot, 0 normal burst
	u_int gtp_max_words ;



	int data_upload(u_int type, u_char *mem, u_int address, int bytes) ;



	volatile u_int *gtp_end_mem ;
	volatile u_int *gtp_cur_mem ;
	int last_short_count ;





	void gtp_reset_rx() ;
	void gtp_reset_rx_buf() ;
	void gtp_reset_rx_fifo() ;

	void gtp_reset_tx_fifo() ;
	void gtp_reset_error() ;
	void gtp_los_is_err(int yes) ;

	pthread_mutex_t mutex ;

	void lock() {
		pthread_mutex_lock(&mutex) ;
	}
	void unlock() {
		pthread_mutex_unlock(&mutex) ;
	}

};


#endif
