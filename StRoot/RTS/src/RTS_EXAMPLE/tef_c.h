#ifndef _TEF_C_H_
#define _TEF_C_H_

#include <sys/types.h>
#include <pthread.h>

class tef_c {
public:
	tef_c(int ix) ;
	~tef_c() ;

	//held in TEF memory at offset 0x40000000
	struct tef_struct_t {
		//filled by TEF
		u_int sw_version ;	//0:0x7EF0xxxx
		u_int all_version ;	//1:hwicap coded date
		u_int bit_version ;	//2:
		u_int prom_version ;	//3:

		u_int loops ;		//4:sign that the CPU is alive

		//filled by PC
		u_int pci_dest_hi ;	//5:upper 32 bits of PCIe physical memory address
		u_int pci_dest_lo ;	//6:lower 32 bits of PCIe physical memory address
		u_int pci_mem	;	//7:


		u_int cmd_go ;		//8:TEF first sets to 0; PC sets to 1; TEF sets to 0 when cmd done
		u_int cmd ;		//9:cmd (PC)

		u_int resA ;
		u_int resB ;
		u_int resC ;
		u_int resD ;
		u_int resE ;
		u_int resF ;


		u_int params[16] ;	//0x10: cmd parameters (PC)
		u_int results[16] ;	//0x20: cmd results (TEF)
	
		// at u_int[0x100] are end-of-run resuts!
	} ;


	int ping() ;

	volatile u_int *gtp_slave[4] ;		// pointer to GTP AXI slave's control block
	volatile u_int *gtp_resmem[4] ;		// from the code point of view (virtual address)
	u_int gtp_physmem[4] ;			// from the point of view of the TEF
	u_int gtp_physmem_rorc[4] ;			// from the point of view of e.g. RORC, absolute
	int gtp_bytes[4] ;

	static volatile u_int *resmem_glo ;	//resmem location
	static unsigned long resmem_bytes_glo ;		//resmem size in bytes
	static unsigned long resmem_hwaddr_glo ;
	static tef_c *tef_all[4] ;

	volatile u_int *resmem ;	//resmem location
	unsigned long resmem_bytes ;		//resmem size in bytes
	unsigned long resmem_hwaddr ;

	//PROM
	int prom_burn(u_int prom_addr, u_int *data, u_int bytes) ;

	//HWICAP
	void hwicap_boot(int image) ;	// 0 boot, 1 user
	static char *hwicap_version(u_int v) ;


	void fifo_ret(int rb, int ix, int no_lck) ;
	void fifo_ret_schedule(int rb, int ix, int no_lck) ;
	//int fifo_ret_check() ;
	u_char ret_scheduled ;

	u_int cmd_run(u_int cmd, u_int *params, int params_cou) ;
	int is_cmd_done(u_int *res) ;
	void loop_check() ;

	int wr_tef_mem(u_int ix, u_int data) ;

	void rb_map() ;

	u_int sw_version ;

	int pll_reset(int loopback) ;	// we do this on the TEF!
	u_int tef0008_setup(int full) ;
	int fmc_type(int type) ;

	int fifo_ret_check() {
		if(ret_scheduled == 0) return 1 ;

		if(tef_s->cmd_go == 0) {
			ret_scheduled = 0 ;
			return 1 ;
		}
		else {
			return 0 ;
		}
	}

private:

	// OLD: GTP related cmds
	void gtp_reset_rx(int rb) ;
	void gtp_reset_rx_buf(int rb) ;
	void gtp_reset_rx_fifo(int rb) ;
	void gtp_reset_tx_fifo(int rb) ;
	void gtp_reset_error(int rb) ;

	// OLD: commands used in RB
	int gtp_rx_errors[4] ;

	void gtp_clear_link(int rb) ;
	void gtp_reset_link(int rb, int loopback_mode) ;

	int gtp_check(int rb) ;
	int gtp_status(int rb) ;

	u_int gtp_read_fifo_word(int rb) ;
	int gtp_write_fifo_word(int rb, u_int data) ;

	int pll_locked() ;

	// on-TEF CPU emulation (will not need that much eventually)
	u_int cpu_read(u_int reg) ;
	u_int cpu_get_bit(u_int reg, int bit) ;

	void cpu_write(u_int reg, u_short val) ;
	void cpu_set_bit(u_int reg, int bit) ;
	void cpu_clr_bit(u_int reg, int bit) ;
	void cpu_pulse_bit(u_int reg, int bit) ;


	// pointers to various devices on the TEF AXI bus
	volatile struct tef_struct_t *tef_s ;	// in memory of the TEF board
	volatile u_int *tef_gpio_rd ;
	volatile u_int *tef_gpio_wr ;

	volatile u_int *spi ;
	volatile u_int *hwicap ;

	static int tef_count ;	// counts boards in system!
	
	int tef_ix ;		// my tef ix...

	int gtp_flavor	;	//0-old CPU based GTP; 1-new AXI based GTP



	pthread_mutex_t mutex ;

	void lock() {
		pthread_mutex_lock(&mutex) ;
	}
	void unlock() {
		pthread_mutex_unlock(&mutex) ;
	}
	

} ;


#endif
