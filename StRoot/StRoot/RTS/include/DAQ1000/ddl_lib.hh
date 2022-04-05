#ifndef _DDL_LIB_H_
#define _DDL_LIB_H_

#include <RORC/rorc_lib.h>	// for the descriptors

#include "rb.hh"

class ddlDriver : public rb {
public:
	ddlDriver(int board, int channel, int fifo_cou, u_int buff_bytes) ;
	~ddlDriver() { ; } ;

	int open(char *vbuff = 0, u_int pbuff = 0) ;	// open device
	void close() ;	// close device

	int start(u_int what=4) ;	// start run
	int stop() ;	// stop run


	int cmd(u_char cmd, u_int param=0, u_int dest=4) ;			// send a command and wait ?
	int read(u_int param=0) ;		// this just issues the STBRD command!

	int write(char *inbuff, int bytes, int no_delay=0, int param=0) ;	// write something to the device

	int link_check() ;

	int busy() ;

	int mark(char *addr, u_int how) ;
	int get(char **addr, u_int *status) ;		// returns word count; 0 for no data; negative for error
	int get_priority(char **addr, u_int *status, int *ix=0) ;		// returns word count; 0 for no data; negative for error
	int free(char *addr) ;		// return the particular buffer to the free pile
	int free_ix(int ix) ;

	int get_free_fifos() ;

	void emu_place_event(int ix, char *data, int bytes) ;
	void emu_go() ;

	u_int status ;	// bitmask: xxx1=requested; xx1x=opened; x1xx=error
//	u_int alloced ;	// how many bytes we used of physmem...
	int start_ix ;

	unsigned int fifo_order[128] ;
	unsigned int fifo_order_cou ;

private:
	// from the initializer
//	char *buff ;
	u_int phys_buff ;
//	int fifo_cou ;
//	u_int buff_bytes ;
	
//	int board ;	// board:	0,1,2
//	int ch ;	// channel:	0=lower fiber,1=upper fiber

	rorcDescriptor_t dev ;

	// storage
//	volatile rorcReadyFifo_t *sfifo ;
	volatile u_int sfifo_p ;

//	char *sbuff ;
	u_int sbuff_p ;

//	volatile rorcReadyFifo_t *rfifo ;
	volatile u_int rfifo_p ;

//	char *rbuff ;
	u_int rbuff_p ;


	int set_base(char *vbuff, u_int pbuff) ;
} ;


#endif
