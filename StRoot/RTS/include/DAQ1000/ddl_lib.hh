#ifndef _DDL_LIB_H_
#define _DDL_LIB_H_

#include <RORC/rorc_lib.h>	// for the descriptors

#include "rb.hh"

class ddlDriver : public rb {
public:
	ddlDriver(int board, int channel, int fifo_cou, uint32_t buff_bytes) ;
	~ddlDriver() { ; } ;

	int open(char *vbuff = 0, uint32_t pbuff = 0) ;	// open device
	void close() ;	// close device

	int start(uint32_t what=4) ;	// start run
	int stop() ;	// stop run


	int cmd(uint8_t cmd, uint32_t param=0, uint32_t dest=4) ;			// send a command and wait ?
	int read(uint32_t param=0) ;		// this just issues the STBRD command!

	int write(char *inbuff, int bytes, int no_delay=0, int param=0) ;	// write something to the device

	int link_check() ;

	int busy() ;

	int mark(char *addr, uint32_t how) ;
	int get(char **addr, uint32_t *status) ;		// returns word count; 0 for no data; negative for error
	int get_priority(char **addr, uint32_t *status, int *ix=0) ;		// returns word count; 0 for no data; negative for error
	int free(char *addr) ;		// return the particular buffer to the free pile
	int free_ix(int ix) ;

	int get_free_fifos() ;

	void emu_place_event(int ix, char *data, int bytes) ;
	void emu_go() ;

	uint32_t status ;	// bitmask: xxx1=requested; xx1x=opened; x1xx=error
//	uint32_t alloced ;	// how many bytes we used of physmem...
	int start_ix ;

	unsigned int fifo_order[128] ;
	unsigned int fifo_order_cou ;

private:
	// from the initializer
//	char *buff ;
	uint32_t phys_buff ;
//	int fifo_cou ;
//	uint32_t buff_bytes ;
	
//	int board ;	// board:	0,1,2
//	int ch ;	// channel:	0=lower fiber,1=upper fiber

	rorcDescriptor_t dev ;

	// storage
//	volatile rorcReadyFifo_t *sfifo ;
	volatile uint32_t sfifo_p ;

//	char *sbuff ;
	uint32_t sbuff_p ;

//	volatile rorcReadyFifo_t *rfifo ;
	volatile uint32_t rfifo_p ;

//	char *rbuff ;
	uint32_t rbuff_p ;


	int set_base(char *vbuff, uint32_t pbuff) ;
} ;


#endif
