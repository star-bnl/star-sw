#ifndef _RB_HH_
#define _RB_HH_

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>


#include <RORC/rorc_lib.h>



class rb {
public:
	// copied over from rorc.h
	typedef rorcReadyFifo_t rbFifo_t ;

	rb(int board, int ch, int fifo_cou, u_int buff_bytes) { emulation = 0 ;}  ;
	virtual ~rb() { } ;



	virtual int cmd(u_char cmd, u_int param=0, u_int dest=4) { LOG(DBG,"Sending cmd") ; return 0 ; } ;
	virtual int read(u_int param=0) { return 0 ; } ;
	virtual int write(char *inbuff, int bytes, int nodelay=0, int param=0) { return 0 ; } ;
	virtual int busy() { return 0 ; } ;
	virtual int config(int emul=0) { return 0 ; } ;		// at config run

	virtual int open(char *vbuff = 0, u_int pbuf = 0) { return 0 ; } ;			
	virtual void close() { return ; } ;			

	virtual int start(u_int w=4) = 0 ;		// at start run
	virtual int stop() = 0 ;			// at stop run

	virtual int get(char **addr, u_int *status) = 0 ;
	virtual int get_priority(char **addr, u_int *status, int *fifo_ix=0) {
		if(fifo_ix) *fifo_ix = -1 ;
		return get(addr,status) ;
	}

	virtual int free(char *addr) = 0 ;
	virtual int free_ix(int ix) = 0 ;

	virtual int mark(char *addr, u_int how=0) = 0 ;

	virtual int inject(int token) { return -1 ; } ;	// for emulation

	virtual int link_check() { return 0 ; } ;

	virtual int get_free_fifos() { return 0 ; } ;

	virtual void emu_place_event(int ix, char *event, int bytes) { return ; } ;
	virtual void emu_go() { return ; } ;
	
	u_int alloced_bytes ;
	int configd ;
	int emulation ;

	int emu_fifo_length[128] ;
	volatile rbFifo_t *sfifo, *rfifo ;	// same for fifos

protected:

	int board ;
	int ch ;	// also port!



	char *buff ;	// buffer addresses
	int fifo_cou ;		// set at code startup
	u_int buff_bytes ;	// set at code startup


	char *rbuff, *sbuff ;	// address of receive/send buffers



} ;


#endif
