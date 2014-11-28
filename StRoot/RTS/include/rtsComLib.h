#ifndef _RTS_COM_STUB_H_
#define _RTS_COM_STUB_H_


#include <iccp.h>


/************* VXWORKS *********************/
#if defined(vxworks)

#include <vxWorks.h>
#include <msgQLib.h>

typedef MSG_Q_ID	RTS_QUEUE_ID ;


/*************** UNIX - empty stubs *************/
#else

typedef int		RTS_QUEUE_ID ;

// mimic vxWorks
#define NO_WAIT		0
#define WAIT_FOREVER	(-1)

#define MSG_PRI_NORMAL	0
#define MSG_PRI_URGENT	1


#endif


extern unsigned short rtsNodeId ;	// reset by application or rtsMsgInit
extern unsigned short rtsMyriNodeId ;	// reset by myrinet
extern unsigned int rtsMyriBoard  ;		// reset by myrinet

extern int (*stub_myriMsgSend)(unsigned short node, ic_msg *m, int board )  ;	// reset by myrinet
extern int (*stub_myriMsgSend_n)(unsigned short *node, ic_msg *m, int board ) ; // reset by myrinet
extern int (*stub_myriMsgSend_m)(unsigned short node, ic_msg *m[], int board ) ;        // reset by myrinet
extern int (*stub_myriMsgSend_nm)(unsigned short *node, ic_msg *m[], int board ) ;      // reset by myrinet


extern int (*rtsSendLocal)(RTS_QUEUE_ID id, char *m, unsigned int bytes, int wait, int prio)  ;	// reset by application

extern int rtsMsgInit(RTS_QUEUE_ID *qlist, unsigned short myid, RTS_QUEUE_ID eth, RTS_QUEUE_ID vme) ;

extern int rtsMsgSend(unsigned short node, ic_msg *m) ;
extern int rtsMsgSend(unsigned short *node, ic_msg *m) ;
extern int rtsMsgSend(unsigned short node, ic_msg *m[]) ;
extern int rtsMsgSend(unsigned short *node, ic_msg *m[]) ;

#endif
