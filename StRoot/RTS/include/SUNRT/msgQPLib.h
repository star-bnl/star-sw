#ifndef _MSG_QP_LIB_H_
#define _MSG_QP_LIB_H_

#define MSG_Q_MAXQUEUES         256


#define WAIT_FOREVER            (-1)
#define NO_WAIT                 0
#define S_objLib_OBJ_TIMEOUT    EAGAIN
#define MSG_PRI_NORMAL		1

#define MSG_Q_TIMEOUT           (-1)
#define MSG_Q_ERROR             (-2)    // OS-specific error i.e. write return!
#define MSG_Q_NOTASK            (-3)    // "no task present" error

#define MSG_R_BLOCK		0x01
#define MSG_W_BLOCK		0x02
#define MSG_D_BLOCK		(MSG_R_BLOCK)	// default


// look at the defaults! They are as Jeff suggested.

extern int msgQPCreate(int task, int msgnum, int msglen, int block = MSG_D_BLOCK, int create = 0) ;
extern int msgQPSend(int task, char *what, int size, int timeout = NO_WAIT, int prio = MSG_PRI_NORMAL)  ;
extern int msgQPReceive(int task, char *where, int size, int timeout = WAIT_FOREVER) ;
extern int msgQPClose(int task) ;	
extern int msgQPDelete(int task) ;

#endif
