#ifndef _MSG_QN_LIB_H_
#define _MSG_QN_LIB_H_

#define MSG_Q_DIR		"/tmp"
#define MSG_Q_MAXQUEUES		256


#define WAIT_FOREVER		(-1)
#define NO_WAIT			0
#define S_objLib_OBJ_TIMEOUT	EAGAIN


#define MSG_Q_LOCK_NONE		0
#define MSG_Q_LOCK		1
#define MSG_Q_LOCK_EXPECT	2


#define MSG_Q_TIMEOUT		(-1)
#define MSG_Q_ERROR		(-2)	// OS-specific error i.e. write returns -1
#define MSG_Q_NOTASK		(-3)	// "no task present" error 

struct msgQNStruct {
	int desc ;
	int len ;
	int locked ;
	int task ;
} ;


#define MSG_Q_ID	int



extern MSG_Q_ID msgQNCreate(int task, int msglen, int lock = MSG_Q_LOCK_NONE) ;
extern int msgQNSend(MSG_Q_ID task, char *what, int size, int timeout = WAIT_FOREVER, int prio = 0) ;
extern int msgQNReceive(MSG_Q_ID task, char *where, int size, int timeout = WAIT_FOREVER) ;
extern int msgQNDelete(MSG_Q_ID task) ;
extern int msgQNLocking(MSG_Q_ID task, int lock) ;
extern int msgQNSize(MSG_Q_ID task, int size) ;
extern int msgQNCheck(MSG_Q_ID task) ;

#endif
