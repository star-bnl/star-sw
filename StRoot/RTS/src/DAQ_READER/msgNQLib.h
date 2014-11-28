#ifndef _MSG_NQ_LIB_H_
#define _MSG_NQ_LIB_H_

// library used in Network Queueus for cline->evpDoor communication ONLY!

#define WAIT_FOREVER	(-1)
#define NO_WAIT                 0
#define S_objLib_OBJ_TIMEOUT    EAGAIN

#define MSG_Q_TIMEOUT           (-1)
#define MSG_Q_ERROR             (-2)    // OS-specific error i.e. write returns -1
#define MSG_Q_NOTASK            (-3)    // "no task present" error


#define MSG_NQ_MAXQUEUES		256	


struct msgNQStruct {
	int desc ;
	int len ;
	int locked ;
	int task ;
} ;



extern int msgNQCreate(char *host, int port, int msglen) ;
extern int msgNQSend(int desc, char *what, int size, int timeout = WAIT_FOREVER, int prio = 0) ;
extern int msgNQReceive(int desc, char *where, int size, int timeout = WAIT_FOREVER) ;
extern int msgNQDelete(int desc) ;
extern int msgNQCheck(int desc) ;

#endif
