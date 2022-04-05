#ifndef _IPC_Q_LIB_HH_
#define _IPC_Q_LIB_HH_

#include <sys/types.h>

#include <SUNRT/clock.h>

struct msgbuf ;

class ipcQClass {
public:
	ipcQClass(int id, int create=1, u_short node_id=0) ;
	~ipcQClass() ;

	void dup(int tsk, int create=1, u_short node_id=0) ;	// makes this que be a duplicate for task tsk as well...

	int send(void *ptr, int bytes, int tmout) ;
	int receive(void *ptr, int bytes, int tmout) ;
	int send(struct msgbuf *msg, int bytes, int tmout) ;
	int receive(struct msgbuf *msg, int bytes, int tmout) ;
	int peek(void) ;

	static int remove(int id, u_short node_id=0) ;
	static ipcQClass *find(u_short node, int task, int crea=0) ;

	int task ;
	u_short node ;

	u_int snd_seq ;
	u_int rcv_seq ;

	u_int snd_ticks[4] ;	// 0 min, 1 av, 2 max, 3 last
	u_int rcv_ticks[4] ;
	u_int msg_ticks[4] ;

	u_char snd_blocked, rcv_blocked ;

	static class ipcQClass *daqTasks[256] ;	// the index is the task number

	static struct ipcQStruct {
		ipcQClass *que ;
		u_short node ;
		int task ;
	} queue_list[256] ;	// the index is just a counter 
	static u_int queue_num ;	// ...with up to queue_num counts


private:
	static const u_int ticker(void) { return getCpuCycle() ; } ;
	int ipc_qid ;
	key_t key ;
} ;

#endif
