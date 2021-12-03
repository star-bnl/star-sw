#ifndef _IPC_Q_LIB_HH_
#define _IPC_Q_LIB_HH_

#include <stdint.h>

#include <SUNRT/clock.h>

struct msgbuf ;

class ipcQClass {
public:
	ipcQClass(int id, int create=1, uint16_t node_id=0) ;
	~ipcQClass() ;

	void dup(int tsk, int create=1, uint16_t node_id=0) ;	// makes this que be a duplicate for task tsk as well...

	int send(void *ptr, int bytes, int tmout) ;
	int receive(void *ptr, int bytes, int tmout) ;
	int send(struct msgbuf *msg, int bytes, int tmout) ;
	int receive(struct msgbuf *msg, int bytes, int tmout) ;
	int peek(void) ;

	static int remove(int id, uint16_t node_id=0) ;
	static ipcQClass *find(uint16_t node, int task, int crea=0) ;

	int task ;
	uint16_t node ;

	uint32_t snd_seq ;
	uint32_t rcv_seq ;

	uint32_t snd_ticks[4] ;	// 0 min, 1 av, 2 max, 3 last
	uint32_t rcv_ticks[4] ;
	uint32_t msg_ticks[4] ;

	uint8_t snd_blocked, rcv_blocked ;

	static class ipcQClass *daqTasks[256] ;	// the index is the task number

	static struct ipcQStruct {
		ipcQClass *que ;
		uint16_t node ;
		int task ;
	} queue_list[256] ;	// the index is just a counter 
	static uint32_t queue_num ;	// ...with up to queue_num counts


private:
	static const uint32_t ticker(void) { return getCpuCycle() ; } ;
	int ipc_qid ;
	key_t key ;
} ;

#endif
