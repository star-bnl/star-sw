#ifndef _ETH_LIB_H_
#define _ETH_LIB_H_


#define ETH_MAX_MSG_SIZE	512
#define ETH_HEARTBEAT_SECONDS	10

#ifndef UNIX	// vxWorks
#include <vxWorks.h>
#include <msgQLib.h>

extern int ethInit(MSG_Q_ID *arrayQue, MSG_Q_ID defaultQue, UINT16 node) ;
extern MSG_Q_ID ethOpen(char *server, int port) ;
extern int ethClose(MSG_Q_ID q) ;
extern int ethSysresetOnReboot ;

#else		// UNIX







#endif	// UNIX


#endif	// _ETH_LIB_H_
