#ifndef MYRINET_HH
#define MYRINET_HH

#include "iccp.h"
#include <stdio.h>
#include "platform.h"

/* Myrinet functions */

#ifdef __CINT__
#define unix
#endif

#ifdef unix 
typedef int INT32;
typedef short int INT16;
typedef unsigned int UINT32;
typedef unsigned short int UINT16;
#define OK 0
#define ERROR 1
#define IMPORT 
typedef int STATUS;
#endif

#if defined(unix) && defined(THREADS)
#include <UNIX/ThreadsMsgQueue.hh>
#endif

#ifndef unix     // Provide these functions to vxWorks only
#include <msgQLib.h>
int myriReadFile(char *filename, void *buffer, int size);
int myriWriteFile(char *filename, void *buffer, int size);
int myriReadFile(char *filename, void *buffer, int size, int node);
int myriWriteFile(char *filename, void *buffer, int size, int node);
#endif

#if defined(__linux__) || defined(__APPLE__)

// pnode should point to a UINT16 or be NULL!
extern void *vxFileRequestServer(void *pnode=NULL);
#endif

int myriInit(pMSGQARRAY_PROTO(msgQueArray), int multinodeSupport=0);

/*
 * Description:
 * Initializes myrinet gm_driver
 * neccessary send/receive buffers for messages
 * creates all necessary semephores
 * spawns myriRcv task
 *
 * Arguments:
 * MSG_Q_ID *msgQueArray   :: msg que array for message delivery
 * MSG_Q_ID errorQue       :: optional que for invalid tasks
 * int noNodeIDConvertion  :: If 0, use DAQ id's in every call
 *                         :: If 1, use normal MYRINET id's
 *
 * Return Values:
 * OK, if initialized correctly, ERROR otherwise
 */

STATUS raw_myriMsgSend(UINT16 targetId, ic_msg *pMsg, int override_source_id);
STATUS myriMsgSend(UINT16 targetId, ic_msg *pMsg, int board=0);


STATUS myriMsgSend(UINT16 *targetList, ic_msg *pMsg, int board=0);
STATUS myriMsgSend(UINT16 targetId, ic_msg *msgList[], int board=0);
STATUS myriMsgSend(UINT16 *targetList, ic_msg *msgList[], int board=0);



/* 
 * Description:
 * Fills in the sourceID and transaction number fields in the payload of 
 * the iccp message buffer. Sends message via MYRINET to requested target.
 *
 * Arguments: 
 * UINT16 targetId:	16bit myrinet nodeID of target
 * ic_msg *pMsg:	pointer the DAQ message
 *
 * Return Values:
 * OK, if sent correctly, otherwise ERROR
 */


STATUS
myriMemcpy(UINT32 pTargetBuff_hi,
	   UINT32 pTargetBuff_lo,
	   UINT32 *pLocalBuff, 
	   UINT32 len,
	   UINT16 targetId,
	   SEM_ID sem=NULL);

// myriMemcpy2 is like myriMemcpy, but sends the data in 
// a message...The max size of the message must be < 32k bytes
// 
// The receiving node must have a handler that knows how to 
// copy the received message into its own memory
//
// No chaser message is required to ensure that the message is received
//

STATUS
myriMemcpy2(UINT32 *pLocalBuff,
	    UINT32 len,
	    UINT16 targetId,
	    SEM_ID sem=NULL);


STATUS myriAddAuxMem(void *start, unsigned long int size);

/*
 * Register extra memory
 * For use with EVB's extended memory
 *
 */

void setMyriLogLevel(int);

UINT16 getDaqNodeId(char *nodename=NULL, int board=0);

int myriStart();

extern void *(*directMsgCallback)(void *,int,UINT16);
extern pTASKID myriRcvTaskId;
extern int myriErrorTask;

void myriFreeMemcpy2Buff(char *buff);
int myriSetMemcpy2Buffs(char *start, int n, int size);

#endif


