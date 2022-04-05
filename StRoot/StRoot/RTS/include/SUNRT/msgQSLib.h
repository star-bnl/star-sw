#ifndef _MSGSQLIB_H_
#define _MSGSQLIB_H_

#include "objLib.h"

typedef int MSG_Q_ID;

// kill all message queues
void msgQSSystemReset();

// MSG_Q_ID is an int shared among all process's (the task)
// You may request a specific process 
// or allow system to create one.
// System created id's are > 256...

// options: NO_SEM        0x00
//          PROCESS_SEM   0x11
//          THREAD_SEM    0x10

STATUS msgQSCreate(int maxMsgs, int maxMsgLength, int options, int id=-1);

// OPTION
// ------
// NO_SEM  -- never blocks.  timeout completely ignored.  EAGAIN if can't send
// 
// xxx_SEM && timeout == NO_WAIT  -- never blocks. EAGAIN if can't send
// xxx_SEM && timeout == WAIT_FOREVER -- blocks forever.  No interrupt.
// xxx_SEM && timeout == int -- blocks, but can be interrupted.  EAGAIN if
//                       interrupted
//
// priority 0 -- add to end.
// priotity 1 -- add to begining
//
STATUS msgQSSend(MSG_Q_ID msgQId, char *buffer, int nBytes, int timeout=NO_WAIT, int priority=0);

STATUS msgQSReceive(MSG_Q_ID msgQId, char *buffer, int maxNBytes, int timeout=WAIT_FOREVER);




#endif
