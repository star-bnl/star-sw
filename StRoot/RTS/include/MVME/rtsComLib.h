#ifndef _RTSCOM_LIB_H_
#define _RTSCOM_LIB_H_

#include <vxWorks.h>
#include <msgQLib.h>

#include <iccp.h>

extern int rtsMsgInit(MSG_Q_ID *qlist, UINT16 myid, MSG_Q_ID ethQue, MSG_Q_ID vmeQue) ;
extern int rtsMsgSend(UINT16 node, struct ic_msg *m) ;


#endif
