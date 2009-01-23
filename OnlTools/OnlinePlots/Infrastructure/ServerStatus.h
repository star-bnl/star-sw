#ifndef ServerStatus_h
#define ServerStatus_h


#include <time.h>

class ServerStatus : public TObject {
 private:
  time_t mReceiveTime;
  time_t mRequestTime;
  int mReceiveType;
  int mRequestType;
 public:

  ServerStatus() : mReceiveTime(0), mRequestTime(0), mReceiveType(0), mRequestType(7) {}
  
 time_t getReceiveTime() { return mReceiveTime;}
 time_t getRequestTime() { return mRequestTime;}
 int    getReceiveType() { return mReceiveType;}
 int    getRequestType() { return mRequestType;}
 void setReceiveTime(time_t n) { mReceiveTime= n;}
 void setRequestTime(time_t n) { mRequestTime= n;}
 void setReceiveType(int n)    { mReceiveType= n;}
 void setRequestType(int n)    { mRequestType= n;}
 double diffTimeInSec() { return fabs(difftime( mRequestTime, mReceiveTime )); }
   
 ClassDef(ServerStatus,1) ;
};



#endif





/***************************************************************************
 *
 * $Id: ServerStatus.h,v 1.1 2009/01/23 16:10:59 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: ServerStatus.h,v $
 * Revision 1.1  2009/01/23 16:10:59  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

