#ifndef EEfeeRunDescr_h
#define EEfeeRunDescr_h
/*********************************************************************
 * $Id: EEfeeRunDescr.h,v 1.1 2003/01/28 23:17:14 balewski Exp $
 *********************************************************************
 * $Log: EEfeeRunDescr.h,v $
 * Revision 1.1  2003/01/28 23:17:14  balewski
 * start
 *
 * Revision 1.5  2002/12/05 21:56:05  balewski
 * pmtID bug fixed
 *
 * Revision 1.4  2002/12/04 20:48:19  balewski
 * new time stamp names
 *
 * Revision 1.3  2002/12/04 16:48:17  zolnie
 * allowed variable comment length
 *
 * Revision 1.2  2002/12/03 23:48:52  zolnie
 * changed back to var length
 *
 * Revision 1.1  2002/11/30 20:04:37  balewski
 * start
 *
 * Revision 1.1  2002/11/25 16:48:41  zolnie
 * Initial revision
 *
 *********************************************************************/

#include "TObject.h"
#include <time.h>



class EEfeeRunDescr : public TObject {
 private:
  time_t mTimeStamp;         //(unix time, GMT) 
  time_t mProcessingTime;    // auxiliary 
  int    mCommentLen; 
  char  *mComment;//[mCommentLen];
  
 public:
  EEfeeRunDescr();
  virtual ~EEfeeRunDescr();
  void  print();
  void  clear();
  void setProcessingTime( time_t t)  { mProcessingTime = t; } ;
  void setTimeStamp( time_t t)  { mTimeStamp = t; } ;
  void  setComment ( const char *str); 
  time_t getTimeStamp() const {return mTimeStamp;}
  time_t getProcessingTime() const {return mProcessingTime;}
  const char * getComment () const {return mComment;}
  
  ClassDef(EEfeeRunDescr,1) 

};
#endif


