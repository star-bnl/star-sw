/***********************************************************************
 *
 *  StMuDebug.h,v 1.0 1999/09/07
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StMuDebug_h
#define StMuDebug_h

#include "TObject.h"

#define DEBUGMESSAGE(x)  if (StMuDebug::level()>0)    cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGMESSAGE1(x) if (StMuDebug::level()>=1) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGMESSAGE2(x) if (StMuDebug::level()>=2) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGMESSAGE3(x) if (StMuDebug::level()>=3) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGVALUE(x)  if (StMuDebug::level()>0)    cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define DEBUGVALUE1(x) if (StMuDebug::level()>=1) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define DEBUGVALUE2(x) if (StMuDebug::level()>=2) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define DEBUGVALUE3(x) if (StMuDebug::level()>=3) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define IFDEBUG(x)  if (StMuDebug::level()>0)    { DEBUGMESSAGE(""); (x);} 
#define IFDEBUG1(x) if (StMuDebug::level()>=1) { DEBUGMESSAGE(""); (x);} 
#define IFDEBUG2(x) if (StMuDebug::level()>=2) { DEBUGMESSAGE(""); (x);} 
#define IFDEBUG3(x) if (StMuDebug::level()>=3) { DEBUGMESSAGE(""); (x);} 


class StMuDebug : public TObject{
 public:
  StMuDebug() {};
  ~StMuDebug() {};
  
  
  static void on() { mDebug = absolut(mDebug); }
  static void setLevel(unsigned int level) { mDebug=level; }
  static void off() { mDebug = -1*absolut(mDebug); }
  static unsigned int level() { return mDebug;}
 private:
  static int absolut(int i) { return (i>0) ? i : -i; }
  static unsigned int mDebug;
  ClassDef(StMuDebug,1)
};

#endif

/***********************************************************************
 *
 * $Log: StMuDebug.h,v $
 * Revision 1.2  2002/04/11 14:19:30  laue
 * - update for RH 7.2
 * - decrease default arrays sizes
 * - add data base readerfor number of events in a file
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 ***********************************************************************/
