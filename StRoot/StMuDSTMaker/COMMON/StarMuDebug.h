/***********************************************************************
 *
 *  StarMuDebug.h,v 1.0 1999/09/07
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StarMuDebug_h
#define StarMuDebug_h

#include "TObject.h"

#define DEBUGMESSAGE(x)  if (StarMuDebug::level())    cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGMESSAGE1(x) if (StarMuDebug::level()>=1) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGMESSAGE2(x) if (StarMuDebug::level()>=2) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGMESSAGE3(x) if (StarMuDebug::level()>=3) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << x << endl;
#define DEBUGVALUE(x)  if (StarMuDebug::level())    cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define DEBUGVALUE1(x) if (StarMuDebug::level()>=1) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define DEBUGVALUE2(x) if (StarMuDebug::level()>=2) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define DEBUGVALUE3(x) if (StarMuDebug::level()>=3) cout << "##### " << __PRETTY_FUNCTION__ << "##### " << (#x) << "=" << x << endl;
#define IFDEBUG(x)  if (StarMuDebug::level())    { DEBUGMESSAGE(""); (x);} 
#define IFDEBUG1(x) if (StarMuDebug::level()>=1) { DEBUGMESSAGE(""); (x);} 
#define IFDEBUG2(x) if (StarMuDebug::level()>=2) { DEBUGMESSAGE(""); (x);} 
#define IFDEBUG3(x) if (StarMuDebug::level()>=3) { DEBUGMESSAGE(""); (x);} 


class StarMuDebug : public TObject{
 public:
  StarMuDebug() {};
  ~StarMuDebug() {};
  
  static void on(unsigned int level=1) { setLevel(level); }
  static void setLevel(unsigned int level) { mDebug=level; }
  static void off() {mDebug=0;}
  static unsigned int level() { return mDebug;}
 private:
  static unsigned int mDebug;
  ClassDef(StarMuDebug,1)
};

#endif

/***********************************************************************
 *
 * $Log: StarMuDebug.h,v $
 * Revision 1.1  2002/03/05 15:41:08  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 ***********************************************************************/
