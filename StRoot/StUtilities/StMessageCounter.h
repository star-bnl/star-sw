// $Id: StMessageCounter.h,v 1.8 1999/07/17 00:23:24 genevb Exp $
// $Log: StMessageCounter.h,v $
// Revision 1.8  1999/07/17 00:23:24  genevb
// Fixed bug when option fields are empty in FORTRAN, and let type limits be set before types are even added
//
// Revision 1.7  1999/06/30 17:24:50  genevb
// Better limit management, remove Bool_t
//
// Revision 1.6  1999/06/29 17:37:31  genevb
// Lots of fixes...
//
// Revision 1.5  1999/06/28 02:40:56  genevb
// Additional backward compatibilit with MSG (msg_enable, msg_enabled, msg_disable
//
// Revision 1.4  1999/06/26 00:24:53  genevb
// Fixed const type mismatches
//
// Revision 1.3  1999/06/25 22:57:57  genevb
// Fixed a small bug in MSG compatibiliti
//
// Revision 1.2  1999/06/24 16:30:42  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:50  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageCounter                                                     //
//                                                                      //
// This class manages message limiting in STAR. It is a singleton.      //
// Limits can be placed on message types (i.e. "I" for info messages)   //
// or on strings in messages (i.e. "dst_track empty")                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessageCounter
#define ClassStMessageCounter

#include "StMessTypeList.h"
#include <strstream.h>

typedef StVector(char*) messCharVec;
typedef StVector(char*)::iterator messCharVecIter;


class StMessageCounter : public ostrstream {
 private:
   static StMessageCounter* mInstance;
   StMessTypeList* messTypeList;
   const char* limitMessage;
   int yesLimits;
   messCharVecIter curString;
   size_t index;

 protected:
   StMessageCounter();
   StMessageCounter(const StMessageCounter&);
   intVector limitTList;
   intVector limitTCountList;
   messCharVec limitList;
   intVector limitNList;
   intVector limitNCountList;
   messCharVec limitWList;
   intVector limitWCountList;
 
 public:
   ~StMessageCounter();
    static StMessageCounter* Instance();
      void SetLimit(char* str, int n=0);
       int GetLimit(char* str);
      void ListLimits();
      void AddType(const char* type);
       int CheckLimit(char* mess, const char* type);
};

#endif
