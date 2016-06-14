/*!
  \class StMessageCounter
  \author G. Van Buren, BNL

  This class manages message limiting in STAR. It is a singleton.
  Limits can be placed on message types (i.e. "I" for info messages)
  or on strings in messages (i.e. "dst_track empty")

*/

#ifndef ClassStMessageCounter
#define ClassStMessageCounter

#include "StMessTypeList.h"
#include <Stsstream.h>
#include <Stiostream.h>

typedef StVector(char*) messCharVec;
typedef StVector(char*)::iterator messCharVecIter;


class StMessageCounter : public ostrstream {
 private:
   static StMessageCounter* mInstance;
   StMessTypeList* messTypeList;
   const char* limitMessage;
   int yesLimits;
   int noLimits;
   messCharVecIter curString;

 protected:
   StMessageCounter();
   StMessageCounter(const StMessageCounter&);
   intVector limitTList;                         // List of type limits
   intVector limitTCountList;                    // List of type counts
   messCharVec limitList;                        // List of strings to limit
   intVector limitNList;                         // List of string limits
   intVector limitNCountList;                    // List of string counts
   messCharVec limitWList;                       // List of waiting types
   intVector limitWNList;                        // List of waiting type limits
 
 public:
   ~StMessageCounter();
    static StMessageCounter* Instance();
      void SetLimit(const char* str, int n=0);
       int GetLimit(const char* str);
      void ListLimits();
      void AddType(const char* type);
       int CheckLimit(char* mess, const char* type);
      void NoLimits() {noLimits = 1;}
};

#endif

// $Id: StMessageCounter.h,v 1.14 2016/06/14 06:26:34 genevb Exp $
// $Log: StMessageCounter.h,v $
// Revision 1.14  2016/06/14 06:26:34  genevb
// better initializations (Coverity)
//
// Revision 1.13  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.12  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.11  2000/06/07 00:05:36  genevb
// Added FixOn(), enforcing no limits on a specific message type/string
//
// Revision 1.10  2000/03/30 16:12:55  genevb
// Add NoLimits() capability to turn off message limiting.
//
// Revision 1.9  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
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
