/*!
  \class StMessage
  \author G. Van Buren, BNL

  This is the class of messages used by StMessageManager in STAR.
  Messages have a type and message specified at instantiation,
  and also include a time-date stamp and options for printing.

*/

#ifndef ClassStMessage
#define ClassStMessage

#ifdef __ROOT__
#include "Rtypes.h"
#endif
#include <time.h>

enum StMessOpt {
  kMessOptNone = 0U,
  kMessOptDash = 1U,
  kMessOptO = (1U << 1),
  kMessOptE = (1U << 2),
  kMessOptT = (1U << 3),
  kMessOptS = (1U << 4),
  kMessOptP = (1U << 5)
  };

class StMessage {
   friend class StMessageManager;

 private:
   static int repeats;
   static void IgnoreRepeats() { repeats=0; }
   static void AllowRepeats() { repeats=1; }

 protected:
   char type[2];
//   char* location;                    //!
//   unsigned long runNumber;
//   pair<long, long> eventId;
   unsigned int option;
   time_t messTime;
   char* message;                     //!

 public:
   StMessage(const char* mess="", const char* ty="I", const char* opt="O");
   StMessage(const StMessage&){option=0;messTime=0;message=0;}
   virtual ~StMessage();
          void PrintInfo();
           int Print(int nChars=-1);
       time_t& GetTime() {return messTime;}
   const char* GetType() const {return type;}
         char* GetMessage() const {return message;}
  unsigned int GetOption() const {return option;}
         char* GetOptions() const;
          void SetOptions(const char* opt);
        size_t GetMemoryUsage();

    static int InitBuffer();
#ifdef __ROOT__
   ClassDef(StMessage,0)
#endif
};

#endif

// $Id: StMessage.h,v 1.17 2016/06/14 06:26:34 genevb Exp $
// $Log: StMessage.h,v $
// Revision 1.17  2016/06/14 06:26:34  genevb
// better initializations (Coverity)
//
// Revision 1.16  2004/04/02 22:17:14  genevb
// Added protected Ignore/AllowRepeats() for friend StBFChain class
//
// Revision 1.15  2003/10/01 20:06:50  genevb
// Initialize and test ostrstream buffer sizes (support for gcc before 3.2)
//
// Revision 1.13  2003/09/25 21:18:14  genevb
// Changed option storage
//
// Revision 1.12  2003/09/22 01:30:41  perev
//  some cleanup
//
// Revision 1.11  2001/05/16 15:50:56  genevb
// Switch TROOT.h to Rtypes.h
//
// Revision 1.10  2001/05/16 15:18:50  genevb
// Need include for TROOT
//
// Revision 1.9  2001/05/14 20:53:20  genevb
// Add features to examine memory use, switch from TDatime to time_t
//
// Revision 1.8  2000/06/10 21:19:02  genevb
// Remove use of virtual functions
//
// Revision 1.7  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.6  1999/08/18 18:28:33  fine
// Various bugs have been fixed. share lib was not loaded under HP
//
// Revision 1.5  1999/07/08 22:58:18  genevb
// Created an abstract interface with StMessMgr.h hiding template implementation from others, a few other small fixes
//
// Revision 1.4  1999/06/30 17:24:50  genevb
// Better limit management, remove Bool_t
//
// Revision 1.3  1999/06/29 17:37:31  genevb
// Lots of fixes...
//
// Revision 1.2  1999/06/24 16:30:41  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:47  genevb
// Introduction of StMessageManager
//
// Revision 1.0  1999/01/27 10:28:29  genevb
//
