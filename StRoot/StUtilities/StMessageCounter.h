// $Id: StMessageCounter.h,v 1.3 1999/06/25 22:57:57 genevb Exp $
// $Log: StMessageCounter.h,v $
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

typedef StVector(Char_t*) messCharVec;
typedef StVector(Char_t*)::iterator messCharVecIter;
typedef StVector(Int_t*) messIntVec;


class StMessageCounter {
 private:
   static StMessageCounter* mInstance;
   StMessTypeList* messTypeList;
   const Char_t* limitMessage;
   Char_t* outMessage;
   Int_t yesLimits;

 protected:
   StMessageCounter();
   StMessageCounter(const StMessageCounter&);
   messIntVec limitTList;
   messIntVec limitTCountList;
   messCharVec limitList;
   messIntVec limitNList;
   messIntVec limitNCountList;
 
 public:
   ~StMessageCounter();
    static StMessageCounter* Instance();
      void SetLimit(Char_t* str, Int_t n=0);
      void ListLimits();
      void AddType();
       int CheckLimit(Char_t* mess, Char_t* type);
   Char_t* GetOutMessage() const {return outMessage;}
   ClassDef(StMessageCounter,0)
};

#endif
