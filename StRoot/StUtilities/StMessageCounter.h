// $Id: StMessageCounter.h,v 1.1 1999/06/23 15:17:50 genevb Exp $
// $Log: StMessageCounter.h,v $
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

 protected:
   StMessageCounter();
   StMessageCounter(const StMessageCounter&);
   messCharVec limitList;
   messIntVec limitNList;
   messIntVec limitNCountList;
   messIntVec limitTList;
   messIntVec limitTCountList;
 
 public:
   ~StMessageCounter();
    static StMessageCounter* Instance();
      void SetLimit(Char_t* str, Int_t n=0);
      void AddType();
   Char_t* CheckLimit(Char_t* mess, Char_t* type);
   ClassDef(StMessageCounter,0)
};

#endif
