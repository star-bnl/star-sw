// $Id: StHFillObject.h,v 1.1 1999/07/29 23:27:34 genevb Exp $
// $Log: StHFillObject.h,v $
// Revision 1.1  1999/07/29 23:27:34  genevb
// Introduction of new class
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHFillObject allows member functions to be histogrammed             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHFillObject
#define STAR_StHFillObject

#include "TNamed.h"
class StHFillObject;
class TList;
typedef float (StHFillObject::* TMemOff) ();  // "Pointer to member" types
typedef float* (StHFillObject::* TMemOffP) ();
typedef float* (StHFillObject::* TMemOffI) ();
typedef float* (StHFillObject::* TMemOffIS) ();

class StMemOff : public TNamed {
 public:
  TMemOff offset;
  StMemOff(Char_t* name, TMemOff value) : TNamed(name,name), offset(value) {}
  virtual ~StMemOff() {}
};


class StHFillObject : public TObject {
 private:
 protected:
  static TList *knownMembers;                  //!
  static TMemOff GetOffset(Char_t* member);

 public:
  StHFillObject();
  virtual ~StHFillObject();
   static void LearnMember(Char_t* member, TMemOff offset);
  virtual void LearnMembers()=0;
   static void Reset();
   static void Setup(Option_t* option, Int_t hists=1);
          void GetValues(Int_t printIt=0);
  virtual void Draw(Option_t* option);
  virtual void Print(Option_t* option);
  virtual void ls(Option_t* option);
  virtual void Update();
  ClassDef(StHFillObject,1)   //virtual base class for Makers
};

#endif
