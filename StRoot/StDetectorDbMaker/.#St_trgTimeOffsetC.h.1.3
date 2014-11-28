#ifndef St_trgTimeOffsetC_h
#define St_trgTimeOffsetC_h

#include "TChair.h"
#include "tables/St_trgTimeOffset_Table.h"

class St_trgTimeOffsetC : public TChair {
 public:
  static St_trgTimeOffsetC* 	instance();
  trgTimeOffset_st 	*Struct(Int_t i = 0) 	   {return ((St_trgTimeOffset*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	   {return GetNRows();}
  Float_t 	offset(Int_t i = 0)     	   {return Struct(i)->offset;}
  Float_t 	laserOffset(Int_t i = 0) 	   {return Struct(i)->laserOffset;}
  Float_t 	laserOffsetW(Int_t i = 0) 	   {return Struct(i)->laserOffsetW;}
  Float_t       triggerTimeOffset(Int_t i = 0)     {return 1e-6*(mLaser ? laserOffset(i)  : offset(i));} // usec
  Float_t       triggerTimeOffsetWest(Int_t i = 0) {return 1e-6*(mLaser ? laserOffsetW(i) :         0);} // usec
  void          SetLaser(Bool_t k = kTRUE)         {mLaser = k;}
 protected:
  St_trgTimeOffsetC(St_trgTimeOffset *table=0) : TChair(table), mLaser(kFALSE) {}
  virtual ~St_trgTimeOffsetC() {fgInstance = 0;}
 private:
  static St_trgTimeOffsetC* fgInstance;
  Bool_t        mLaser;
  ClassDefChair(St_trgTimeOffset, trgTimeOffset_st )
  ClassDef(St_trgTimeOffsetC,1) //C++ TChair for trgTimeOffset table class
};
#endif
