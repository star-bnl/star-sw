#ifndef St_trgTimeOffsetC_h
#define St_trgTimeOffsetC_h

#include "TChair.h"
#include "tables/St_trgTimeOffset_Table.h"

class St_trgTimeOffsetC : public TChair {
 public:
  static St_trgTimeOffsetC* 	instance();
  trgTimeOffset_st 	*Struct(Int_t i = 0) 	{return ((St_trgTimeOffset*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t 	offset(Int_t i = 0) 	        {return Struct(i)->offset;}
  Float_t 	laserOffset(Int_t i = 0) 	{return Struct(i)->laserOffset;}
 protected:
  St_trgTimeOffsetC(St_trgTimeOffset *table=0) : TChair(table) {}
  virtual ~St_trgTimeOffsetC() {SafeDelete(fgInstance);}
 private:
  static St_trgTimeOffsetC* fgInstance;
  ClassDefChair(St_trgTimeOffset, trgTimeOffset_st )
  ClassDef(St_trgTimeOffsetC,1) //C++ TChair for trgTimeOffset table class
};
#endif
