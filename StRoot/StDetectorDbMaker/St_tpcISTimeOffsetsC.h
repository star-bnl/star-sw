#ifndef St_tpcISTimeOffsetsC_h
#define St_tpcISTimeOffsetsC_h

#include "TChair.h"
#include "tables/St_tpcISTimeOffsets_Table.h"

class St_tpcISTimeOffsetsC : public TChair {
 public:
  static St_tpcISTimeOffsetsC* 	instance();
  tpcISTimeOffsets_st 	*Struct(Int_t i = 0) {return ((St_tpcISTimeOffsets*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  Float_t* 	offset(Int_t i = 0) 	     {return Struct(i)->offset;}
 protected:
  St_tpcISTimeOffsetsC(St_tpcISTimeOffsets *table=0) : TChair(table) {}
  virtual ~St_tpcISTimeOffsetsC() {fgInstance = 0;}
 private:
  static St_tpcISTimeOffsetsC* fgInstance;
  ClassDefChair(St_tpcISTimeOffsets, tpcISTimeOffsets_st )
  ClassDef(St_tpcISTimeOffsetsC,1) //C++ TChair for tpcISTimeOffsets table class
};
#endif
