#ifndef St_tpcOSTimeOffsetsC_h
#define St_tpcOSTimeOffsetsC_h

#include "TChair.h"
#include "tables/St_tpcOSTimeOffsets_Table.h"

class St_tpcOSTimeOffsetsC : public TChair {
 public:
  static St_tpcOSTimeOffsetsC* 	instance();
  tpcOSTimeOffsets_st 	*Struct(Int_t i = 0) {return ((St_tpcOSTimeOffsets*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  Float_t* 	offset(Int_t i = 0) 	     {return Struct(i)->offset;}
 protected:
  St_tpcOSTimeOffsetsC(St_tpcOSTimeOffsets *table=0) : TChair(table) {}
  virtual ~St_tpcOSTimeOffsetsC() {fgInstance = 0;}
 private:
  static St_tpcOSTimeOffsetsC* fgInstance;
  ClassDefChair(St_tpcOSTimeOffsets, tpcOSTimeOffsets_st )
  ClassDef(St_tpcOSTimeOffsetsC,1) //C++ TChair for tpcOSTimeOffsets table class
};
#endif
