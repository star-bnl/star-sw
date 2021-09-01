#ifndef St_GatingGridTimesC_h
#define St_GatingGridTimesC_h

#include "TChair.h"
#include "tables/St_GatingGridTimes_Table.h"

class St_GatingGridTimesC : public TChair {
 public:
  static St_GatingGridTimesC* 	instance();
  GatingGridTimes_st 	*Struct(Int_t i = 0) 	const {return ((St_GatingGridTimes*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	t0(Int_t i = 0) 	const {return Struct(i)->t0;}
  Float_t 	tau(Int_t i = 0) 	const {return Struct(i)->tau;}
 protected:
  St_GatingGridTimesC(St_GatingGridTimes *table=0) : TChair(table) {}
  virtual ~St_GatingGridTimesC() {fgInstance = 0;}
 private:
  static St_GatingGridTimesC* fgInstance;
  ClassDefChair(St_GatingGridTimes, GatingGridTimes_st )
  ClassDef(St_GatingGridTimesC,1) //C++ TChair for GatingGridTimes table class
};
#endif
