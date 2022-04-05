#ifndef St_tpcCalibResolutionsC_h
#define St_tpcCalibResolutionsC_h

#include "TChair.h"
#include "tables/St_tpcCalibResolutions_Table.h"

class St_tpcCalibResolutionsC : public TChair {
 public:
  static St_tpcCalibResolutionsC* 	instance();
  tpcCalibResolutions_st 	*Struct(Int_t i = 0) 	{return ((St_tpcCalibResolutions*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t 	SpaceCharge(Int_t i = 0) 	{return Struct(i)->SpaceCharge;}
  Float_t 	GridLeak(Int_t i = 0)	 	{return Struct(i)->GridLeak;}
 protected:
  St_tpcCalibResolutionsC(St_tpcCalibResolutions *table=0) : TChair(table) {}
  virtual ~St_tpcCalibResolutionsC() {fgInstance = 0;}
 private:
  static St_tpcCalibResolutionsC* fgInstance;
  ClassDefChair(St_tpcCalibResolutions, tpcCalibResolutions_st )
  ClassDef(St_tpcCalibResolutionsC,1) //C++ TChair for tpcCalibResolutions table class
};
#endif
