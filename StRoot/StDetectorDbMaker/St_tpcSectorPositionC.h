#ifndef St_tpcSectorPositionC_h
#define St_tpcSectorPositionC_h

#include "TChair.h"
#include "tables/St_tpcSectorPosition_Table.h"

class St_tpcSectorPositionC : public TChair {
 public:
  static St_tpcSectorPositionC* 	instance();
  tpcSectorPosition_st 	*Struct(Int_t i = 0) 	        {return ((St_tpcSectorPosition*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	        {return GetNRows();}
  Float_t 	innerSectorLocalxShift(Int_t i = 0) 	{return Struct(i)->innerSectorLocalxShift;}
  Float_t 	innerSectorLocalyShift(Int_t i = 0) 	{return Struct(i)->innerSectorLocalyShift;}
  Float_t 	innerSectorRotationAngle(Int_t i = 0) 	{return Struct(i)->innerSectorRotationAngle;}
  Float_t 	innerSectorCovMatrix(Int_t i = 0) 	{return Struct(i)->innerSectorCovMatrix;}
  Float_t 	outerSectorLocalxShift(Int_t i = 0) 	{return Struct(i)->outerSectorLocalxShift;}
  Float_t 	outerSectorLocalyShift(Int_t i = 0) 	{return Struct(i)->outerSectorLocalyShift;}
  Float_t 	outerSectorRotationAngle(Int_t i = 0) 	{return Struct(i)->outerSectorRotationAngle;}
  Float_t 	outerSectorCovMatrix(Int_t i = 0) 	{return Struct(i)->outerSectorCovMatrix;}
 protected:
  St_tpcSectorPositionC(St_tpcSectorPosition *table=0) : TChair(table) {}
  virtual ~St_tpcSectorPositionC() {fgInstance = 0;}
 private:
  static St_tpcSectorPositionC* fgInstance;
  ClassDefChair(St_tpcSectorPosition, tpcSectorPosition_st )
  ClassDef(St_tpcSectorPositionC,1) //C++ TChair for tpcSectorPosition table class
};
#endif
