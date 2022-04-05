#ifndef St_tpcDriftVelocityC_h
#define St_tpcDriftVelocityC_h

#include "TChair.h"
#include "tables/St_tpcDriftVelocity_Table.h"

class St_tpcDriftVelocityC : public TChair {
 public:
  static St_tpcDriftVelocityC* 	instance();
  tpcDriftVelocity_st 	*Struct(Int_t i = 0) 	        {return ((St_tpcDriftVelocity*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	        {return GetNRows();}
  Float_t 	laserDriftVelocityEast(Int_t i = 0) 	{return Struct(i)->laserDriftVelocityEast;}
  Float_t 	laserDriftVelocityWest(Int_t i = 0) 	{return Struct(i)->laserDriftVelocityWest;}
  Float_t 	cathodeDriftVelocityEast(Int_t i = 0) 	{return Struct(i)->cathodeDriftVelocityEast;}
  Float_t 	cathodeDriftVelocityWest(Int_t i = 0) 	{return Struct(i)->cathodeDriftVelocityWest;}
#if 0
  Float_t 	scaleY(Int_t i = 0) 	                {return Struct(i)->scaleY;}
#endif
 protected:
  St_tpcDriftVelocityC(St_tpcDriftVelocity *table=0) : TChair(table) {}
  virtual ~St_tpcDriftVelocityC() {fgInstance = 0;}
 private:
  static St_tpcDriftVelocityC* fgInstance;
  ClassDefChair(St_tpcDriftVelocity, tpcDriftVelocity_st )
  ClassDef(St_tpcDriftVelocityC,1) //C++ TChair for tpcDriftVelocity table class
};
#endif
