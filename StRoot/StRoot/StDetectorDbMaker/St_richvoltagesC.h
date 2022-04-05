#ifndef St_richvoltagesC_h
#define St_richvoltagesC_h

#include "TChair.h"
#include "tables/St_richvoltages_Table.h"

class St_richvoltagesC : public TChair {
 public:
  static St_richvoltagesC* 	instance();
  richvoltages_st 	*Struct(Int_t i = 0) 	{return ((St_richvoltages*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	startStatusTime(Int_t i = 0) 	{return Struct(i)->startStatusTime;}
  UInt_t 	endStatusTime(Int_t i = 0) 	{return Struct(i)->endStatusTime;}
  UInt_t 	status(Int_t i = 0) 	        {return Struct(i)->status;}
 protected:
  St_richvoltagesC(St_richvoltages *table=0) : TChair(table) {}
  virtual ~St_richvoltagesC() {fgInstance = 0;}
 private:
  static St_richvoltagesC* fgInstance;
  ClassDefChair(St_richvoltages, richvoltages_st )
  ClassDef(St_richvoltagesC,1) //C++ TChair for richvoltages table class
};
#endif
