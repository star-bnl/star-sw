#ifndef St_tpcHighVoltagesC_h
#define St_tpcHighVoltagesC_h

#include "TChair.h"
#include "tables/St_tpcHighVoltages_Table.h"

class St_tpcHighVoltagesC : public TChair {
 public:
  static St_tpcHighVoltagesC* 	instance();
  tpcHighVoltages_st 	*Struct(Int_t i = 0) 	{return ((St_tpcHighVoltages*) instance()->Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return instance()->GetNRows();}
  Float_t 	cathode(Int_t i = 0) 	{return Struct(i)->cathode;}
  Float_t 	gatedGridRef(Int_t i = 0) 	{return Struct(i)->gatedGridRef;}
  Double_t      getCathodeVoltage() {return cathode();}
  Double_t      getGGVoltage() {return gatedGridRef();}
  
 protected:
  St_tpcHighVoltagesC(St_tpcHighVoltages *table=0) : TChair(table) {}
  virtual ~St_tpcHighVoltagesC() {SafeDelete(fgInstance);}
 private:
  static St_tpcHighVoltagesC* fgInstance;
  ClassDefChair(St_tpcHighVoltages, tpcHighVoltages_st )
  ClassDef(St_tpcHighVoltagesC,1) //C++ TChair for tpcHighVoltages table class
};
#endif
