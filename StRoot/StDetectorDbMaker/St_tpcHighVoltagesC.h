#ifndef St_tpcHighVoltagesC_h
#define St_tpcHighVoltagesC_h

#include "TChair.h"
#include "tables/St_tpcHighVoltages_Table.h"

class St_tpcHighVoltagesC : public TChair {
 public:
  static St_tpcHighVoltagesC* 	instance();
  tpcHighVoltages_st 	*Struct(Int_t i = 0)  {return ((St_tpcHighVoltages*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                  {return GetNRows();}
  Float_t 	cathode(Int_t i = 0)          {return Struct(i)->cathode;}
  Float_t 	gatedGridRef(Int_t i = 0)     {return Struct(i)->gatedGridRef;}
  Float_t* 	gridLeakWallTip(Int_t i = 0)  {return Struct(i)->gridLeakWallTip;}
  Float_t* 	gridLeakWallSide(Int_t i = 0) {return Struct(i)->gridLeakWallSide;}
  Double_t      getCathodeVoltage()           {return cathode();}
  Double_t      getGGVoltage()                {return gatedGridRef();}
  Double_t      getGridLeakWallTip(Int_t sector = 1)  {return gridLeakWallTip()[sector-1];}
  Double_t      getGridLeakWallSide(Int_t sector = 1) {return gridLeakWallSide()[sector-1];}
  
 protected:
  St_tpcHighVoltagesC(St_tpcHighVoltages *table=0) : TChair(table) {}
  virtual ~St_tpcHighVoltagesC() {fgInstance = 0;}
 private:
  static St_tpcHighVoltagesC* fgInstance;
  ClassDefChair(St_tpcHighVoltages, tpcHighVoltages_st )
  ClassDef(St_tpcHighVoltagesC,1) //C++ TChair for tpcHighVoltages table class
};
#endif
