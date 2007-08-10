#ifndef St_ftpcGasSystemC_h
#define St_ftpcGasSystemC_h

#include "TChair.h"
#include "tables/St_ftpcGasSystem_Table.h"

class St_ftpcGasSystemC : public TChair {
 public:
  static St_ftpcGasSystemC* 	instance();
  ftpcGasSystem_st 	*Struct(Int_t i = 0) {return ((St_ftpcGasSystem*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	     {return Struct(i)->runNumber;}
  UInt_t 	timeOffset(Int_t i = 0)      {return Struct(i)->timeOffset;}
  Double_t 	westO2ppm(Int_t i = 0) 	     {return Struct(i)->westO2ppm;}
  Double_t 	westO2mv(Int_t i = 0) 	     {return Struct(i)->westO2mv;}
  Double_t 	eastO2ppm(Int_t i = 0) 	     {return Struct(i)->eastO2ppm;}
  Double_t 	eastO2mv(Int_t i = 0) 	     {return Struct(i)->eastO2mv;}
  Double_t 	extO2ppm(Int_t i = 0) 	     {return Struct(i)->extO2ppm;}
  Double_t 	extO2mv(Int_t i = 0) 	     {return Struct(i)->extO2mv;}
  Double_t 	westH2Odp(Int_t i = 0) 	     {return Struct(i)->westH2Odp;}
  Double_t 	eastH2Odp(Int_t i = 0) 	     {return Struct(i)->eastH2Odp;}
  Double_t 	flowAr(Int_t i = 0) 	     {return Struct(i)->flowAr;}
  Double_t 	flowCO2(Int_t i = 0) 	     {return Struct(i)->flowCO2;}
 protected:
  St_ftpcGasSystemC(St_ftpcGasSystem *table=0) : TChair(table) {}
  virtual ~St_ftpcGasSystemC() {fgInstance = 0;}
 private:
  static St_ftpcGasSystemC* fgInstance;
  ClassDefChair(St_ftpcGasSystem, ftpcGasSystem_st )
  ClassDef(St_ftpcGasSystemC,1) //C++ TChair for ftpcGasSystem table class
};
#endif
