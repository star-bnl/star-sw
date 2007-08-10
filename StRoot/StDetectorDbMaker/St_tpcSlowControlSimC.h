#ifndef St_tpcSlowControlSimC_h
#define St_tpcSlowControlSimC_h

#include "TChair.h"
#include "tables/St_tpcSlowControlSim_Table.h"

class St_tpcSlowControlSimC : public TChair {
 public:
  static St_tpcSlowControlSimC* 	instance();
  tpcSlowControlSim_st 	*Struct(Int_t i = 0) 	        {return ((St_tpcSlowControlSim*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	        {return GetNRows();}
  Double_t 	driftVelocity(Int_t i = 0) 	        {return Struct(i)->driftVelocity;}
  Double_t 	driftVoltage(Int_t i = 0) 	        {return Struct(i)->driftVoltage;}
  Double_t 	innerSectorAnodeVoltage(Int_t i = 0) 	{return Struct(i)->innerSectorAnodeVoltage;}
  Double_t 	innerSectorGatingGridV(Int_t i = 0) 	{return Struct(i)->innerSectorGatingGridV;}
  Double_t 	outerSectorAnodeVoltage(Int_t i = 0) 	{return Struct(i)->outerSectorAnodeVoltage;}
  Double_t 	outerSectorGatingGridV(Int_t i = 0) 	{return Struct(i)->outerSectorGatingGridV;}
  Double_t 	innerSectorGasGain(Int_t i = 0) 	{return Struct(i)->innerSectorGasGain;}
  Double_t 	innerSectorGasGainVzero(Int_t i = 0) 	{return Struct(i)->innerSectorGasGainVzero;}
  Double_t 	innerSectorGasGainb(Int_t i = 0) 	{return Struct(i)->innerSectorGasGainb;}
  Double_t 	outerSectorGasGain(Int_t i = 0) 	{return Struct(i)->outerSectorGasGain;}
  Double_t 	outerSectorGasGainVzero(Int_t i = 0) 	{return Struct(i)->outerSectorGasGainVzero;}
  Double_t 	outerSectorGasGainb(Int_t i = 0) 	{return Struct(i)->outerSectorGasGainb;}
  Double_t 	hallPressure(Int_t i = 0) 	        {return Struct(i)->hallPressure;}
  Double_t 	hallTemperature(Int_t i = 0) 	        {return Struct(i)->hallTemperature;}
 protected:
  St_tpcSlowControlSimC(St_tpcSlowControlSim *table=0) : TChair(table) {}
  virtual ~St_tpcSlowControlSimC() {fgInstance = 0;}
 private:
  static St_tpcSlowControlSimC* fgInstance;
  ClassDefChair(St_tpcSlowControlSim, tpcSlowControlSim_st )
  ClassDef(St_tpcSlowControlSimC,1) //C++ TChair for tpcSlowControlSim table class
};
#endif
