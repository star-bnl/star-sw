#ifndef St_tpcGasC_h
#define St_tpcGasC_h

#include "TChair.h"
#include "tables/St_tpcGas_Table.h"

class St_tpcGasC : public TChair {
 public:
  static St_tpcGasC* 	instance();
  tpcGas_st 	*Struct(Int_t i = 0) 	        {return ((St_tpcGas*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t 	barometricPressure(Int_t i = 0) {return Struct(i)->barometricPressure;}
  Float_t 	inputTPCGasPressure(Int_t i = 0){return Struct(i)->inputTPCGasPressure;}
  Float_t 	nitrogenPressure(Int_t i = 0) 	{return Struct(i)->nitrogenPressure;}
  Float_t 	gasPressureDiff(Int_t i = 0) 	{return Struct(i)->gasPressureDiff;}
  Float_t 	inputGasTemperature(Int_t i = 0){return Struct(i)->inputGasTemperature;}
  Float_t 	outputGasTemperature(Int_t i =0){return Struct(i)->outputGasTemperature;}
  Float_t 	flowRateArgon1(Int_t i = 0) 	{return Struct(i)->flowRateArgon1;}
  Float_t 	flowRateArgon2(Int_t i = 0) 	{return Struct(i)->flowRateArgon2;}
  Float_t 	flowRateMethane(Int_t i = 0) 	{return Struct(i)->flowRateMethane;}
  Float_t 	percentMethaneIn(Int_t i = 0) 	{return Struct(i)->percentMethaneIn;}
  Float_t 	ppmOxygenIn(Int_t i = 0) 	{return Struct(i)->ppmOxygenIn;}
  Float_t 	flowRateExhaust(Int_t i = 0) 	{return Struct(i)->flowRateExhaust;}
  Float_t 	percentMethaneOut(Int_t i = 0) 	{return Struct(i)->percentMethaneOut;}
  Float_t 	ppmWaterOut(Int_t i = 0) 	{return Struct(i)->ppmWaterOut;}
  Float_t 	ppmOxygenOut(Int_t i = 0) 	{return Struct(i)->ppmOxygenOut;}
  Float_t 	flowRateRecirculation(Int_t i=0){return Struct(i)->flowRateRecirculation;}
 protected:
  St_tpcGasC(St_tpcGas *table=0) : TChair(table) {}
  virtual ~St_tpcGasC() {fgInstance = 0;}
 private:
  static St_tpcGasC* fgInstance;
  ClassDefChair(St_tpcGas, tpcGas_st )
  ClassDef(St_tpcGasC,1) //C++ TChair for tpcGas table class
};
#endif
