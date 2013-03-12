#ifndef St_tpcElectronicsC_h
#define St_tpcElectronicsC_h

#include "TChair.h"
#include "tables/St_tpcElectronics_Table.h"
#include "St_starClockOnlC.h"
class St_tpcElectronicsC : public TChair {
 public:
  static St_tpcElectronicsC* 	instance();
  tpcElectronics_st 	*Struct(Int_t i = 0) 	{return ((St_tpcElectronics*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	numberOfTimeBins(Int_t i = 0) 	{return Struct(i)->numberOfTimeBins;}
  Double_t 	nominalGain(Int_t i = 0) 	{return Struct(i)->nominalGain;}
  //  Double_t 	samplingFrequency(Int_t i = 0) 	{return Struct(i)->samplingFrequency;}  obsolete
  Double_t      samplingFrequency(Int_t i = 0) {return 1e-6*St_starClockOnlC::instance()->CurrentFrequency(i);}
  Double_t 	tZero(Int_t i = 0) 	        {return Struct(i)->tZero;}
  Double_t 	adcCharge(Int_t i = 0) 	        {return Struct(i)->adcCharge;}
  Double_t 	adcConversion(Int_t i = 0) 	{return Struct(i)->adcConversion;}
  Double_t 	averagePedestal(Int_t i = 0) 	{return Struct(i)->averagePedestal;}
  Double_t 	shapingTime(Int_t i = 0) 	{return Struct(i)->shapingTime;}
  Double_t 	tau(Int_t i = 0) 	        {return Struct(i)->tau;}
 protected:
  St_tpcElectronicsC(St_tpcElectronics *table=0) : TChair(table) {}
  virtual ~St_tpcElectronicsC() {fgInstance = 0;}
 private:
  static St_tpcElectronicsC* fgInstance;
  ClassDefChair(St_tpcElectronics, tpcElectronics_st )
  ClassDef(St_tpcElectronicsC,1) //C++ TChair for tpcElectronics table class
};
#endif
