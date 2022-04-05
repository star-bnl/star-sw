#ifndef St_ftpcVoltageC_h
#define St_ftpcVoltageC_h

#include "TChair.h"
#include "tables/St_ftpcVoltage_Table.h"

class St_ftpcVoltageC : public TChair {
 public:
  static St_ftpcVoltageC* 	instance();
  ftpcVoltage_st 	*Struct(Int_t i = 0) 	{return ((St_ftpcVoltage*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  Double_t 	cathodeVEast(Int_t i = 0) 	{return Struct(i)->cathodeVEast;}
  Double_t 	cathodeVWest(Int_t i = 0) 	{return Struct(i)->cathodeVWest;}
  Double_t 	anodeV1East(Int_t i = 0) 	{return Struct(i)->anodeV1East;}
  Double_t 	anodeV2East(Int_t i = 0) 	{return Struct(i)->anodeV2East;}
  Double_t 	anodeV3East(Int_t i = 0) 	{return Struct(i)->anodeV3East;}
  Double_t 	anodeV4East(Int_t i = 0) 	{return Struct(i)->anodeV4East;}
  Double_t 	anodeV5East(Int_t i = 0) 	{return Struct(i)->anodeV5East;}
  Double_t 	anodeV6East(Int_t i = 0) 	{return Struct(i)->anodeV6East;}
  Double_t 	anodeV1West(Int_t i = 0) 	{return Struct(i)->anodeV1West;}
  Double_t 	anodeV2West(Int_t i = 0) 	{return Struct(i)->anodeV2West;}
  Double_t 	anodeV3West(Int_t i = 0) 	{return Struct(i)->anodeV3West;}
  Double_t 	anodeV4West(Int_t i = 0) 	{return Struct(i)->anodeV4West;}
  Double_t 	anodeV5West(Int_t i = 0) 	{return Struct(i)->anodeV5West;}
  Double_t 	anodeV6West(Int_t i = 0) 	{return Struct(i)->anodeV6West;}
  Double_t      getCathodeVEast()           	{return cathodeVEast();}
  Double_t      getAnodeV1East()            	{return anodeV1East();}
  Double_t      getAnodeV2East()            	{return anodeV2East();}
  Double_t      getAnodeV3East()            	{return anodeV3East();}
  Double_t      getAnodeV4East()            	{return anodeV4East();}
  Double_t      getAnodeV5East()            	{return anodeV5East();}
  Double_t      getAnodeV6East()            	{return anodeV6East();}
  Double_t      getCathodeVWest()           	{return cathodeVWest();}
  Double_t      getAnodeV1West()            	{return anodeV1West();}
  Double_t      getAnodeV2West()            	{return anodeV2West();}
  Double_t      getAnodeV3West()            	{return anodeV3West();}
  Double_t      getAnodeV4West()            	{return anodeV4West();}
  Double_t      getAnodeV5West()            	{return anodeV5West();}
  Double_t      getAnodeV6West()            	{return anodeV6West();}
 protected:
  St_ftpcVoltageC(St_ftpcVoltage *table=0) : TChair(table) {}
  virtual ~St_ftpcVoltageC() {fgInstance = 0;}
 private:
  static St_ftpcVoltageC* fgInstance;
  ClassDefChair(St_ftpcVoltage, ftpcVoltage_st )
  ClassDef(St_ftpcVoltageC,1) //C++ TChair for ftpcVoltage table class
};
#endif
