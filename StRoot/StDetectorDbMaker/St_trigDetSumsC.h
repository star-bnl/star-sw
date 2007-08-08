#ifndef St_trigDetSumsC_h
#define St_trigDetSumsC_h

#include "TChair.h"
#include "tables/St_trigDetSums_Table.h"

class St_trigDetSumsC : public TChair {
 public:
  St_trigDetSumsC(St_trigDetSums *table) : TChair(table) {SafeDelete(fgInstance); fgInstance = this;}
  virtual ~St_trigDetSumsC() {fgInstance = 0;}
  static St_trigDetSumsC* 	instance()      {return fgInstance;}
  trigDetSums_st 	*Struct(Int_t i = 0) 	{return ((St_trigDetSums*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	timeOffset(Int_t i = 0) 	{return Struct(i)->timeOffset;}
  Double_t 	ctbWest(Int_t i = 0) 	        {return Struct(i)->ctbWest;}
  Double_t 	ctbEast(Int_t i = 0) 	        {return Struct(i)->ctbEast;}
  Double_t 	ctbTOFp(Int_t i = 0) 	        {return Struct(i)->ctbTOFp;}
  Double_t 	tofp(Int_t i = 0) 	        {return Struct(i)->tofp;}
  Double_t 	zdcWest(Int_t i = 0) 	        {return Struct(i)->zdcWest;}
  Double_t 	zdcEast(Int_t i = 0) 	        {return Struct(i)->zdcEast;}
  Double_t 	zdcX(Int_t i = 0) 	        {return Struct(i)->zdcX;}
  Double_t 	mult(Int_t i = 0) 	        {return Struct(i)->mult;}
  Double_t 	L0(Int_t i = 0) 	        {return Struct(i)->L0;}
  Double_t 	bbcX(Int_t i = 0) 	        {return Struct(i)->bbcX;}
  Double_t 	bbcXctbTOFp(Int_t i = 0) 	{return Struct(i)->bbcXctbTOFp;}
  Double_t 	bbcWest(Int_t i = 0) 	        {return Struct(i)->bbcWest;}
  Double_t 	bbcEast(Int_t i = 0) 	        {return Struct(i)->bbcEast;}
  Double_t 	bbcYellowBkg(Int_t i = 0) 	{return Struct(i)->bbcYellowBkg;}
  Double_t 	bbcBlueBkg(Int_t i = 0) 	{return Struct(i)->bbcBlueBkg;}
  Double_t 	pvpdWest(Int_t i = 0) 	        {return Struct(i)->pvpdWest;}
  Double_t 	pvpdEast(Int_t i = 0) 	        {return Struct(i)->pvpdEast;}
 protected:
 private:
  static St_trigDetSumsC* fgInstance;
  ClassDefChair(St_trigDetSums, trigDetSums_st )
  ClassDef(St_trigDetSumsC,1) //C++ TChair for trigDetSums table class
};
#endif
