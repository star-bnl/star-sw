#ifndef St_ftpcGasOutC_h
#define St_ftpcGasOutC_h

#include "TChair.h"
#include "tables/St_ftpcGasOut_Table.h"

class St_ftpcGasOutC : public TChair {
 public:
  static St_ftpcGasOutC* 	instance();
  ftpcGasOut_st *Struct(Int_t i = 0) 	{return ((St_ftpcGasOut*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            {return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	{return Struct(i)->runNumber;}
  Double_t 	gasOutEast(Int_t i = 0) {return Struct(i)->gasOutEast;}
  Double_t 	gasOutWest(Int_t i = 0) {return Struct(i)->gasOutWest;}
  Double_t 	body1East(Int_t i = 0) 	{return Struct(i)->body1East;}
  Double_t 	body1West(Int_t i = 0) 	{return Struct(i)->body1West;}
  Double_t 	body2East(Int_t i = 0) 	{return Struct(i)->body2East;}
  Double_t 	body2West(Int_t i = 0) 	{return Struct(i)->body2West;}
  Double_t 	body3East(Int_t i = 0) 	{return Struct(i)->body3East;}
  Double_t 	body3West(Int_t i = 0) 	{return Struct(i)->body3West;}
  Double_t 	body4East(Int_t i = 0) 	{return Struct(i)->body4East;}
  Double_t 	body4West(Int_t i = 0) 	{return Struct(i)->body4West;}
  Double_t 	body5East(Int_t i = 0) 	{return Struct(i)->body5East;}
  Double_t 	body5West(Int_t i = 0) 	{return Struct(i)->body5West;}
  Double_t 	body6East(Int_t i = 0) 	{return Struct(i)->body6East;}
  Double_t 	body6West(Int_t i = 0) 	{return Struct(i)->body6West;}
 protected:
  St_ftpcGasOutC(St_ftpcGasOut *table=0) : TChair(table) {}
  virtual ~St_ftpcGasOutC() {fgInstance = 0;}
 private:
  static St_ftpcGasOutC* fgInstance;
  ClassDefChair(St_ftpcGasOut, ftpcGasOut_st )
  ClassDef(St_ftpcGasOutC,1) //C++ TChair for ftpcGasOut table class
};
#endif
