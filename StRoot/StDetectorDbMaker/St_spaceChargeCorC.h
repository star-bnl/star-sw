#ifndef St_spaceChargeCorC_h
#define St_spaceChargeCorC_h

#include "TChair.h"
#include "tables/St_spaceChargeCor_Table.h"
#include "StDetectorDbMagnet.h"
#include "StChain/StChain.h"

class St_spaceChargeCorC : public TChair {
 public:
  spaceChargeCor_st 	*Struct(Int_t i = 0) 	{return ((St_spaceChargeCor*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Double_t 	fullFieldB(Int_t i = 0) 	{return Struct(i)->fullFieldB;}
  Double_t 	halfFieldB(Int_t i = 0) 	{return Struct(i)->halfFieldB;}
  Double_t 	zeroField(Int_t i = 0) 	        {return Struct(i)->zeroField;}
  Double_t 	halfFieldA(Int_t i = 0) 	{return Struct(i)->halfFieldA;}
  Double_t 	fullFieldA(Int_t i = 0) 	{return Struct(i)->fullFieldA;}
  Double_t 	satRate(Int_t i = 0) 	        {return Struct(i)->satRate;}
  Float_t 	factor(Int_t i = 0) 	        {return Struct(i)->factor;}
  Float_t 	detector(Int_t i = 0) 	        {return Struct(i)->detector;}
  Float_t 	offset(Int_t i = 0) 	        {return Struct(i)->offset;}
  Float_t 	getEWRatio(Int_t i = 0)	        {return Struct(i)->ewratio;}
  Double_t      getSpaceChargeCorrection(Double_t scaleFactor, Int_t i = 0){
    Double_t value = 0;
    if(scaleFactor < -.75 && scaleFactor > -1.25) value = fullFieldB(i);
    else if(scaleFactor < -0.25)	          value = halfFieldB(i);
    else if(scaleFactor < .25)	                  value = zeroField(i);
    else if(scaleFactor < 0.75)	                  value = halfFieldA(i);
    else if(scaleFactor < 1.25)	                  value = fullFieldA(i);
    return value;
  }
  Double_t getSpaceChargeCorrection(){return  getSpaceChargeCorrection(StDetectorDbMagnet::instance()->getScaleFactor());}
  Double_t getSpaceChargeCoulombs(Double_t scaleFactor);
  Double_t getSpaceChargeCoulombs(){return getSpaceChargeCoulombs(StDetectorDbMagnet::instance()->getScaleFactor());}
  Double_t getSpaceChargeSatRate(Int_t i = 0) {return satRate(i);}
  Float_t  getSpaceChargeFactor(Int_t i = 0)  {return factor(i);}
  Float_t  getSpaceChargeDetector(Int_t i = 0){return detector(i);}
  Float_t  getSpaceChargeOffset(Int_t i = 0)  {return offset(i);}
  TString  getSpaceChargeString(Double_t scaleFactor);
  TString  getSpaceChargeString(){return getSpaceChargeString(StDetectorDbMagnet::instance()->getScaleFactor());}

 protected:
  St_spaceChargeCorC(St_spaceChargeCor *table=0) : TChair(table) {}
  virtual ~St_spaceChargeCorC() {}
 private:
  ClassDefChair(St_spaceChargeCor, spaceChargeCor_st )
  ClassDef(St_spaceChargeCorC,1) //C++ TChair for spaceChargeCor table class
};

class St_spaceChargeCorR1C : public St_spaceChargeCorC {
 public:
  static St_spaceChargeCorR1C* 	instance();
 protected:
  St_spaceChargeCorR1C(St_spaceChargeCor *table=0) : St_spaceChargeCorC(table) {}
  virtual ~St_spaceChargeCorR1C() {fgInstance = 0;}
 private:
  static St_spaceChargeCorR1C* fgInstance;
  ClassDef(St_spaceChargeCorR1C,1) //C++ TChair for spaceChargeCor table class
};
class St_spaceChargeCorR2C : public St_spaceChargeCorC {
 public:
  static St_spaceChargeCorR2C* 	instance();
 protected:
  St_spaceChargeCorR2C(St_spaceChargeCor *table=0) : St_spaceChargeCorC(table) {}
  virtual ~St_spaceChargeCorR2C() {fgInstance = 0;}
 private:
  static St_spaceChargeCorR2C* fgInstance;
  ClassDef(St_spaceChargeCorR2C,1) //C++ TChair for spaceChargeCor table class
};
#endif
