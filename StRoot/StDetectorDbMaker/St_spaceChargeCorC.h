#ifndef St_spaceChargeCorC_h
#define St_spaceChargeCorC_h

#include "TChair.h"
#include "tables/St_spaceChargeCor_Table.h"
#include "StDetectorDbRichScalers.h"
#include "StDetectorDbMagnet.h"
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
  Double_t      getSpaceChargeCorrection(Double_t scaleFactor){
    Double_t value = 0;
    if(scaleFactor < -.75 && scaleFactor > -1.25) value = fullFieldB();
    else if(scaleFactor < -0.25)	          value = halfFieldB();
    else if(scaleFactor < .25)	                  value = zeroField();
    else if(scaleFactor < 0.75)	                  value = halfFieldA();
    else if(scaleFactor < 1.25)	                  value = fullFieldA();
    return value;
  }
  Double_t getSpaceChargeCorrection(){return  getSpaceChargeCorrection(StDetectorDbMagnet::instance()->getScaleFactor());}
  Double_t getSpaceChargeCoulombs(Double_t scaleFactor){
    StDetectorDbRichScalers* scalers = StDetectorDbRichScalers::instance();
    Double_t mult = 0;
    if (! scalers ) return mult;
    switch ((int) this->getSpaceChargeDetector()) {
    case (0) : mult = scalers->getMult(); break;
    case (1) : mult = scalers->getBBCX(); break;
    case (2) : mult = scalers->getZDCX(); break;
    case (3) : mult = scalers->getZDCEast()+scalers->getZDCWest(); break;
    case (4) : mult = scalers->getBBCEast()+scalers->getBBCWest(); break;
    default  : mult = 0.;
    }
    Double_t saturation = getSpaceChargeSatRate();
    Double_t correction = getSpaceChargeCorrection(scaleFactor);
    Double_t factor     = getSpaceChargeFactor();
    Double_t offset     = getSpaceChargeOffset();
    Double_t intens = (mult < saturation) ? mult : saturation;
    return factor * (intens-offset) * correction ;
  };
  Double_t getSpaceChargeCoulombs(){return getSpaceChargeCoulombs(StDetectorDbMagnet::instance()->getScaleFactor());}
  Double_t getSpaceChargeSatRate() {return satRate();}
  Float_t  getSpaceChargeFactor()  {return factor();}
  Float_t  getSpaceChargeDetector(){return detector();}
  Float_t  getSpaceChargeOffset()  {return offset();}

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
  virtual ~St_spaceChargeCorR1C() {SafeDelete(fgInstance);}
 private:
  static St_spaceChargeCorR1C* fgInstance;
  ClassDef(St_spaceChargeCorR1C,1) //C++ TChair for spaceChargeCor table class
};
class St_spaceChargeCorR2C : public St_spaceChargeCorC {
 public:
  static St_spaceChargeCorR2C* 	instance();
 protected:
  St_spaceChargeCorR2C(St_spaceChargeCor *table=0) : St_spaceChargeCorC(table) {}
  virtual ~St_spaceChargeCorR2C() {SafeDelete(fgInstance);}
 private:
  static St_spaceChargeCorR2C* fgInstance;
  ClassDef(St_spaceChargeCorR2C,1) //C++ TChair for spaceChargeCor table class
};
#endif
