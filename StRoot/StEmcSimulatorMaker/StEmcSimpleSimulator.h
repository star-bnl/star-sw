//////////////////////////////////////////////////////////////////////////
//
// StEmcSimpleSimulator
//
// This class provides simple transition from deposit energy to 
// ADC using ADC's scale, energy scale and sample fraction
// function. It is correct for case when the main fluctuations 
// come from shower fluctuations.
// 
// mode = 0; Testing mode. Keep trace the deposit energy instead 
//           of energy.
// mode = 1; Simple transition.
//
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StEmcSimpleSimulator
#define STAR_StEmcSimpleSimulator

#include "StEmcVirtualSimulator.h"
#include "tables/St_controlEmcPmtSimulator_Table.h"

class StEmcSimpleSimulator : public StEmcVirtualSimulator{
private:
  
protected:
  UInt_t                       mDetector;
  St_controlEmcPmtSimulator    mControl;

  Int_t    mKeySet;
  // for transition from energy to adc and vise versa
  Int_t    mMode;
  Int_t    mMaxAdc;
  Double_t mMaxEnergy;
  Double_t mC1;            // reverse for calibration coefficient
  // for pedestal
  UInt_t   mPedType;
  Double_t mPedMean;
  Double_t mPedRMS;
  // sampling fraction coefficient
  Double_t mSF[3];
  // Working variable
  Double_t mSinTheta;
  Double_t mDe;
  Double_t mRadc;
  Int_t    mAdc;
  
  Bool_t   mPrint;

  void  checkAdc();

public: 
  StEmcSimpleSimulator(UInt_t det);
  virtual ~StEmcSimpleSimulator() {/* nothing */};
  void setControlDefault(UInt_t det);
  void setControl(controlEmcPmtSimulator_st* var);
  void setDetector(UInt_t var) {mDetector = var;}

  Double_t sampleFraction(const Double_t);

  St_controlEmcPmtSimulator* getControl()  {return &mControl;}
  UInt_t                     getDetector() {return mDetector;}

  virtual void    init();
  virtual void    setPedestal(const UInt_t type, const Float_t pedMean, const Float_t pedRMS);
  virtual void    setParameters(const Float_t calibCoeff,const UInt_t type, const Float_t pedMean, const Float_t pedRMS);  

  Int_t   getPedestalType() {return mPedType;}

  virtual Double_t getPedestal(const Int_t type, const Double_t pedMean, const Double_t pedRMS);
  virtual Double_t deductPedestal(const Int_t type, const Int_t adc, const Double_t pedMean);
  virtual Int_t    getAdc(const Double_t de, const Double_t eta);
  virtual Float_t  getEnergy();
  virtual void    print();
  void            setPrint(Bool_t a) { mPrint = a;}

  Double_t getSinTheta(Double_t eta);

  ClassDef(StEmcSimpleSimulator, 1)   // Simple Emc simulator
};
#endif
//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcSimpleSimulator.h,v 1.3 2003/09/23 15:19:51 suaide Exp $
//  $Log: StEmcSimpleSimulator.h,v $
//  Revision 1.3  2003/09/23 15:19:51  suaide
//  fixed bugs and modifications for embedding
//
//  Revision 1.2  2002/06/04 16:09:36  pavlinov
//  added option with DB(pedestal ans calibration  coefficients
//
//  Revision 1.1  2000/10/23 22:53:14  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////

