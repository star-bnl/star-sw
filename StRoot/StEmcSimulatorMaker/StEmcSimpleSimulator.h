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

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif
#include "TMath.h"

#include "StEmcVirtualSimulator.h"
#include "tables/St_controlEmcPmtSimulator_Table.h"

class StEmcSimpleSimulator : public StEmcVirtualSimulator{
private:
protected:
  UInt_t                       mDetector;
  St_controlEmcPmtSimulator    mControl;
  // Working variable
  Double_t mC1;
  Double_t mSinTheta;
  Double_t mDe;
  Double_t mRadc;
  Int_t    mAdc;

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
  virtual Int_t   getAdc(const Double_t, const Double_t);
  virtual Float_t getEnergy();
  virtual void    print();

  ClassDef(StEmcSimpleSimulator, 1)   // Simple Emc simulator
};
#endif
//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcSimpleSimulator.h,v 1.1 2000/10/23 22:53:14 pavlinov Exp $
//  $Log: StEmcSimpleSimulator.h,v $
//  Revision 1.1  2000/10/23 22:53:14  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////

