#include "StEmcSimpleSimulator.h"
#include "../StEmcUtil/emcInternalDef.h"

ClassImp(StEmcSimpleSimulator)

StEmcSimpleSimulator::StEmcSimpleSimulator(UInt_t det=1):StEmcVirtualSimulator()
{ setControlDefault(det); }

void StEmcSimpleSimulator::setControlDefault(UInt_t det=1)
{
  //
  // Define default value for Simple simulator
  // Value of sfCoeff see in pams/emc/inc/samplefrac_def.h
  // Also  see pams/emc/kumac/init_ems.kumac
  // 
  mDetector     = det;
  mControl.Set(1);
  controlEmcPmtSimulator_st controlW;

  controlW.mode = 1;
  switch (det){
    case BEMC:
      controlW.maxAdc     = 3500;   // 12 bit (max 4096)
      controlW.maxEnergy  = 60.0;   // in GeV
      controlW.sfCoeff[0] = 14.69;
      controlW.sfCoeff[1] = -0.1022;
      controlW.sfCoeff[2] = 0.7484;
      break;
    case BPRS:
      controlW.maxAdc     = 220;    // 8 bit (max 256)
      controlW.maxEnergy  = 1.0;    // in 
      controlW.sfCoeff[0] = 14.69;  // The same as for BEMC
      controlW.sfCoeff[1] = -0.1022;
      controlW.sfCoeff[2] = 0.7484;
      break;
    case BSMDE:
      controlW.maxAdc     = 900;    // 10 bit (max 1024)
      controlW.maxEnergy  = 25.0;   // in GeV
      controlW.sfCoeff[0] = 0.1185e+6;
      controlW.sfCoeff[1] = -0.3292e+5;
      controlW.sfCoeff[2] = 0.3113e+5;
      break;
    case BSMDP:
      controlW.maxAdc     = 900;    // 10 bit (max 1024)
      controlW.maxEnergy  = 25.0;   // in GeV
      controlW.sfCoeff[0] = 0.1260e+6;
      controlW.sfCoeff[1] = -0.1395e+5;
      controlW.sfCoeff[2] = 0.1971e+5;
      break;
    default:
      printf("<W> Wrong value of #det %i \n", mDetector);
  }
  mControl.AddAt(&controlW,0);
  init();
}

void StEmcSimpleSimulator::setControl(controlEmcPmtSimulator_st* var) {
  if(var) {
    mControl.AddAt(var,0);
    init();
  }
}

void StEmcSimpleSimulator::init()
{
  //
  // mC1 - coefficient for transition from energy to adc
  //       (reverse for calibration coefficient).
  //
  if(mControl[0].maxEnergy > 0.0){
    mC1 = (Double_t)mControl[0].maxAdc / mControl[0].maxEnergy;
  }
}

void StEmcSimpleSimulator::print()
{
  printf(" <I> Simple Simulator for detector %i \n", mDetector);
  printf("     Max Adc     %d \n", mControl[0].maxAdc);
  printf("     Max Energy  %5.1fGev for eta = 0 \n", mControl[0].maxEnergy);
  printf("     sample fraction function => %10.2f  %10.2f*x + %10.2f*x*x\n",
  mControl[0].sfCoeff[0], mControl[0].sfCoeff[1], mControl[0].sfCoeff[2]);
}

Double_t StEmcSimpleSimulator::sampleFraction(const Double_t eta)
{
  //
  // See  pams/emc/util/samplefraction.c
  // 
  Double_t x = fabs(eta);
  return mControl[0].sfCoeff[0]+mControl[0].sfCoeff[1]*x+mControl[0].sfCoeff[2]*x*x;
}

void StEmcSimpleSimulator::checkAdc()
{
  mAdc  = (Int_t)mRadc;
  mAdc  = (mAdc>0) ? mAdc : 0;
  mAdc  = (mAdc<mControl[0].maxAdc) ? mAdc: mControl[0].maxAdc;
}

Int_t StEmcSimpleSimulator::getAdc(const Double_t de, const Double_t eta)
{
  mDe = de;
  switch (mControl[0].mode){
  case 0:
    mAdc = -999; // No transition; keep deposit energy for energy;
    break;
  case 1:
  // 1./cosh(eta) = sin(theta)
    mSinTheta  = 1./TMath::CosH(eta);
    mRadc = de*sampleFraction(eta)*mSinTheta*mC1;
    checkAdc();
    break;
  }
  return mAdc;
}

Float_t StEmcSimpleSimulator::getEnergy()
{
  //
  // Calculate energy for ideal calibration.
  //
  static Float_t e;

  if(mControl[0].mode == 0) e = (Float_t)mDe;
  else                      e = (Float_t)((Double_t)mAdc/(mC1*mSinTheta)); 

  return e;
}
//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcSimpleSimulator.cxx,v 1.1 2000/10/23 22:53:14 pavlinov Exp $
//  $Log: StEmcSimpleSimulator.cxx,v $
//  Revision 1.1  2000/10/23 22:53:14  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////
