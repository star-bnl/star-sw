#include "StEmcPmtSimulator.h"
#include "StEmcUtil/emcInternalDef.h"
#include "StMessMgr.h"

ClassImp(StEmcPmtSimulator)

StEmcPmtSimulator::StEmcPmtSimulator(UInt_t det=1):StEmcSimpleSimulator(det)
{ setControlDefault(det); }

Bool_t StEmcPmtSimulator::setControlDefault(UInt_t det=1)
{
  //
  // Define default value for  simulator
  // Value of sfCoeff see in pams/emc/inc/samplefrac_def.h
  // Also  see pams/emc/kumac/init_ems.kumac.
  // Valid only for BEMC and BPRS
  // 
  switch (det){
    case BEMC:
      mControl[0].mode       = 3;
      mControl[0].typeOfPmt  = 1;
      mControl[0].npheMip    = 63.;
      mControl[0].depMip     = 0.0198; // in GeV       
      break;
    case BPRS:
      mControl[0].mode       = 3;
      mControl[0].typeOfPmt  = 1;
      mControl[0].npheMip    = 6.;
      mControl[0].depMip     = 0.002;  // in GeV       
      break;
    default:
      gMessMgr->Error()<<"StEmcPmtSimulator::setControlDefault => wrong value of #det "
                       <<mDetector<<endm;
      return kFALSE;
  }

  mMode      = mControl[0].mode;
  mTypeOfPmt = mControl[0].typeOfPmt;
  mNpheMip   = mControl[0].npheMip;
  mDepMip    = mControl[0].depMip;

  init();

  return kTRUE;
}

void StEmcPmtSimulator::init()
{
  //
  // If mode has wrong value then switch to full simulation.
  //
  if(mMode<0 || mMode>4) mMode=3;
  if(mMaxEnergy > 0.0){
    mC1 = mMaxAdc  / mMaxEnergy;
    mC2 = mNpheMip / mDepMip;
    mC3 = (mMaxAdc  * mDepMip) / (mMaxEnergy * mNpheMip);
  }
  if     (mMode==3) mVer = 0; // full (slow) simulation;
  else if(mMode==4) mVer = 1; // fast (approximate) simulation;
}

void StEmcPmtSimulator::print()
{
  Char_t* cmode[2]={"Slow", "Fast"};

  StEmcSimpleSimulator::print();

  if(mMode>=3){
    printf(" <I>  Addition infor for Pmt Simulator\n");
    printf("     MIP deposit  energy   for eta      => %7.6f Gev\n",mDepMip);
    printf("     Number of PHE for MIP for eta      => %7.1f\n", mNpheMip);
    printf("     Type of PMT                        => %2i\n --\n", mTypeOfPmt);
    printf("     Ideal calibration coefficient      => %8.3f\n", mC1);
    printf("     Number of PHE on one Gev           => %7.1f\n", mC2); 
    printf("     Coefficient for calculation a gain => %7.6f\n", mC3); 
    printf("     Mode of %s simulation \n", cmode[mVer]);
  }
}

Int_t StEmcPmtSimulator::getAdc(const Double_t de, const Double_t eta)
{
  static Float_t gain, adcped=0.0, gnoise=0.0; // Rykov's notation 
  static Float_t sf, amu;
  static Int_t   nphe;

  mDe = de;
  if     (mMode==0 || mMode==1){
    mAdc = StEmcSimpleSimulator::getAdc(de,eta);
  } else if(mMode==2 || mMode==3){
  // 1./cosh(eta) = sin(theta)
     mSinTheta  = getSinTheta(eta);      // depend from mKeySet
     sf         = sampleFraction(eta);
     amu        = de * mC2;              // Mean value of PHE
     nphe       = mRandom.Poisson(amu);  // Number of primary electrons
     gain       = mC3 * mSinTheta * sf;

     if(mMode == 2){
        mRadc = gain * (Float_t)nphe;
        if(mPedType) mRadc += getPedestal(mPedType, mPedMean, mPedRMS);
     } else { // mode==3
        if(mPedType) {
	   adcped = mPedMean;
           gnoise = mPedRMS;
        } 
        mPmtSignal.setAllParameters(gain, adcped, gnoise);   // adcped and gnoise equal 0 now 
        mRadc = (Float_t)mPmtSignal.getAdc(nphe, mVer);
     }
     checkAdc();
  } else gMessMgr->Warning()<<"StEmcSimulatorMaker => StEmcPmtSimulator::getAdc => mode is wrong "
			  <<mMode<<endm;

  return mAdc;
}

Float_t  StEmcPmtSimulator::getEnergy()
{
  return StEmcSimpleSimulator::getEnergy();
}

void  StEmcPmtSimulator::setParameters
(const Float_t calibCoeff,const UInt_t type, const Float_t pedMean, const Float_t pedRMS)
{
  StEmcSimpleSimulator::setParameters(calibCoeff, type, pedMean, pedRMS);
  // see init(); mC1 - defined in simple simulator
  mC2 = mNpheMip / mDepMip;
  mC3 = (mMaxAdc  * mDepMip) / (mMaxEnergy * mNpheMip);
}


//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcPmtSimulator.cxx,v 1.3 2002/06/04 16:09:34 pavlinov Exp $
//  $Log: StEmcPmtSimulator.cxx,v $
//  Revision 1.3  2002/06/04 16:09:34  pavlinov
//  added option with DB(pedestal ans calibration  coefficients
//
//  Revision 1.2  2001/05/14 01:30:13  pavlinov
//  Cleanup
//
//  Revision 1.1  2000/10/23 22:53:13  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////
