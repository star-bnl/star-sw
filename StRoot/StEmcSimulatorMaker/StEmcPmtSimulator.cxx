#include "StEmcPmtSimulator.h"
#include "../StEmcUtil/emcInternalDef.h"
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
  init();
  return kTRUE;
}

void StEmcPmtSimulator::init()
{
  //
  // If mode has wrong value then switch to full simulation.
  //
  if(mControl[0].mode<0 || mControl[0].mode>4) mControl[0].mode=3;
  if(mControl[0].maxEnergy > 0.0){
    mC1 = (Double_t)mControl[0].maxAdc / mControl[0].maxEnergy;
    mC2 = (Double_t)(mControl[0].npheMip / mControl[0].depMip);
    mC3 = (Double_t)((mControl[0].maxAdc    * mControl[0].depMip)/
                     (mControl[0].maxEnergy * mControl[0].npheMip));
  }
  if     (mControl[0].mode==3) mVer = 0; // full (slow) simulation;
  else if(mControl[0].mode==4) mVer = 1; // fast (approximate) simulation;
}

void StEmcPmtSimulator::print()
{
  Char_t* cmode[2]={"Slow", "Fast"};

  StEmcSimpleSimulator::print();

  if(mControl[0].mode>=3){
    printf(" <I>  Addition infor for Pmt Simulator\n");
    printf("     MIP deposit  energy   for eta      => %7.6f Gev\n",mControl[0].depMip);
    printf("     Number of PHE for MIP for eta      => %7.1f\n",mControl[0].npheMip);
    printf("     Type of PMT                        => %2i\n --\n",mControl[0].typeOfPmt);
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
  if     (mControl[0].mode==0 || mControl[0].mode==1){
    mAdc = StEmcSimpleSimulator::getAdc(de,eta);
  }
  else if(mControl[0].mode==2 || mControl[0].mode==3){
  // 1./cosh(eta) = sin(theta)
    mSinTheta  = 1./TMath::CosH(eta);
    sf         = sampleFraction(eta);
    amu        = de * mC2;              // Mean value of PHE
    nphe       = mRandom.Poisson(amu);  // Number of primary electrons
    gain       = mC3 * mSinTheta * sf;  // Must be check  !!

    if(mControl[0].mode == 2){
      mRadc = gain * (Float_t)nphe;
    }
    else{ // mode==3
      mPmtSignal.setAllParameters(gain, adcped, gnoise);   // adcped and gnoise equal 0 now 
      mRadc = (Float_t)mPmtSignal.getAdc(nphe, mVer);
    }
    checkAdc();
  }
  else gMessMgr->Warning()<<"StEmcSimulatorMaker => StEmcPmtSimulator::getAdc => mode is wrong "
			  <<mControl[0].mode<<endm;

  return mAdc;
}

//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcPmtSimulator.cxx,v 1.1 2000/10/23 22:53:13 pavlinov Exp $
//  $Log: StEmcPmtSimulator.cxx,v $
//  Revision 1.1  2000/10/23 22:53:13  pavlinov
//  First working C++ version
//
//////////////////////////////////////////////////////////////////////////
