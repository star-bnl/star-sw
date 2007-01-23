#include "StEmcPmtSimulator.h"
#include "StEmcUtil/others/emcInternalDef.h"

ClassImp(StEmcPmtSimulator)

StEmcPmtSimulator::StEmcPmtSimulator(UInt_t det=1):StEmcSimpleSimulator(det)
{
    setControlDefault(det);
}
Bool_t StEmcPmtSimulator::setControlDefault(UInt_t det=1)
{
    //
    // Define default value for  simulator
    // Value of sfCoeff see in pams/emc/inc/samplefrac_def.h
    // Also  see pams/emc/kumac/init_ems.kumac.
    // Valid only for BEMC and BPRS
    //
    switch (det)
    {
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
        LOG_ERROR <<"StEmcPmtSimulator::setControlDefault => wrong value of #det "<<mDetector<<endm;
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
    if(mMode<0 || mMode>4)
        mMode = 3;
    if(mMaxEnergy > 0.0)
    {
        mC1 = mMaxAdc  / mMaxEnergy;
        mC2 = mNpheMip / mDepMip;
        mC3 = (mMaxAdc  * mDepMip) / (mMaxEnergy * mNpheMip);
        mC4 = 1;
    }
    if     (mMode==3)
        mVer = 0; // full (slow) simulation;
    else if(mMode==4)
        mVer = 1; // fast (approximate) simulation;
}

void StEmcPmtSimulator::print()
{
    Char_t* cmode[2]={"Slow", "Fast"};
    StEmcSimpleSimulator::print();
    if(mMode>=3)
    {
        LOG_INFO << Form(" Addition info for Pmt Simulator") << endm;
        LOG_INFO << Form("     MIP deposit  energy   for eta      => %7.6f Gev",mDepMip) << endm;
        LOG_INFO << Form("     Number of PHE for MIP for eta      => %7.1f", mNpheMip) << endm;
        LOG_INFO << Form("     Type of PMT                        => %2i --", mTypeOfPmt) << endm;
        LOG_INFO << Form("     Ideal calibration coefficient      => %8.3f", mC1) << endm;
        LOG_INFO << Form("     Number of PHE on one Gev           => %7.1f", mC2) << endm;
        LOG_INFO << Form("     Coefficient for calculation a gain => %7.6f", mC3) << endm;
        LOG_INFO << Form("     Mode of %s simulation", cmode[mVer]) << endm;
    }
}

Int_t StEmcPmtSimulator::getAdc(const Double_t de, const Double_t eta)
{
    static Float_t gain, adcped=0.0, gnoise=0.0; // Rykov's notation
    static Float_t sf, amu;
    static Int_t   nphe;

    mDe = de;
    if     (mMode==0 || mMode==1)
    {
        mAdc = StEmcSimpleSimulator::getAdc(de,eta);
    }
    else if(mMode==2 || mMode==3)
    {
        mSinTheta  = getSinTheta(eta);      // depend from mKeySet
        sf         = sampleFraction(eta);
        amu        = de * mC2;              // Mean value of PHE
        nphe       = mRandom.Poisson(amu);  // Number of primary electrons
        gain       = mC3 * mSinTheta * sf;

        if(mMode == 2)
        {
            mRadc = gain * (Float_t)nphe;
            if(mPedType)
                mRadc += getPedestal(mPedType, mPedMean, mPedRMS);
        }
        else
        {
            if(mPedType)
            {
                adcped = mPedMean;
                gnoise = mPedRMS;
            }
            mPmtSignal.setAllParameters(gain, adcped, gnoise);   // adcped and gnoise equal 0 now
            mRadc = (Float_t)mPmtSignal.getAdc(nphe, mVer);
        }
        checkAdc();
        Float_t ADC = (Float_t) mAdc;
        ADC*= mC4;
        mAdc=(Int_t) ADC; // add gain uncertainty in the simulation. This is not considered when converting to energy
    }
	else {
		LOG_WARN <<"StEmcSimulatorMaker => StEmcPmtSimulator::getAdc => mode is wrong "<<mMode<<endm;
	}
    return mAdc;
}
Float_t  StEmcPmtSimulator::getEnergy()
{
    return StEmcSimpleSimulator::getEnergy();
}
void  StEmcPmtSimulator::setParameters(const Float_t calibCoeff,const UInt_t type, const Float_t pedMean, const Float_t pedRMS, const Float_t gainUnc)
{
    StEmcSimpleSimulator::setParameters(calibCoeff, type, pedMean, pedRMS,gainUnc);
    // see init(); mC1 - defined in simple simulator
    mC2 = mNpheMip / mDepMip;
    mC3 = (mMaxAdc  * mDepMip) / (mMaxEnergy * mNpheMip);
}

//////////////////////////////////////////////////////////////////////////
//  $Id: StEmcPmtSimulator.cxx,v 1.9 2007/01/23 19:44:24 kocolosk Exp $
//  $Log: StEmcPmtSimulator.cxx,v $
//  Revision 1.9  2007/01/23 19:44:24  kocolosk
//  few additional logger fixes
//
//  Revision 1.8  2007/01/22 19:13:39  kocolosk
//  use STAR logger for all output
//
//  Revision 1.7  2005/03/21 21:36:38  suaide
//  fixed problem with chain
//
//  Revision 1.6  2004/08/06 13:24:47  suaide
//  New features added and fixed some bugs in the database
//
//  Revision 1.5  2003/09/23 15:19:43  suaide
//  fixed bugs and modifications for embedding
//
//  Revision 1.4  2003/01/23 03:09:02  jeromel
//  Include modif
//
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
