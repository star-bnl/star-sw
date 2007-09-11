// $Id: StPmtSignal.cxx,v 1.5 2007/09/11 21:49:15 kocolosk Exp $

#include "StPmtSignal.h"

#include "TMath.h"

#include "StMessMgr.h"

ClassImp(StPmtSignal)

StPmtSignal::StPmtSignal(float pmtGain, float cathodeNoise, float dynodeNoise) :
    TNamed("HAMAMATSU-R6427", "Bases used in Beam test-98"), 
    mPmtGain(pmtGain), mCathodeNoise(cathodeNoise), mDynodeNoise(dynodeNoise) {
    
    float tmpArray[mNumDynodes] = {2.,2.,1.,1.,1.,1.,1.,1., 2., 3., 4.};
    for(int i=0; i<mNumDynodes; i++) mNodeVoltage[i] = tmpArray[i];
    
    //calculate secondary electron conversion coefficients
    float voltageProduct = 1.0;
    for(int i=0; i<mNumDynodes; i++) {
        voltageProduct *= mNodeVoltage[i];
    }
    float globalCoeff = TMath::Power( double(mPmtGain/voltageProduct), 1.0/(mNumDynodes) );
    for(int i=0; i<mNumDynodes; i++) {
        mSecondaryCoeff[i] = globalCoeff * mNodeVoltage[i];
    }
    
    //calculate inverted partial gains after each dynode
    mDynodeGain[0] = 1.0;
    for(int i=0; i<mNumDynodes; i++) {
        mDynodeGain[i+1] = mDynodeGain[i] / mSecondaryCoeff[i];
    }
    
    //calculate g1=G-1 constants (see SN301)
    mG1[mNumDynodes] = 0.0;
    for(int i=mNumDynodes; i>0; i--) {
        mG1[i-1] = (1.0 + mG1[i]) / mSecondaryCoeff[i-1];
    }
    
    //only for fast simulator
    mDNW[mNumDynodes] = 0.0;
    for(int i=mNumDynodes; i>0; i--) {
        mDNW[i-1] = mDynodeNoise * (1.0+mG1[i]) * mDynodeGain[i] * mDynodeGain[i] + mDNW[i];
    }
    
    LOG_DEBUG << "StPmtSignal properties for " << this->GetName() << " and " << this->GetTitle() << endm;
    LOG_DEBUG << "nDynodes = " << mNumDynodes << endm;
    LOG_DEBUG << "cathode noise = " << mCathodeNoise << endm;
    LOG_DEBUG << "dynode noise = " << mDynodeNoise << endm;
    LOG_DEBUG << "approximate PMT gain = " << mPmtGain << endm;
}

int StPmtSignal::getAdc(int nPhotoElectrons, simulatorVersion version) {
    double nElectrons = nPhotoElectrons + mRandom.PoissonD(mCathodeNoise);

    double ADC = 0.0;
    
    if(version == kFastSimulator) {
        int i;
        for(i=0; i<mNumDynodes; i++) {
            if(nElectrons >= 100) break;
            nElectrons = mRandom.PoissonD( mSecondaryCoeff[i] * nElectrons + mDynodeNoise );
        }
        ADC += mTotalGain * (nElectrons + mDynodeNoise*mG1[i]) * mDynodeGain[i];
        ADC += mRandom.Gaus(mPedestalMean, TMath::Sqrt(mTotalGain*(ADC*mG1[i]*mDynodeGain[i] + mTotalGain*mDNW[i]) + mPedestalRMS*mPedestalRMS) );
    }
    else if(version == kFullSimulator) {
        for(int i=0; i<mNumDynodes; i++) {
            nElectrons = mRandom.PoissonD( mSecondaryCoeff[i] * nElectrons + mDynodeNoise );
        }
        ADC += mTotalGain * nElectrons * mDynodeGain[mNumDynodes];
        ADC += mRandom.Gaus(mPedestalMean, mPedestalRMS);
    }
    else {
        LOG_ERROR << version << " is not a valid simulator version " << endm;
    }
    
    int ret = static_cast<int>(TMath::Floor(ADC)); //why does floor() return a double? stupid
    LOG_DEBUG << Form("hit has photoElectronGain = (%f/%d) = %e", nElectrons, nPhotoElectrons, static_cast<float>(nElectrons/nPhotoElectrons)) << endm;
    
    return ret;
}

/*****************************************************************************
 * $Log: StPmtSignal.cxx,v $
 * Revision 1.5  2007/09/11 21:49:15  kocolosk
 * complete overhaul of the BEMC simulator
 * http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *****************************************************************************/
