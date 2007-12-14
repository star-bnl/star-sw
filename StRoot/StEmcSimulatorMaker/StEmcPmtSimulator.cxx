// $Id: StEmcPmtSimulator.cxx,v 1.14 2007/12/14 03:53:44 kocolosk Exp $

#include "StEmcPmtSimulator.h"

#include "StMessMgr.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StEmcRawHit.h"
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTables.h"

ClassImp(StEmcPmtSimulator)

StEmcPmtSimulator::StEmcPmtSimulator(StDetectorId det, StEmcSimulatorMode mode) 
    : StEmcSimpleSimulator(det, mode) 
{ 
    switch(mDetectorId) {
        case kBarrelEmcTowerId:
            mMipPhotoElectrons = 63.0;
            mMipEnergyDeposit = 0.0198;
            break;
            
        case kBarrelEmcPreShowerId:
            mMipPhotoElectrons = 6.0;
            mMipEnergyDeposit = 0.002;
            break;
            
        default:
            LOG_ERROR << "StEmcPmtSimulator isn't configured for " << mDetectorId << endm;
    }
    
    switch(mMode) {
        case kPrimarySecondaryFullMode:
            mVer = StPmtSignal::kFullSimulator;
            break;
            
        case kPrimarySecondaryFastMode:
            mVer = StPmtSignal::kFastSimulator;
            break;
            
        default: break;
    }
}

StEmcRawHit* StEmcPmtSimulator::makeRawHit(const StMcCalorimeterHit *mcHit) {
    // remember the problem with negative sub -- let's be careful
    if(mcHit->module() <= 0 || mcHit->eta() <= 0 || mcHit->sub() <= 0) { 
        LOG_ERROR << "These quantities must all be positive: module=" << mcHit->module() 
                  << " eta=" << mcHit->eta() << " sub=" << mcHit->sub() << endm;
        return NULL;
    }
    
    StEmcRawHit *rawHit = new StEmcRawHit(mDetectorId, mcHit->module(), mcHit->eta(), mcHit->sub(), 0);
    
    float pseudoRapidity; mGeom->getEta(mcHit->module(), mcHit->eta(), pseudoRapidity);
    int softId; mGeom->getId(mcHit->module(), mcHit->eta(), mcHit->sub(), softId);
    
    double photoElectrons = mcHit->dE() * mMipPhotoElectrons / mMipEnergyDeposit;
    photoElectrons = mRandom.PoissonD(photoElectrons);
    
    // get the calibration value from DB if available, otherwise use ideal value
    float calib = mTables->calib(mDetectorId-8, softId);
    
    // totalGain is ADC per incident photoElectron in this case
    double totalGain = calib ? samplingFraction(pseudoRapidity) * (mMipEnergyDeposit / mMipPhotoElectrons) / calib : 0.0;
    
    double pedMean(0.0), pedRMS(0.0);
    if(!mEmbeddingMode) {
        pedMean = mTables->pedestal(mDetectorId-8, softId);
        pedRMS  = mTables->pedestalRMS(mDetectorId-8, softId);
    }
    
    double ADC(0.0);
    switch(mMode) {
        case kTestMode: case kSimpleMode: {
            delete rawHit;
            return StEmcSimpleSimulator::makeRawHit(mcHit);
        }
            
        case kPrimaryOnlyMode: {
            ADC  = totalGain * photoElectrons;
            ADC += mRandom.Gaus(pedMean, pedRMS);
            break;
        }
            
        case kPrimarySecondaryFullMode: case kPrimarySecondaryFastMode: {   
            mPmtSignal.setTotalGain(totalGain);
            mPmtSignal.setPedestalMean(pedMean);
            mPmtSignal.setPedestalRMS(pedRMS);
            
            ADC = mPmtSignal.getAdc(static_cast<int>(photoElectrons), mVer);
            break;
        }
    }
    
    // finally smear with any specified calibration jitter
    ADC = pedMean + (ADC-pedMean) * mRandom.Gaus(mCalibScale, mCalibSpread);
    
    // check for a valid ADC range
    double maxADC = mRandom.Gaus(mMaxADC, mMaxADCSpread);
    if(ADC < 0)         ADC = 0.0;
    if(ADC > maxADC)    ADC = maxADC;
    
    rawHit->setAdc(static_cast<unsigned int>(ADC));
    
    float energy = (rawHit->adc() - pedMean) * calib;
    rawHit->setEnergy(energy);
    
    LOG_DEBUG << Form("det=%2d  softId=%5d  dE=%e  sF=%.1f  ADC=%4d  ped=%6.2f  rms=%4.2f  energy=%.4f",
        mDetectorId, softId, mcHit->dE(), samplingFraction(pseudoRapidity), rawHit->adc(), 
        pedMean, pedRMS, rawHit->energy()) << endm;
    
    return rawHit;
}

/*****************************************************************************
 *  $Log: StEmcPmtSimulator.cxx,v $
 *  Revision 1.14  2007/12/14 03:53:44  kocolosk
 *  bugfix so channels with zero calibrations still get a simulated pedestal
 *
 *  Revision 1.13  2007/12/12 22:12:24  kocolosk
 *  calibration spread should only operate on ped-subtracted ADCs, not raw
 *
 *  Revision 1.12  2007/10/08 15:28:35  kocolosk
 *  setMaximumAdc(Spread) methods allow for better simulation of BSMD ADC response
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2507.html
 *
 *  Revision 1.11  2007/09/12 13:31:45  kocolosk
 *  two small changes to suppress compiler warnings
 *
 *  Revision 1.10  2007/09/11 21:49:13  kocolosk
 *  complete overhaul of the BEMC simulator
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *  Revision 1.9  2007/01/23 19:44:24  kocolosk
 *  few additional logger fixes
 *
 *  Revision 1.8  2007/01/22 19:13:39  kocolosk
 *  use STAR logger for all output
 *
 *  Revision 1.7  2005/03/21 21:36:38  suaide
 *  fixed problem with chain
 *
 *  Revision 1.6  2004/08/06 13:24:47  suaide
 *  New features added and fixed some bugs in the database
 *
 *  Revision 1.5  2003/09/23 15:19:43  suaide
 *  fixed bugs and modifications for embedding
 *
 *  Revision 1.4  2003/01/23 03:09:02  jeromel
 *  Include modif
 *
 *  Revision 1.3  2002/06/04 16:09:34  pavlinov
 *  added option with DB(pedestal ans calibration  coefficients
 *
 *  Revision 1.2  2001/05/14 01:30:13  pavlinov
 *  Cleanup
 *
 *  Revision 1.1  2000/10/23 22:53:13  pavlinov
 *  First working C++ version
 *****************************************************************************/
