/***************************************************************************
 *
 * $Id: StTrsAnalogSignalGenerator.cc,v 1.4 1999/04/23 19:19:49 lasiuk Exp $
 *
 * Author: brian Nov 3, 1998 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsAnalogSignalGenerator.cc,v $
 * Revision 1.4  1999/04/23 19:19:49  lasiuk
 * add delay to centroid of signal:
 * Calculated in constructor (mTimeShiftOfSignalCentroid)
 * and applied in in signalsampler()
 *
 * Revision 1.3  1999/02/28 20:13:29  lasiuk
 * noise additions
 *
 * Revision 1.2  1999/01/18 21:00:32  lasiuk
 * add fractionSampled(); reorder initialization
 *
 * Revision 1.1  1998/11/10 17:12:23  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/08 17:02:43  lasiuk
 * vector allocator, TRUE/true
 *
 * Revision 1.1  1998/11/04 18:46:11  lasiuk
 * remove gas gain functionality (now in WireHistogram)
 * initialization in base class
 * incorporate signalSampler
 *
 **************************************************************************/
#include "StTrsAnalogSignalGenerator.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

HepJamesRandom  StTrsAnalogSignalGenerator::mEngine;
RandGauss       StTrsAnalogSignalGenerator::mGaussDistribution(mEngine);

StTrsAnalogSignalGenerator::StTrsAnalogSignalGenerator(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
    : mGeomDb(geo), mSCDb(sc), mElectronicsDb(el), mSector(sec),
      mDeltaRow(0),
      mDeltaPad(0),
      mSignalThreshold(0.*volt),
      mSuppressEmptyTimeBins(true)
{
    //
    // signal generation
    mSigma1 = mElectronicsDb->shapingTime();
    mSigma2 = 2.*mSigma1;
    mTau    = mElectronicsDb->tau();
    mGain   = mElectronicsDb->nominalGain();

    mSamplingFrequency = mElectronicsDb->samplingFrequency();
    fractionSampled();
    //
    // noise
    mAddNoise = false;
    mAddNoiseUnderSignalOnly = false;
    mNoiseRMS = 0.;
    //
    // Time  Shift
    mTimeShiftOfSignalCentroid = 3.*mSCDb->driftVelocity()*mTau;
    PR(mTimeShiftOfSignalCentroid);
}

void StTrsAnalogSignalGenerator::fractionSampled()
{
    // Charge collected:
    // Q(t) = Q ln(1+t/to)
    // total collection time tmax = 62.5us
    // sampling time is mSigma1   = 180 ns

    // should really come from a dataBase, but where??
    double to = 1.5*nanosecond;
    double tmax = 62500.*nanosecond;
    
    mFractionSampled = log(1+mSigma1/to)/log(1+tmax/to);
}
