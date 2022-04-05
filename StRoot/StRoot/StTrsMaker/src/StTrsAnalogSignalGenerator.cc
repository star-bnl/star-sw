/***************************************************************************
 *
 * $Id: StTrsAnalogSignalGenerator.cc,v 1.12 2003/09/02 17:59:19 perev Exp $
 *
 * Author: brian Nov 3, 1998 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsAnalogSignalGenerator.cc,v $
 * Revision 1.12  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.11  2000/07/30 02:47:04  long
 *  mDeltaPad(0)---> mDeltaPad(2)
 *
 * Revision 1.10  2000/06/23 00:12:40  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
 * Revision 1.9  2000/02/10 01:21:49  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.8  2000/01/10 23:14:30  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.7  1999/11/10 15:46:25  calderon
 * Made changes to reduce timing, including:
 * Made coordinate transfrom a data member of StTrsAnalogSignalGenerator
 * Added upper-lower bound instead of symmetric cut.
 * Revived checking if signal is above threshold.
 *
 * Revision 1.6  1999/10/04 15:28:31  long
 * change on the value of mTimeShiftOfSignalCentroid
 *
 * Revision 1.6  1999/10/01  17:15:00  Hui Long
 * mTimeShiftOfSignalCentroid(See Note in the code..
 *
 * Revision 1.5  1999/04/27 15:05:40  lasiuk
 * itime shift in ns
 *
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
    : mGeomDb(geo), mSCDb(sc), mElectronicsDb(el),
      transformer(gStTpcDb),
      mDeltaPad(2),
      mDeltaRow(1),
      mSector(sec),
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
   //  mTimeShiftOfSignalCentroid = 3.* mTau;
    mTimeShiftOfSignalCentroid =mElectronicsDb->tZero();//HL,8/31/99
    PR(mTimeShiftOfSignalCentroid/nanosecond);

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
    
    mFractionSampled = ::log(1+mSigma1/to)/::log(1+tmax/to);
}
