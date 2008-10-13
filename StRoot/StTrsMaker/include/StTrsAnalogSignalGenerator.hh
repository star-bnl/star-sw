/***************************************************************************
 *
 * $Id: StTrsAnalogSignalGenerator.hh,v 1.10 2008/10/13 19:56:10 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Abstract Class to define the Analog signal generator
 *
 ***************************************************************************
 *
 * $Log: StTrsAnalogSignalGenerator.hh,v $
 * Revision 1.10  2008/10/13 19:56:10  fisyak
 * Account that Z-offset is sector dependent
 *
 * Revision 1.9  2005/09/09 22:12:48  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.8  2000/02/10 01:21:47  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.7  2000/01/10 23:11:31  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.6  1999/11/10 15:45:50  calderon
 * Made changes to reduce timing, including:
 * Made coordinate transfrom a data member of StTrsAnalogSignalGenerator
 * Added upper-lower bound instead of symmetric cut.
 * Revived checking if signal is above threshold.
 *
 * Revision 1.5  1999/04/23 19:20:55  lasiuk
 * add mTimeShiftOfSignalCentroid
 *
 * Revision 1.4  1999/02/28 20:13:40  lasiuk
 * noise additions
 *
 * Revision 1.3  1999/01/18 21:00:52  lasiuk
 * add fractionSampled(); reorder initialization
 *
 * Revision 1.2  1999/01/18 10:20:23  lasiuk
 * add tau
 *
 * Revision 1.1  1998/11/10 17:12:08  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:02:32  lasiuk
 * vector allocator, TRUE/true
 *
 * Revision 1.4  1998/11/04 21:27:04  lasiuk
 * unresolved symbol--forgot to set virtual function to zero (inducedChargeOnPad)
 *
 * Revision 1.3  1998/11/04 18:46:20  lasiuk
 * remove gas gain functionality (now in WireHistogram)
 * initialization in base class
 * incorporate signalSampler
 *
 * Revision 1.2  1998/10/22 14:58:12  lasiuk
 * image charge returns double and uses PRF integral
 *
 * Revision 1.1  1998/06/30 22:54:10  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_ANALOG_SIGNAL_GENERATOR_HH
#define ST_TRS_ANALOG_SIGNAL_GENERATOR_HH

#include <utility>
#include <vector>

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::min;
using std::max;
#endif

#include "Randomize.h"

#include "StTrsAnalogSignal.hh"
#include "StTpcGeometry.hh"
#include "StTpcSlowControl.hh"
#include "StTpcElectronics.hh"
#define TPC_DATABASE_PARAMETERS
#ifndef TPC_DATABASE_PARAMETERS
#include "StTpcCoordinateTransform.hh"
#else
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#endif
#include "StTrsWireHistogram.hh"
#include "StTrsSector.hh"

class StTrsAnalogSignalGenerator {
public:
    StTrsAnalogSignalGenerator(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);
    virtual ~StTrsAnalogSignalGenerator() {/* nopt */}

    // Charge Induction
  virtual void inducedChargeOnPad(StTrsWireHistogram*, Int_t sector)     	= 0;

    // Sampling
    virtual void   sampleAnalogSignal()                      	= 0;
    virtual double signalSampler(double, StTrsAnalogSignal&) 	= 0;

    // access and set
    void           setDeltaPad(int);
    void           setDeltaRow(int);
    void           setSignalThreshold(double);
    void           setSuppressEmptyTimeBins(bool);
    virtual void   setNormalFactor(double FudgeFactor) {assert(0);}
    // noise generation
    void           addNoise(bool);
    void           setNoiseRMS(double);
    void           generateNoiseUnderSignalOnly(bool);
    
protected:
    void           fractionSampled();
    double         generateNoise()    const;
    
protected:
    StTpcGeometry*       mGeomDb;
    StTpcSlowControl*    mSCDb;
    StTpcElectronics*    mElectronicsDb;
    StTpcCoordinateTransform transformer;
    int                  mDeltaPad;
    int                  mDeltaRow;
    pair<int, int>       mRowLimits;
    pair<int, int>       mPadLimits;

    StTrsSector*         mSector;    // MAKE SURE IT IS CONTAINED BY REFERENCE

    //sampling
    double                    mSignalThreshold;
    bool                      mSuppressEmptyTimeBins;
    StTrsAnalogSignal         mElectronicSignal;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal> mDiscreteAnalogTimeSequence;    
    vector<StTrsAnalogSignal>::iterator mTimeSequenceIterator;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> > mDiscreteAnalogTimeSequence;    
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator mTimeSequenceIterator;
#endif
    // db parameters
    double mSigma1;
    double mSigma2;
    double mTau;
    double mSamplingFrequency;
    double mGain;
    double mFractionSampled;

    // Noise Generation
    bool   mAddNoise;
    bool   mAddNoiseUnderSignalOnly;
    double mNoiseRMS;


    // Time Shift for electronics
    double mTimeShiftOfSignalCentroid;

    
    static HepJamesRandom mEngine;
    static RandGauss      mGaussDistribution;
};
void inline StTrsAnalogSignalGenerator::setDeltaRow(int dr) { mDeltaRow = (dr >=0) ? dr : 0;}
void inline StTrsAnalogSignalGenerator::setDeltaPad(int dp) { mDeltaPad = (dp >=0) ? dp : 0;}
void inline StTrsAnalogSignalGenerator::setSignalThreshold(double th) { mSignalThreshold = th;}
void inline StTrsAnalogSignalGenerator::setSuppressEmptyTimeBins(bool su) {mSuppressEmptyTimeBins = su;}
void inline StTrsAnalogSignalGenerator::addNoise(bool v) {mAddNoise = v;}
void inline StTrsAnalogSignalGenerator::setNoiseRMS(double v) {mNoiseRMS = v*mGain;}
void inline StTrsAnalogSignalGenerator::generateNoiseUnderSignalOnly(bool v) {mAddNoiseUnderSignalOnly = v;}
double inline StTrsAnalogSignalGenerator::generateNoise() const {return fabs(mGaussDistribution.shoot(0.,mNoiseRMS));}
#endif
