/***************************************************************************
 *
 * $Id: StTrsAnalogSignalGenerator.hh,v 1.3 1999/01/18 21:00:52 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Abstract Class to define the Analog signal generator
 *
 ***************************************************************************
 *
 * $Log: StTrsAnalogSignalGenerator.hh,v $
 * Revision 1.3  1999/01/18 21:00:52  lasiuk
 * add fractionSampled(); reorder initialization
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

#include "StTrsAnalogSignal.hh"
#include "StTpcGeometry.hh"
#include "StTpcSlowControl.hh"
#include "StTpcElectronics.hh"

#include "StTrsWireHistogram.hh"
#include "StTrsSector.hh"

class StTrsAnalogSignalGenerator {
public:
    StTrsAnalogSignalGenerator(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);
    virtual ~StTrsAnalogSignalGenerator() {/* nopt */}

    // Charge Induction
    virtual void inducedChargeOnPad(StTrsWireHistogram*)     = 0;

    // Sampling
    virtual void   sampleAnalogSignal()                      = 0;
    virtual double signalSampler(double, StTrsAnalogSignal&) = 0;

    // access and set
    void           setDeltaPad(int);
    void           setDeltaRow(int);
    void           setSignalThreshold(double);
    void           setSuppressEmptyTimeBins(bool);

protected:
    void           fractionSampled();
    
protected:
    StTpcGeometry*       mGeomDb;
    StTpcSlowControl*    mSCDb;
    StTpcElectronics*    mElectronicsDb;

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
};
void inline StTrsAnalogSignalGenerator::setDeltaRow(int dr) { mDeltaRow = (dr >=0) ? dr : 0;}
void inline StTrsAnalogSignalGenerator::setDeltaPad(int dp) { mDeltaPad = (dp >=0) ? dp : 0;}
void inline StTrsAnalogSignalGenerator::setSignalThreshold(double th) { mSignalThreshold = th;}
void inline StTrsAnalogSignalGenerator::setSuppressEmptyTimeBins(bool su) {mSuppressEmptyTimeBins = su;}
#endif
