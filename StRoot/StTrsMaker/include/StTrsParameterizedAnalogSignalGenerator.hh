/***************************************************************************
 *
 *  StTrsParameterizedAnalogSignalGenerator.hh,v 1.1 1999/10/01 
 *      17:15:00 HUi Long Exp $
 *
 * Author: Hui Long
 ***************************************************************************
 * 
 * Description: Head file for the StTrsParameterizedAnalogSignalGenerator.cc.
 *              
 *
 ***************************************************************************
 *
 * $Log : v1.1 Hui Long$
 *
 **************************************************************************/
#ifndef ST_TRS_PARAMETERIZED_ANALOG_SIGNAL_GENERATOR_HH
#define ST_TRS_PARAMETERIZED_ANALOG_SIGNAL_GENERATOR_HH

#include <iostream.h>
#include "StTrsAnalogSignalGenerator.hh"

class StTrsParameterizedAnalogSignalGenerator : public  StTrsAnalogSignalGenerator {
public:


public:
    ~StTrsParameterizedAnalogSignalGenerator();
    //StTrsParameterizedAnalogSignalGenerator(const StTrsParameterizedAnalogSignalGenerator&);
    //StTrsParameterizedAnalogSignalGenerator& operator=(const StTrsParameterizedAnalogSignalGenerator&);

    static StTrsAnalogSignalGenerator* instance();
    static StTrsAnalogSignalGenerator* instance(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);

    // charge generation
    void   inducedChargeOnPad(StTrsWireHistogram*);

    // sampling
    void   sampleAnalogSignal();
    double signalSampler(double, StTrsAnalogSignal&);

private:
    // sampling
    double realShaperResponse(double, StTrsAnalogSignal&);
  // error function table initialization
    void    errorFunctionTableBuilder();
    double  erf_fast(double) const;

protected:
    StTrsParameterizedAnalogSignalGenerator(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);

private:
  static StTrsAnalogSignalGenerator* mInstance;

  double         mDriftVelocity;
  double         mTimeBinWidth;
  double         mTau;
  double         mSamplingFrequency;
  double         mPadRespondFunctionSigma;

#ifndef ST_NO_DEF_TEMPLATE_ARGS
  vector<double, allocator<double> > mChargeFraction;
  vector<double, allocator<double> > mErrorFuctionTable;
#else
  vector<double, allocator<double> > mChargeFraction;
  vector<double, allocator<double> > mErrorFuctionTable;
#endif
   double        mNumberOfEntriesInTable;
  double         mRangeOfTable ;
};

#endif
