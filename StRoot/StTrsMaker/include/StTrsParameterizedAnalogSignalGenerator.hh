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
  double         mFractionSampled;  
  double         mPadResponseFunctionSigma;
const double         mPadResponseFunctionSigmaOuter;
const double         mPadResponseFunctionSigmaInner;
  double         mChargeFraction[4]; 
#ifndef ST_NO_TEMPLATE_DEF_ARGS
  vector<double> mErrorFunctionTable;
#else
  vector<double, allocator<double> > mErrorFunctionTable;
#endif
  double         mNumberOfEntriesInTable;
  double         mRangeOfTable ;
    //To avoid initialization, define the following data members.
    StTpcPadCoordinate    mTpcRaw;
    int            mCentralPad;
    int            mCentralRow;
    int            mNumberOfRows;
    int            mNumberOfInnerRows;
    double         mFrischGrid;
    double yb1, yb2, yb3, yb4;
    double rowNormalization;
    double padWidth, padLength;
    double zoffset, wire_to_plane_coupling;
    double xCentroidOfPad, yCentroidOfPad;
    double delx, gridMinusZ, sigma_x, localXDirectionCoupling;
    double dely, constant, localYDirectionCoupling;
    double timeOfSignal, chargeOfSignal;
    double t, tzero, K, sigmaLoverTau, lambda;
    
};

inline double StTrsParameterizedAnalogSignalGenerator::signalSampler(double tt, StTrsAnalogSignal& sig)
{
    //
    // This is where the function for the Signal Sampling is selected
    // Add a function that returns the amplitude of a signal at
    // a time 't' given the position in time and amplitude of all
    // the other signals (contained in the StTrsAnalogSignal 'sig'
  

  return realShaperResponse(tt,sig);
  
}

#endif
