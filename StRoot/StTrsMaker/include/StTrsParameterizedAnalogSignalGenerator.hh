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



#include "StTpcDb/StTpcDb.h"
#include "TRandom.h"
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
    double addNoise(double );

private:
    // sampling
    double realShaperResponse(double, StTrsAnalogSignal&);
  // error function table initialization
    void    errorFunctionTableBuilder(); 
    void    localArrayBuilder();
    double  erf_fast(double) const;

protected:
    StTrsParameterizedAnalogSignalGenerator(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);

private:
  static StTrsAnalogSignalGenerator* mInstance; 
  StTpcDb* gTpcDbPtr;
  
  double         mDriftVelocity;
  double         mTimeBinWidth;
  double         mTau;
  double         mSamplingFrequency;
  double         mFractionSampled;  
  double         mPadResponseFunctionSigma;
const double         mPadResponseFunctionSigmaOuter;
const double         mPadResponseFunctionSigmaInner;
 
#ifndef ST_NO_TEMPLATE_DEF_ARGS
 
   vector<double> mChargeFractionOuter; 
  vector<double> mChargeFractionInner;
  vector<double> mErrorFunctionTable; 
  vector<double> mYb;
#else

  vector<double, allocator<double> > mChargeFractionOuter; 
  vector<double, allocator<double> > mChargeFractionInner;
  vector<double, allocator<double> > mErrorFunctionTable; 
  vector<double, allocator<double> > mYb;
#endif
  double         mNumberOfEntriesInTable;
  double         mRangeOfTable ;
  int            mPadsAtRow[200];
    //To avoid initialization, define the following data members.
    StTpcPadCoordinate    mTpcRaw;
    int            mCentralPad;
    int            mCentralRow;
    int            mNumberOfRows;
    int            mNumberOfInnerRows;
    double         mFrischGrid;
 
    double rowNormalization;
    double padWidth, padLength;
    double zoffset, wire_to_plane_coupling;
    double xCentroid[100][500], yCentroid[100],SignalInTimeBin[1000],gain[100][500];
    double delx, gridMinusZ, sigma_x, localXDirectionCoupling[500];
    double dely, constant, localYDirectionCoupling;
    double timeOfSignal, chargeOfSignal;
    double t, tzero, K, sigmaLoverTau, lambda,lambdasqr;
    double mAdcConversion;
    double landauConstant,landauMean,landauSigma,expConstant,expSlope,landauCut,
    GausConstant[6], GausMean[6], GausSigma2[6],ExpConstant[6],ExpSlope[6],cutT[6];
    
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


