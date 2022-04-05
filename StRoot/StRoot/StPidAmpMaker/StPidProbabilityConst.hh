#ifndef StPidProbabilityConst_hh
#define StPidProbabilityConst_hh

//#include "SystemOfUnits.h"

#define mNPBins           100 //from 0 to 2GeV
#define mNEtaBins         10  //from 0 to 1.
#define mNNHitsBins       6   //from 0 to 30. (if >30, belongs to the last bin)
#define mNDedxBins        300  //for histogram bins. not for outputbins.
#define mNChargeBins      2   //0-minus 1-plus
#define mNDcaBins         2   //dca<3cm-bin0 dca>=3cm-bin1
#define mMultiplicityBins 3

#define mPStart         0.
#define mPEnd           2.
#define mEtaStart       0.
#define mEtaEnd         1.
#define mNNHitsStart    0
#define mNNHitsEnd      30  //if >30, belongs to the last bin.
#define mDedxStart      0.
#define mDedxEnd        1.2e-5

#define mMultiplicityBin1End 120 //uncorrectedNumberOfNegativePrimaries from StuRefMult
#define mMultiplicityBin2End 215 //if >194 belongs to the last mult. bin

#define mDcaCut         3 //*centimeter
#define NParameters     7  //number of parameters for BetheBloch function


#define junction1       0.2 //p<0.2, pion + e only
#define junction2       0.44 //0.2<p<0.44 pion+e+kaon
#define junction3       0.85 //0.44<p<0.85 pion+e+kaon+proton

#endif


