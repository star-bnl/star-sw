/*!
 * \class StSsdClusterControl
 * \author B.Hippolyte   
 * \date 2003
 *
 *  Storage class for the clusterisation/matching parameters
 *
 */

#ifndef STAR_StSsdClusterControl
#define STAR_StSsdClusterControl
#include "Rtypes.h"
class St_clusterControl;
class StSsdClusterControl{
 public:
  StSsdClusterControl();
  StSsdClusterControl(St_clusterControl *clusterCtrl);
 ~StSsdClusterControl();

 Float_t  getHighCut();
 Float_t  getTestTolerance();
 Int_t    getClusterTreat();
 Float_t  getAdcTolerance();
 Float_t  getMatchMean();
 Float_t  getMatchSigma();

 void   setHighCut(Float_t highCut);
 void   setTestTolerance(Float_t testTolerance);
 void   setClusterTreat(Int_t clusterTreat);
 void   setAdcTolerance(Float_t adcTolerance);
 void   setMatchMean(Float_t matchMean);
 void   setMatchSigma(Float_t matchSigma);

 void   printParameters();

 private:
 Float_t  mHighCut;       //!  High cut on the central strip for starting a cluster (s/n unit)
 Float_t  mTestTolerance; //!  Set to 20%
 long   mClusterTreat;  //!  Max number of clusters in a solvable package
 Float_t  mAdcTolerance;  //!  Set to 20%
 Float_t  mMatchMean;     //!  Charge matching mean value  (depends on the gains)
 Float_t  mMatchSigma;    //!  Charge matching sigma value (depends on the noise) 
};

inline Float_t StSsdClusterControl::getHighCut()       { return mHighCut; }
inline Float_t StSsdClusterControl::getTestTolerance() { return mTestTolerance; }
inline Int_t   StSsdClusterControl::getClusterTreat()  { return mClusterTreat; }
inline Float_t StSsdClusterControl::getAdcTolerance()  { return mAdcTolerance; }
inline Float_t StSsdClusterControl::getMatchMean()     { return mMatchMean; }
inline Float_t StSsdClusterControl::getMatchSigma()    { return mMatchSigma; }

inline void  StSsdClusterControl::setHighCut(Float_t highCut)             {mHighCut = highCut;}
inline void  StSsdClusterControl::setTestTolerance(Float_t testTolerance) {mTestTolerance = testTolerance;}
inline void  StSsdClusterControl::setClusterTreat(Int_t clusterTreat)     {mClusterTreat = clusterTreat;}
inline void  StSsdClusterControl::setAdcTolerance(Float_t adcTolerance)   {mAdcTolerance = adcTolerance;}
inline void  StSsdClusterControl::setMatchMean(Float_t matchMean)         {mMatchMean = matchMean;}
inline void  StSsdClusterControl::setMatchSigma(Float_t matchSigma)       {mMatchSigma = matchSigma;}

#endif

 
