//$Id: StSstClusterControl.h,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstClusterControl.h,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STAR_StSstClusterControl
#define STAR_StSstClusterControl
#include "Rtypes.h"
class St_clusterControl;
class StSstClusterControl{
 public:
  StSstClusterControl();
  StSstClusterControl(St_clusterControl *clusterCtrl);
 ~StSstClusterControl();

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

inline Float_t StSstClusterControl::getHighCut()       { return mHighCut; }
inline Float_t StSstClusterControl::getTestTolerance() { return mTestTolerance; }
inline Int_t   StSstClusterControl::getClusterTreat()  { return mClusterTreat; }
inline Float_t StSstClusterControl::getAdcTolerance()  { return mAdcTolerance; }
inline Float_t StSstClusterControl::getMatchMean()     { return mMatchMean; }
inline Float_t StSstClusterControl::getMatchSigma()    { return mMatchSigma; }

inline void  StSstClusterControl::setHighCut(Float_t highCut)             {mHighCut = highCut;}
inline void  StSstClusterControl::setTestTolerance(Float_t testTolerance) {mTestTolerance = testTolerance;}
inline void  StSstClusterControl::setClusterTreat(Int_t clusterTreat)     {mClusterTreat = clusterTreat;}
inline void  StSstClusterControl::setAdcTolerance(Float_t adcTolerance)   {mAdcTolerance = adcTolerance;}
inline void  StSstClusterControl::setMatchMean(Float_t matchMean)         {mMatchMean = matchMean;}
inline void  StSstClusterControl::setMatchSigma(Float_t matchSigma)       {mMatchSigma = matchSigma;}

#endif

 
