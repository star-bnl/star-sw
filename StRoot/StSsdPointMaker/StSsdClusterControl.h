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
class St_clusterControl;
class StSsdClusterControl{
 public:
  StSsdClusterControl();
  StSsdClusterControl(St_clusterControl *clusterCtrl);
 ~StSsdClusterControl();

 float  getHighCut();
 float  getTestTolerance();
 int    getClusterTreat();
 float  getAdcTolerance();
 float  getMatchMean();
 float  getMatchSigma();

 void   setHighCut(float highCut);
 void   setTestTolerance(float testTolerance);
 void   setClusterTreat(int clusterTreat);
 void   setAdcTolerance(float adcTolerance);
 void   setMatchMean(float matchMean);
 void   setMatchSigma(float matchSigma);

 void   printParameters();

 private:
 float  mHighCut;       //!  High cut on the central strip for starting a cluster (s/n unit)
 float  mTestTolerance; //!  Set to 20%
 long   mClusterTreat;  //!  Max number of clusters in a solvable package
 float  mAdcTolerance;  //!  Set to 20%
 float  mMatchMean;     //!  Charge matching mean value  (depends on the gains)
 float  mMatchSigma;    //!  Charge matching sigma value (depends on the noise) 
};

inline float StSsdClusterControl::getHighCut()       { return mHighCut; }
inline float StSsdClusterControl::getTestTolerance() { return mTestTolerance; }
inline int   StSsdClusterControl::getClusterTreat()  { return mClusterTreat; }
inline float StSsdClusterControl::getAdcTolerance()  { return mAdcTolerance; }
inline float StSsdClusterControl::getMatchMean()     { return mMatchMean; }
inline float StSsdClusterControl::getMatchSigma()    { return mMatchSigma; }

inline void  StSsdClusterControl::setHighCut(float highCut)             {mHighCut = highCut;}
inline void  StSsdClusterControl::setTestTolerance(float testTolerance) {mTestTolerance = testTolerance;}
inline void  StSsdClusterControl::setClusterTreat(int clusterTreat)     {mClusterTreat = clusterTreat;}
inline void  StSsdClusterControl::setAdcTolerance(float adcTolerance)   {mAdcTolerance = adcTolerance;}
inline void  StSsdClusterControl::setMatchMean(float matchMean)         {mMatchMean = matchMean;}
inline void  StSsdClusterControl::setMatchSigma(float matchSigma)       {mMatchSigma = matchSigma;}

#endif

 
