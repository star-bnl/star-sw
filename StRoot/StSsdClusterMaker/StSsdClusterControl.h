#ifndef STAR_StSsdClusterControl
#define STAR_StSsdClusterControl

class StSsdClusterControl{
 public:
  StSsdClusterControl();
 ~StSsdClusterControl();

     float     getHighCut();
     float     getTestTolerance();
     
     long      getClusterTreat();
     float     getAdcTolerance();
     float     getMatchMean();
     float     getMatchSigma();

     void      setHighCut(float highCut);
     void      setTestTolerance(float testTolerance);

     void      setClusterTreat(long clusterTreat);
     void      setAdcTolerance(float adcTolerance);
     void      setMatchMean(float matchMean);
     void      setMatchSigma(float matchSigma);

     void      printParameters();

 private:
     float     mHighCut;       //!
     float     mTestTolerance; //!

     long      mClusterTreat;  //!
     float     mAdcTolerance;  //!
     float     mMatchMean;     //!
     float     mMatchSigma;    //!
};
#endif

 
