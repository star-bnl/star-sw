// $Id: StSsdClusterControl.h,v 1.2 2005/05/17 14:16:38 lmartin Exp $
//
// $Log: StSsdClusterControl.h,v $
// Revision 1.2  2005/05/17 14:16:38  lmartin
// CVS tags added
//
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

 
