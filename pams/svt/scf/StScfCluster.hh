#ifndef STSCFCLUSTER_HH
#define STSCFCLUSTER_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StScfStrip.hh"

class StScfCluster
{
 public:
  StScfCluster(int rNCluster);
  ~StScfCluster();
  int           getNCluster();
  int           getFirstStrip();
  int           getClusterSize();
  int           getFlag();
  int           getTotAdc();
  int           getFirstAdc();
  int           getLastAdc();
  int           getTotNoise();
  float         getStripMean();
  int           getIdMcHit(int iR);
  StScfCluster* getPrevCluster();
  StScfCluster* getNextCluster();  

  void          setPrevCluster(StScfCluster *rPrevCluster);
  void          setNextCluster(StScfCluster *rNextCluster);
  void          setNCluster(int rNCluster);
  void          setFirstStrip(int rFirstStrip);
  void          setClusterSize(int rClusterSize);
  void          setFlag(int rFlag);
  void          setTotAdc(int rTotAdc);
  void          setFirstAdc(int rFirstAdc);
  void          setLastAdc(int rLastAdc);
  void          setTotNoise(int rTotNoise);
  void          setStripMean(float rStripMean);
  void          setIdMcHit(int rIdMcHit, int iR);
  void          update(StScfStrip *ptr,float rWeight);
  void          copyTo(StScfCluster *ptrClone);
  
private:
  int           mNCluster;
  int           mFirstStrip;
  int           mClusterSize;
  int           mFlag;
  int           mTotAdc;
  int           mFirstAdc;
  int           mLastAdc;
  int           mTotNoise;
  float         mStripMean;
  int          *mIdMcHit;
  StScfCluster *mPrevCluster;
  StScfCluster *mNextCluster;
};

#endif
