#ifndef STSSDCLUSTER_HH
#define STSSDCLUSTER_HH
#include "StSsdStrip.hh"

class StSsdCluster
{
 public:
  StSsdCluster(int rNCluster);
  StSsdCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, int rTotNoise, float rStripMean,  int rFlag, int *rMcHit);
  StSsdCluster(const StSsdCluster & originalCluster);
  ~StSsdCluster();

  StSsdCluster& operator=(const StSsdCluster originalCluster);

  void          setNCluster(int rNCluster);
  void          setFirstStrip(int rFirstStrip);
  void          setClusterSize(int rClusterSize);
  void          setTotAdc(int rTotAdc);
  void          setFirstAdc(int rFirstAdc);
  void          setLastAdc(int rLastAdc);
  void          setTotNoise(int rTotNoise);
  void          setStripMean(float rStripMean);
  void          setFlag(int rFlag);
  void          setIdMcHit(int rIdMcHit, int iR);

  void          setPrevCluster(StSsdCluster *rPrevCluster);
  void          setNextCluster(StSsdCluster *rNextCluster);

  int           getNCluster();
  int           getFirstStrip();
  int           getClusterSize();
  int           getTotAdc();
  int           getFirstAdc();
  int           getLastAdc();
  int           getTotNoise();
  float         getStripMean();
  int           getFlag();
  int           getIdMcHit(int iR);

  StSsdCluster* getPrevCluster();
  StSsdCluster* getNextCluster();  

  StSsdCluster* giveCopy(); 
  void          copyTo(StSsdCluster *ptrClone);
  void          update(StSsdStrip *ptr,float rWeight);
  
private:
  int           mNCluster;
  int           mFirstStrip;
  int           mClusterSize;
  int           mTotAdc;
  int           mFirstAdc;
  int           mLastAdc;
  int           mTotNoise;
  float         mStripMean;
  int           mFlag;
  int          *mIdMcHit;

  StSsdCluster *mPrevCluster;
  StSsdCluster *mNextCluster;
};

#endif
