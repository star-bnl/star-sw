#ifndef STSCECLUSTER_HH
#define STSCECLUSTER_HH

class StSceCluster
{
 public:
  StSceCluster(int rNCluster);
  StSceCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, int rTotNoise, float rStripMean,  int rFlag, int *rMcHit);
  StSceCluster(const StSceCluster & originalCluster);
  ~StSceCluster();

  StSceCluster& operator=(const StSceCluster originalCluster);
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
  void          setPrevCluster(StSceCluster *rPrevCluster);
  void          setNextCluster(StSceCluster *rNextCluster);

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
  StSceCluster* getPrevCluster();
  StSceCluster* getNextCluster();  

  StSceCluster* giveCopy();
  
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
  StSceCluster *mPrevCluster;
  StSceCluster *mNextCluster;
};
#endif
