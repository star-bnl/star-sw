#ifndef STSCMCLUSTER_HH
#define STSCMCLUSTER_HH

class StScmCluster
{
 public:
  StScmCluster(int rNCluster);
  StScmCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, int rTotNoise, float rStripMean,  int rFlag, int *rMcHit);
  StScmCluster(const StScmCluster & originalCluster);
  ~StScmCluster();

  StScmCluster& operator=(const StScmCluster originalCluster);
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
  void          setPrevCluster(StScmCluster *rPrevCluster);
  void          setNextCluster(StScmCluster *rNextCluster);

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
  StScmCluster* getPrevCluster();
  StScmCluster* getNextCluster();  

  StScmCluster* giveCopy();
  
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
  StScmCluster *mPrevCluster;
  StScmCluster *mNextCluster;
};
#endif
