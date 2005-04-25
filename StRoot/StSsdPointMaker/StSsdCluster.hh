// $Id: StSsdCluster.hh,v 1.3 2005/04/25 14:13:23 bouchet Exp $
//
// $Log: StSsdCluster.hh,v $
// Revision 1.3  2005/04/25 14:13:23  bouchet
// new method makeScfCtrlHistograms and makeScmCtrlHistograms and Clusternoise is coded as a float
//
// Revision 1.2  2005/03/18 14:24:20  lmartin
// missing CVS header added
//

#ifndef STSSDCLUSTER_HH
#define STSSDCLUSTER_HH
#include "StSsdStrip.hh"

class StSsdCluster
{
 public:
  StSsdCluster(int rNCluster);
  StSsdCluster(int rNCluster, int rFirstStrip, int rClusterSize, int rTotAdc, int rFirstAdc, int rLastAdc, float rTotNoise, float rStripMean,  int rFlag, int *rMcHit);
  StSsdCluster(const StSsdCluster & originalCluster);
  ~StSsdCluster();

  StSsdCluster& operator=(const StSsdCluster originalCluster);

  void          setNCluster(int rNCluster);
  void          setFirstStrip(int rFirstStrip);
  void          setClusterSize(int rClusterSize);
  void          setTotAdc(int rTotAdc);
  void          setFirstAdc(int rFirstAdc);
  void          setLastAdc(int rLastAdc);
  void          setTotNoise(float rTotNoise);
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
  float         getTotNoise();
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
  float         mTotNoise;
  float         mStripMean;
  int           mFlag;
  int          *mIdMcHit;

  StSsdCluster *mPrevCluster;
  StSsdCluster *mNextCluster;
};

#endif
