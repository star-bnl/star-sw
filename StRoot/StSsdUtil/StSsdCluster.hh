// $Id: StSsdCluster.hh,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdCluster.hh,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
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
  StSsdCluster(Int_t rNCluster);
  StSsdCluster(Int_t rNCluster, Int_t rFirstStrip, Int_t rClusterSize, Int_t rTotAdc, Int_t rFirstAdc, Int_t rLastAdc, Float_t rTotNoise, Float_t rStripMean,  Int_t rFlag, Int_t *rMcHit);
  StSsdCluster(const StSsdCluster & originalCluster);
  ~StSsdCluster();

  StSsdCluster& operator=(const StSsdCluster originalCluster);

  void          setNCluster(Int_t rNCluster);
  void          setFirstStrip(Int_t rFirstStrip);
  void          setClusterSize(Int_t rClusterSize);
  void          setTotAdc(Int_t rTotAdc);
  void          setFirstAdc(Int_t rFirstAdc);
  void          setLastAdc(Int_t rLastAdc);
  void          setTotNoise(Float_t rTotNoise);
  void          setStripMean(Float_t rStripMean);
  void          setFlag(Int_t rFlag);
  void          setIdMcHit(Int_t rIdMcHit, Int_t iR);

  void          setPrevCluster(StSsdCluster *rPrevCluster);
  void          setNextCluster(StSsdCluster *rNextCluster);

  Int_t           getNCluster();
  Int_t           getFirstStrip();
  Int_t           getClusterSize();
  Int_t           getTotAdc();
  Int_t           getFirstAdc();
  Int_t           getLastAdc();
  Float_t         getTotNoise();
  Float_t         getStripMean();
  Int_t           getFlag();
  Int_t           getIdMcHit(Int_t iR);

  StSsdCluster* getPrevCluster();
  StSsdCluster* getNextCluster();  

  StSsdCluster* giveCopy(); 
  void          copyTo(StSsdCluster *ptrClone);
  void          update(StSsdStrip *ptr,Float_t rWeight);
  
private:
  Int_t           mNCluster;
  Int_t           mFirstStrip;
  Int_t           mClusterSize;
  Int_t           mTotAdc;
  Int_t           mFirstAdc;
  Int_t           mLastAdc;
  Float_t         mTotNoise;
  Float_t         mStripMean;
  Int_t           mFlag;
  Int_t          *mIdMcHit;

  StSsdCluster *mPrevCluster;
  StSsdCluster *mNextCluster;
};

#endif
