//$Id: StSstCluster.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstCluster.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTCLUSTER_HH
#define STSSTCLUSTER_HH
#include "StSstStrip.hh"

class StSstCluster
{
 public:
  StSstCluster(Int_t rNCluster);
  StSstCluster(Int_t rNCluster, Int_t rFirstStrip, Int_t rClusterSize, Int_t rTotAdc, Int_t rFirstAdc, Int_t rLastAdc, Float_t rTotNoise, Float_t rStripMean,  Int_t rFlag, Int_t *rMcHit);
  StSstCluster(const StSstCluster & originalCluster);
  ~StSstCluster();

  StSstCluster& operator=(const StSstCluster originalCluster);

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
  void          setPrevCluster(StSstCluster *rPrevCluster);
  void          setNextCluster(StSstCluster *rNextCluster);

  Int_t         getNCluster();
  Int_t         getFirstStrip();
  Int_t         getClusterSize();
  Int_t         getTotAdc();
  Int_t         getFirstAdc();
  Int_t         getLastAdc();
  Float_t       getTotNoise();
  Float_t       getStripMean();
  Int_t         getFlag();
  Int_t         getIdMcHit(Int_t iR);

  StSstCluster* getPrevCluster();
  StSstCluster* getNextCluster();  

  StSstCluster* giveCopy(); 
  void          copyTo(StSstCluster *ptrClone);
  void          update(StSstStrip *ptr,Float_t rWeight);
  
private:
  Int_t         mNCluster;
  Int_t         mFirstStrip;
  Int_t         mClusterSize;
  Int_t         mTotAdc;
  Int_t         mFirstAdc;
  Int_t         mLastAdc;
  Float_t       mTotNoise;
  Float_t       mStripMean;
  Int_t         mFlag;
  Int_t        *mIdMcHit;

  StSstCluster *mPrevCluster;
  StSstCluster *mNextCluster;
};

#endif
