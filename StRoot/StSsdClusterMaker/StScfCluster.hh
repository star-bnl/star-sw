// $Id: StScfCluster.hh,v 1.5 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScfCluster.hh,v $
// Revision 1.5  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.4  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:33  lmartin
// CVS tags added
//
#ifndef STSCFCLUSTER_HH
#define STSCFCLUSTER_HH
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StScfStrip.hh"

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
  float         getTotNoise();
  float         getStripMean();
  int           getIdMcHit(int iR);
  int           getIdMcTrack(int iR);
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
  void          setTotNoise(float rTotNoise);
  void          setStripMean(float rStripMean);
  void          setIdMcHit(int rIdMcHit, int iR);
  void          setIdMcTrack(int rIdMcTrack, int iR);
  void          update(StScfStrip *ptr,float rWeight,int iSide);
  void          copyTo(StScfCluster *ptrClone);
  
private:
  int           mNCluster;
  int           mFirstStrip;
  int           mClusterSize;
  int           mFlag;
  int           mTotAdc;
  int           mFirstAdc;
  int           mLastAdc;
  float         mTotNoise;
  float         mStripMean;
  int          *mIdMcHit;
  int          *mIdMcTrack;
  StScfCluster *mPrevCluster;
  StScfCluster *mNextCluster;
};

#endif
