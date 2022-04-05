/***************************************************************************
 *
 * $Id: StSvtAnalysedHybridClusters.hh,v 1.10 2006/05/08 13:52:10 fisyak Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Analysis Object BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysedHybridClusters.hh,v $
 * Revision 1.10  2006/05/08 13:52:10  fisyak
 * Fill StSvtHits directly into StEvent (if it exists), add local coordinate, add handle for drift velocity hack correction
 *
 * Revision 1.9  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.8  2003/04/05 22:36:22  caines
 * Fix filling on local coords so its time and anode not cm
 *
 * Revision 1.7  2002/01/05 21:46:00  caines
 * Include t0 correction in hit
 *
 * Revision 1.6  2001/11/12 22:58:01  caines
 * Add functions for filling hits from srs data
 *
 * Revision 1.5  2001/08/07 20:52:15  caines
 * Implement better packing of svt hardware and charge values
 *
 * Revision 1.4  2001/05/04 14:20:05  caines
 * Improved historgramming
 *
 * Revision 1.3  2001/04/25 18:23:21  perev
 * HPcorrs
 *
 * Revision 1.2  2000/08/24 04:27:56  caines
 * Fixed casting warnings so compiles without errors on linux
 *
 * Revision 1.1  2000/08/21 13:04:29  caines
 * First version of hit fitting routines
 *
 * Revision
 *
 **************************************************************************/
#ifndef STSVTANALYSEDHYBRIDCLUSTERS_HH
#define STSVTANALYSEDHYBRIDCLUSTERS_HH
#include <string.h>
#include "StSvtClassLibrary/StSvtHybridObject.hh" 
#include "StarClassLibrary/StThreeVector.hh"
#include "StEvent/StSvtHit.h"

class StSvtAnalysis;
class scs_spt_st;
class StSvtWaferCoordinate;

typedef struct StSvtHitData
{
 
 int id;
 int id_cluster;
 int id_globtrk;
 int id_match;
 int id_track;
 int id_simtrk;  

 int peakAdc;
 int numOfAnodesInClu;
 int numOfPixelsInClu;
 double mom2[2];
  float uv[2];
  float anode;
  float timeb;
 StSvtHitData(){memset(this,0,sizeof(StSvtHitData));}
} StSvtHitData;



class StSvtAnalysedHybridClusters : public StSvtHybridObject
{
 public:

  StSvtAnalysedHybridClusters(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtAnalysedHybridClusters();
  
  void setMembers(int numOfClu, int index);
  int setSvtHit(StSvtAnalysis* mSvtAnalysis, float T0Jitter);
  int setSvtHit(scs_spt_st* mSrsHit, StSvtWaferCoordinate* WaferCoord);

  StSvtHitData* svtHitData();
  StSvtHit* svtHit();
  void ReSize();
  int numOfHits();
  StThreeVector<double>* WaferPosition();

  

 private:
    
    int mNumOfHits;
    unsigned int mHardWarePosition;
    StSvtHitData* mSvtHitData;     //!
    StSvtHit* mSvtHit;          //!   
    StThreeVector<double>* mPos;        //!      

};

inline StSvtHitData* StSvtAnalysedHybridClusters::svtHitData(){ return  mSvtHitData;}
inline StSvtHit* StSvtAnalysedHybridClusters::svtHit(){ return mSvtHit;}
inline int StSvtAnalysedHybridClusters::numOfHits(){ return mNumOfHits;}
inline StThreeVector<double>* StSvtAnalysedHybridClusters::WaferPosition(){ return mPos;}
#endif



