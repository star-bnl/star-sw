/***************************************************************************
 *
 * $Id: StSvtAnalysedHybridClusters.hh,v 1.2 2000/08/24 04:27:56 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Analysis Object BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysedHybridClusters.hh,v $
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

#include "StSvtClassLibrary/StSvtHybridObject.hh" 
#include "StarClassLibrary/StThreeVector.hh"
#include "StSvtHit.h"

class StSvtAnalysis;


typedef struct StSvtHitData
{
 
 int id;
 int id_cluster;
 int id_globtrk;
 int id_match;
 int id_track;

 int peakAdc;
 int numOfAnodesInClu;
 int numOfPixelsInClu;
    
 double mom2[2];

} StSvtHitData;



class StSvtAnalysedHybridClusters : public StSvtHybridObject
{
 public:

  StSvtAnalysedHybridClusters(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtAnalysedHybridClusters();
  
  void setMembers(int numOfClu, int wafer, int ladder);
  int setSvtHit(StSvtAnalysis* mSvtAnalysis);

  StSvtHitData* svtHitData();
  StSvtHit* svtHit();
  int numOfHits();
  StThreeVector<double>* WaferPosition();
  

 private:
    
    int mNumOfHits;
    ulong mHardWarePosition;
    StSvtHitData* mSvtHitData;     //!
    StSvtHit* mSvtHit;          //!   
    StThreeVector<double>* mPos;        //!      

};

inline StSvtHitData* StSvtAnalysedHybridClusters::svtHitData(){ return  mSvtHitData;}
inline StSvtHit* StSvtAnalysedHybridClusters::svtHit(){ return mSvtHit;}
inline int StSvtAnalysedHybridClusters::numOfHits(){ return mNumOfHits;}
inline StThreeVector<double>* StSvtAnalysedHybridClusters::WaferPosition(){ return mPos;}
#endif



