/***************************************************************************
 *
 * $Id: StSvtAnalysedHybridClusters.cc,v 1.4 2001/11/12 22:58:06 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Analysis Object BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysedHybridClusters.cc,v $
 * Revision 1.4  2001/11/12 22:58:06  caines
 * Add functions for filling hits from srs data
 *
 * Revision 1.3  2001/08/07 20:52:15  caines
 * Implement better packing of svt hardware and charge values
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
#include <iostream.h>
#include <math.h>

#include "StSvtAnalysis.hh"
#include "StSvtAnalysedHybridClusters.hh"
#include "tables/St_scs_spt_Table.h"


StSvtAnalysedHybridClusters::StSvtAnalysedHybridClusters(int barrel, int ladder, int wafer, int hybrid):StSvtHybridObject(barrel,ladder,wafer,hybrid)
{ 
 mNumOfHits = 0;
 mSvtHitData = NULL;
 mSvtHit = NULL;

}

StSvtAnalysedHybridClusters::~StSvtAnalysedHybridClusters()
{ 
 delete [] mSvtHitData;
 delete [] mSvtHit;
 delete [] mPos;
}

void StSvtAnalysedHybridClusters::setMembers(int numOfClu, int index)
{

  // If numOfClu small default to making arrays of size 10
 
  if( numOfClu < 10) mNumOfHits = 10; 
  mHardWarePosition = index;
  mSvtHitData = new StSvtHitData[mNumOfHits];
  mSvtHit = new StSvtHit[mNumOfHits];
  mPos = new StThreeVector<double>[mNumOfHits];
  // But remember actual number to date

  mNumOfHits = numOfClu;

}

void StSvtAnalysedHybridClusters::ReSize(){

 int hitSize = mNumOfHits+10; 
 StSvtHitData *tmp = new StSvtHitData[hitSize];
 for (int i=0; i<(mNumOfHits); i++) tmp[i] = mSvtHitData[i];
 delete mSvtHitData;
 mSvtHitData = tmp; 

 StSvtHit *tmp2 = new StSvtHit[hitSize];
 for (int i=0; i<(mNumOfHits); i++) tmp2[i] = mSvtHit[i];
 delete mSvtHit;
 mSvtHit = tmp2; 

 StThreeVector<double> *tmp3 = new StThreeVector<double>[hitSize];
 for (int i=0; i<(mNumOfHits-1); i++) tmp3[i] = mPos[i];
 delete [] mPos;
 mPos = tmp3; 
}

int StSvtAnalysedHybridClusters::setSvtHit(StSvtAnalysis* mSvtAnalysis)
{

 StThreeVectorF mGlobalPos;

 for(int hit = 0; hit < mNumOfHits; hit++)
  {
    mSvtHitData[hit].id = mSvtAnalysis->GetCluID(hit);
    mSvtHitData[hit].id_cluster = mSvtAnalysis->GetCluDeconvID(hit);
    mSvtHitData[hit].id_globtrk = 0;
    mSvtHitData[hit].id_track = 0;
    mSvtHitData[hit].id_match = 0;

    
    mSvtHit[hit].setFlag((unsigned char)(mSvtAnalysis->GetCluFlag(hit)));
    mSvtHit[hit].setHardwarePosition((long)mHardWarePosition<<4);
    mSvtHit[hit].setCharge(mSvtAnalysis->GetCluCharge(hit));
    
   
    mPos[hit].setX((float)mSvtAnalysis->GetMeanClusterTimeBin(hit));
    mPos[hit].setY((float)mSvtAnalysis->GetMeanClusterAnode(hit));
    mPos[hit].setZ(0.0);
    
    mGlobalPos.setX(sqrt(mSvtAnalysis->GetCluXCov(hit)));
    mGlobalPos.setY(sqrt(mSvtAnalysis->GetCluYCov(hit)));
    mGlobalPos.setZ(0.0042);

    mGlobalPos.setX(0.1);
    mGlobalPos.setY(0.1);
    mGlobalPos.setZ(0.1);
    
    mSvtHit[hit].setPositionError(mGlobalPos); 

    
    mGlobalPos.setX(-9999.0);
    mGlobalPos.setY(-9999.0);
    mGlobalPos.setZ(-9999.0);
    mSvtHit[hit].setPosition(mGlobalPos);  //invokes StMeasuredPoint::setPosition(StThreeVectorF&)


    mSvtHitData[hit].peakAdc = mSvtAnalysis->GetCluPeakAdc(hit);
    mSvtHitData[hit].numOfAnodesInClu = mSvtAnalysis->GetCluNumAnodes(hit);
    mSvtHitData[hit].numOfPixelsInClu = mSvtAnalysis->GetCluNumPixels(hit);
    mSvtHitData[hit].mom2[0] = mSvtAnalysis->GetSecondMomClusterTimeBin(hit);
    mSvtHitData[hit].mom2[1] = mSvtAnalysis->GetSecondMomClusterAnode(hit);

   }  


return 0;
}


int StSvtAnalysedHybridClusters::setSvtHit(scs_spt_st* mSrsHit)
{

  StThreeVectorF mGlobalPos;

  mSvtHitData[mNumOfHits].id = mSrsHit->id;
  mSvtHitData[mNumOfHits].id_cluster = 0;
  mSvtHitData[mNumOfHits].id_globtrk = 0;
  mSvtHitData[mNumOfHits].id_track = 0;
  mSvtHitData[mNumOfHits].id_match = 0;
  
  
  mSvtHit[mNumOfHits].setFlag((unsigned char)(mSrsHit->flag));
  mSvtHit[mNumOfHits].setHardwarePosition(mHardWarePosition<<4);
  mSvtHit[mNumOfHits].setCharge(mSrsHit->de[0]);
  
  
  mPos[mNumOfHits].setX(mSrsHit->xl[0]);
  mPos[mNumOfHits].setY(mSrsHit->xl[1]);
  mPos[mNumOfHits].setZ(mSrsHit->xl[2]);
  
  mGlobalPos.setX(mSrsHit->res[0]);
  mGlobalPos.setY(mSrsHit->res[1]);
  mGlobalPos.setZ(mSrsHit->res[2]);
  
  mSvtHit[mNumOfHits].setPositionError(mGlobalPos); 
  
  
  mGlobalPos.setX(mSrsHit->x[0]);
  mGlobalPos.setY(mSrsHit->x[1]);
  mGlobalPos.setZ(mSrsHit->x[2]);
  mSvtHit[mNumOfHits].setPosition(mGlobalPos);  //invokes StMeasuredPoint::setPosition(StThreeVectorF&)
  
  
  mSvtHitData[mNumOfHits].peakAdc = 0;
  mSvtHitData[mNumOfHits].numOfAnodesInClu = 0;
  mSvtHitData[mNumOfHits].numOfPixelsInClu = 0;
  mSvtHitData[mNumOfHits].mom2[0] = mSrsHit->mom2[0];
  mSvtHitData[mNumOfHits].mom2[1] = mSrsHit->mom2[1];
  
  mNumOfHits++;
  return 0;
}
