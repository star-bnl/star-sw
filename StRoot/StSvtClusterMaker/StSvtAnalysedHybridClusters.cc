/***************************************************************************
 *
 * $Id: StSvtAnalysedHybridClusters.cc,v 1.3 2001/08/07 20:52:15 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Analysis Object BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysedHybridClusters.cc,v $
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

 mNumOfHits = numOfClu;
 mHardWarePosition = index;
 mSvtHitData = new StSvtHitData[mNumOfHits];
 mSvtHit = new StSvtHit[mNumOfHits];
 mPos = new StThreeVector<double>[mNumOfHits];
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

