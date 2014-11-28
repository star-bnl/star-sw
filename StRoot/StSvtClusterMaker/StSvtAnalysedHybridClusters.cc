/***************************************************************************
 *
 * $Id: StSvtAnalysedHybridClusters.cc,v 1.16 2009/11/23 16:44:55 fisyak Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Analysis Object BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysedHybridClusters.cc,v $
 * Revision 1.16  2009/11/23 16:44:55  fisyak
 * Remove references to tables
 *
 * Revision 1.15  2005/11/09 22:08:36  fisyak
 * Use for IdTruth id_mctrack (instead of id_mchit)
 *
 * Revision 1.14  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.13  2004/11/24 02:41:35  jeromel
 * Minor protection issue on delete
 *
 * Revision 1.12  2004/01/27 02:30:29  perev
 * LeakOff
 *
 * Revision 1.11  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.10  2003/07/17 22:49:31  caines
 * Change errors to 300 microns
 *
 * Revision 1.9  2003/04/05 22:36:10  caines
 * Fix filling on local coords so its time and anode not cm
 *
 * Revision 1.8  2002/05/08 23:07:54  caines
 * T0 jitter stuff
 *
 * Revision 1.7  2002/05/08 16:03:52  caines
 * Fix again memory leak - accidentally pput back in data has to be a const() not data()
 *
 * Revision 1.6  2002/01/05 21:45:56  caines
 * Include t0 correction in hit
 *
 * Revision 1.5  2001/11/21 19:02:43  caines
 * Set mNumOfHits properly as for real data I was setting arrays to size 0 then filling
 *
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
#include <Stiostream.h>
#include <math.h>
#include <assert.h>

#include "StSvtAnalysis.hh"
#include "StSvtAnalysedHybridClusters.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "tables/St_scs_spt_Table.h"

StSvtAnalysedHybridClusters::StSvtAnalysedHybridClusters(int barrel, int ladder, int wafer, int hybrid):StSvtHybridObject(barrel,ladder,wafer,hybrid)
{ 
 mNumOfHits        = 0;
 mHardWarePosition = 0;
 mSvtHitData       = 0;
 mSvtHit           = 0;
 mPos              = 0;
}

StSvtAnalysedHybridClusters::~StSvtAnalysedHybridClusters()
{ 
 if (mSvtHitData) delete [] mSvtHitData;
 if (mSvtHit)     delete [] mSvtHit;
 if (mPos)        delete [] mPos;
}

void StSvtAnalysedHybridClusters::setMembers(int numOfClu, int index)
{

  int HitSize;

  // If numOfClu small default to making arrays of size 10
  // But remember how many you really have 
  mNumOfHits = numOfClu;

  HitSize = ( numOfClu < 10) ? 10 : mNumOfHits; 
  mHardWarePosition = index;
  assert( !mSvtHitData && !mSvtHit && !mPos);
   
  //cout << "DEBUG:: mSvtHitData" << (int *) mSvtHitData << endl;
  //cout << "DEBUG:: mSvtHit    " << (int *) mSvtHit     << endl;
  //cout << "DEBUG:: mPos       " << (int *) mPos        << endl; 
   
  mSvtHitData = new StSvtHitData[HitSize];
  mSvtHit     = new StSvtHit[HitSize];
  mPos        = new StThreeVector<double>[HitSize];

}

void StSvtAnalysedHybridClusters::ReSize(){

 int hitSize = mNumOfHits+10; 
 StSvtHitData *tmp = new StSvtHitData[hitSize];
 for (int i=0; i<(mNumOfHits); i++) tmp[i] = mSvtHitData[i];
 delete [] mSvtHitData;
 mSvtHitData = tmp; 

 StSvtHit *tmp2 = new StSvtHit[hitSize];
 for (int i=0; i<(mNumOfHits); i++) tmp2[i] = mSvtHit[i];
 delete [] mSvtHit;
 mSvtHit = tmp2; 

 StThreeVector<double> *tmp3 = new StThreeVector<double>[hitSize];
 for (int i=0; i<(mNumOfHits-1); i++) tmp3[i] = mPos[i];
 delete [] mPos;
 mPos = tmp3; 
}

int StSvtAnalysedHybridClusters::setSvtHit(StSvtAnalysis* mSvtAnalysis,
					   float T0Jitter)
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
    mSvtHit[hit].setIdTruth(mSvtAnalysis->GetTruth(hit));
    
    // Take jitter out of t0 caused by SVT clock being 8 3rds of RHIC clock
    mPos[hit].setX((float)mSvtAnalysis->GetMeanClusterTimeBin(hit)+T0Jitter);
    mPos[hit].setY((float)mSvtAnalysis->GetMeanClusterAnode(hit));
    mPos[hit].setZ(0.0);
    
    mGlobalPos.setX(::sqrt(mSvtAnalysis->GetCluXCov(hit)));
    mGlobalPos.setY(::sqrt(mSvtAnalysis->GetCluYCov(hit)));
    mGlobalPos.setZ(0.0042);
    
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

int StSvtAnalysedHybridClusters::setSvtHit(scs_spt_st* mSrsHit, 
					   StSvtWaferCoordinate *WaferCoord)
{

  StThreeVectorF mGlobalPos;

  mSvtHitData[mNumOfHits].id = mSrsHit->id;
  mSvtHitData[mNumOfHits].id_cluster = 0;
  mSvtHitData[mNumOfHits].id_globtrk = 0;
  mSvtHitData[mNumOfHits].id_track = 0;
  mSvtHitData[mNumOfHits].id_match = 0;
  
  
  mSvtHit[mNumOfHits].setFlag((unsigned char)(mSrsHit->flag));
  mSvtHit[mNumOfHits].setHardwarePosition(mHardWarePosition<<4);
  mSvtHit[mNumOfHits].setCharge(mSrsHit->de[0]*300000); // put GEANT dE roughly into ADC counts
  mSvtHit[mNumOfHits].setIdTruth(mSrsHit->id_mctrack,100); // put truth
  
  mPos[mNumOfHits].setX(WaferCoord->timebucket());
  mPos[mNumOfHits].setY(WaferCoord->anode());
  mPos[mNumOfHits].setZ(0.0);
  
  mGlobalPos.setX(mSrsHit->x[0]);
  mGlobalPos.setY(mSrsHit->x[1]);
  mGlobalPos.setZ(mSrsHit->x[2]);
  mSvtHit[mNumOfHits].setPosition(mGlobalPos);  //invokes StMeasuredPoint::setPosition(StThreeVectorF&)
  
  
  mSvtHitData[mNumOfHits].peakAdc =(int) (mSvtHit[mNumOfHits].charge()/4.); //Put in a PEAK ADC value of quarter of charge
  mSvtHitData[mNumOfHits].numOfAnodesInClu = 0;
  mSvtHitData[mNumOfHits].numOfPixelsInClu = 0;
  mSvtHitData[mNumOfHits].mom2[0] = mSrsHit->mom2[0];
  mSvtHitData[mNumOfHits].mom2[1] = mSrsHit->mom2[1];
  
  mNumOfHits++;
  return 0;
}
