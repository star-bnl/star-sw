/***************************************************************************
 *
 * $Id: StSvtGeantHits.cc,v 1.7 2005/07/23 03:37:34 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: keep geant hits for evaluation in hit maker
 *
 ***************************************************************************
 *
 * $Log: StSvtGeantHits.cc,v $
 * Revision 1.7  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.6  2004/03/30 21:27:12  caines
 * Remove asserts from code so doesnt crash if doesnt get parameters it just quits with kStErr
 *
 * Revision 1.4  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 * Revision 1.3  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
 * Revision 1.2  2001/03/19 22:25:52  caines
 * Catch wrong wafer ids more elegantly
 *
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#include <string.h>
#include "StSvtGeantHits.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"

ClassImp(StSvtGeantHits)

StSvtGeantHits::StSvtGeantHits(int barrel, int ladder, int wafer, int hybrid):StSvtHybridObject(barrel,ladder,wafer,hybrid)
{
 mNumOfHits = 0;
 mPeak        = new float[MAX_HITS];
 mWaferCoord  = new StSvtWaferCoordinate[MAX_HITS];
 mGlobalCoord = new StGlobalCoordinate[MAX_HITS];
 mLocalCoord  = new StSvtLocalCoordinate[MAX_HITS];
 mIdTrack     = new int[MAX_HITS];

 memset(mPeak   ,0,sizeof(mPeak   [0])*MAX_HITS);
 memset(mIdTrack,0,sizeof(mIdTrack[0])*MAX_HITS);

}

StSvtGeantHits::~StSvtGeantHits()
{
  delete [] mWaferCoord;
  delete [] mGlobalCoord;
  delete [] mLocalCoord;
  delete [] mIdTrack;
}

void  StSvtGeantHits::setNumOfHits(int nhits)
{
 mNumOfHits = nhits;
}

void  StSvtGeantHits::setPeak(int index ,float peak)
{
 mPeak[index] = peak;
}

void  StSvtGeantHits::setGeantHit(int index ,int* svtAtt, float* AnTime)
{
  if (index > MAX_HITS)
    return;

  mWaferCoord[index].setLayer(svtAtt[1]);
  mWaferCoord[index].setLadder(svtAtt[2]);
  mWaferCoord[index].setWafer(svtAtt[3]);
  mWaferCoord[index].setHybrid(svtAtt[4]);
  mWaferCoord[index].setAnode(AnTime[0]);
  mWaferCoord[index].setTimeBucket(AnTime[1]);
} 

void  StSvtGeantHits::setGeantHit(int index ,StSvtWaferCoordinate* waferCoord)
{
  if (index > MAX_HITS)
    return;

  mWaferCoord[index].setLayer(waferCoord->layer());
  mWaferCoord[index].setLadder(waferCoord->ladder());
  mWaferCoord[index].setWafer(waferCoord->wafer());
  mWaferCoord[index].setHybrid(waferCoord->hybrid());
  mWaferCoord[index].setAnode(waferCoord->anode());
  mWaferCoord[index].setTimeBucket(waferCoord->timebucket());
} 
void  StSvtGeantHits::setTrackId(int index ,int idtrk)
{
  mIdTrack[index] = idtrk;
}

int StSvtGeantHits::getTrackId(int index)
{
  return mIdTrack[index];
}

void StSvtGeantHits::setGlobalCoord( int index, StThreeVector<double>* x ){

  mGlobalCoord[index].setPosition(*x);
}

void StSvtGeantHits::setLocalCoord( int index, StThreeVector<double>* x ){

  mLocalCoord[index].setPosition(*x);
}
