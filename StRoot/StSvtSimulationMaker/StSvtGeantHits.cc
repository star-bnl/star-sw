/***************************************************************************
 *
 * $Id: StSvtGeantHits.cc,v 1.1 2000/11/30 20:47:48 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: keep geant hits for evaluation in hit maker
 *
 ***************************************************************************
 *
 * $Log: StSvtGeantHits.cc,v $
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#include "StSvtGeantHits.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"

ClassImp(StSvtGeantHits)

StSvtGeantHits::StSvtGeantHits(int barrel, int ladder, int wafer, int hybrid):StSvtHybridObject(barrel,ladder,wafer,hybrid)
{
 mNumOfHits = 0;
 mWaferCoord = new StSvtWaferCoordinate[MAX_HITS];
}

StSvtGeantHits::~StSvtGeantHits()
{}

void  StSvtGeantHits::setNumOfHits(int nhits)
{
 mNumOfHits = nhits;
}

void  StSvtGeantHits::setGeantHit(int index ,StSvtWaferCoordinate& waferCoord)
{
  if (index > MAX_HITS)
    return;

  mWaferCoord[index].setLayer(waferCoord.layer());
  mWaferCoord[index].setLadder(waferCoord.ladder());
  mWaferCoord[index].setWafer(waferCoord.wafer());
  mWaferCoord[index].setHybrid(waferCoord.hybrid());
  mWaferCoord[index].setAnode(waferCoord.anode());
  mWaferCoord[index].setTimeBucket(waferCoord.timebucket());
} 
