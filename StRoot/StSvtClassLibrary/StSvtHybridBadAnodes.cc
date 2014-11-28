/***************************************************************************
 *
 * $Id: StSvtHybridBadAnodes.cc,v 1.1 2002/02/15 22:44:06 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 *
 ***************************************************************************
 *
 * Description: Flags Bad anodes on hybrids
 *
 ***************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                         // 
// It represents the BadAnodes on a hybrid data.                           //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtHybridBadAnodes.hh"

StSvtHybridBadAnodes::StSvtHybridBadAnodes()
{
  mNBadAnodes = 0;

  for (int i=0;i<MAX_NUMBER_OF_ANODES;i++) {
    mBadAnode[i] = 0;
    mOverloadedAdc[i] = 0;
    mNullAdc[i] = 0;
    mHighOccup[i] = 0;
    mBadRMS[i] = 0;
  }
}

StSvtHybridBadAnodes::StSvtHybridBadAnodes(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  mNBadAnodes = 0;

  for (int i=0;i<MAX_NUMBER_OF_ANODES;i++) {
    mBadAnode[i] = 0;
    mOverloadedAdc[i] = 0;
    mNullAdc[i] = 0;
    mHighOccup[i] = 0;
    mBadRMS[i] = 0;
  }
}

StSvtHybridBadAnodes::~StSvtHybridBadAnodes()
{}

void StSvtHybridBadAnodes::setBadAnode(int anode)
{
  if (!mBadAnode[anode-1]) {
    mBadAnode[anode-1] = kTRUE;
    mNBadAnodes++;
  }
}

void StSvtHybridBadAnodes::setNotBadAnode(int anode)
{
  if (mBadAnode[anode-1]) {
    mBadAnode[anode-1] = kFALSE;
    mNBadAnodes--;
  }
}

Bool_t StSvtHybridBadAnodes::isBadAnode(int anode)
{ 
  return mBadAnode[anode-1];
}

void StSvtHybridBadAnodes::addOverloadedAdc(int anode, Bool_t isBad, int nEvents)
{
  if (isBad) {
    mOverloadedAdc[anode-1] = (mOverloadedAdc[anode-1]*(nEvents-1) + 1)/nEvents;
    setBadAnode(anode);
  }
  else
    mOverloadedAdc[anode-1] = (mOverloadedAdc[anode-1]*(nEvents-1))/nEvents;
}

void StSvtHybridBadAnodes::addNullAdc(int anode, Bool_t isBad, int nEvents)
{
  if (isBad) {
    mNullAdc[anode-1] = (mNullAdc[anode-1]*(nEvents-1) + 1)/nEvents;
    setBadAnode(anode);
  }
  else
    mNullAdc[anode-1] = (mNullAdc[anode-1]*(nEvents-1))/nEvents;
}

void StSvtHybridBadAnodes::addHighOccup(int anode, Bool_t isBad, int nEvents)
{
  if (isBad) {
    mHighOccup[anode-1] = (mHighOccup[anode-1]*(nEvents-1) + 1)/nEvents;
    setBadAnode(anode);
  }
  else
    mHighOccup[anode-1] = (mHighOccup[anode-1]*(nEvents-1))/nEvents;
}

void StSvtHybridBadAnodes::addBadRMS(int anode, Bool_t isBad, int nEvents)
{
  if (isBad) {
    mBadRMS[anode-1] = (mBadRMS[anode-1]*(nEvents-1) + 1)/nEvents;
    setBadAnode(anode);
  }
  else
    mBadRMS[anode-1] = (mBadRMS[anode-1]*(nEvents) + 1)/nEvents;
}

void StSvtHybridBadAnodes::reset()
{
  mNBadAnodes = 0;

  for (int i=0;i<MAX_NUMBER_OF_ANODES;i++) {
    mBadAnode[i] = 0;
    mOverloadedAdc[i] = 0;
    mNullAdc[i] = 0;
    mHighOccup[i] = 0;
    mBadRMS[i] = 0;
  }
}
