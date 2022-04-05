/****************************************************************
 * $Id: StMuBTofUtil.cxx,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Feb 2009
 *
 *****************************************************************
 *
 * Description:
 * Convert StBTofHitCollection in StEvent to 
 * StMuBTofHitCollection in MuDst
 *
 *****************************************************************
 *
 * $Log: StMuBTofUtil.cxx,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 *
 ****************************************************************/
#include "StEvent.h"
#include "StTrack.h"
#include "StBTofCollection.h"
#include "StBTofHit.h"
#include "StMuBTofUtil.h"
#include "StMuBTofHitCollection.h"

ClassImp(StMuBTofUtil)

StMuBTofUtil::StMuBTofUtil()
{ }

StMuBTofUtil::~StMuBTofUtil()
{ }

StMuBTofHitCollection* StMuBTofUtil::getMuBTofHit(StBTofCollection *tofcol)
{
  if(!tofcol) return NULL;
  StMuBTofHitCollection* muTofHit = new StMuBTofHitCollection();
  fillMuBTofHit(muTofHit, tofcol);
  return muTofHit;
}

void StMuBTofUtil::fillMuBTofHit(StMuBTofHitCollection* muTofHit, StBTofCollection* tofcol)
{
  if( !tofcol || (!tofcol->rawHitsPresent()) ) return;
  if(!muTofHit) return;

  if( tofcol->hitsPresent() ) {
    StSPtrVecBTofHit &tofHits = tofcol->tofHits();
    
    for(size_t i=0; i < tofHits.size(); i++) {
      StBTofHit *aHit = tofHits[i];
      if(!aHit) continue;
      StMuBTofHit* tofMuHit = new StMuBTofHit(aHit); 
      muTofHit->push_back(tofMuHit);
    }  
  }
  return;
}
