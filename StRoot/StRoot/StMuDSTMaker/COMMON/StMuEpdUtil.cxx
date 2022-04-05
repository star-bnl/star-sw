/****************************************************************
 *
 * Author: Mike Lisa
 *
 *****************************************************************
 *
 * Description:
 * Convert StEpdHitCollection in StEvent to 
 * StMuEpdHitCollection in MuDst
 *
 *****************************************************************
 *
 * patterned after StMuBTofUtil.cxx
 *
 *
 ****************************************************************/
#include "StEvent.h"
#include "StTrack.h"
#include "StEpdCollection.h"
#include "StEpdHit.h"
#include "StMuEpdUtil.h"
#include "StMuEpdHitCollection.h"

ClassImp(StMuEpdUtil)

StMuEpdUtil::StMuEpdUtil()
{ }

StMuEpdUtil::~StMuEpdUtil()
{ }

StMuEpdHitCollection* StMuEpdUtil::getMuEpdHit(StEpdCollection *epdcol)
{
  if(!epdcol) return NULL;
  StMuEpdHitCollection* muEpdHit = new StMuEpdHitCollection();
  fillMuEpdHit(muEpdHit, epdcol);
  return muEpdHit;
}

void StMuEpdUtil::fillMuEpdHit(StMuEpdHitCollection* muEpdHit, StEpdCollection* epdcol)
{
  if( !epdcol )return;
  if(!muEpdHit) return;

  if( epdcol->hitsPresent() ) {
    StSPtrVecEpdHit &epdHits = epdcol->epdHits();
    
    for(size_t i=0; i < epdHits.size(); i++) {
      StEpdHit *aHit = epdHits[i];
      if(!aHit) continue;
      StMuEpdHit* epdMuHit = new StMuEpdHit(aHit); 
      muEpdHit->push_back(epdMuHit);
    }  
  }
  return;
}
