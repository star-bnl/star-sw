/***************************************************************************
 *
 * $Id: StRichPIDAlgorithm.cxx,v 2.0 2000/08/09 16:26:19 gans Exp $
 *
 * Author: Matt Horsley, apr 8,2000
 ***************************************************************************
 *
 * Description:
 **************************************************************************/
#include <typeinfo>
#include <math.h>
#include "StRichPIDAlgorithm.h"
#include "StEvent/StTrack.h"
#include "StRichPIDTraits.h"
#include "StEvent/StTrackGeometry.h"

static const char rcsid[] = "$Id: StRichPIDAlgorithm.cxx,v 2.0 2000/08/09 16:26:19 gans Exp $";

StRichPIDAlgorithm::StRichPIDAlgorithm() {
  
  pionminus   = StPionMinus::instance();
  kaonminus   = StKaonMinus::instance();
  electron    = StElectron::instance();
  antiproton  = StAntiProton::instance();

  pionplus   = StPionPlus::instance();
  kaonplus   = StKaonPlus::instance();
  positron   = StPositron::instance();
  proton     = StProton::instance();  

}


StParticleDefinition*
StRichPIDAlgorithm::operator() (const StTrack& track, const StSPtrVecTrackPidTraits& vec) {

    mostLikelyCandidate = 0;    
    return  mostLikelyCandidate;
}


const StRichPIDTraits*
StRichPIDAlgorithm::traits() const { return mElTraits; }
 
StRichPIDTraits*
StRichPIDAlgorithm::getTrait(StParticleDefinition* part) {
  if (part==electron || part==positron)  return mElTraits;
  if (part==pionminus || part==pionplus) return mPiTraits;
  if (part==kaonminus || part==kaonplus) return mKaTraits;
  if (part==antiproton || part==proton)  return mPrTraits;  
  return 0;
}

StParticleDefinition* 
StRichPIDAlgorithm::getMostLikelyCandidate() {
  return mostLikelyCandidate;
}


StParticleDefinition* 
StRichPIDAlgorithm::getSecondMostLikelyCandidate() {
  return secondMostLikelyCandidate;
}


