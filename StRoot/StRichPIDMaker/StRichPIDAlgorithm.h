/***************************************************************************
 *
 * $Id: StRichPIDAlgorithm.h,v 1.1 2000/05/19 19:09:15 horsley Exp $
 *
 * Author: Matt Horsley apr 8, 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#ifndef StRichPIDAlgorithm_hh
#define StRichPIDAlgorithm_hh

#include <vector>
#include "StFunctional.h"
#include "StEnumerations.h"
#include "StParticleDefinition.hh"
#include "StParticleTypes.hh"


#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StRichPIDTraits;

class StRichPIDAlgorithm : public StPidAlgorithm {
public:
  StRichPIDAlgorithm();
  StParticleDefinition*  operator() (const StTrack&, const StSPtrVecTrackPidTraits&);
  const  StRichPIDTraits* traits() const;
  StParticleDefinition* getMostLikelyCandidate();
  StParticleDefinition* getSecondMostLikelyCandidate();
  StRichPIDTraits*      getTrait(StParticleDefinition*);
    
private:
  
  StRichPIDTraits*        mElTraits;     //!
  StRichPIDTraits*        mPiTraits;     //!
  StRichPIDTraits*        mKaTraits;     //!
  StRichPIDTraits*        mPrTraits;     //!


  const StTrack*                mTrack;      //!
  StParticleDefinition*   mostLikelyCandidate;      //!
  StParticleDefinition*   secondMostLikelyCandidate;     //!


  StPionMinus*  pionminus;
  StKaonMinus*  kaonminus;
  StElectron*   electron;
  StAntiProton* antiproton;

  StPionPlus*  pionplus;
  StKaonPlus*  kaonplus;
  StPositron*  positron;
  StProton*    proton;




};
#endif


