/***************************************************************************
 *
 * $Id: StRichPIDAlgorithm.h,v 2.0 2000/08/09 16:26:19 gans Exp $
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
#include "StEvent/StFunctional.h"
#include "StEvent/StEnumerations.h"
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


