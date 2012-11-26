/*
 * Created by S. Gliske, May 2012
 *
 * Description: Container used in the EEmcAnalysisTree.  Represents a
 * candidate parent particle, which decays (decayed) into two
 * candidate particles.
 *
 * Note: this does not explicitly depend on the STAR framework, and so
 * the trees can be read outside of the STAR framework.  Note: data
 * members are all public, to allow a lighter weight implementation. from
 * the TTree.
 *
 */

#ifndef EEmc2ParticleCandidate_H__
#define EEmc2ParticleCandidate_H__

#include <Rtypes.h>
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcParticleCandidate.h"

class EEmc2ParticleCandidate_t : public EEmcParticleCandidate_t {

 public:
   // constructor
   EEmc2ParticleCandidate_t();
   EEmc2ParticleCandidate_t( const EEmcParticleCandidate_t& childA, const EEmcParticleCandidate_t& childB );

   Int_t hitIdx2;
   Float_t Z, D;

 private:
   ClassDef( EEmc2ParticleCandidate_t, 2);
};

inline EEmc2ParticleCandidate_t::EEmc2ParticleCandidate_t() : EEmcParticleCandidate_t(), hitIdx2(hitIdx1) { /* */ };

#endif

/*
 * $Id: EEmc2ParticleCandidate.h,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmc2ParticleCandidate.h,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
