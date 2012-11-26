/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header.
 *
 */

// z location of the EEMC SMD layer in cm
#define SMDZ 279.542

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcParticleCandidate.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcHit.h"

EEmcParticleCandidate_t::EEmcParticleCandidate_t( Int_t hitIdx_, const EEmcHit_t& hit, TVector3& vertex ) : 
   PID(1000), hitIdx1(hitIdx_), E(hit.eTow), M(0), PT(0),
   position( hit.x, hit.y, SMDZ ),
   momentum( (position-vertex).Unit()*E )
{
   // invalid vertex, assume vertex at the origin
   if( vertex.Z() < -300 )
      momentum = position.Unit()*E;

   // do this after above check
   PT = momentum.Perp();
};

ClassImp( EEmcParticleCandidate_t );

/*
 * $Id: EEmcParticleCandidate.cxx,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcParticleCandidate.cxx,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
