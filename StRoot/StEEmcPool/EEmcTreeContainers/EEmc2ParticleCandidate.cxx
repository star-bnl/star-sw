/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header.
 *
 */

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmc2ParticleCandidate.h"

#include <cmath>
#include <iostream>
using std::cerr;
using std::endl;

EEmc2ParticleCandidate_t::EEmc2ParticleCandidate_t( const EEmcParticleCandidate_t& childA, const EEmcParticleCandidate_t& childB ){
   const EEmcParticleCandidate_t* child1 = &childA;
   const EEmcParticleCandidate_t* child2 = &childB;

   // sort by higher energy
   if( child1->E < child2->E ){
      child1 = &childB;
      child2 = &childA;
   };

   PID = 2000;
   hitIdx1 = child1->hitIdx1;
   hitIdx2 = child2->hitIdx1;

   E = child1->E + child2->E;
   Z = E ? ( child1->E - child2->E ) / E : 0;
   position = ( child1->position + child2->position ) * 0.5;
   momentum = ( child1->momentum + child2->momentum );
   D = ( child1->position - child2->position ).Mag();  // assume same Z pos

   PT = momentum.Perp();
   M = E*E - momentum.Mag2();
   M = ( M > 0 ? sqrt(M) : -sqrt(-M) );
};

ClassImp( EEmc2ParticleCandidate_t );

/*
 * $Id: EEmc2ParticleCandidate.cxx,v 1.1 2012/11/26 19:04:29 sgliske Exp $
 * $Log: EEmc2ParticleCandidate.cxx,v $
 * Revision 1.1  2012/11/26 19:04:29  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
