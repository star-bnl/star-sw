/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header.
 *
 */

#include <Rtypes.h>
#include <TObject.h>

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcHit.h"

EEmcHit_t::EEmcHit_t() : uClusIdx(-1), vClusIdx(-1), x(0), y(0), eta(0), phi(0), eTow(0), ePost(0), ePre1(0), ePre2(0), 
                         centralTowerIdx(-1), numUsedTowers(0) {

   for( Int_t i=0; i<kMaxNumTowers; ++i ){
      usedTowerIdx[i] = -1;
      usedTowerWeight[i] = 0;
   };
};

void EEmcHit_t::Clear( const Option_t* ){
   uClusIdx = -1;
   vClusIdx = -1;
   x = y = eta = phi = 0;
   eTow = ePre1 = ePre2 = ePost = 0;

   centralTowerIdx = -1;
   numUsedTowers = 0;

   for( Int_t i=0; i<kMaxNumTowers; ++i ){
      usedTowerIdx[i] = -1;
      usedTowerWeight[i] = 0;
   };
};

ClassImp( EEmcHit_t );

/*
 * $Id: EEmcHit.cxx,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcHit.cxx,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
