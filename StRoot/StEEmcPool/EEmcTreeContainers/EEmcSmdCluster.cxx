/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header.
 *
 */

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcSmdCluster.h"

EEmcSmdCluster_t::EEmcSmdCluster_t() : meanPos(0), width(0), energy(0), sector(0), inLayerV(0),
                                       seedStripIdx(-1), numUsedStrips(0) {

   for( Int_t i = 0; i< kMaxClusterSize; ++i ){
      usedStripIdx[i] = -1;
      usedStripWeight[i] = 0;
   };

};

void EEmcSmdCluster_t::Clear( const Option_t* ){
   meanPos = width = energy = 0;
   sector = 0;
   inLayerV = 0;

   seedStripIdx = -1;
   numUsedStrips = 0;

   for( Int_t i = 0; i< kMaxClusterSize; ++i ){
      usedStripIdx[i] = -1;
      usedStripWeight[i] = 0;
   };
};

ClassImp( EEmcSmdCluster_t );

/*
 * $Id: EEmcSmdCluster.cxx,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcSmdCluster.cxx,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
