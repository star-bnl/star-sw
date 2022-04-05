/*
 * \class StEEmcEnergyApportionerIU_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See header file for description.
*/

#include <vector>
#include <Rtypes.h>

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"
#include "StRoot/StEEmcPool/StEEmcPointMaker/eeTowerFunction.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StRoot/StEEmcPool/StEEmcGeoId/StEEmcGeoId.h"

#include "StEEmcEnergyApportionerIU.h"
#include "StSimpleCluster.h"
#include "StEEmcHit.h"
#include "StESMDClustersPerSector.h"

StEEmcEnergyApportionerIU_t::StEEmcEnergyApportionerIU_t() : StEEmcEnergyApportioner_t(),
                                                             mCheckTowerBits(1),
                                                             weightFunc( &StEEmcEnergyApportionerIU_t::smdSumWeightFunc ){
   mIsReady = 1;
};

Int_t StEEmcEnergyApportionerIU_t::find( EEmcEnergy_t* eemcEnergyPtr,
                                         const StSimpleClusterVec_t& towerClusterVec,
                                         const StESMDClustersVec_t &smdClusterVec,
                                         StEEmcHitVec_t& hitVec ){
   if( !hitVec.empty() ){
      Int_t nInvalid = 0;

      // Weight per tower per hit.  Vector matches vector of hits.
      // The sparseVec is indexed by tower index, and holds the relative
      // contribution of each hit to the tower (i.e. non-normalized
      // weight).
      std::vector< sparseVec_t > wPerTowerPerHit( hitVec.size() );

      // temporary, to hold indexes of neighbors
      std::vector< Int_t > neighborIndices;

      // to hold denominators (i.e. map used as a sparse vector)
      sparseVec_t denominator;
      sparseVec_t::iterator denomIter, weightIter;

      // iterate over hits
      StEEmcHitVec_t::iterator hitIter = hitVec.begin();

      //LOG_INFO << "eee " << "---------------> Energy Apportioner <---------------" << endm;

      static Int_t  kEEmcNumPhiBins = kEEmcNumSubSectors * kEEmcNumSectors;
      Int_t hitIdx = 0;
      for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter, ++hitIdx ){
         //LOG_INFO << "eee " << '\t' << (*hitIter) << endm;

         // get new map
         sparseVec_t& thisMap = wPerTowerPerHit[ hitIdx ];

         // clear the neighbors
         neighborIndices.clear();

         // various indexing methods
         Int_t towIdx = hitIter->getTowerIdx();
         Short_t etaBin = 0;
         Short_t phiBin = 0;
         StEEmcGeoId_t::decodeTow( towIdx, phiBin, etaBin );

         // fill usedTowerIndices with all neighbors of the main hit tower
         Int_t phiBinLeft = (phiBin ? phiBin-1 : kEEmcNumPhiBins-1);
         Int_t phiBinRight = (phiBin+1 < kEEmcNumPhiBins ? phiBin+1 : 0);

         neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBinLeft,  etaBin ) );
         neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBin,      etaBin ) );
         neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBinRight, etaBin ) );

         if( etaBin < kEEmcNumEtas -1 ){
            // not at upper limit
            neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBinLeft,  etaBin + 1 ) );
            neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBin,      etaBin + 1 ) );
            neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBinRight, etaBin + 1 ) );
         };

         if( etaBin > 0 ){
            // not at lower limit
            neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBinLeft,  etaBin - 1 ) );
            neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBin,      etaBin - 1 ) );
            neighborIndices.push_back( StEEmcGeoId_t::encodeTow( phiBinRight, etaBin - 1 ) );
         };


         // compute contribution of this hit to each tower
         for( UInt_t i=0; i<neighborIndices.size(); ++i ){
            Int_t& thisTowIdx = neighborIndices[i];
            Float_t contribution = (this->*weightFunc)( *hitIter, thisTowIdx );

            thisMap[ thisTowIdx ] = contribution;

            denomIter = denominator.find(  thisTowIdx );
            if( denomIter == denominator.end() ){
               denominator[ thisTowIdx ] = contribution;
            } else {
               denominator[ thisTowIdx ] += contribution;
            };
         };
      };

      // to save indices and weights
      std::vector< Short_t > usedTowerIndices;
      std::vector< Float_t > usedTowerWeights;

      //LOG_INFO << "eee " << "numerators" << endm;

      // Now that have all the denominators, re-iterate and compute the
      // energies.

//       cout << "----------------------------------------" << endl;
      hitIdx = 0;
      for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter, ++hitIdx ){

         // get new map
         sparseVec_t& weightVec = wPerTowerPerHit[ hitIdx ];

         // clear
         usedTowerIndices.clear();
         usedTowerWeights.clear();

         Float_t hitEnergy = 0;
         for( weightIter = weightVec.begin(); weightIter != weightVec.end(); ++weightIter ){

            EEmcElement_t element = eemcEnergyPtr->eTow.getByIdx( weightIter->first );

//             cout << " towIdx " << weightIter->first << " energy " << element.energy << " fail " << element.fail
//                  << " hit " << hitIdx << " weight " << weightIter->second << "/" << denominator[ weightIter->first ] << endl;

            if( !mCheckTowerBits || !element.fail ){
               weightIter->second /= denominator[ weightIter->first ];
               Float_t energy = element.energy;
               energy *= weightIter->second;

               if( energy ){
                  hitEnergy += energy;
                  usedTowerIndices.push_back( weightIter->first );
                  usedTowerWeights.push_back( weightIter->second );
               };
            };
         };

//          cout << "main Idx = " << hitIter->getTowerIdx() << " hitEnergy = " << hitEnergy << endl;
         hitIter->setEnergy( hitEnergy );
         hitIter->setUsedTowers( usedTowerIndices, usedTowerWeights );

         if( hitEnergy <= 0 ){
            hitIter->setIsValid(0);
            ++nInvalid;
         };
//          cout << "--------------------" << endl;
      };

      // check for invalid
      while( nInvalid ){
         // find first invalid one
         for( hitIter = hitVec.begin(); hitIter != hitVec.end() && hitIter->isValid(); ++hitIter ){ /* */};

         // remove it
         hitVec.erase( hitIter );
         --nInvalid;
      };
   };

   return kStOK;
};

/// Considers both the SMD energy and a leakage function
Float_t StEEmcEnergyApportionerIU_t::smdSumAndLeakageWeightFunc( const StEEmcHit_t &hit, Int_t thisTowIdx ){

   // assume towerer neighbors hit tower, or wouldn't have gotten to this point

   // Get eta & phi bins for this tower
   Short_t phiBin = 0, etaBin = 0;
   StEEmcGeoId_t::decodeTow( thisTowIdx, phiBin, etaBin );
   Double_t xTower[2] = { (Double_t) phiBin, (Double_t) etaBin };

   // Get eta & phi bins for the tower beneath the point, i.e. hit
   Int_t towIdx = hit.getTowerIdx();
   Short_t etaHitTower, phiHitTower;
   StEEmcGeoId_t::decodeTow( towIdx, phiHitTower, etaHitTower );

   // Get the position of the point on the endcap.
   // Return 0 if not on the endcap.
   Int_t sec,sub,eta;
   Float_t dphi,deta;

   if ( !mEEmcGeomSimple.getTower( hit.getPosition(), sec, sub, eta, dphi, deta ) )
      return 0;

   dphi /= 2.0;
   deta /= 2.0;

   // Position of the point in fractional eta, phi space
   Double_t xHit[2] = {
      phiHitTower + dphi,
      etaHitTower + deta
   };

   Double_t funcVal = eeTowerFunction( xTower, xHit );

   //LOG_INFO << "eee " << "tower IDs " << tower.index() << ' ' << hitTower.index() << ", eefunc = " << funcVal << endm;

   if( funcVal < 0 )
      funcVal = 0;

   if( funcVal > 0 )
      funcVal *= ( hit.getEnergyU()*hit.getWeightU() + hit.getEnergyV()*hit.getWeightV() );

   return funcVal;
};

Float_t StEEmcEnergyApportionerIU_t::smdSumWeightFunc( const StEEmcHit_t &hit, Int_t thisTowIdx ){
   return ( hit.getEnergyU()*hit.getWeightU() + hit.getEnergyV()*hit.getWeightV() );
};

void StEEmcEnergyApportionerIU_t::setWeightFunction( WeightFunction_t funcType ){
   switch( funcType ){
   case SMD_SUM:
      weightFunc = &StEEmcEnergyApportionerIU_t::smdSumWeightFunc;
      break;
   case SMD_SUM_AND_LEAKAGE:
      weightFunc = &StEEmcEnergyApportionerIU_t::smdSumAndLeakageWeightFunc;
      break;
   };
};

ClassImp( StEEmcEnergyApportionerIU_t );

/*
 * $Id: StEEmcEnergyApportionerIU.cxx,v 1.2 2014/07/28 19:54:52 skoby Exp $ 
 * $Log: StEEmcEnergyApportionerIU.cxx,v $
 * Revision 1.2  2014/07/28 19:54:52  skoby
 * explicit cast to satisfy C++11
 *
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
