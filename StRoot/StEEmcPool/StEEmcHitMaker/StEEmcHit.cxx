/*!
 *
 * \class StEEmcHit_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Root stuff for the class.  See the header file.
 */

#include "StEEmcHit.h"

void StEEmcHit_t::setNumUsedTowers( UInt_t n ){
   mUsedTowerIndices.Set( n );
   mUsedTowerWeights.Set( n );
};

void StEEmcHit_t::addUsedTower( Int_t localIndex, Short_t towIndex, Float_t weight ){
   if( localIndex <= mUsedTowerIndices.GetSize() && localIndex <= mUsedTowerWeights.GetSize() && localIndex > -1 ){
      mUsedTowerIndices[ localIndex ] = towIndex;
      mUsedTowerWeights[ localIndex ] = weight;
   };
};

void StEEmcHit_t::setUsedTowers( std::vector< Short_t >& usedIndices, std::vector< Float_t >& weights ){
   UInt_t n = usedIndices.size();
   UInt_t n2 = weights.size();

   // if different sizes, just copy as many as both are valid
   if( n2 < n )
      n = n2;

   mUsedTowerIndices.Set( n );
   mUsedTowerWeights.Set( n );


   for( UInt_t i = 0; i<n; ++i ){
      mUsedTowerIndices[ i ] = usedIndices[i];
      mUsedTowerWeights[ i ] = weights[i];
   };
};


std::ostream &operator<<( std::ostream &out, const StEEmcHit_t &hit ){
   out << *static_cast< const StSimpleHit_t* >( &hit ) << ", sec = ";
   out << hit.getSector() << ", u ";
   out << hit.getClusIDu() << ' ';
   out << hit.getEnergyU() << ' ';
   out << hit.getWeightU() << ", v ";
   out << hit.getClusIDv() << ' ';
   out << hit.getEnergyV() << ' ';
   out << hit.getWeightV() << ", tow ";
   out << hit.getTowerIdx() << ", ttest ";
   out << hit.getTtest2();

   return out;
};

ClassImp(StEEmcHit_t);

/*
 * $Id: StEEmcHit.cxx,v 1.1 2012/11/26 19:05:54 sgliske Exp $ 
 * $Log: StEEmcHit.cxx,v $
 * Revision 1.1  2012/11/26 19:05:54  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
