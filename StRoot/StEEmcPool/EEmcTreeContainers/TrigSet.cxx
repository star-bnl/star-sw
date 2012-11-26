/*
 * Created by S. Gliske, Aug 2012
 *
 * Description: Container to hold the set of triggers.
 *
 */

#include "StRoot/StEEmcPool/./EEmcTreeContainers/TrigSet.h"

#include <Rtypes.h>
#include <TObject.h>

#include <set>
#include <vector>
//#include <iostream>

TrigSet::TrigSet() : nTrigs(0), mTrigArray(0) { /* */ };

TrigSet::~TrigSet(){
   if( mTrigArray )
   delete mTrigArray;
};


void TrigSet::Clear( const Option_t* opt ){
   nTrigs = 0;
   delete mTrigArray;
   mTrigArray = 0;

   mTrigSet.clear();
};

// modifier
void TrigSet::insert( const std::vector< UInt_t >& trigVec ){

   // set size
   nTrigs = trigVec.size();

   // dealloc if needed
   if( mTrigArray )
      delete mTrigArray;

   // alloc
   mTrigArray = new UInt_t[ trigVec.size() ];

   // copy
   std::copy( trigVec.begin(), trigVec.end(), mTrigArray );
   std::copy( trigVec.begin(), trigVec.end(), std::inserter( mTrigSet, mTrigSet.end() ) );
};

// set the set from the array
void TrigSet::resync() const {
   mTrigSet.clear();

   std::copy( mTrigArray, &mTrigArray[nTrigs], std::inserter( mTrigSet, mTrigSet.end() ) );

//    for( UInt_t* p = mTrigArray; p != &mTrigArray[nTrigs]; ++p )
//       mTrigSet.insert( *p );
};

void TrigSet::copyToVector( std::vector< UInt_t >& vec ) const {
   vec.clear();
   vec.reserve( nTrigs );
   vec.insert( vec.end(), mTrigArray, &mTrigArray[nTrigs] );
};

ClassImp( TrigSet );

/*
 * $Id: TrigSet.cxx,v 1.1 2012/11/26 19:04:31 sgliske Exp $
 * $Log: TrigSet.cxx,v $
 * Revision 1.1  2012/11/26 19:04:31  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
