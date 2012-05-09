/***************************************************************************
 *
 * $Id: StEEmcFgtCorrelatorA.cxx,v 1.1 2012/05/09 17:26:26 sgliske Exp $
 * Author: S. Gliske, May 2012
 *
 ***************************************************************************
 *
 * Description: see header
 *
 ***************************************************************************
 *
 * $Log: StEEmcFgtCorrelatorA.cxx,v $
 * Revision 1.1  2012/05/09 17:26:26  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StEEmcFgtCorrelatorA.h"

#include "StEEmcRawMapMaker.h"
#include "StThreeVectorF.hh"

#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StRoot/StEEmcPool/StEEmcPointMap/StEEmcPointMap.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StPrimaryVertex.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuDst.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuEvent.h"


#define DEBUG
#define CLUSTER_WIDTH 4
#define CLUSTER_SIZE 9
#define FAIL_FLAG -1234

// constructors
StEEmcFgtCorrelatorA::StEEmcFgtCorrelatorA( const Char_t* name, const Char_t* rawMapMkrName ) :
   StMaker( name ), mInputType(1), mInputName("MuDst"), mEEmcRawMapMkr(0) {

   mEEmcRawMapMkr = static_cast< StEEmcRawMapMaker* >( GetMaker( rawMapMkrName ) );
   assert( mEEmcRawMapMkr );
   assert( CLUSTER_SIZE == CLUSTER_WIDTH*2+1 );   // fix your consts
   assert( FAIL_FLAG < 0 );
};

// deconstructor
StEEmcFgtCorrelatorA::~StEEmcFgtCorrelatorA(){ /* */ };

// init
Int_t StEEmcFgtCorrelatorA::Init(){

};

Int_t StEEmcFgtCorrelatorA::Make(){
   Int_t ierr = loadVertex();

   const StEEmcRawMap& eemcMap = mEEmcRawMapMkr->getMap( 4 );

   std::vector< Int_t > seedIdxVec;
   std::vector< Float_t > mipClusPosVec[12];  // still in u/v indexing, but with fraction position between strips
   std::vector< TVector3 > mipPosVec;         // 3D space points

   StEEmcRawMap::const_iterator mapIter;
   if( !eemcMap.empty() ){

      // first, find all with signal in range
      for( mapIter = eemcMap.begin(); mapIter != eemcMap.end(); ++mapIter ){
         Int_t idx = mapIter->first;
         const StEEmcRawMapData& data = mapIter->second;

         Float_t resp = data.rawAdc - data.ped;
         if( data.fail || data.stat )
            resp = 0;
         if( resp && data.pedSigma )
            resp /= data.pedSigma;

         if( resp < mMipMax && resp > mMipMin )
            seedIdxVec.push_back( idx );
      };
   };

#ifdef DEBUG
   LOG_INFO << GetEventNumber() << " found " << seedIdxVec.size() << " MIP seeds out of " << eemcMap.size() << " strips" << endm;
#endif

   Int_t nMipClus = 0;
   if( !seedIdxVec.size() ){
      std::vector< Int_t >::const_iterator seedIter;

      Float_t clusterShape[CLUSTER_SIZE];

      for( seedIter = seedIdxVec.begin(); seedIter != seedIdxVec.end(); ++seedIter ){

         // clear cluster shape
         for( Float_t *p = clusterShape; p != &clusterShape[CLUSTER_SIZE]; ++p )
            *p = 0;

         // compute min and max index for the specific u/v and sector
         Int_t first = ((*seedIter)/kEEmcNumStrips)*kEEmcNumStrips;
         Int_t last = first + kEEmcNumStrips;

         // compute range of the cluster
         Int_t low  = std::max( first, *seedIter - CLUSTER_WIDTH );
         Int_t high = std::min( last,  *seedIter + CLUSTER_WIDTH + 1 );

         // copy response values to the cluster shape array
         for( Int_t i = 0; i<CLUSTER_SIZE; ++i ){
            if( i >= low && i < high ){
               mapIter = eemcMap.find( *seedIter - 1 );

               if( mapIter != eemcMap.end() ){
                  const StEEmcRawMapData& data = mapIter->second;

                  Float_t resp = data.rawAdc - data.ped;
                  if( resp && data.pedSigma )
                     resp /= data.pedSigma;
                  if( data.fail || data.stat )
                     resp = FAIL_FLAG;  // magic value to flag bad strip

                  clusterShape[i] = resp;
               };
            };
         };

         // now check the cluster shape to see if it qualifies as a MIP

         // ensure exactly one of the adjacent have signal
         Bool_t pass = (( clusterShape[CLUSTER_WIDTH-1] > mSigThres ) ^ ( clusterShape[CLUSTER_WIDTH+1] > mSigThres ));

         // ensure adjacent are good strips and that they are less than the seed
         if( pass && (clusterShape[CLUSTER_WIDTH+1] == FAIL_FLAG || clusterShape[CLUSTER_WIDTH-1] == FAIL_FLAG) )
            pass = 0;
         if( pass && clusterShape[CLUSTER_WIDTH+1] > clusterShape[CLUSTER_WIDTH] )
            pass = 0;
         if( pass && clusterShape[CLUSTER_WIDTH-1] > clusterShape[CLUSTER_WIDTH] )
            pass = 0;

         // ensure the farther strips have no signal
         for( Int_t i = 0; i<CLUSTER_WIDTH-1 && pass; ++i )
            if( clusterShape[i] > mSigThres )
               pass = 0;
         for( Int_t i = CLUSTER_WIDTH+2; i<CLUSTER_SIZE && pass; ++i )
            if( clusterShape[i] > mSigThres )
               pass = 0;

         if( pass ){
            // determine position better
            Float_t posA = CLUSTER_WIDTH;
            Float_t wA = clusterShape[CLUSTER_WIDTH];
            Float_t posB = (( clusterShape[CLUSTER_WIDTH+1] > clusterShape[CLUSTER_WIDTH-1] ) ? CLUSTER_WIDTH+1 : CLUSTER_WIDTH-1 );
            Float_t wB = std::max( clusterShape[CLUSTER_WIDTH+1], clusterShape[CLUSTER_WIDTH-1] );

            ++nMipClus;
            Int_t sec = (*seedIter)/kEEmcNumStrips/2;
            mipClusPosVec[sec].push_back(( posA*wA + posB*wB ) / ( wA + wB ));
         };
      };
   };

#ifdef DEBUG
   LOG_INFO << "FOUND " << nMipClus << " possible MIP clusters" << endm;
#endif

   if( nMipClus ){
      std::vector< Float_t >::iterator mipClusPosIter;  // still in u/v indexing, but with fraction position between strips

      for( Int_t sec = 0; sec<kEEmcNumSectors; ++sec ){
         if( mipClusPosVec[sec].size() == 2 ){
            Float_t idx1 = mipClusPosVec[sec][0];
            Float_t idx2 = mipClusPosVec[sec][1];
            Bool_t isV1 = ((Int_t)idx1/kEEmcNumStrips)%2;
            Bool_t isV2 = ((Int_t)idx2/kEEmcNumStrips)%2;

            if( isV1 ^ isV2 ){
               Float_t idxU = ( isV1 ? idx2 : idx1 );
               Float_t idxV = ( isV1 ? idx1 : idx2 );
               Float_t x = 0, y = 0;

               StEEmcPointMap_t::instance().convertStripUVtoXY( sec, idxU, idxV, x, y );
               mipPosVec.push_back( TVector3( x, y, kEEmcZSMD ) );
            };
         };
      };
   };

#ifdef DEBUG
   LOG_INFO << "FOUND " << mipPosVec.size() << " MIP points" << endm;
#endif

   return ierr;
};

void StEEmcFgtCorrelatorA::Clear( Option_t *opt ){
   // ???
};

Int_t StEEmcFgtCorrelatorA::setInput( const Char_t *name, Int_t type ){
   Int_t ierr = kStOk;

   if( type == 0 || type == 1){
      mInputType = type;
      mInputName = name;
   } else {
      LOG_ERROR << "Invalid input type" << endm;
      ierr = kStFatal;
   };
   return ierr;
};

Int_t StEEmcFgtCorrelatorA::loadVertex(){
   Int_t ierr = kStErr;

   if( mInputType ){
      // load from MuDst
      const StMuDst* muDst = (const StMuDst*)GetInputDS( mInputName.data() );
      assert( muDst );

      StMuEvent *event = muDst->event();
      if( event ){
         const StThreeVectorF& v = event->primaryVertexPosition();

         if( v.z() > -300 && v.z() < 300 ){
            ierr = kStOk;
            mVertex.SetXYZ( v.x(), v.y(), v.z() );
         };
      };
   } else {
      // load from StEvent
      const StEvent *event = (const StEvent*)GetInputDS( mInputName.data() );
      assert( event );
      const StPrimaryVertex* vertex = event->primaryVertex();

      if( vertex ){
         const StThreeVectorF& v = vertex->position();

         if( v.z() > -300 && v.z() < 300 ){
            ierr = kStOk;
            mVertex.SetXYZ( v.x(), v.y(), v.z() );
         };
      };
   };

   return ierr;
};

ClassImp(StEEmcFgtCorrelatorA);
