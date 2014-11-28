/***************************************************************************
 *
 * $Id: StEEmcFgtCorrelatorA.cxx,v 1.2 2012/05/09 21:11:58 sgliske Exp $
 * Author: S. Gliske, May 2012
 *
 ***************************************************************************
 *
 * Description: see header
 *
 ***************************************************************************
 *
 * $Log: StEEmcFgtCorrelatorA.cxx,v $
 * Revision 1.2  2012/05/09 21:11:58  sgliske
 * updates
 *
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
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"


#define DEBUG
#define CLUSTER_WIDTH 4
#define CLUSTER_SIZE 9
#define FAIL_FLAG -1234

// constructors
StEEmcFgtCorrelatorA::StEEmcFgtCorrelatorA( const Char_t* name, const Char_t* rawMapMkrName ) :
   StMaker( name ), mInputType(1), mInputName("MuDst"), mEEmcRawMapMkr(0),
   mMipMin(5), mMipMax(50), mSigThres(3) {

   mEEmcRawMapMkr = static_cast< StEEmcRawMapMaker* >( GetMaker( rawMapMkrName ) );
   assert( mEEmcRawMapMkr );
   assert( CLUSTER_SIZE == CLUSTER_WIDTH*2+1 );   // fix your consts
   assert( FAIL_FLAG < 0 );
};

// deconstructor
StEEmcFgtCorrelatorA::~StEEmcFgtCorrelatorA(){ /* */ };

// init
Int_t StEEmcFgtCorrelatorA::Init(){
   return kStOk;
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

#ifdef DEBUG2
   LOG_INFO << "zzz " << GetEventNumber() << " found " << seedIdxVec.size() << " MIP seeds out of " << eemcMap.size() << " strips" << endm;
#endif

   Int_t nMipClus = 0;
   Int_t nFailBothAdj = 0, nFailFail = 0, nFailSideHigh = 0, nFailNonIso = 0;

   if( !seedIdxVec.empty() ){
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
         Int_t offset = *seedIter - CLUSTER_WIDTH;
         Int_t low  = std::max( first, *seedIter - CLUSTER_WIDTH );
         Int_t high = std::min( last,  *seedIter + CLUSTER_WIDTH + 1 );

         // copy response values to the cluster shape array
         for( Int_t i = offset; i<offset+CLUSTER_SIZE; ++i ){
            if( i >= low && i < high ){
               mapIter = eemcMap.find( i );

               if( mapIter != eemcMap.end() ){
                  const StEEmcRawMapData& data = mapIter->second;

                  Float_t resp = data.rawAdc - data.ped;
                  if( resp && data.pedSigma )
                     resp /= data.pedSigma;
                  if( data.fail || data.stat )
                     resp = FAIL_FLAG;  // magic value to flag bad strip

                  clusterShape[i-offset] = resp;
               };
            };
         };

#ifdef DEBUG_CLUS_SHAPE
         LOG_INFO << "zzz cluster shape: ";
         for( Int_t i=0; i<CLUSTER_SIZE; ++i )
            LOG_INFO << clusterShape[i] << ' ';
         LOG_INFO << endm;
#endif

         // now check the cluster shape to see if it qualifies as a MIP

         // ensure exactly one of the adjacent have signal
         Bool_t pass = (( clusterShape[CLUSTER_WIDTH-1] > mSigThres ) ^ ( clusterShape[CLUSTER_WIDTH+1] > mSigThres ));
         if( !pass )
            ++nFailBothAdj;


         //cout << "zzz " << clusterShape[CLUSTER_WIDTH-1] << ' ' << clusterShape[CLUSTER_WIDTH+1] << endl;

         // ensure adjacent are good strips and that they are less than the seed
         if( pass && (clusterShape[CLUSTER_WIDTH+1] == FAIL_FLAG || clusterShape[CLUSTER_WIDTH-1] == FAIL_FLAG) ){
            ++nFailFail;
            pass = 0;
         };
         if( pass && ( clusterShape[CLUSTER_WIDTH+1] > clusterShape[CLUSTER_WIDTH] || clusterShape[CLUSTER_WIDTH-1] > clusterShape[CLUSTER_WIDTH] ) ){
            ++nFailSideHigh;
            pass = 0;
         };

         Bool_t passOlder = pass;
         // ensure the farther strips have no signal
         for( Int_t i = 0; i<CLUSTER_WIDTH-1 && pass; ++i )
            if( clusterShape[i] > mSigThres )
               pass = 0;
         for( Int_t i = CLUSTER_WIDTH+2; i<CLUSTER_SIZE && pass; ++i )
            if( clusterShape[i] > mSigThres )
               pass = 0;

         if( passOlder && !pass )
            ++nFailNonIso;

         if( pass ){
            // determine position better
            Float_t posA = *seedIter;
            Float_t wA = clusterShape[CLUSTER_WIDTH];
            Float_t posB = (( clusterShape[CLUSTER_WIDTH+1] > clusterShape[CLUSTER_WIDTH-1] ) ? *seedIter+1 : *seedIter-1 );
            Float_t wB = std::max( clusterShape[CLUSTER_WIDTH+1], clusterShape[CLUSTER_WIDTH-1] );

            ++nMipClus;
            Int_t sec = (*seedIter)/kEEmcNumStrips/2;
            mipClusPosVec[sec].push_back(( posA*wA + posB*wB ) / ( wA + wB ));
         };
      };
   };

#ifdef DEBUG2
   LOG_INFO << "zzz FOUND " << nMipClus << " possible MIP clusters, "
            << "failed " << nFailBothAdj << ' ' << nFailFail << ' ' << nFailSideHigh << ' ' << nFailNonIso << endm;
#endif

   if( nMipClus ){
      std::vector< Float_t >::iterator mipClusPosIter;  // still in u/v indexing, but with fraction position between strips

      // limit to only sectors 6-12, since only have half coverage of FGT in 2012
      for( Int_t sec = 6; sec<kEEmcNumSectors; ++sec ){
#ifdef DEBUG3
         LOG_INFO << "zzz \t sector " << sec << " has " << mipClusPosVec[sec].size() << " MIP clusters " << endm;

         for( UInt_t j = 0; j<mipClusPosVec[sec].size(); ++j ){
            Float_t idx = mipClusPosVec[sec][j];
            Int_t idx2 = idx;
            Int_t strip = idx2 % kEEmcNumStrips;
            Bool_t isV = (idx2 / kEEmcNumStrips)%2;
            Int_t sec = (idx2 / kEEmcNumStrips)/2;

            LOG_INFO << "zzz \t\t " << idx << ' ' << sec << ' ' << (isV ? 'v' : 'u') << ' ' << strip << ' ' << strip+idx-idx2 << endm;
         };
#endif

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
   if( !mipPosVec.empty() ){
      LOG_INFO << "zzz EVENT " << GetEventNumber() << " FOUND " << mipPosVec.size() << " MIP points" << endm;
   };
#endif

   if( !mipPosVec.empty() ){
#ifdef DEBUG
      LOG_INFO << "zzz -> vertex " << mVertex.X() << ' ' << mVertex.Y() << ' ' << mVertex.Z() << endm;
#endif

      for( UInt_t j = 0; j < mipPosVec.size(); ++j ){
         TVector3& smdPt = mipPosVec[j];
         TVector3 delta = smdPt - mVertex;

#ifdef DEBUG
         LOG_INFO << "zzz -> ESMD point " << smdPt.X() << ' ' << smdPt.Y() << ' ' << smdPt.Z() << endm;
#endif
         for( Int_t disc = 0; disc<kFgtNumDiscs; ++disc ){
            Float_t z = StFgtGeom::getDiscZ( disc );
            Float_t alpha = ( z - mVertex.Z() ) / ( smdPt.Z() - mVertex.Z() );
            TVector3 pos = alpha*delta + mVertex;

#ifdef DEBUG
            LOG_INFO << "zzz ----> disc " << disc+1 << " line passes through " << pos.X() << ' ' << pos.Y() << ' ' << pos.Z() << endm;
#endif
         };
      };
   };

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
