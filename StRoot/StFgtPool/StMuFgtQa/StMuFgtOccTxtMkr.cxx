/***************************************************************************
 *
 * $Id: StMuFgtOccTxtMkr.cxx,v 1.1 2012/03/06 01:32:35 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtOccTxtMkr.cxx,v $
 * Revision 1.1  2012/03/06 01:32:35  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StMuFgtOccTxtMkr.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuDst.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtStrip.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtCluster.h"


// constructor
StMuFgtOccTxtMkr::StMuFgtOccTxtMkr( const Char_t* name ) : StMaker( name ) { /* */ };

// deconstructor
StMuFgtOccTxtMkr::~StMuFgtOccTxtMkr(){ /* */ };

// make
Int_t StMuFgtOccTxtMkr::Make(){
   Int_t ierr = kStOk;

   // get pointer to input

   const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");
   TClonesArray *fgtStrips = 0;
   TClonesArray *fgtClusters = 0;
   if( !muDst ){
      LOG_FATAL << "pointer to StMuDst is null" << endm;
      ierr = kStFatal;
   };

   if( !ierr ){
      fgtStrips = muDst->fgtArray( muFgtStrips );

      if( !fgtStrips ){
         LOG_FATAL << "pointer to muFgtStrips is null" << endm;
         ierr = kStFatal;
      };
   };

   if( !ierr ){
      fgtClusters = muDst->fgtArray( muFgtClusters );

      if( !fgtClusters ){
         LOG_FATAL << "pointer to muFgtClusters is null" << endm;
         ierr = kStFatal;
      };
   };

   if( !ierr ){
      // have pointers to the data, now do some counting
      Int_t nStrips = fgtStrips->GetEntriesFast();
      Int_t nClusters = fgtClusters->GetEntriesFast();
      Int_t nRclus[kFgtNumOctantsPerDisc*kFgtNumDiscs], nPhiClus[kFgtNumOctantsPerDisc*kFgtNumDiscs];

      // zero counts
      for( Int_t i = 0; i < kFgtNumOctantsPerDisc*kFgtNumDiscs; ++i )
         nRclus[i] = nPhiClus[i] = 0;

      for( Int_t i = 0; i < nClusters; ++i ){
         StMuFgtCluster* clus = static_cast< StMuFgtCluster* >( (*fgtClusters)[i] );
         if( clus ){
            // determine which octant this is in
            Int_t geoId = clus->getCentralStripGeoId();

            Short_t disc,  quad,  strip;
            Char_t  layer;
            StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

            Int_t idx = (quad*2 + ( StFgtGeom::getOctant( layer, strip ) == 'S' ))*kFgtNumDiscs + disc;
            assert( idx < kFgtNumOctantsPerDisc*kFgtNumDiscs ); // ensure decoding is working

            if( layer == 'R' )
               ++nRclus[idx];
            else 
               ++nPhiClus[idx];
         };
      };

      cout << "--------------------------------------------------------------" << endl;
      cout << "Event #" << GetEventNumber() << ", number of strips " << nStrips << endl << endl;

      if( nStrips > 0 ){
         cout << "disc # | layer";
         for( Char_t quad = 'A'; quad <= 'D'; ++quad )
            cout << " | " << quad << ".L | " << quad << ".S";
         cout << endl;

         for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
            for( Int_t layerIsR = 0; layerIsR < 2; ++layerIsR ){
               cout << "     " << disc+1 << " |   " << (layerIsR ? 'R' : 'P') << "  ";
               for( Int_t quad = 0; quad < kFgtNumQuads; ++quad ){
                  for( Int_t oct = 0; oct < 2; ++oct ){

                     Int_t idx = (quad*2 + oct)*kFgtNumDiscs + disc;
                     cout << " | " << Form("%3d", ( layerIsR ? nRclus[idx] : nPhiClus[idx] ) );
                  };
               };
               cout << endl;
            };
         };
      };
   };

   return ierr;
};

ClassImp(StMuFgtOccTxtMkr);
