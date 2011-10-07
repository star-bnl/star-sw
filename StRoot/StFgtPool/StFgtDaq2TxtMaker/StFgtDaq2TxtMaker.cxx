/*!
 * \class StFgtDaq2TxtMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtDaq2TxtMaker.cxx,v 1.1 2011/10/07 19:55:37 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtDaq2TxtMaker.cxx,v $
 * Revision 1.1  2011/10/07 19:55:37  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtDaq2TxtMaker.h"

#include <string>
#include <fstream>

#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StRoot/StEvent/StFgtEvent/StFgtDisc.h"

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtUtil/geometry/StFgtCosmicTestStandGeom.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeomDefs.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StRoot/StFgtRawMaker/StFgtRawBase.h"
#include "StRoot/StFgtRawMaker/StFgtRawMaker.h"
#include "StRoot/StFgtRawMaker/StFgtCosmicMaker.h"

// constructors
StFgtDaq2TxtMaker::StFgtDaq2TxtMaker( const Char_t* name,
                                      const Char_t* fgtRawBaseName,
                                      const Char_t* outputfile,
                                      Short_t quadId )
   : StMaker( name ),
     mInputName( fgtRawBaseName ),
     mFgtEventPtr( 0 ),
     mFileName( outputfile ),
     mQuad( quadId ),
     mDataSize2( 0 ),
     mIsCosmic( 1 ) { /* */ };

// deconstructor
StFgtDaq2TxtMaker::~StFgtDaq2TxtMaker(){ /* */ };


const Int_t StFgtDaq2TxtMaker::mDataSize1 = 7*128;


Int_t StFgtDaq2TxtMaker::Init(){
   Int_t ierr = kStOk;

   // get the data
   TObject *dataMaker = GetMaker( mInputName.data() );

   if( !dataMaker ){
      LOG_FATAL << "::Init() could not get pointer to a maker with name '" << mInputName << "'" << endm;
      ierr = kStFatal;
   };

   if( !ierr ){
      if( dataMaker->InheritsFrom( "StFgtCosmicMaker" ) ){
         StFgtCosmicMaker* maker = static_cast< StFgtCosmicMaker* >( dataMaker );
         mFgtEventPtr = maker->getFgtEventPtr();
      } else if ( dataMaker->InheritsFrom( "StFgtRawMaker" ) ){
         StFgtRawMaker* maker = static_cast< StFgtRawMaker* >( dataMaker );
         mFgtEventPtr = maker->getFgtEventPtr();
      };

      if( !mFgtEventPtr ){
         LOG_FATAL << "::Init() could not get pointer to StFgtEvent" << endm;
         ierr = kStFatal;
      };
   };

   // set the output
   LOG_INFO << "Opening file '" << mFileName << "' for output" << endm;
   mFout.open( mFileName.data() );
   if( !mFout ){
      LOG_FATAL << "error opening file '" << mFileName << "'" << endm;
      ierr = kStFatal;
   };

   mDataSize2 = 10*6;
   if( mIsCosmic )
      mDataSize2 = 10*3;

   for( Int_t i=0; i<mDataSize1; ++i )
      mData[i] = new Short_t [mDataSize2];

   return ierr;
};

void StFgtDaq2TxtMaker::Clear(const Option_t* opts ){
   for( Int_t i=0; i<mDataSize1; ++i )
      for( Short_t *p = mData[i]; p != &mData[i][mDataSize2]; ++p )
         (*p) = 0;
};


Int_t StFgtDaq2TxtMaker::Make(){
   Int_t ierr = kStOk;

   // fill the data array
   ierr = kStErr;
   for( Int_t discId=0; discId<6; ++discId ){
      StFgtDisc *discPtr = mFgtEventPtr->getDiscPtr( discId );

      if( discPtr ){
         ierr = kStOk;
         StFgtRawHitArray &hitArray = discPtr->getRawHitArray();

         Int_t n = hitArray.getEntries();
         for( Int_t i = 0; i<n; ++i ){

            StFgtRawHit *hit = hitArray.getRawHitPtr( i );
            if( hit ){
               Int_t geoId = hit->getGeoId();
               Short_t adc = hit->getAdc();
               Int_t tb = hit->getTimeBin();

               Short_t disc, quad, strip;
               Char_t layer;
               StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

               if( quad == mQuad ){
                  Int_t rdo, arm, apv, channel;
                  if( mIsCosmic )
                     StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId,rdo,arm,apv,channel);
                  else
                     StFgtGeom::getNaiveElecCoordFromGeoId(geoId,rdo,arm,apv,channel);

                  apv %= 12;
                  mData[ 7*channel + tb ][ disc*10 + apv] = adc;
               };
            };
         };
      };
   };

   Int_t k=0;
   for( Int_t chan = 0; chan < 128; ++chan ){
      for( Int_t tb = 0; tb < 7; ++tb, ++k ){
         mFout << chan << ' ' << tb << ' ';
         Short_t *dataPtr = mData[ k ];

         for( Short_t *p = dataPtr; p != &dataPtr[mDataSize2]; ++p )
            mFout  << ' ' << *p;
         mFout << endl;
      };
   };
   mFout << endl << endl << endl;

   return ierr;
};

Int_t StFgtDaq2TxtMaker::Finish(){
   mFout.close();

   return kStOk;
};

ClassImp(StFgtDaq2TxtMaker);
