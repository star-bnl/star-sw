/*!
 * \class StFgtDaq2TxtMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtDaq2TxtMaker.cxx,v 1.4 2012/01/30 10:42:23 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtDaq2TxtMaker.cxx,v $
 * Revision 1.4  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.3  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.2  2011/11/01 18:55:08  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/10/07 19:55:37  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtDaq2TxtMaker.h"

#include <string>
#include <fstream>

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtUtil/geometry/StFgtCosmicTestStandGeom.h"


// constructors
StFgtDaq2TxtMaker::StFgtDaq2TxtMaker( const Char_t* name,
                                      const Char_t* outputfile,
                                      Short_t quadId )
   : StMaker( name ),
     mFileName( outputfile ),
     mQuad( quadId ),
     mDataSize2( 0 ),
     mIsCosmic( 1 ) { /* */ };

// deconstructor
StFgtDaq2TxtMaker::~StFgtDaq2TxtMaker(){ /* */ };


const Int_t StFgtDaq2TxtMaker::mDataSize1 = 7*128;


Int_t StFgtDaq2TxtMaker::Init(){
   Int_t ierr = kStOk;

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

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   if( !ierr ){
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;

            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               Int_t geoId = (*stripIter)->getGeoId();
               for( Int_t tb = 0; tb < kFgtNumTimeBins; ++tb ){
                  Short_t adc = (*stripIter)->getAdc(tb);

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
