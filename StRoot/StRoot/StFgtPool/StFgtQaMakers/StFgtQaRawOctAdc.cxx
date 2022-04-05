/***************************************************************************
 *
 * $Id: StFgtQaRawOctAdc.cxx,v 1.3 2012/03/05 20:35:15 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaRawOctAdc.cxx,v $
 * Revision 1.3  2012/03/05 20:35:15  sgliske
 * update--still not really working
 *
 * Revision 1.2  2012/01/31 12:53:28  sgliske
 * updates
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.3  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.2  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.1  2012/01/24 11:55:49  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtQaRawOctAdc.h"
#include "StFgtDbMaker/StFgtDbMaker.h"
#include "StFgtDbMaker/StFgtDb.h"

#include <string>
#include <TH2F.h>

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

// constructors
StFgtQaRawOctAdc::StFgtQaRawOctAdc( const Char_t* name, Int_t tb ) :
   StMaker( name ), mHistVec( kFgtNumOctants, (TH2F*)0 ), mTimeBin( tb ), mAdcBins(256), mAdcMin(0), mAdcMax(6*mAdcBins) {
   // nothing else
};

// deconstructor
StFgtQaRawOctAdc::~StFgtQaRawOctAdc(){
   HistVec_t::iterator iter;
   for( iter = mHistVec.begin(); iter != mHistVec.end(); ++iter )
      if( *iter ) 
         delete *iter;
};

Int_t StFgtQaRawOctAdc::Init(){
   Int_t ierr = kStOk;

   std::stringstream ss;

   Char_t octName[2] = { 'L', 'S' };
   Int_t histIdx = 0;
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      for( Int_t quad = 0; quad < kFgtNumDiscs; ++quad ){
         for( Int_t oct = 0; oct < 2; ++oct, ++histIdx ){
            ss.str("");
            ss.clear();
            ss << disc+1 << (Char_t)(quad+'A') << octName[oct];
            std::string label = ss.str();

            ss.str("");
            ss.clear();
            ss << "h" << GetName() << "_" << histIdx;

            Int_t chanPerOct = kFgtNumChannels*kFgtApvsPerOct;
            mHistVec[ histIdx ] = new TH2F( ss.str().data(),
                                            ( std::string("Octant " ) + label +
                                              "; Number of Strips; Number of Events").data(),
                                            chanPerOct, 0, chanPerOct, mAdcBins, mAdcMin, mAdcMax );
         };
      };
   };

   return ierr;
};

Int_t StFgtQaRawOctAdc::Make(){
   Int_t ierr = kStOk;

   StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
   if( !fgtDbMkr ){
      LOG_FATAL << "Error finding StFgtDbMaker" << endm;
      ierr = kStFatal;
   };

   if( !ierr ){
      StFgtDb *fgtTables = fgtDbMkr->getDbTables();

      if( !fgtTables ){
         LOG_FATAL << "Error finding StFgtDb" << endm;
         ierr = kStFatal;
      };
   };

   StEvent* eventPtr = 0;
   StFgtCollection *fgtCollectionPtr = 0;

   eventPtr = (StEvent*)GetInputDS("StEvent");
   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent in '" << ClassName() << "'" << endm;
      ierr = kStErr;
   } else {
      fgtCollectionPtr=eventPtr->fgtCollection();

      if( !fgtCollectionPtr) {
         LOG_ERROR << "Error getting pointer to StFgtCollection in '" << ClassName() << "'" << endm;
         ierr = kStErr;
      };
   };

   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){

      StFgtStripCollection *stripCollectionPtr = 0;
      if( !ierr ){
         stripCollectionPtr = fgtCollectionPtr->getStripCollection( disc );
      };

      if( stripCollectionPtr ){
         const StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
         StSPtrVecFgtStripConstIterator stripIter;

         for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
            Int_t rdo, arm, apv, channel;
            (*stripIter)->getElecCoords( rdo, arm, apv, channel );

            Char_t layer, oct = StFgtGeom::getOctant( apv );
            Short_t disc2, quad, strip;
            StFgtGeom::decodeGeoId( (*stripIter)->getGeoId(), disc2, quad, layer, strip );

            int histIdx = disc2*kFgtNumQuads*2 + quad*2 + (oct=='S');

            mHistVec[ histIdx ]->Fill( ((apv%12)%5)*kFgtNumChannels + channel, (*stripIter)->getAdc( mTimeBin ) );
         }; 
      };
   };

   return ierr;
};

ClassImp(StFgtQaRawOctAdc);
