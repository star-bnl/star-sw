/***************************************************************************
 *
 * $Id: StFgtQaHighStrips.cxx,v 1.2 2012/01/31 12:53:28 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaHighStrips.cxx,v $
 * Revision 1.2  2012/01/31 12:53:28  sgliske
 * updates
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.1  2012/01/30 17:18:09  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtQaHighStrips.h"

#include <string>
#include <TH2F.h>

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"

// constructors
StFgtQaHighStrips::StFgtQaHighStrips( const Char_t* name ) :
   StMaker( name ), mHist2D( 0 ), mHistVec( kFgtNumOctants, (TH1F*)0 ), mMaxNum( 10 ) {
   // that's all
};

// deconstructor
StFgtQaHighStrips::~StFgtQaHighStrips(){
   if( mHist2D )
      delete mHist2D;

   HistVec_t::iterator iter;
   for( iter = mHistVec.begin(); iter != mHistVec.end(); ++iter )
      if( *iter ) 
         delete *iter;
};

Int_t StFgtQaHighStrips::Init(){
   Int_t ierr = kStOk;

   std::stringstream ss;
   ss << "h" << GetName() << "_2D";
   std::string name = ss.str();

   mHist2D = new TH2F( ss.str().data(), "Number of High Strips per Event per Octant; Octant; Number of Strips",
                       kFgtNumOctants, 0, kFgtNumOctants, mMaxNum, 0, mMaxNum );

   Char_t octName[2] = { 'L', 'S' };
   Int_t histIdx = 0;
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      for( Int_t quad = 0; quad < kFgtNumDiscs; ++quad ){
         for( Int_t oct = 0; oct < 2; ++oct, ++histIdx ){
            ss.str("");
            ss.clear();
            ss << disc+1 << (Char_t)(quad+'A') << octName[oct];

            std::string label = ss.str();
            mHist2D->GetXaxis()->SetBinLabel( histIdx, label.data() );

            ss.str("");
            ss.clear();
            ss << "h" << GetName() << "_" << histIdx;

            mHistVec[ histIdx ] = new TH1F( ss.str().data(),
                                            ( std::string("Number of High Strips per Event for Octant" ) + label +
                                              "; Number of Strips; Number of Events").data(),
                                            mMaxNum, 0, mMaxNum );
         };
      };
   };

   return ierr;
};

Int_t StFgtQaHighStrips::Make(){
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

   vector< Int_t > numPerOct( kFgtNumOctants, 0 );

   for( Int_t disc = 0; disc < kFgtNumDiscs && !ierr; ++disc ){
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
            ++numPerOct[ histIdx ];
         };
      };
   };

   vector< Int_t >::iterator iter;
   Int_t histIdx = 0;

   for( iter = numPerOct.begin(); iter != numPerOct.end(); ++iter, ++histIdx ){
      if( *iter ){
         mHist2D->Fill( histIdx+1, *iter );
         mHistVec[ histIdx ]->Fill( *iter );
      };
   };

   return ierr;
};

ClassImp(StFgtQaHighStrips);
