/***************************************************************************
 *
 * $Id: StFgtQaRawOctAdc.cxx,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaRawOctAdc.cxx,v $
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

#include <string>
#include <TH2F.h>

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

// constructors
StFgtQaRawOctAdc::StFgtQaRawOctAdc( const Char_t* name, Int_t rdo, Int_t arm, Int_t apvStart, Int_t tb ) :
   StMaker( name ), mHist( 0 ), mRdo( rdo ), mArm( arm ), mApvStart( apvStart ), mTimeBin( tb ){
   // nothing else
};

// deconstructor
StFgtQaRawOctAdc::~StFgtQaRawOctAdc(){
   if( mHist )
      delete mHist;
};

Int_t StFgtQaRawOctAdc::Init(){
   Int_t ierr = kStOk;

   std::stringstream ss;
   ss << "h" << GetName();
   std::string name = ss.str();

   ss.str("");
   ss.clear();
   ss << "Rdo " << mRdo << " Arm " << mArm << " Apv " << mApvStart << "-" << mApvStart+5;
   ss << "; " << kFgtNumChannels << "*(apv-" << mApvStart << ")+channel; ADC value";

   mHist = new TH2F( name.data(), ss.str().data(), 640, 0, 640, 256, 0, 1536 );

   return ierr;
};

Int_t StFgtQaRawOctAdc::Make(){
   Int_t ierr = kStOk;

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

            if( rdo == mRdo && arm == mArm && apv >= mApvStart && apv - mApvStart < 5 )
               mHist->Fill( (apv-mApvStart)*kFgtNumChannels + channel, (*stripIter)->getAdc( mTimeBin ) );
         }; 
      };
   };

   return ierr;
};

ClassImp(StFgtQaRawOctAdc);
