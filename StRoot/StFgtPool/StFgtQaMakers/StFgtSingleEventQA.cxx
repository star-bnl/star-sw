/***************************************************************************
 *
 * $Id: StFgtSingleEventQA.cxx,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtSingleEventQA.cxx,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.5  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.4  2012/01/26 13:13:12  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.3  2012/01/24 06:27:35  sgliske
 * debugged a bit more
 *
 * Revision 1.2  2012/01/24 05:45:31  sgliske
 * debugged--mostly :)
 *
 * Revision 1.1  2012/01/24 03:32:16  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtSingleEventQA.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

#include <string>
#include <TFile.h>
#include <TH2F.h>

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"

// constructors
StFgtSingleEventQA::StFgtSingleEventQA( const Char_t* name ) : StMaker( name ), mTFile( 0 ) {
   // nothing
};

// deconstructor
StFgtSingleEventQA::~StFgtSingleEventQA(){
   if( mTFile ){
      //mTFile->Close();
      delete mTFile;
   };
};

Int_t StFgtSingleEventQA::Init(){
   Int_t ierr = kStOk;

   mEventNum = 0;

   if( mFilename.empty() ){
      mFilename = "fgtSingleEvents.root";
      LOG_INFO << "Empty filename.  Defaulting to '" << mFilename << "'" << endl;
   };

   mTFile = new TFile( mFilename.data(), "RECREATE" );
   if( !mTFile->IsOpen() ){
      LOG_FATAL << "Error opening file '" << mFilename << "' "<< endm;
      ierr = kStFatal;
   };

   return ierr;
};

Int_t StFgtSingleEventQA::Make(){
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   mFgtCollectionPtr = 0;

   eventPtr = (StEvent*)GetInputDS("StEvent");
   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent in '" << ClassName() << "'" << endm;
      ierr = kStErr;
   } else {
      mFgtCollectionPtr=eventPtr->fgtCollection();

      if( !mFgtCollectionPtr) {
         LOG_ERROR << "Error getting pointer to StFgtCollection in '" << ClassName() << "'" << endm;
         ierr = kStErr;
      };
   };

   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      StFgtStripCollection *stripCollectionPtr = 0;
      if( !ierr ){
         stripCollectionPtr = mFgtCollectionPtr->getStripCollection( disc );
      };

      if( stripCollectionPtr ){
         TH2F *hP[kFgtNumOctantsPerDisc*kFgtNumTimeBins], *hR[kFgtNumOctantsPerDisc*kFgtNumTimeBins];
         for( TH2F** hPptr = hP, **hRptr = hR; hPptr != &hP[kFgtNumOctantsPerDisc*kFgtNumTimeBins]; ++hPptr, ++hRptr )
            (*hPptr) = (*hRptr) = 0;

         const StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
         StSPtrVecFgtStripConstIterator stripIter;

         for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
            Int_t geoId = (*stripIter)->getGeoId();

            for( Int_t tb = 0; tb < kFgtNumTimeBins; ++tb ){
               Short_t adc = (*stripIter)->getAdc(tb);

               // to store position data
               Short_t disc, quad, strip;
               Char_t layer;
               StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

               // Note: r strips 0-279 and phi strips 0-359 are for
               // the half of the quadrant with local coordinate phi
               // in the range of pi/4 and pi/2.  Note: phi increases
               // for decreasing phi strip number.  r strips in
               // 400-679 and phi strips in 360-719 are for the local
               // coordinate phi in the range 0 to pi/4.  Short side
               // is for phi strips near 720, i.e. short side has
               // smaller local phi values than long side.
               Bool_t octIsShort = ( layer == 'R' ? (strip > 279) : (strip > 359) );

               Int_t bin = ( quad*2 + octIsShort )*kFgtNumTimeBins + tb;
               if( !hP[bin] )
                  makeHists( mEventNum, tb, disc, quad, octIsShort, hP[bin], hR[bin] );

               if( layer == 'P' ){
                  Double_t pos, high = 0, low = 0;
                  StFgtGeom::getPhysicalCoordinate( geoId, disc, quad, layer, pos, low, high );

                  hP[bin]->SetBinContent( strip+1, 2, adc );  // larger r
                  if( low < 19.125 )
                     hP[bin]->SetBinContent( strip+1, 1, adc ); // smaller r--not all strips extend here
               } else {
                  hR[bin]->SetBinContent( 1, strip+1, adc );
               };
            };
         };

         mTFile->cd();
         if( mTFile && mTFile->IsOpen() )
            for( TH2F **hPptr = hP, **hRptr = hR; hPptr != &hP[kFgtNumOctantsPerDisc*kFgtNumTimeBins]; ++hPptr, ++hRptr )
               if( (*hPptr) ){
                  (*hPptr)->Write();
                  (*hRptr)->Write();
               };
      };
   };

   ++mEventNum;
   return ierr;
};

Int_t StFgtSingleEventQA::Finish(){
   mTFile->Close();
   return kStOk;
};

void StFgtSingleEventQA::makeHists( Int_t ev, Int_t tb, Int_t disc, Int_t quad, Bool_t isShort, TH2F* &hP, TH2F* &hR ){
   std::stringstream ss;
   ss << "h" 
      << "_e" << ev
      << "_t" << tb
      << "_d" << disc
      << "_q" << quad
      << ( isShort ? 'S' : 'L' );
   std::string nameP = ss.str() + "_P";
   std::string nameR = ss.str() + "_R";

   mTFile->cd();
   if( isShort ){
      hP = new TH2F( nameP.data(), "",  360, 0, 360, 2, 0, 2 );
      hR = new TH2F( nameR.data(), "", 1, 0, 0.7854, 280, 0, 280 );
   } else {
      hP = new TH2F( nameP.data(), "",  360,    360,    720,   2, 0, 2 );
      hR = new TH2F( nameR.data(), "",    1, 0.7854, 1.5708, 280, 400, 680 );
   };
};

ClassImp(StFgtSingleEventQA);
