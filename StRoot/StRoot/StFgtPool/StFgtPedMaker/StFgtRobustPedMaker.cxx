/*!
 * \class StFgtRobustPedMaker 
 * \author S. Gliske, Jan 2012
 */

/***************************************************************************
 *
 * $Id: StFgtRobustPedMaker.cxx,v 1.3 2013/02/10 14:43:55 akio Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header
 *
 ***************************************************************************
 *
 * $Log: StFgtRobustPedMaker.cxx,v $
 * Revision 1.3  2013/02/10 14:43:55  akio
 * slightly modified to gain better accuracy
 *
 * Revision 1.2  2012/01/31 16:47:47  wwitzke
 * Changed for cosmic test stand change.
 *
 * Revision 1.1  2012/01/31 08:52:51  sgliske
 * StFgtPedMaker moved to StFgtPool
 *
 * Revision 1.7  2012/01/30 10:42:22  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.6  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.5  2012/01/24 06:57:42  sgliske
 * forgot to take out unused get geoId
 *
 * Revision 1.4  2012/01/24 06:56:42  sgliske
 * directly use elec coord domian--no geoIds
 *
 * Revision 1.3  2012/01/18 18:53:01  sgliske
 * minor bug fix
 *
 * Revision 1.2  2012/01/17 21:56:26  sgliske
 * Short_t geoId -> Int_t geoId
 *
 * Revision 1.1  2012/01/17 20:08:49  sgliske
 * creation
 *
 *
 **************************************************************************/

#include <string>
#include "StFgtRobustPedMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

#define ONE_OVER_TWICE_SQRT_LOG_TWO 0.6005612

// constructor
StFgtRobustPedMaker::StFgtRobustPedMaker( const Char_t* name ) :
   StFgtPedMaker( name ), mNumBins(100) , mMaxAdc(1250), mNumSmooth(10), mInternalEventNum(0) {

   // set to all zeros
   mHistVec.assign( mDataVec.size(), (TH1F*)0 );
};

// make, i.e. compute histograms
// actual pedistals are computed in ::Finish()
Int_t StFgtRobustPedMaker::Make(){
   Int_t ierr = kStOk;

   //cout << "StFgtRobustPedMaker::Make() internal event number " << mInternalEventNum++ << endl;

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
      std::stringstream ss;
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;

            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               for( Int_t timeBin = 0; timeBin < kFgtNumTimeBins; ++timeBin ){
		 Bool_t pass = ((mTimeBinMask==0 || ( (1<<timeBin) & mTimeBinMask)) && timeBin >= 0 && timeBin < kFgtNumTimeBins);
                  if( pass ){
                     Short_t adc = (*stripIter)->getAdc( timeBin );

                     if( adc ){

		        Int_t t = timeBin;
		        if(mTimeBinMask==0) t=0;

                        Int_t rdo, arm, apv, channel;
                        (*stripIter)->getElecCoords( rdo, arm, apv, channel );
                        Int_t elecId = StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );

                        Int_t code = kFgtNumTimeBins * elecId + t;

                        TH1F* hist = mHistVec[ code ];
                        if( !hist ){
                           ss.str("");
                           ss.clear();
                           ss << "hStripADC_" << code;
                           hist = new TH1F( ss.str().data(), "", mNumBins, 0, mMaxAdc );
                           mHistVec[ code ] = hist;
                        };

                        hist->Fill( float(adc) );
                     };
                  };
               };
            };
         };
      };
   };

   return ierr;
};

// save as needed
Int_t StFgtRobustPedMaker::Finish(){
   Int_t ierr = kStOk;

   if( !mHasFinished ){
      cout << "StFgtRobustPedMaker::Finish()" << endl;

      std::vector< TH1F* >::iterator mHistVecIter;

      //TF1 gaus( "gaus", "[0]*TMath::Gaus( x, [1], [2] )", 0, mMaxAdc );

      for( mHistVecIter = mHistVec.begin(); mHistVecIter != mHistVec.end(); ++mHistVecIter ){
         // for easier-to-read code
         TH1F *hist = *mHistVecIter;

         if( hist ){
            // smooth for good measure
            if( mNumSmooth ) hist->Smooth( mNumSmooth );

            // pedistal value is MPV
            Int_t maxLoc = hist->GetMaximumBin();
            Float_t halfMax = 0.5*hist->GetBinContent( maxLoc );
            Float_t pedValue = hist->GetBinCenter( maxLoc );
            Float_t mean = hist->GetMean();

            // estimate the half width at half max on both sides
            Int_t binIdxR;
            for( binIdxR = maxLoc + 1; binIdxR <= hist->GetNbinsX() && hist->GetBinContent( binIdxR ) > halfMax; ++binIdxR );
            Float_t HWHMR = ( hist->GetBinCenter( binIdxR ) - pedValue );

            // estimate the sigma from the HWHM on the left
            Int_t binIdxL;
            for( binIdxL = maxLoc - 1; binIdxL > 0 && hist->GetBinContent( binIdxL ) > halfMax; --binIdxL );
            Float_t HWHML = ( pedValue - hist->GetBinCenter( binIdxL ) );

            // estimate sigma
            Float_t sigma = ( HWHML + HWHMR ) * ONE_OVER_TWICE_SQRT_LOG_TWO;
            Float_t rms = hist->GetRMS();
	    Float_t sig3 = 3.0*rms;
	    if(sig3<100) sig3=100;
	    
            // restrict to with three sigma
            hist->GetXaxis()->SetRangeUser(mean-sig3, mean+sig3);

            // update mean and sigma
            Float_t mean2 = hist->GetMean();
            Float_t rms2  = hist->GetRMS();

            // estimate percentage in one sigma
            Float_t fracClose = hist->Integral(hist->FindBin(mean2-rms2), hist->FindBin(mean2+rms2)) / hist->GetEntries();

            // save to map, note:
            // sum == mean
            // sumsq == st. dev. or RMS, etc

            Int_t code = std::distance( mHistVec.begin(), mHistVecIter );
            pedData_t &data = mDataVec[ code ];
            data.n = hist->GetEntries();
	    data.ped = mean2;
            data.RMS = rms2;
            data.fracClose = fracClose;

	    //int eid=code/kFgtNumTimeBins;
	    //printf("Eleid=%5d Max=%6.2f Mean=%6.2f %6.2f %6.2f  RMS=%6.2f %6.2f %6.2f  f=%6.2f\n",
	    // 	      eid,halfMax*2,pedValue,mean,mean2,sigma,rms,rms2,fracClose);

         };
      };

      ierr = saveToFile();
   };

   return ierr;
};

ClassImp( StFgtRobustPedMaker );
