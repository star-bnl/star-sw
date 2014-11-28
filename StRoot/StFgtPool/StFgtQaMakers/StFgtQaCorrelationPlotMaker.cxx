/*!
 * \class StFgtQaCorrelationPlotMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtQaCorrelationPlotMaker.cxx,v 1.2 2012/01/31 16:48:34 wwitzke Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaCorrelationPlotMaker.cxx,v $
 * Revision 1.2  2012/01/31 16:48:34  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.9  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.8  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.7  2012/01/17 21:56:26  sgliske
 * Short_t geoId -> Int_t geoId
 *
 * Revision 1.6  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.5  2011/10/10 17:41:26  sgliske
 * Debugged option for vs. r or phi strips
 *
 * Revision 1.4  2011/10/07 20:56:18  sgliske
 * adding StFgtQaClusterChargePerAPV
 *
 * Revision 1.3  2011/09/30 19:05:31  sgliske
 * general update
 *
 * Revision 1.2  2011/09/30 17:08:34  sgliske
 * Update for geoId->elecCoord function now in StFgtCosmicTestStandGeom
 *
 * Revision 1.1  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 *
 **************************************************************************/

#include "StFgtQaCorrelationPlotMaker.h"

#include <string>
#include <vector>
#include <map>
#include <TH2F.h>
#include <TROOT.h>
#include <TStyle.h>

#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"


#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"

// constructors
StFgtQaCorrelationPlotMaker::StFgtQaCorrelationPlotMaker( const Char_t* name,
                                                          Short_t discId,
                                                          Short_t quadId,
                                                          const Char_t* quadName ) :
   StFgtQaMaker( name, discId, quadId, quadName ),
   mComputeCor(1), mCovHist(0), mCorHist(0), mMatrix(0), mSum(0), mN(0) {

   // flag that will ignore peds from external sources
   mDoSubtrPeds = 0;
};

StFgtQaCorrelationPlotMaker::StFgtQaCorrelationPlotMaker(const StFgtQaCorrelationPlotMaker&){
   std::cerr << "TODO" << endl;
   throw 0;
};

// deconstructor
StFgtQaCorrelationPlotMaker::~StFgtQaCorrelationPlotMaker(){
   if( mCovHist )
      delete mCovHist;
   if( mCorHist )
      delete mCorHist;
   if( mMatrix ){
      for( Int_t i=0; i<mNbins; ++i )
         delete[] mMatrix[i];
      delete[] mMatrix;
   };
   if( mSum )
      delete mSum;
   if( mN )
      delete mN;
};

// equals operator
StFgtQaCorrelationPlotMaker& StFgtQaCorrelationPlotMaker::operator=(const StFgtQaCorrelationPlotMaker&){
   std::cerr << "TODO" << endl;
   throw 0;
};

Int_t StFgtQaCorrelationPlotMaker::Init(){
   Int_t ierr = StFgtQaMaker::Init();

   if( mDoVsStrip == 'r' )
      mDoVsStrip = 'R';  // combine 'r' and 'R'

   if( !ierr ){
      // Take care of the histogram

      if( mCovHist )
         delete mCovHist;
      if( mCorHist )
         delete mCorHist;
      if( mMatrix ){
         for( Int_t i=0; i<mNbins; ++i )
            delete[] mMatrix[i];
         delete[] mMatrix;
      };
      if( mSum )
         delete[] mSum;
      if( mN )
         delete[] mN;

      std::string name;
      std::stringstream ss;
      ss << GetName() << "_" << mDiscId << "_" << mQuadId << "_" << mDoVsStrip << mComputeCor;
      ss >> name;

      //cout << "title = " << title << endl;

      mXmin = 0;
      mXmax = 1280;
      mNbins = 1280;
      if( mDoVsStrip == 'R' || mDoVsStrip == 'P' ){
         mXmin = 0;
         mXmax = 720;
      };

      mXbins = mXmax;

      mNbins = mXbins/mBinFactorX;
      mCovHist = new TH2F( name.data(), "", mNbins, mXmin, mXmax, mNbins, mXmin, mXmax );

      cout << "Size of TH2F is " << mNbins << ' ' << mXmin << ' ' << mXmax << endl;

      std::string title;
      getTitle( 0, title );
      mCovHist->SetTitle( title.data() );

      if( mComputeCor ){
         mCorHist = new TH2F( (name + "_cor").data(), "", mNbins, mXmin, mXmax, mNbins, mXmin, mXmax );
         getTitle( 1, title );
         mCorHist->SetTitle( title.data() );
      };

      mN = new Int_t [mNbins];
      mSum = new Float_t [mNbins];
      mMatrix = new Float_t* [mNbins];
      for( Int_t i=0; i<mNbins; ++i ){
         mN[i] = 0;
         mSum[i] = 0;
         mMatrix[i] = new Float_t [mNbins];
         for( Int_t j=0; j<mNbins; ++j )
            mMatrix[i][j] = 0;
      };
   };

   return ierr;
};

Int_t StFgtQaCorrelationPlotMaker::Make(){
   Int_t ierr = StFgtQaMaker::Make();

   // faster(?) and cleaner if copy over pointers to a vector
   std::map< Int_t, Float_t > hitMap;
   std::map< Int_t, Int_t > countMap;

   StFgtStripCollection *stripCollectionPtr = 0;
   if( !ierr ){
      stripCollectionPtr = mFgtCollectionPtr->getStripCollection( mDiscId );
   };

   if( stripCollectionPtr ){
      const StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
      StSPtrVecFgtStripConstIterator stripIter;
      stripWeightMap_t::iterator stripMapIter;

      for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
         Int_t geoId = (*stripIter)->getGeoId();
         Short_t adc = (*stripIter)->getAdc( mTimeBin );

         // to store position data
         Short_t disc, quad, strip;
         Char_t layer;

         StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

         if( disc == mDiscId && quad == mQuadId ){
            Int_t idx = 0;
            Bool_t pass = 0;

            if( mDoVsStrip == 'R' || mDoVsStrip == 'P' ){
               pass = ( layer == mDoVsStrip );
               idx = strip;
            } else {
               pass = 1;
               Int_t rdo, arm, apv, channel;
               StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId,rdo,arm,apv,channel);
               idx = 128*(apv%12) + channel;
            };

            if( pass ){
               idx /= mBinFactorX;
               hitMap[idx] += adc;
               countMap[idx] += 1;
            };
         };
      };
   };

//    if( mDoVsStrip == 'R' || mDoVsStrip == 'P' )
//       cout << mDoVsStrip << " Number of hits = " << hitMap.size() << endl;

   // now contribute to sum
   if( !hitMap.empty() ){

      std::map< Int_t, Float_t >::iterator iter1, iter2;

      for( iter1 = hitMap.begin(); iter1 != hitMap.end(); ++iter1 ){
         iter1->second /= countMap[ iter1->first ];

         // count
         ++(mN[ iter1->first ]);

         // the sum
         // cout << "bin " << iter1->first << endl;
         mSum[ iter1->first ] += iter1->second;

         // diagonal
         mMatrix[ iter1->first ][ iter1->first ] += iter1->second * iter1->second;

         // lower diagonal
         for( iter2 = hitMap.begin(); iter2 != iter1; ++iter2 ){
            Int_t idx1 = iter1->first;
            Int_t idx2 = iter2->first;
            if( idx2 > idx1 ){
               Int_t temp = idx2;
               idx2 = idx1;
               idx1 = temp;
            };
            mMatrix[ idx1 ][ idx2 ] += iter1->second * iter2->second;
         };
      };
   };

   return ierr;
};

// scale the histogram, fill the other triangle, and possibly convert covariance to correlation
Int_t StFgtQaCorrelationPlotMaker::Finish(){
   Int_t ierr = kStOK;

   // expect all mN's to be the same
   Int_t N = mN[0];
   Int_t i;
   for( i = 0; i < mNbins && !ierr; ++i )
      ierr = ( mN[i] != N && mN[i] );

   if( ierr ){
      cerr << "ERROR: number of counts per channel not consistant" << endl;
      cerr << "Bin[0] = " << mN[0] << endl;
      cerr << "Bin[" << i << "] = " << mN[i] << endl;
   };

   // scale by N (faster to do in later loops, but cleaner to have it
   // in its own loop)
   for( Int_t i = 0; i < mNbins && !ierr; ++i ){
      if( !N )
         N = 1;

      mSum[i] /= N;

      for( Int_t j = 0; j <= i && !ierr; ++j )
         mMatrix[i][j] /= N;
   };

   // n/(n-1) factor
   Float_t nFactor = ( N>1 ? N/((Float_t)(N-1)) : 1 );

   // copy to the histogram
   for( Int_t i = 0; i < mNbins && !ierr; ++i ){
      for( Int_t j = 0; j <= i && !ierr; ++j ){

         Float_t cov = mMatrix[i][j] - mSum[i]*mSum[j];
         cov *= nFactor;
         mCovHist->SetBinContent( i+1, j+1, cov );
         mCovHist->SetBinContent( j+1, i+1, cov );
      };
   };

   if( mComputeCor ){
      for( Int_t i = 0; i < mNbins && !ierr; ++i ){
         Float_t sigmaSq1 = mCovHist->GetBinContent( i+1, i+1 );
         for( Int_t j = 0; j <= i && !ierr; ++j ){
            Float_t sigmaSq2 = mCovHist->GetBinContent( j+1, j+1 );
            Float_t cov = mCovHist->GetBinContent( i+1, j+1 );
            Float_t denom = sigmaSq1 * sigmaSq2;
            denom = (denom > 0 ? sqrt(denom) : 0 );
            Float_t cor = (denom ? cov/denom : 0);

            if( cor > 1 || cor < -1 ){
               if( i == j ){
                  cor = 1;
               } else {
                  ierr = 127;
                  cerr << "FATAL ERROR: abs. val. of correlation coefficient is greater than 1" << endl;
                  cerr << cor << ' ' << cov << ' ' << sigmaSq1 << ' ' << sigmaSq2 << endl;
                  cerr << mDoVsStrip << endl;
               };
            };

            mCorHist->SetBinContent( j+1, i+1, cor );
            mCorHist->SetBinContent( i+1, j+1, cor );
         };
      };
   };

   return ierr;
};

void StFgtQaCorrelationPlotMaker::getTitle( Bool_t isCor, std::string& title ){
   std::stringstream ss;

   if( mDoVsStrip == 'R' )
      ss << "r-strips: ";
   else if( mDoVsStrip == 'P' )
      ss << "#phi-strips: ";

   if( isCor )
      ss << "Correlation Coefficients ";
   else
      ss << "Covariance ";

   if( mDoVsStrip == 'R' || mDoVsStrip == 'P' )
      ss << "vs. Strip";
   else
      ss << "vs. Channel";

   ss << ", Quad " << mQuadName;

   if( mDoVsStrip == 'R' )
      ss << "; r strip id.";
   else if( mDoVsStrip == 'P' )
      ss << "; #phi strip id.";
   else
      ss << "; 128x(APV Num) + Channel Id.";

   title = ss.str();
};

ClassImp(StFgtQaCorrelationPlotMaker);
