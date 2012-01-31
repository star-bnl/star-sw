/***************************************************************************
 *
 * $Id: StFgtCosmicTrackQA.cxx,v 1.2 2012/01/31 16:48:34 wwitzke Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 18 2011 
 *
 ***************************************************************************
 *
 * Description:  Plotting class for different histograms concerning 
 * cosmicstand tracks
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrackQA.cxx,v $
 * Revision 1.2  2012/01/31 16:48:34  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.1  2012/01/31 09:26:16  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.11  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.10  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.9  2011/12/07 17:19:59  ckriley
 * minor update
 *
 * Revision 1.8  2011/11/25 20:20:04  ckriley
 * added statusmaker functionality
 *
 * Revision 1.7  2011/11/09 21:03:20  ckriley
 * working version with current containers
 *
 * Revision 1.6  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.4  2011/10/26 20:39:18  ckriley
 * compilation fix
 *
 * Revision 1.3  2011/10/25 20:43:32  ckriley
 * added more plots
 *
 * Revision 1.2  2011/10/25 15:42:45  ckriley
 * minor change
 *
 * Revision 1.1  2011/10/25 15:41:53  ckriley
 * creation of StFgtCosmicTrackQA
 *
 **************************************************************************/

#include "StFgtCosmicTrackQA.h"

#include <string>
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "StMaker.h"

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"


#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StEvent/StFgtPoint.h"

// constructors
StFgtCosmicTrackQA::StFgtCosmicTrackQA( const Char_t* name,
                                          Short_t discId,
                                          Short_t quadId,
                                          const Char_t* quadName ) :
   StFgtQaMaker( name, discId, quadId, quadName ),
   mPath(""), mFileNameBase(""), mFileNameKey( quadName )/*,mHist(0)*/{

    // set the style to plain
    gROOT->SetStyle("Plain");

    // few defaults
    mXbins = 1280;
    mXmin = 0;
    mXmax = mXbins;

    mYbins = 4096;
    mYmin = 0;
    mYmax = mYbins;
};

StFgtCosmicTrackQA::StFgtCosmicTrackQA(const StFgtCosmicTrackQA&){
  std::cerr << "TODO" << endl;
  throw 0;
};

// deconstructor
StFgtCosmicTrackQA::~StFgtCosmicTrackQA(){
/*
    if( mHistAPV ) {
      for( Int_t i=0;i<10;i++) {
        delete mHistAPV[i];
      }
    }
    if( mHist1Dcluster ) {
      for( Int_t i=0;i<2;i++) {
        delete mHist1Dcluster[i];
      }
    }
    if( mHist1Dclusters ) 
      delete mHist1Dclusters;
    if( mHist2Dpoint )
      delete mHist2Dpoint;
*/
};

// equals operator
StFgtCosmicTrackQA& StFgtCosmicTrackQA::operator=(const StFgtCosmicTrackQA&){
  std::cerr << "TODO" << endl;
  throw 0;
};

// initializing CosmicTrackQA
Int_t StFgtCosmicTrackQA::Init(){
  Int_t ierr = StFgtQaMaker::Init();

  if( !ierr ){
/*
    // Take care of the histogram
    if( mHistAPV ) {
      for( Int_t i=0;i<10;i++) {
        delete mHistAPV[i];
      }
    }
    if( mHist1Dcluster ) {
      for( Int_t i=0;i<2;i++) {
        delete mHist1Dcluster[i];
      }
    }
    if( mHist1Dclusters ) 
      delete mHist1Dclusters;
    if( mHist2Dpoint ) 
      delete mHist2Dpoint;
*/
      // note: 10 APV chips plots per Quadrant
      // naming the APV plots
      std::string name, nameforhist, title;
      {
        std::stringstream ss;
        ss << GetName() << "_" << mDiscId << "_" << mQuadId;
        ss >> name;
      };
      // APV chip binning

      // regular
      mXmin = -200;
      mXmax = 500;
      mXbins = 140;
/*
      // 5sigma
      mXmin = 0;
      mXmax = 4000;
      mXbins = 200;
*/
      string channelId[10]={"0-127","128-255","256-383","384-511","512-639","640-767","768-895","896-1023","1024-1151","1152-1279"};

      // easiest way to ensure stream is cleared is construct a new one
      for(int i=0;i<10;i++) {
          std::stringstream sx, sy, ss, sn, snx, sny;
          ss << " Quad " << mQuadName << ", APV " << i 
             << ", Channels " << channelId[i] << "; adc-ped";
          sn << name;
          sn << "_" << i;
          sn >> nameforhist;
          title = ss.str();
          //cout << "title = " << title << endl;

          mHistAPV[i] = new TH1F( nameforhist.data(), title.data(), mXbins/mBinFactorX,  mXmin, mXmax );
      }

      // naming test hist by plane
{
       std::string name1DR, name1DP, title1DR, title1DP;
      {
        std::stringstream ss, sr, sp;
        ss << GetName() << "_" << mDiscId << "_" << mQuadId;
        sr << ss.str() << "_Rtest";
        sp << ss.str() << "_Ptest";
        sr >> name1DR;
        sp >> name1DP;
      };
      // test plot binning
      mXmin = 0;
      mXmax = 720;
      mXbins = 720;

      // easiest way to ensure stream is cleared is construct a new one
      std::stringstream ss, sr ,sp;
      ss << " Quad " << mQuadName << " hits";
      sr << ss.str() << " in R; strip";
      sp << ss.str() << " in Phi; strip";
      title1DR = sr.str();
      title1DP = sp.str();
      //cout << "title = " << title1DR << endl;

      testHist[0] = new TH1F( name1DR.data(), title1DR.data(), mXbins/mBinFactorX,  /*mXmin, mXmax*/ 0,720 );
      testHist[1] = new TH1F( name1DP.data(), title1DP.data(), mXbins/mBinFactorX,  /*mXmin, mXmax*/ 0,720 );
}


      // naming 1D clusters by plane
      std::string name1D, name1DR, name1DP, title1D, title1DR, title1DP;
      {
        std::stringstream ss, sr, sp;
        ss << GetName() << "_" << mDiscId << "_" << mQuadId;
        sr << ss.str() << "_R";
        sp << ss.str() << "_P";
        ss << "_C";
        ss >> name1D;
        sr >> name1DR;
        sp >> name1DP;
      };
      // 1D cluster plot binning

      // regular
      mXmin = 0;
      mXmax = 4000;
      mXbins = 400;

/*
      // 5sigma
      mXmin = 0;
      mXmax = 6000;
      mXbins = 200;
*/
      // easiest way to ensure stream is cleared is construct a new one
      std::stringstream ss, sr ,sp;
      ss << " Quad " << mQuadName << " clusters";
      sr << ss.str() << " in R; adc sum";
      sp << ss.str() << " in Phi; adc sum";
      ss << " in R & Phi; adc sum";
      title1D  = ss.str();
      title1DR = sr.str();
      title1DP = sp.str();
      //cout << "title = " << title1DR << endl;

      mHist1Dclusters   = new TH1F( name1D.data(), title1D.data(), mXbins/mBinFactorX,  mXmin, mXmax/*8000*/ );
      mHist1Dcluster[0] = new TH1F( name1DR.data(), title1DR.data(), mXbins/mBinFactorX,  mXmin, mXmax );
      mHist1Dcluster[1] = new TH1F( name1DP.data(), title1DP.data(), mXbins/mBinFactorX,  mXmin, mXmax );


      // naming 2D point plot
      std::string name2D, title2D;
      {
        std::stringstream ss;
        ss << GetName() << "_" << mDiscId << "_" << mQuadId;
        ss >> name2D;
      };
      // 2Dcluster plot binning
      mXmin = 0;
      mXmax = 40;
      mXbins = 40;
    
      // easiest way to ensure stream is cleared is construct a new one
      std::stringstream sstream;
      sstream << " Quad " << mQuadName << " cluster points; cm; cm";
      title2D = sstream.str();
      //cout << "title = " << title2D << endl;

      mHist2Dpoint = new TH2F( name2D.data(), title2D.data(), mXbins/mBinFactorX,  mXmin, mXmax, mXbins/mBinFactorX, mXmin, mXmax );

   };
   return ierr;
};

// collect hit info for QA
Int_t StFgtCosmicTrackQA::Make(){
   Int_t ierr = StFgtQaMaker::Make();

   StFgtStripCollection *stripCollectionPtr = 0;
   if( !ierr ){
      stripCollectionPtr = mFgtCollectionPtr->getStripCollection( mDiscId );
   };

   if( stripCollectionPtr ){

      const StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
      StSPtrVecFgtStripConstIterator stripIter;

      for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
         // this ADC is adc-ped if StFgtCorAdcMaker is called first
         Int_t geoId = (*stripIter)->getGeoId();
         Short_t adc = (*stripIter)->getAdc( mTimeBin );

         // to store position data
         Short_t disc, quad; //, strip;
         Char_t layer;

         Double_t pos, high, low;

         //StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );
         StFgtGeom::getPhysicalCoordinate( geoId, disc, quad, layer, pos, high, low );
         if( disc == mDiscId && quad == mQuadId ){
            Int_t rdo, arm, apv, channel;
            StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId,rdo,arm,apv,channel);
                     
            // fill histogram per apv chip
            if(disc==2) apv -= 12;  // for disc2, apv# range from 12-23
            mHistAPV[apv]->Fill ( adc );
         };
 
         // looking for hot channels simpleClusterAlgo use this
         Short_t stripId;
         StFgtGeom::decodeGeoId(geoId,disc,quad,layer,stripId);
         if(layer=='R') testHist[0]->Fill( stripId );
         else           testHist[1]->Fill( stripId );
      };
   };


   StFgtHitCollection *hitCollectionPtr = 0;
   if( !ierr ){
      hitCollectionPtr = mFgtCollectionPtr->getHitCollection( mDiscId );
   };

   if( hitCollectionPtr ){
      const StSPtrVecFgtHit &hitVec = hitCollectionPtr->getHitVec();
      StSPtrVecFgtHitConstIterator hitIter;

      Short_t adcSum=0;
      // LOG_INFO << "We have " << hitVec.size() << " clusters " << endm;
      for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter ){
         Bool_t haveR=false;

         haveR=((*hitIter)->getLayer()=='R');

/*         // looking for hot channels maxClusterAlgo use this
         Int_t geoId = (*hitIter)->getCentralStripGeoId();
         //Int_t rdo, arm, apv, channel;
         // StFgtCosmicTestStandGeom::getNaive...
         Short_t disc, quad, stripId;
         Char_t layer;
         StFgtGeom::decodeGeoId(geoId,disc,quad,layer,stripId);
         if(haveR) testHist[0]->Fill( stripId );
         else      testHist[1]->Fill( stripId );
*/

         if( haveR ) mHist1Dcluster[0]->Fill( (*hitIter)->charge() );
         else        mHist1Dcluster[1]->Fill( (*hitIter)->charge() );
         adcSum += (*hitIter)->charge();
      };
      //if(adcSum>=200) mHist1Dclusters->Fill( adcSum );
      if(adcSum!=0) mHist1Dclusters->Fill( adcSum ); // for ped files
   };

   StFgtPointCollection *pointCollectionPtr = 0;
   if( !ierr ){
      pointCollectionPtr = mFgtCollectionPtr->getPointCollection();
   };

   if( pointCollectionPtr ){
      const StSPtrVecFgtPoint &pointVec = pointCollectionPtr->getPointVec();
      StSPtrVecFgtPointConstIterator pointIter;

      // LOG_INFO << "We have " << pointVec.size() << " points " << endm;
      for( pointIter = pointVec.begin(); pointIter != pointVec.end(); ++pointIter ){
         if( (*pointIter)->getDisc() == mDiscId ){
            Float_t R = (*pointIter)->getPositionR();
            Float_t Phi = (*pointIter)->getPositionPhi();

            mHist2Dpoint->Fill( R*cos(Phi+.2618) , R*sin(Phi+.2618) );
         };
      };
   };

   return ierr;
};

// make the histogram pretty and save it to a file, if given a filename base
Int_t StFgtCosmicTrackQA::Finish(){
   //cout << "StFgtCosmicTrackQA::Finish()" << endl;
   // remove hot bins (over 3 sigma say) -> I actually want to look at specific strip...  for now just look at removing hot bins
/*   Float_t avg2Dpoint = 0;
   Float_t sigma = 0;
   Int_t numXbins = 40;
   Int_t numYbins = 40;
   for(Int_t i=0;i<numXbins;i++) {
     for(Int_t j=0;j<numYbins;j++) {
       Float_t con2Dpoint = mHist2Dpoint -> GetBinContent(i+1,j+1);
       avg2Dpoint += con2Dpoint;
     }
   }
   avg2Dpoint /= numXbins*numYbins;
   for(Int_t i=0;i<numXbins;i++) {
     for(Int_t j=0;j<numYbins;j++) {
       Float_t con2Dpoint = mHist2Dpoint -> GetBinContent(i+1,j+1);
       sigma += (con2Dpoint-avg2Dpoint)*(con2Dpoint-avg2Dpoint);
     }
   }
   sigma /= numXbins*numYbins;
   sigma = sqrt(sigma);
   for(Int_t i=0;i<numXbins;i++) {
     for(Int_t j=0;j<numYbins;j++) {
       Float_t con2Dpoint = mHist2Dpoint -> GetBinContent(i+1,j+1);
       if(con2Dpoint > avg2Dpoint+3*sigma)
         mHist2Dpoint -> SetBinContent(i+1,j+1,0);
     }
   }
*/
/*
   // Find hot strips and for removal > 5 sigma
   Float_t avgStripHitsR = 0, avgStripHitsP = 0;
   Float_t sigmaR = 0, sigmaP = 0;
   Int_t numXbins = 720;
   for(Int_t i=0;i<numXbins;i++) {
     Float_t conStripHitsR = testHist[0] -> GetBinContent(i+1);
     Float_t conStripHitsP = testHist[1] -> GetBinContent(i+1);
     avgStripHitsR += conStripHitsR;
     avgStripHitsP += conStripHitsP;
   }
   avgStripHitsR /= numXbins;
   avgStripHitsP /= numXbins;
   for(Int_t i=0;i<numXbins;i++) {
     Float_t conStripHitsR = testHist[0] -> GetBinContent(i+1);
     sigmaR += (conStripHitsR-avgStripHitsR)*(conStripHitsR-avgStripHitsR);
     Float_t conStripHitsP = testHist[1] -> GetBinContent(i+1);
     sigmaP += (conStripHitsP-avgStripHitsP)*(conStripHitsP-avgStripHitsP);
   }
   sigmaR /= numXbins;
   sigmaP /= numXbins;
   sigmaR = sqrt(sigmaR);
   sigmaP = sqrt(sigmaP);
   cout << "\nFor " << mFileNameKey << "\nHot R channels: ";
   for(Int_t i=0;i<numXbins;i++) {
     Float_t conStripHitsR = testHist[0] -> GetBinContent(i+1);
     if(conStripHitsR > avgStripHitsR+5*sigmaR)
       cout << i << " ";
   }
   cout << "\nHot Phi channels: ";
   for(Int_t i=0;i<numXbins;i++) {
     Float_t conStripHitsP = testHist[1] -> GetBinContent(i+1);
     if(conStripHitsP > avgStripHitsP+5*sigmaP)
       cout << i << " ";
   }
   cout << endl;
*/
   if( !mFileNameBase.empty() ){

      // filename
      std::string filename;
      if( !mPath.empty() )
         filename = mPath + "/";
      filename += mFileNameBase;
      if( !mFileNameKey.empty() )
         filename += std::string( "." ) + mFileNameKey;
      filename += ".eps";

      // draw
      gROOT->SetStyle("Plain");
      TCanvas *canAPV = new TCanvas( "fgtTrackQAcanAPV", "fgtTrackQAcanAPV", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      Int_t pad=1;
      canAPV->Divide(5,2);
      for( Int_t i=0;i<10;i++) {
        canAPV->cd(pad);
        mHistAPV[i]->Draw();
        pad++;
      }
  
      TCanvas *can2Dpoint = new TCanvas( "fgtTrackQAcan2Dpoint", "fgtTrackQAcan2Dpoint", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      mHist2Dpoint->Draw();
      
 
      TCanvas *can1Dcluster = new TCanvas( "fgtTrackQAcan1Dcluster", "fgtTrackQAcan1Dcluster", 900, 400 );
      gStyle->SetOptStat(100000);
      gStyle->SetOptDate(0);
      can1Dcluster->Divide(2,1);
      can1Dcluster->cd(1);
      mHist1Dcluster[0]->Draw();
      can1Dcluster->cd(2);
      mHist1Dcluster[1]->Draw();

      TCanvas *can1Dclusters = new TCanvas( "fgtTrackQAcan1Dclusters", "fgtTrackQAcan1Dclusters", 900, 400 );
      gStyle->SetOptStat(100000);
      gStyle->SetOptDate(0);
      mHist1Dclusters->Draw();

      canAPV->Print( filename.data() );
      can2Dpoint->Print( filename.data() );
      can1Dcluster->Print( filename.data() );
      can1Dclusters->Print( filename.data() );
      delete canAPV;
      delete can2Dpoint;
      delete can1Dcluster;
      delete can1Dclusters;
   };

   return kStOk;
};

void StFgtCosmicTrackQA::Clear( Option_t *opt ) {

};


ClassImp(StFgtCosmicTrackQA);
