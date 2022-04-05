/***************************************************************************
 *
 * $Id: StFgtCosmicTrackPlots.cxx,v 1.2 2012/01/31 23:37:17 avossen Exp $ 
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 18 2011 
 *
 ***************************************************************************
 *
 * Description:  Plotting class for different histograms concerning 
 * cosmicstand tracks of middle quadrant
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrackPlots.cxx,v $
 * Revision 1.2  2012/01/31 23:37:17  avossen
 * fixed StFgtCosmicTrackMaker paths
 *
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.3  2012/01/31 16:56:43  wwitzke
 * Changing for cosmic test stand.
 *
 * Revision 1.2  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.1  2012/01/17 20:03:05  sgliske
 * moved from StFgtQaMakers
 *
 * Revision 1.12  2011/12/07 17:19:59  ckriley
 * minor update
 *
 * Revision 1.11  2011/11/25 20:20:04  ckriley
 * added statusmaker functionality
 *
 * Revision 1.10  2011/11/09 21:03:19  ckriley
 * working version with current containers
 *
 * Revision 1.9  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.7  2011/10/26 20:39:18  ckriley
 * compilation fix
 *
 * Revision 1.6  2011/10/25 20:43:32  ckriley
 * added more plots
 *
 * Revision 1.5  2011/10/25 15:38:51  ckriley
 * more plots
 *
 * Revision 1.3  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#include "StFgtCosmicTrackPlots.h"

#include <string>
#include <TH1F.h>
#include <TProfile.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "StMaker.h"

#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"
#include "StRoot/StFgtPool/StFgtCosmicTrackMaker/StFgtCosmicTrackMaker.h"
#include "StRoot/StFgtPool/StFgtCosmicTrackMaker/StFgtCosmicTrack.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StEvent/StFgtPoint.h"

// constructors
StFgtCosmicTrackPlots::StFgtCosmicTrackPlots( const Char_t* name,
                                              const Char_t* cosmicTrackerName,
                                              Short_t discId,
                                              Short_t quadId,
                                              const Char_t* quadName ) :
  StMaker( name ), mFgtTrackMakerName( cosmicTrackerName ), mTrackVecPtr( 0 ),
  mDiscId( discId ), mQuadId( quadId ),
  mXbins( 200 ), mXmin( -50 ), mXmax( 50 ),
  mBinFactorX( 1 ), mQuadName( quadName ),
  mPath(""), mFileNameBase(""), mFileNameKey( quadName )/*,mHist(0)*/ {

    // set the style to plain
    gROOT->SetStyle("Plain");

    // few defaults
    mXbins = 60;
    mXmin = -30;
    mXmax = 30;
};

StFgtCosmicTrackPlots::StFgtCosmicTrackPlots(const StFgtCosmicTrackPlots&){
  std::cerr << "TODO" << endl;
  throw 0;
};

// deconstructor
StFgtCosmicTrackPlots::~StFgtCosmicTrackPlots(){
/*
    if( mHistX ) {
      for( Int_t i=0;i<4;i++) {
        for( Int_t j=0;j<4;j++) {
          delete mHistX[i][j];
        }
      }
      //delete mHistX;
    }
    if( mHistY ) {
      for( Int_t i=0;i<4;i++) {
        for( Int_t j=0;j<4;j++) {
          delete mHistY[i][j];
        }
      }
      //delete mHistY;
    }
    for( Int_t i=0;i<2;i++) {
     if( mHist1Dcluster[i] )
      mHist1Dcluster[i]=0;
    }
    if( mHist1Dclusters )
      delete mHist1Dclusters;
    if( mHistTracks ) 
      delete mHistTracks;
    if( mHist2Dpoint ) 
      delete mHist2Dpoint;
    if( mProfileEfficiency )
      delete mProfileEfficiency;
    if( mHist2Dcluster )
      delete mHist2Dcluster;
    if( mProfileChi2 )
      delete mProfileChi2;
    if( mProfileX )
      delete mProfileX;
    if( mProfileY )
      delete mProfileY;
*/
};

// equals operator
StFgtCosmicTrackPlots& StFgtCosmicTrackPlots::operator=(const StFgtCosmicTrackPlots&){
  std::cerr << "TODO" << endl;
  throw 0;
};

// initialize CosmicTrackPlots
Int_t StFgtCosmicTrackPlots::Init(){
  Int_t ierr = kStOk;

   // getting track data
   TObject *dataMaker = GetMaker( mFgtTrackMakerName.data() );
   if( !dataMaker ){
      LOG_FATAL << "::Init() could not get pointer to a maker with name '" << mFgtTrackMakerName << "'" << endm;
      ierr = kStFatal;
   };

   mTrackVecPtr = 0;
   StFgtCosmicTrackMaker *trackMaker = 0;
   if( !ierr ){
      trackMaker = static_cast< StFgtCosmicTrackMaker* >( dataMaker );
      mTrackVecPtr = &(trackMaker->getCosmicTrackVec());
   };

  if( !ierr ){
/*
    // Take care of the histogram
    if( mHistX ) {
      for( Int_t i=0;i<4;i++) {
        for( Int_t j=0;j<4;j++) {
          delete[] mHistX[i][j];
        }
      }
      //delete mHistX;
    }
    if( mHistY ) {
      for( Int_t i=0;i<4;i++) {
        for( Int_t j=0;j<4;j++) {
          delete[] mHistY[i][j];
        }
      }
      //delete mHistY;
    }
    for( Int_t i=0;i<2;i++) {
     if( mHist1Dcluster[i] )
      mHist1Dcluster[i]=0;
    }
    if( mHist1Dclusters )
      delete mHist1Dclusters;
    if( mHistTracks )
      delete mHistTracks;
    if( mHist2Dpoint )
      delete mHist2Dpoint;
    if( mProfileEfficiency )
      delete mProfileEfficiency;
    if( mHist2Dcluster )
      delete mHist2Dcluster;
    if( mProfileChi2 )
      delete mProfileChi2;
    if( mProfileX )
      delete mProfileX;
    if( mProfileY )
      delete mProfileY;
*/

      // naming the plots
      std::string name, nameforhistX, nameforhistY, titleX, titleY;
      {
        std::stringstream ss;
        ss << GetName() << "_" << mDiscId << "_" << mQuadId;
        ss >> name;
      };

      // easiest way to ensure stream is cleared is construct a new one
      {
        std::stringstream sx, sy, ss, snx, sny;
        std::string profTitleX, profTitleY, nameforprofX, nameforprofY;
        sx << "#deltax vs X for Quad " << mQuadName << ";X cm;#deltax cm";
        sy << "#deltay vs Y for Quad " << mQuadName << ";Y cm;#deltay cm";
        snx << "X" << name;
        sny << "Y" << name;
        snx >> nameforprofX;
        sny >> nameforprofY;
        profTitleX = sx.str();
        profTitleY = sy.str();
        mProfileX = new TProfile( nameforprofX.data(), profTitleX.data(), 8, 0., 40. );
        mProfileY = new TProfile( nameforprofY.data(), profTitleY.data(), 8, 0., 40. );
      }

      // naming 1D clusters by plane -> 50 channels per bin
{
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
      mXmin = 0;
      mXmax = 6000;
      mXbins = 40;

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

      mHist1Dclusters   = new TH1F( name1D.data(), title1D.data(), mXbins/mBinFactorX,  mXmin, /*mXmax*/8000 );
      mHist1Dcluster[0] = new TH1F( name1DR.data(), title1DR.data(), mXbins/mBinFactorX,  mXmin, mXmax );
      mHist1Dcluster[1] = new TH1F( name1DP.data(), title1DP.data(), mXbins/mBinFactorX,  mXmin, mXmax );
}

      // naming mHistTracks, mHistEfficiency, mHist2Dcluster and mHistChi2
      {
        std::stringstream st, st2, sc, se, s2, snt, snt2, snc, sne, sn2, ss, ssn;
        std::string nameT, nameT2, nameC, nameE, name2, nameS, 
                    titleT, titleT2, titleC, titleE, title2, titleS;
        st << "Reconstructed tracks on Quad " << mQuadName << "; cm; cm";
        snt << name << "_tracks";
        st2 << "Reconstructed registered tracks on Quad " << mQuadName << "; cm; cm";
        snt2 << name << "_realTracks";
        se << "Efficiency of Quad " << mQuadName << "; cm; cm";
        sne << name << "_efficiency";
        s2 << "Cluster points on Quad " << mQuadName 
           << " of registered tracks; cm; cm";
        sn2 << name << "_realPoints";
        sc << "Chi2 values on Quad " << mQuadName << "; cm; cm";
//        sc << "Distance from track to hit on Quad " << mQuadName << "; cm; cm";
        snc << name << "_chi2";
        ss << "Cluster points on Quad " << mQuadName << "; cm; cm";
        ssn << name << "_points";
        titleT = st.str();
        nameT  = snt.str();
        titleT2= st2.str();
        nameT2 = snt2.str();
        titleE = se.str();
        nameE  = sne.str();
        title2 = s2.str();
        name2  = sn2.str();
        titleC = sc.str(); 
        nameC  = snc.str();
        titleS = ss.str();
        nameS  = ssn.str();

        mXmin = 0;
        mXmax = 40;
        mXbins = 8; 
        mBinFactorX = 2;

        mHistTracks = new TH2F( nameT.data(), titleT.data(), mXbins, mXmin, mXmax, mXbins, mXmin, mXmax );
        mHistRealTracks = new TH2F( nameT2.data(), titleT2.data(), mXbins,mXmin, mXmax, mXbins, mXmin, mXmax );
        mHist2Dpoint = new TH2F( nameS.data(), titleS.data(), mXbins,  mXmin, mXmax, mXbins, mXmin, mXmax );
        mProfileEfficiency = new TProfile2D( nameE.data(), titleE.data(), mXbins/mBinFactorX, mXmin, mXmax, mXbins/mBinFactorX, mXmin, mXmax );
        mHist2Dcluster = new TH2F( name2.data(), title2.data(), mXbins*5, mXmin, mXmax, mXbins*5, mXmin, mXmax );
        mProfileChi2 = new TProfile2D( nameC.data(), titleC.data(), mXbins, mXmin, mXmax, mXbins, mXmin, mXmax );
      }

      mXmin = -30;
      mXmax = 30;
      mXbins = 60;
      mBinFactorX = 1;

      // naming APV histograms
      for(int i=0;i<4;i++) {
        for(int j=0;j<4;j++) {
          std::stringstream sx, sy, ss, sn, snx, sny;
          snx << "X" << name;
          sny << "Y" << name;
          sx << " #deltax:";
          sy << " #deltay:";
          ss << " Quad " << mQuadName << " ";
          if (i==0 && j==0) {ss << "X:0-10 Y:0-10;"; sn << "_1";}
          else if (i==0 && j==1) {ss << "X:0-10 Y:10-20"; sn << "_5";}
          else if (i==0 && j==2) {ss << "X:0-10 Y:20-30"; sn << "_9";}
          else if (i==0 && j==3) {ss << "X:0-10 Y:30-40"; sn << "_13";}
          else if (i==1 && j==0) {ss << "X:10-20 Y:0-10"; sn << "_2";}
          else if (i==1 && j==1) {ss << "X:10-20 Y:10-20"; sn << "_6";}
          else if (i==1 && j==2) {ss << "X:10-20 Y:20-30"; sn << "-10";}
          else if (i==1 && j==3) {ss << "X:10-20 Y:30-40"; sn << "_14";}
          else if (i==2 && j==0) {ss << "X:20-30 Y:0-10"; sn << "_3";}
          else if (i==2 && j==1) {ss << "X:20-30 Y:10-20"; sn << "_7";}
          else if (i==2 && j==2) {ss << "X:20-30 Y:20-30"; sn << "_11";}
          else if (i==2 && j==3) {ss << "X:20-30 Y:30-40"; sn << "_15";}
          else if (i==3 && j==0) {ss << "X:30-40 Y:0-10"; sn << "_4";}
          else if (i==3 && j==1) {ss << "X:30-40 Y:10-20"; sn << "_8";}
          else if (i==3 && j==2) {ss << "X:30-40 Y:20-30"; sn << "_12";}
          else if (i==3 && j==3) {ss << "X:30-40 Y:30-40"; sn << "_16";}

          snx << sn.str();
          sny << sn.str();
          snx >> nameforhistX;
          sny >> nameforhistY;
          sx << ss.str() << ";#deltax cm";
          sy << ss.str() << ";#deltay cm";
          titleX = sx.str();
          titleY = sy.str();
          //cout << "title = " << title << endl;

          mHistX[j][i] = new TH1F( nameforhistX.data(), titleX.data(), mXbins/mBinFactorX,  mXmin, mXmax );
          mHistY[j][i] = new TH1F( nameforhistY.data(), titleY.data(), mXbins/mBinFactorX,  mXmin, mXmax );
        }
      }
   };
   return ierr;
};

//  fill histograms with track data
Int_t StFgtCosmicTrackPlots::Make(){
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection *fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   if( !ierr ){
      Bool_t isTrack = false;

      StFgtCosmicTrackVec::iterator trackIter = mTrackVecPtr->begin();
      for( trackIter = mTrackVecPtr->begin(); trackIter != mTrackVecPtr->end(); ++trackIter ){

         // extract track info
         Float_t a      = trackIter->getLineParameterA();
         Float_t b      = trackIter->getLineParameterB();
         Float_t x_0    = trackIter->getLineParameterX_0();
         Float_t y_0    = trackIter->getLineParameterY_0();
         Float_t dX     = trackIter->getVarX();
         Float_t dY     = trackIter->getVarY();
         Float_t chi2   = trackIter->getChiSquare();
         Float_t hitX   = trackIter->getHitX();
         Float_t hitY   = trackIter->getHitY();
         isTrack = trackIter->getIsTrack();
         Float_t Z;
         if(mDiscId==0)      Z = 30.48; // 12 inches in cm, top quadrant
         else if(mDiscId==1) Z = 15.24; // 6 inches in cm, middle quadrant
         else if(mDiscId==2) Z = 0.;    // take origin as bottom quadrant 
         else return kStFatal;  // no info for this disc
         if(abs(a)>0.5 || abs(b)>0.5)
            continue; // look at cosmictrack near perpendicular
         Float_t x = a*Z+x_0;   // where track line passes through quad
         Float_t y = b*Z+y_0;

         // now bin accordingly
         Int_t region=0;
         if(x<40) {
            if(y<40) region=16;
            if(y<30) region=12;
            if(y<20) region= 8;
            if(y<10) region= 4;
         }
         if(x<30) {
            if(y<40) region=15;
            if(y<30) region=11;
            if(y<20) region= 7;
            if(y<10) region= 3;
         }
         if(x<20) {
            if(y<40) region=14;
            if(y<30) region=10;
            if(y<20) region= 6;
            if(y<10) region= 2;
         }
         if(x<10) {
            if(y<40) region=13;
            if(y<30) region= 9;
            if(y<20) region= 5;
            if(y<10) region= 1;
         }
         if(region==1) {mHistX[0][0]->Fill(dX); mHistY[0][0]->Fill(dY);}
         else if(region==2) {mHistX[0][1]->Fill(dX); mHistY[0][1]->Fill(dY);}
         else if(region==3) {mHistX[0][2]->Fill(dX); mHistY[0][2]->Fill(dY);}
         else if(region==4) {mHistX[0][3]->Fill(dX); mHistY[0][3]->Fill(dY);}
         else if(region==5) {mHistX[1][0]->Fill(dX); mHistY[1][0]->Fill(dY);}
         else if(region==6) {mHistX[1][1]->Fill(dX); mHistY[1][1]->Fill(dY);}
         else if(region==7) {mHistX[1][2]->Fill(dX); mHistY[1][2]->Fill(dY);}
         else if(region==8) {mHistX[1][3]->Fill(dX); mHistY[1][3]->Fill(dY);}
         else if(region==9) {mHistX[2][0]->Fill(dX); mHistY[2][0]->Fill(dY);}
         else if(region==10) {mHistX[2][1]->Fill(dX); mHistY[2][1]->Fill(dY);}
         else if(region==11) {mHistX[2][2]->Fill(dX); mHistY[2][2]->Fill(dY);}
         else if(region==12) {mHistX[2][3]->Fill(dX); mHistY[2][3]->Fill(dY);}
         else if(region==13) {mHistX[3][0]->Fill(dX); mHistY[3][0]->Fill(dY);}
         else if(region==14) {mHistX[3][1]->Fill(dX); mHistY[3][1]->Fill(dY);}
         else if(region==15) {mHistX[3][2]->Fill(dX); mHistY[3][2]->Fill(dY);}
         else if(region==16) {mHistX[3][3]->Fill(dX); mHistY[3][3]->Fill(dY);}
         else continue;

         // filling histograms
         mProfileX->Fill(x,dX);
         mProfileY->Fill(y,dY);

         if(mDiscId==1) {   // for middle disc
            mHistTracks->Fill(x,y);
            mProfileChi2->Fill(x,y,chi2);

            mHist2Dpoint->Fill( hitX , hitY );
            if(isTrack) {
               mHist2Dcluster->Fill( hitX , hitY );
               mHistRealTracks->Fill(x,y);
            }
            Int_t isTrackInt = 0;
            if(isTrack) isTrackInt = 1;
            mProfileEfficiency->Fill(x,y,isTrackInt);
         }
      }

      if( isTrack ) {
        StFgtHitCollection *hitCollectionPtr = 0;
        if( !ierr ){
          hitCollectionPtr = fgtCollectionPtr->getHitCollection( mDiscId );
        };

        if( hitCollectionPtr ){
          const StSPtrVecFgtHit &hitVec = hitCollectionPtr->getHitVec();
          StSPtrVecFgtHitConstIterator hitIter;

          Short_t adcSum=0;
           //LOG_INFO << "We have " << hitVec.size() << " clusters " << endm;
          for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter ){
            Bool_t haveR=false;

            if((*hitIter)->charge() < 200 ) continue;
            haveR=((*hitIter)->getLayer()=='R');

            if( haveR ) mHist1Dcluster[0]->Fill( (*hitIter)->charge() );
            else        mHist1Dcluster[1]->Fill( (*hitIter)->charge() );
            adcSum += (*hitIter)->charge();
          };
          if(adcSum!=0) mHist1Dclusters->Fill( adcSum );
        };
      };
   };

  return ierr;
};

// make the histogram pretty and save it to a file, if given a filename base
Int_t StFgtCosmicTrackPlots::Finish(){
   //cout << "StFgtCosmicTrackPlots::Finish()" << endl;

/*  old style of efficiency (not per event)
   for(int i=0;i<8;i++) {
     for(int j=0;j<8;j++) {
       Float_t con2Dpoint = mHist2Dpoint -> GetBinContent(i+1,j+1);
       Float_t conTrack   = mHistTracks  -> GetBinContent(i+1,j+1);
       Float_t x = i*5+1;  // for correctly binning into efficiency profile
       Float_t y = j*5+1;  // mHist2Dpoint and tracks have 8x8 bins
       if(con2Dpoint!=0) mProfileEfficiency->Fill(x,y,conTrack/con2Dpoint);
     }
   }
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
      TCanvas *canX = new TCanvas( "fgtTrackQAcanX", "fgtTrackQAcanX", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      Int_t pad=1;
      canX->Divide(4,4);
      for( Int_t i=3;i>=0;i--) {
        for( Int_t j=0;j<4;j++) {
          canX->cd(pad);
          mHistX[i][j]->Draw();
          pad++;
        }
      }
  
      TCanvas *canY = new TCanvas( "fgtTrackQAcanY", "fgtTrackQAcanY", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      pad=1;
      canY->Divide(4,4);
      for( Int_t i=3;i>=0;i--) {
        for( Int_t j=0;j<4;j++) {
          canY->cd(pad);
          mHistY[i][j]->Draw();
          pad++;
        }
      }

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
 
      TCanvas *canTrack = new TCanvas( "fgtTrackQATrack", "fgtTrackQATrack", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      gStyle->SetPalette(1);
      mHistTracks->Draw("colz");

      TCanvas *canRealTrack = new TCanvas( "fgtTrackQARealTrack", "fgtTrackQARealTrack", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      gStyle->SetPalette(1);
      mHistTracks->Draw("colz");

      TCanvas *canEfficiency = new TCanvas( "fgtTrackQAEf", "fgtTrackQAEf", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      gStyle->SetPalette(1);
      mProfileEfficiency->Draw("colz");

      TCanvas *can2Dcluster = new TCanvas( "fgtTrackQApoint", "fgtTrackQApoint", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      gStyle->SetPalette(1);
      mHist2Dcluster->Draw("colz");

      TCanvas *canChi = new TCanvas( "fgtTrackQAChi", "fgtTrackQAChi", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      gStyle->SetPalette(1);
      mProfileChi2->Draw("colz");

      TCanvas *canX2 = new TCanvas( "fgtTrackQAcanX2", "fgtTrackQAcanX2", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      mProfileX->Draw();

      TCanvas *canY2 = new TCanvas( "fgtTrackQAcanY2", "fgtTrackQAcanY2", 900, 400 );
      gStyle->SetOptStat(0);
      gStyle->SetOptDate(0);
      mProfileY->Draw();


      canX->Print( filename.data() );
      canY->Print( filename.data() );
      can1Dcluster->Print( filename.data() );
      can1Dclusters->Print( filename.data() );
      canTrack->Print( filename.data() );
      canRealTrack->Print( filename.data() );
      canEfficiency->Print( filename.data() );
      can2Dcluster->Print( filename.data() );
      canChi->Print( filename.data() );
      canX2->Print( filename.data() );
      canY2->Print( filename.data() );
      delete canX;
      delete canY;
      delete can1Dcluster;
      delete can1Dclusters;
      delete canTrack;
      delete canRealTrack;
      delete canEfficiency;
      delete can2Dcluster;
      delete canChi;
      delete canX2;
      delete canY2;
   };

   return kStOk;
};

void StFgtCosmicTrackPlots::Clear( Option_t *opt ) {

};


ClassImp(StFgtCosmicTrackPlots);
