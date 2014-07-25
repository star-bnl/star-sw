/***************************************************************************
 *
 * $Id: StFgtPedStatQA.cxx,v 1.4 2014/07/25 18:46:00 xuanli Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header
 *
 ***************************************************************************
 *
 * $Log: StFgtPedStatQA.cxx,v $
 * Revision 1.4  2014/07/25 18:46:00  xuanli
 * c++ 11 test
 *
 * Revision 1.3  2012/01/31 16:48:34  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.2  2012/01/31 09:31:37  sgliske
 * includes updated for things moved to StFgtPooll
 *
 * Revision 1.1  2012/01/31 09:26:16  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.14  2012/01/28 20:10:12  avossen
 * addec cluster uncertainty
 *
 * Revision 1.13  2012/01/28 13:06:31  sgliske
 * fixed some indexing issues in StFgtStatusMaker and StFgtPedStatQA
 *
 * Revision 1.12  2012/01/26 13:04:28  sgliske
 * Updated to use StFgtConsts and
 * bug fix in apv numbering
 *
 * Revision 1.11  2012/01/26 11:39:39  sgliske
 * bin indexing matches new convention
 *
 * Revision 1.10  2012/01/24 09:23:56  sgliske
 * bug fix, again
 *
 * Revision 1.9  2012/01/24 08:42:22  sgliske
 * bug fix
 *
 * Revision 1.8  2012/01/24 08:22:43  sgliske
 * Fixed title of 1D histos
 *
 * Revision 1.6  2012/01/24 08:11:12  sgliske
 * Bug fixes
 *
 * Revision 1.5  2012/01/18 19:00:52  sgliske
 * minor bug fix
 *
 * Revision 1.4  2012/01/18 18:53:01  sgliske
 * minor bug fix
 *
 * Revision 1.3  2012/01/18 18:20:26  sgliske
 * directly use elec coord domian as fundamental domain
 *
 * Revision 1.2  2012/01/17 22:25:07  sgliske
 * removed hack for DB and
 *  fixed bug in writing status to txt file
 *
 * Revision 1.1  2012/01/17 20:10:02  sgliske
 * creation
 *
 *
 *
 **************************************************************************/

#include <string>
#include "StFgtPedStatQA.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedMaker.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedMaker.h"
#include "StRoot/StFgtPool/StFgtStatusMaker/StFgtStatusMaker.h"
#include "StRoot/StFgtPool/StFgtStatusMaker/StFgtStatusMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

#include <TCanvas.h>
#include <TPave.h>
#include <TPaveText.h>
#include <TPaveStats.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TH1.h>
#include <TH2.h>
#include <TFile.h>


// constructor
StFgtPedStatQA::StFgtPedStatQA( const Char_t* name,
                                const Char_t* pedMkrName,
                                const Char_t* statMkrName  ) :
   StMaker( name ), mPedMkrName( pedMkrName ), mPedMkr(0), mStatMkrName( statMkrName ), mStatMkr(0), mTimeBin(2),
   maxAdcPed( 27.5 ), maxAdcRMS( 47.5 ), maxAdcFrac( 47.5 ) {

   mFilenameArr[0] = &mFilenameTxt;
   mFilenameArr[1] = &mFilenameRoot;
   mFilenameArr[2] = &mFilenamePdf;
};

// deconstructor
StFgtPedStatQA::~StFgtPedStatQA(){
   // nothing
};

// initialize
Int_t StFgtPedStatQA::Init(){
   Int_t ierr = kStOk;

   // make sure the file can be opened

   for( Int_t i=0; i<3 && !ierr; ++i ){
      if( !mFilenameArr[i]->empty() ){
         std::ofstream fout( mFilenameArr[i]->data() );
         if( !fout ){
            LOG_FATAL << "Error opening file '" << mFilenameArr[i] << "'" << endm;
            ierr = kStFatal;
         };
      };
   };

   // make sure there's a pedmaker
   mPedMkr = static_cast< StFgtPedMaker* >( GetMaker( mPedMkrName.data() ) );
   if( !mPedMkr ){
      LOG_FATAL << "Cannot find StFgtPedMaker" << endm; 
      ierr = kStFatal;
   };

   // make sure it will compute the peds for this timebine
   if( !ierr ){
      if( !mPedMkr->mTimeBinMask & (1<<mTimeBin) ){
         LOG_WARN << "StFgtPedMaker is not set to compute the time bin needed for StFgtPedStatQA" << endm;
         mPedMkr->mTimeBinMask |= (1<<mTimeBin);
         LOG_WARN << "StFgtPedMaker::mTimeBinMask set to 0x" << std::hex << mPedMkr->mTimeBinMask << std::dec << endm;
      };
   };

   // make sure there's a status maker
   mStatMkr = static_cast< StFgtStatusMaker* >( GetMaker( mStatMkrName.data() ) );
   if( !mStatMkr ){
      LOG_FATAL << "Cannot find StFgtStatusMaker" << endm; 
      ierr = kStFatal;
   };

   return ierr;
};

// here is the real computation of the maker
Int_t StFgtPedStatQA::Finish(){
   Int_t ierr = kStOk;

   if( !mStatMkr->mHasFinished )
      ierr = mStatMkr->Finish();

   cout << "StFgtPedStatQA::Finish()" << endl;

   // need to program this better in the future, so these values can
   // be changed by users at runtime
   enum { kNumApv = kFgtApvsPerQuad*kFgtNumDiscs*kFgtNumQuads,
          kNumUsedApv = kNumApv, kMaxADC = 1500, kMaxRMS = 205, kBins = 100, kMaxAliveYaxis = 132 };

   Bool_t doMakeHists = !mFilenamePdf.empty() || !mFilenameRoot.empty();
   Bool_t doMakeTxt = !mFilenameTxt.empty();

   std::ofstream *foutPtr;

   if( doMakeTxt ){
      foutPtr = new std::ofstream( mFilenameTxt.data(), std::ios_base::out & std::ios_base::trunc );
      if( !(*foutPtr) ){
         LOG_FATAL << "Error opening file '" << mFilenameTxt.data() << "'" << endm;
         ierr = kStFatal;
      };
   };


   TFile *tfilePtr = 0;

   TH1F *hPed[kNumApv], *hRMS[kNumApv], *hFrac[kNumApv];
   TH2F *hPed2D = 0;
   TH2F *hRMS2D = 0;
   TH2F *hFrac2D = 0;
   TH1F *hAlive = 0;

   Int_t canWidth = 850, canHeight = 1100;
   std::stringstream ss;

   if( !ierr && doMakeHists ){
      gROOT->SetStyle( "Plain" );
      gStyle->SetPalette(1);
      gStyle->SetOptStat(111110);
      gStyle->SetPaperSize(TStyle::kUSLetter);

      if( !mFilenameRoot.empty() )
         tfilePtr = new TFile ( mFilenameRoot.data(), "RECREATE", "FGT Ped QA" );

      for( Int_t i=0; i<kNumApv; ++i ){
         ss.str("");
         ss.clear();
         ss << i;

         hPed[i] = new TH1F( (std::string("hPed_") + ss.str() ).data(), "", kBins/3, 0, kMaxADC );
         hRMS[i] = new TH1F( (std::string("hRMS_") + ss.str() ).data(), "", kBins/3, 0, kMaxRMS );
         hFrac[i] = new TH1F( (std::string("hFrac_") + ss.str() ).data(), "", kBins/3, 0, 1 );
      };

      hPed2D = new TH2F( "hPed2D", "", kNumUsedApv, 0, kNumUsedApv, kBins, 0, kMaxADC );
      hRMS2D = new TH2F( "hRMS2D", "", kNumUsedApv, 0, kNumUsedApv, kBins, 0, kMaxRMS );
      hFrac2D = new TH2F( "hFrac2D", "", kNumUsedApv, 0, kNumUsedApv, kBins, 0, 1 );
      hAlive = new TH1F( "hAlive", "", kNumUsedApv, 0, kNumUsedApv );
   };

   StFgtPedMaker::pedDataVec_t& pedVec = mPedMkr->mDataVec;
   StFgtPedMaker::pedDataVec_t::iterator pedVecIter;

   StFgtDb *fgtTables = 0;
   if( !mPedMkr->mDbMkrName.empty() ){
      if( !mPedMkr->mFgtDbMkr ){
         LOG_FATAL << "Pointer to Fgt DB Maker is null" << endm;
         ierr = kStFatal;
      };
      if( !ierr ){
         fgtTables = mPedMkr->mFgtDbMkr->getDbTables();

         if( !fgtTables ){
            LOG_FATAL << "Pointer to Fgt DB Tables is null" << endm;
            ierr = kStFatal;
         };
      };
   };

   Int_t numnonzero = 0;
   if( !ierr ){
      Int_t pedIdx = 0;
      for( pedVecIter = pedVec.begin(); pedVecIter != pedVec.end() && !ierr; ++pedVecIter, ++pedIdx ){
         if( pedVecIter->n ){
            ++numnonzero;

            Int_t timebin = pedIdx % kFgtNumTimeBins;

            Int_t elecId = pedIdx / kFgtNumTimeBins;
            UChar_t status = mStatMkr->mStatus[ elecId ];

            UChar_t invalidID = (1<<mStatMkr->INVALID_ID);
            if( status != invalidID ){
               Int_t rdo, arm, apv, channel;
               StFgtGeom::getElecCoordFromElectId( elecId, rdo, arm, apv, channel );
               assert( apv < 22 );
               assert( rdo < 3 );
               assert( arm < 6 );

               // DEBUG
               if( fgtTables ){
                  Int_t rdo2, arm2, apv2, channel2;

                  Int_t geoId = fgtTables->getGeoIdFromElecCoord( rdo, arm, apv, channel );

                  fgtTables->getElecCoordFromGeoId(geoId, rdo2, arm2, apv2, channel2 );
                  Int_t elecId2 = StFgtGeom::getElectIdFromElecCoord( rdo2, arm2, apv2, channel2 );

                  if( elecId2 != elecId && geoId > -1 ){
                     cout << "ERROR: " << elecId << " -> " << rdo << ' ' << arm << ' ' << apv << ' ' << channel;
                     cout << " -> " << geoId << " -> " << rdo2 << ' ' << arm2 << ' ' << apv2 << ' ' << channel2;
                     cout << " -> " << elecId2 << endl;
                  };
               };

               Int_t geoId = 0;
               if( fgtTables ){
                  geoId = fgtTables->getGeoIdFromElecCoord( rdo, arm, apv, channel );
               } else {
                  ierr = kStFatal;
                  LOG_ERROR << "Error: not supported for use without the DB" << endm;
               };
               Short_t disc, quad, strip;
               Char_t layer;
               StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

               // bin index
//                if( apv > 9 )
//                   apv -= 2;
//                Int_t histIdx = apv+20*(rdo-1+2*arm);  // on a scale of 0-239
               Int_t histIdx =  (apv%12) + 10*( quad + 4*disc );

               if( doMakeTxt ){
                  (*foutPtr) << elecId << ' ' << rdo << ' ' << arm << ' '  << apv << ' ' << channel << " | ";
                  (*foutPtr) << geoId << ' ' << disc << ' ' << quad << ' ' << layer << ' ' << strip << " | ";
                  (*foutPtr) << timebin << ' ' << pedVecIter ->ped << ' ' << pedVecIter->RMS << ' ' << pedVecIter->fracClose << " | ";
                  (*foutPtr) << "0x" << std::hex << (UInt_t)(status) << std::dec << " | " << histIdx << endl;
               };

               if( timebin == mTimeBin && doMakeHists ){
                  hPed[histIdx]->Fill( pedVecIter->ped );
                  hRMS[histIdx]->Fill( pedVecIter->RMS );
                  hFrac[histIdx]->Fill( pedVecIter->fracClose );

                  hPed2D->Fill( histIdx, pedVecIter->ped );
                  hRMS2D->Fill( histIdx, pedVecIter->RMS );
                  hFrac2D->Fill( histIdx, pedVecIter->fracClose );

                  if( hAlive->GetBinContent( histIdx+1 ) < 1 ){
                     StFgtStatusMaker::apvData_t &data = mStatMkr->mApvData[ elecId / kFgtNumChannels ];
                     hAlive->SetBinContent( histIdx+1, kFgtNumChannels-data.numDead );
                     if( ( apv >= kFgtApvsPerAssembly ? apv-kFgtApvGap : apv ) % kFgtApvsPerOct == kFgtApvsPerOct/2 ){
                        ss.str("");
                        ss.clear();
                        ss << disc+1 << (Char_t)(quad+'A') << StFgtGeom::getOctant( apv );

                        hAlive->GetXaxis()->SetBinLabel( histIdx+1,  ss.str().data() );
                        hPed2D->GetXaxis()->SetBinLabel( histIdx+1,  ss.str().data() );
                        hRMS2D->GetXaxis()->SetBinLabel( histIdx+1,  ss.str().data() );
                        hFrac2D->GetXaxis()->SetBinLabel( histIdx+1, ss.str().data() );
                     };
                  };
               };
            };
         };
      };
   };

   LOG_INFO << "---> PedVec has " << numnonzero << " of " << pedVec.size() << " nonzero entries" << endm;

   if( doMakeTxt ){
      delete foutPtr;
      foutPtr = 0;
   };

   if( !ierr && doMakeHists ){
      for( Int_t histIdx = 0; histIdx < kNumApv; ++histIdx ){
         Bool_t used = 1;

         // histIdx =  (apv%12) + 10*( quad + 4*disc );

         Int_t i = histIdx;
         Int_t apvMod = i % 12;
         i /= 10;
         Int_t quad = i % 4;
         i /= 4;
         Int_t disc = i;

         Bool_t octIsShort = ( StFgtGeom::getOctant( apvMod  ) == 'S' );
         Int_t geoId = StFgtGeom::encodeGeoId( disc, quad, 'P', octIsShort*700 );
         if( geoId > -1 ){
            assert( fgtTables );
            Int_t rdo, arm, apv, channel;

            // note: the rdo/arm will be correct, but not the apv or channel
            fgtTables->getElecCoordFromGeoId( geoId, rdo, arm, apv, channel );

            ss.str("");
            ss.clear();
            ss << "Rdo. " << rdo << ", Arm " << arm << ", APV%12 " << apvMod << ", Octant ";
            ss << disc+1 << (Char_t)(quad+'A') << (octIsShort ? 'S' : 'L');
         } else {
            used = 0;
         };

         if( used ){
            hPed[histIdx]->SetTitle( ( ss.str() + "; Ped Value [ADC]; Counts" ).data() );
            hRMS[histIdx]->SetTitle( ( ss.str() + "; Ped St. Dev. [ADC]; Counts" ).data() );
            hFrac[histIdx]->SetTitle( ( ss.str() + "; Frac. of Integral Near Ped.; Counts" ).data() );
         } else {
            hPed[histIdx]->SetTitle( "" );
            hRMS[histIdx]->SetTitle( "" );
            hFrac[histIdx]->SetTitle( "" );
         };
      };
      hPed2D->SetTitle("Pedestals per APV; ; ADC");
      hRMS2D->SetTitle("Pedestal RMS per APV; ; ADC");
      hFrac2D->SetTitle("Fraction of Integral Near Pedestal per APV; ; Fraction");
      hAlive->SetTitle("Number Good Strips per APV; ; Number");

      if( !mFilenameRoot.empty() ){
         hPed2D->Write();
         hRMS2D->Write();
         hFrac2D->Write();
         hAlive->Write();
         for( Int_t i=0; i<kNumApv; ++i ){
            if( !std::string(hPed[i]->GetTitle()).empty() ){
               hPed[i]->Write();
               hRMS[i]->Write();
               hFrac[i]->Write();
            };
         };
      };

      hAlive->SetFillColor(kYellow-9);
      hAlive->SetLineColor(kBlue);

      if( !mFilenamePdf.empty() ){
         // draw
         TCanvas *can1 = new TCanvas( "fgtCan1", "fgt Ped QA", canWidth, canHeight );
         TCanvas *can2 = new TCanvas( "fgtCan2", "fgt Ped QA", canWidth, canHeight );

         hPed2D->SetMaximum( maxAdcPed );
         hRMS2D->SetMaximum( maxAdcRMS );
         hFrac2D->SetMaximum( maxAdcFrac );
         hAlive->SetMaximum( kMaxAliveYaxis );

         can1->Divide( 1, 4 );

         TH1 *h2D[] = { hAlive, hPed2D, hRMS2D, hFrac2D };
         Float_t highCut[]={ kFgtNumChannels,                    mStatMkr->mMaxPed, mStatMkr->mMaxRMS, mStatMkr->mMaxFrac };
	 Float_t fMaxDead = kFgtNumChannels-mStatMkr->mMaxDead;
         Float_t lowCut[] ={ fMaxDead, mStatMkr->mMinPed, mStatMkr->mMinRMS, mStatMkr->mMinFrac };

         for( Int_t i=0; i<4; ++i ){
            can1->cd(i+1);
            gPad->SetLeftMargin( 0.06 );
            gPad->SetRightMargin( 0.05 );
            gPad->SetBottomMargin( 0.11 );
            gPad->SetGridx(1);
            gPad->SetGridy(0);

            h2D[i]->SetStats(0);
            //h2D[i]->LabelsOption( "a", "X" ); // order bins by label
            h2D[i]->Draw( i ? "COLZ" : "" );
            h2D[i]->GetXaxis()->SetTitleOffset(0.9);
            h2D[i]->GetXaxis()->SetTitleSize(0.06);
            h2D[i]->GetXaxis()->SetLabelSize(0.06);
            h2D[i]->GetYaxis()->SetTitleOffset(1.2);
            h2D[i]->GetYaxis()->SetTitleSize(0.06);
            h2D[i]->GetYaxis()->SetLabelSize(0.06);

            TLine *lineA = new TLine ( 0, highCut[i], kNumUsedApv, highCut[i] );
            lineA->SetLineColor(kRed);
            lineA->Draw();

            TLine *lineB = new TLine ( 0, lowCut[i], kNumUsedApv, lowCut[i] );
            lineB->SetLineColor(kRed);
            lineB->Draw();

            gPad->Update();
            gPad->Modified();

            TPaveText *title = (TPaveText*)(gPad->GetPrimitive("title"));
            if( title ){
               title->SetX1NDC( 0.045 );
               title->SetX2NDC( 0.55 );
               title->SetY1NDC( 0.91 ) ;
               title->SetY2NDC( 0.999 );
               title->SetBorderSize(0);
               title->SetTextAlign( 12 );
               title->SetTextColor(kBlue);
               title->Draw();
            };

            TPave *palette = (TPave*)(gPad->GetPrimitive("palette"));
            if( palette ){
               palette->SetX1NDC( 0.955 );
               palette->SetX2NDC( 0.985 );
               palette->Draw();
            };
         };

         can1->Print( (mFilenamePdf + "[").data() );  // open the file
         can1->Print( mFilenamePdf.data() );

         can2->Divide( 3, 5 );
         for( Int_t i=0; i<kNumApv; ++i ){
            for( Int_t j=0; j<3; ++j ){
               can2->cd( (i%5)*3+1+j );

               TH1F *hist = 0;
               Float_t max = 0;
               if( j == 0 ){
                  hist = hPed[i];
                  max = maxAdcPed;
               } else if( j ==1 ){
                  hist = hRMS[i];
                  max = maxAdcRMS;
               } else {
                  hist = hFrac[i];
                  max = maxAdcFrac;
               };
               hist->SetMaximum( max );

               gPad->SetBottomMargin(0.15);
               hist->GetXaxis()->SetTitleSize(0.06);
               hist->GetXaxis()->SetLabelSize(0.06);
               hist->GetYaxis()->SetTitleOffset(0.85);
               hist->GetYaxis()->SetTitleSize(0.06);
               hist->GetYaxis()->SetLabelSize(0.06);
               hist->SetLineColor(kBlue);

               hist->Draw();
               gPad->Update();
               gPad->Modified();
               TPaveText *title = (TPaveText*)(gPad->GetPrimitive("title"));
               if( title ){
                  title->SetX1NDC( 0 );
                  title->SetX2NDC( 0.55 );
                  title->SetY1NDC( 0.91 ) ;
                  title->SetY2NDC( 0.999 );
                  title->SetBorderSize(0);
                  title->SetTextAlign( 12 );
                  title->SetTextColor(kRed);
                  title->Draw();
               };

               TPaveStats *stats = (TPaveStats*)(gPad->GetPrimitive("stats"));
               if( stats ){
                  stats->SetX1NDC( 0.68 );
                  stats->SetX2NDC( 0.99 );
                  stats->SetY1NDC( 0.68 ) ;
                  stats->SetY2NDC( 0.99 );
                  stats->Draw();
               };
            };

            if( (i+1) % 5 == 0 && !std::string(hPed[i]->GetTitle()).empty() ){
               can2->Print( mFilenamePdf.data() );
            };
         };

         can2->Print( ( mFilenamePdf + "]").data() );  // close the file
      };

      if( !mFilenameRoot.empty() )
         tfilePtr->Close();
   };

   return ierr;
};

ClassImp( StFgtPedStatQA );
