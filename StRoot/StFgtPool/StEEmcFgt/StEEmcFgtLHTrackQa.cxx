/***************************************************************************
 *
 * $Id: StEEmcFgtLHTrackQa.cxx,v 1.4 2012/04/13 15:08:43 sgliske Exp $
 * Author: S. Gliske, April 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StEEmcFgtLHTrackQa.cxx,v $
 * Revision 1.4  2012/04/13 15:08:43  sgliske
 * updates
 *
 * Revision 1.3  2012/04/12 17:12:05  sgliske
 * update to not use A2EMaker but StEEmcRawMaker
 *
 * Revision 1.2  2012/04/11 22:13:24  sgliske
 * update
 *
 *
 **************************************************************************/

#include <vector>
#include <TVector3.h>
#include <TROOT.h>
#include <TFile.h>
#include "StEEmcFgtLHTrackQa.h"
#include "StRoot/StFgtPool/StFgtTracking/StFgtLHTracking.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcRawMapMaker.h"

#define DEBUG

StEEmcFgtLHTrackQa::StEEmcFgtLHTrackQa( const Char_t* name, const Char_t* rawMapMkrName, const Char_t* fgtLHTkrName ) :
   StMaker( name ), mEEmcRawMapMkr(0), mFgtLHTkr(0), mThres(3) {

   mEEmcRawMapMkr = static_cast< StEEmcRawMapMaker* >( GetMaker( rawMapMkrName ) );
   mFgtLHTkr = static_cast< StFgtLHTracking* >( GetMaker( fgtLHTkrName ) );

   for( Int_t i=0; i<4; ++i )
      mSig[i] = mSigPer[i] = 0;
};

// deconstructor
StEEmcFgtLHTrackQa::~StEEmcFgtLHTrackQa(){ /* */ };

Int_t StEEmcFgtLHTrackQa::Init(){

   std::stringstream ss;

   gROOT->cd();
   for( Int_t i=0; i<4; ++i ){
      ss.str("");
      ss.clear();
      ss << "hSig_" << i;
      mSig[i] = new TH1F( ss.str().data(), ";; Counts", 2, -0.5, 1.5 );

      ss.str("");
      ss.clear();
      ss << "hSigPer_" << i;
      mSigPer[i] = new TH1F( ss.str().data(), ";; Counts", 2, -0.5, 1.5 );

      mSig[i]->GetXaxis()->SetBinLabel( 1, "Noise" );
      mSig[i]->GetXaxis()->SetBinLabel( 2, "Signal" );
      mSigPer[i]->GetXaxis()->SetBinLabel( 1, "Noise" );
      mSigPer[i]->GetXaxis()->SetBinLabel( 2, "Signal" );
   };

   assert( mEEmcRawMapMkr );
   assert( mFgtLHTkr );

   return kStOK;
};

Int_t StEEmcFgtLHTrackQa::Make(){
   assert( mEEmcRawMapMkr );
   assert( mFgtLHTkr );

   const StFgtLHTrackVec& trackVec = mFgtLHTkr->getTrackVec();
   const StEEmcRawMap* eemcMap[4];
   Bool_t eemcEmpty = 1;
   for( Int_t i=0; i<4; ++i ){
      eemcMap[i] = &mEEmcRawMapMkr->getMap( i );
      eemcEmpty &= eemcMap[i]->empty();
   };

#ifdef DEBUG2
   if( !eemcMap[0]->empty() ){
      LOG_INFO << "\t-> Towers has size " << eemcMap[0]->size() << endm;
   };
#endif

   if( !trackVec.empty() && !eemcEmpty ){
      StFgtLHTrackVec::const_iterator trackVecIter;

#ifdef DEBUG
      LOG_INFO << "Event " << GetEventNumber() << " # tracks " << trackVec.size() << endm;
      //LOG_INFO << "\t-> Towers has size " << eemcMap[0]->size() << endm;
#endif

      std::map< Int_t, Int_t > towMap;
      std::map< Int_t, Int_t >::iterator towMapIter;
      
      EEmcGeomSimple& eemcGeom = EEmcGeomSimple::Instance();
      Int_t sec, sub, etabin;

      for( trackVecIter = trackVec.begin(); trackVecIter != trackVec.end(); ++trackVecIter ){

         // get the x and y position at the endcap
         Double_t x = trackVecIter->line.bx + trackVecIter->line.mx*kEEmcZSMD;
         Double_t y = trackVecIter->line.by + trackVecIter->line.my*kEEmcZSMD;
         TVector3 pos( x, y, kEEmcZSMD );

         Bool_t found = eemcGeom.getTower( pos, sec, sub, etabin );

#ifdef DEBUG
         LOG_INFO << "\tTrack points to (x, y) = ( " << x << ", " << y << "), (eta,phi) = ( " << pos.Eta() << ", " << pos.Phi() << ") at SMDZ = tower at " << sec << ' ' << sub << ' ' << etabin << endm;
#endif

         if( found ){
            Int_t index = etabin + kEEmcNumEtas*( sub + kEEmcNumSubSectors*sec );

            towMapIter = towMap.find( index );
            if( towMapIter == towMap.end() )
               towMap[index] = 1;
            else
               ++(towMapIter->second);
         };
      };

      StEEmcRawMap::const_iterator rawMapIter;

      for( towMapIter = towMap.begin(); towMapIter != towMap.end(); ++towMapIter ){
         Int_t index = towMapIter->first;
         Int_t num = towMapIter->second;

#ifdef DEBUG
         LOG_INFO << "\t---> Accessing tower index " << index << endm;
#endif

         for( Int_t i=0; i<4; ++i ){
            rawMapIter = eemcMap[i]->find( index );

            if( rawMapIter != eemcMap[i]->end() ){
               const StEEmcRawMapData& data = rawMapIter->second;

               Double_t val = (data.rawAdc - data.ped) / data.pedSigma;

               mSig[i]->Fill( (Int_t)(val > mThres) );
               mSigPer[i]->Fill( (Int_t)(val/num > mThres) );

               cout << "EEMC tower: " << i << ' ' << index << ' ' << data.rawAdc << ' ' << data.ped << ' ' << data.pedSigma << ' ' << val << ' ' << num
                    << (val > mThres ? " SIGNAL" : " ") << endl;
            };
         };
      };
   };

   return kStOK;
};

Int_t StEEmcFgtLHTrackQa::Finish(){
   Int_t ierr = kStOk;

   if( !mFileOutName.empty() ){
      TFile *f = new TFile( mFileOutName.data(), "RECREATE" );

      if( !f->IsOpen() ){
         LOG_ERROR << "Error opening file '" << mFileOutName << "'" << endl;
         ierr = kStErr;
      } else {
         f->cd();

         for(Int_t i=0; i<4; ++i ){
            mSig[i]->Write();
            mSigPer[i]->Write();
         };
      };
   };

   return ierr;
};

ClassImp(StEEmcFgtLHTrackQa);
