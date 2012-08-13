// $Id: AliHLTTPCCAPerformanceBase.cxx,v 1.12 2012/08/13 19:35:05 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCAPerformanceBase.h"
#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCAMCPoint.h"
#endif
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCATracklet.h"

#include "AliHLTTPCCADisplay.h"


#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"

#include <string>
using std::string;

void AliHLTTPCCAPerformanceBase::SetNewEvent( const AliHLTTPCCAGBTracker * const Tracker,
                                AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                                AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                                AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  fTracker = Tracker;
  
  fHitLabels = hitLabels;
  fMCTracks = mcTracks;
  fLocalMCPoints = localMCPoints;
  
  nMCTracks = (*fMCTracks).Size();

  fEff = AliHLTTPCEfficiencies();
  mcData.resize(0);
  recoData.resize(0);

  const double MaxMomentum = 5.;
  const int MaxNHits    = 50.;
  const double MaxChi2 = 10.;
  const THistoInfo tmp[NHisto]=
  {
    THistoInfo( "resY",       "track Y resolution [cm]",  30, -0.25,  0.25 ),
    THistoInfo( "resZ",       "track Z resolution [cm]",  30, -0.7,   0.7 ),
    THistoInfo( "resSinPhi",  "track SinPhi resolution ", 30, -0.03,  0.03 ),
    THistoInfo( "resDzDs",    "track DzDs resolution ",   30, -0.02, 0.02 ),
    THistoInfo( "resPt",      "track Pt resolution",      30, -0.3,   0.3 ),
    THistoInfo( "pullY",      "track Y pull",             30, -7.,   7. ),
    THistoInfo( "pullZ",      "track Z pull",             30, -7.,   7. ),
    THistoInfo( "pullSinPhi", "track SinPhi pull",        30, -7.,   7. ),
    THistoInfo( "pullDzDs",   "track DzDs pull",          30, -7.,   7. ),
    THistoInfo( "pullQPt",    "track Q/Pt pull",          30, -7.,   7. ),
    
    THistoInfo( "resYHit",       "track Y resolution [cm]",  30, -0.25,  0.25 ),
    THistoInfo( "resZHit",       "track Z resolution [cm]",  30, -1.,   1. ),
    THistoInfo( "pullYHit",      "track Y pull",             30, -7.,   7. ),
    THistoInfo( "pullZHit",      "track Z pull",             30, -7.,   7. ),

    THistoInfo( "ghostsLength",   "N Ghosts vs N Hits",       MaxNHits+1, 0.,   MaxNHits ),    // nGhosts vs nHits in reco track
    THistoInfo( "ghostsRMom",     "N Ghosts vs Reco Momentum",     50, 0.,           MaxMomentum ), // nGhosts vs momentum of reco track
    THistoInfo( "ghostsMCMom",    "N Ghosts vs MC Momentum",     50, 0.,           MaxMomentum ), // nGhosts vs momentum of MC track
    THistoInfo( "ghostsChi2",     "N Ghosts vs Chi-square",   50, 0.,           MaxChi2*10 ),
    THistoInfo( "ghostsLengthAndRMom",   "N Ghosts vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum),
    THistoInfo( "ghostsLengthAndMCMom",   "N Ghosts vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum),
    THistoInfo( "ghostsLengthAndChi2",  "N Ghosts vs N Hits and Chi2",           MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxChi2*10),
    
    THistoInfo( "recosLength",   "N Reco Tracks vs N Hits",   MaxNHits+1, 0.,   MaxNHits ),    // vs nHits in reco track
    THistoInfo( "recosRMom",      "N Reco Tracks vs Momentum", 50, 0.,           MaxMomentum ), // vs momentum of reco track
    THistoInfo( "recosMCMom",      "N Reco Tracks vs Momentum", 50, 0.,           MaxMomentum ), // vs momentum of reco track
    THistoInfo( "recosChi2",     "N Reco Tracks vs Chi-square",   50, 0.,       MaxChi2 ),
    THistoInfo( "recosEffVsMCNHits", "Reconstruction Efficiency vs N Hits",   MaxNHits+1, 0.,   MaxNHits ),   // eff vs nHits in MC track
    THistoInfo( "recosEffVsMCMom",   "Reconstruction Efficiency vs Momentum", 50, 0.,           MaxMomentum ),// eff vs mom in MC track
    THistoInfo( "recosLengthAndRMom",   "N Reco Tracks vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum),
    THistoInfo( "recosLengthAndMCMom",   "N Reco Tracks vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum),
    THistoInfo( "recosLengthAndChi2",  "N Reco Tracks vs N Hits and Chi2",           MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxChi2)
  };
  for (int iHisto = 0; iHisto < NHisto; iHisto++){
    fHistosInfo[iHisto] = tmp[iHisto];
  }

} // void AliHLTTPCCAPerformanceBase::SetNewEvent

AliHLTTPCCAPerformanceBase::AliHLTTPCCAPerformanceBase():
  fStatNEvents(0),fTracker(0),fHitLabels(0),fMCTracks(0),fLocalMCPoints(0),nRecoTracks(0),nMCTracks(0),fHistoDir(0)
{
  for( int i=0; i < NHisto; i++ ){
    fHistos[i] = 0;
  }
}

AliHLTTPCCAPerformanceBase::~AliHLTTPCCAPerformanceBase()
{
  if (fHistoDir == 0) // don't write in file
    for( int i = 0; i < NHisto; i++ ){
      if (fHistos[i]) delete fHistos[i];
    }
}

void AliHLTTPCCAPerformanceBase::CreateHistos(string histoDir, TFile* outFile)
{
  TDirectory *curdir = gDirectory;
  if ( (histoDir != "") && outFile) {  // create in file
    if (outFile) outFile->cd();
    fHistoDir = outFile->mkdir( TString(histoDir) );
    fHistoDir->cd();
    gDirectory->mkdir( "TrackFit" );
    gDirectory->cd( "TrackFit" );

    int ih = 0; // i of Histo
    for( int i = 0; i < NTracksPulls + NHitsPulls; i++, ih++ ){ // TODO separate Track & Hits pulls
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }

    gDirectory->cd( ".." );
    gDirectory->mkdir( "Ghosts" );
    gDirectory->cd( "Ghosts" );
    
    for( int i = 0; i < NGhostsHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }
    for( int i = 0; i < NGhostsProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NGhosts2DHisto; i++, ih++ ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
        //      fHistos[ih]->SetDrawOption("colz"); doesn't help
    }
    
    gDirectory->cd( ".." );
    gDirectory->mkdir( "RecoTracks" );
    gDirectory->cd( "RecoTracks" );
    
    for( int i = 0; i < NRecoTracksHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }
    for( int i = 0; i < NRecoTracksProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NRecoTracks2DHisto; i++, ih++ ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
        //      fHistos[ih]->SetDrawOption("colz"); doesn't help
    }
    
    gDirectory->cd( ".." );
    curdir->cd();    
  }
  else{ // create not in file
    static int iaddName = 0; // haven't any subfolders so create with different names
    TString addName = TString(iaddName);
    int ih = 0; // i of Histo
    for( int i = 0; i < NTracksPulls + NHitsPulls + NGhostsHisto; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }
    for( int i = 0; i < NGhostsProfiles; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }
    for( int i = 0; i < NGhosts2DHisto; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
    }
        
    for( int i = 0; i < NRecoTracksHisto; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }
    for( int i = 0; i < NRecoTracksProfiles; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
    }
    for( int i = 0; i < NRecoTracks2DHisto; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
    }
    
    for( int i = 0; i < NHisto; i++ ){
      fHistos[i]->SetDirectory(0);
    }
  }
  SetHistoCreated();
}

void AliHLTTPCCAPerformanceBase::WriteHistos()
{
  if(fHistoDir) WriteDir2Current( fHistoDir );
}

TH1 *AliHLTTPCCAPerformanceBase::GetHisto(const char* name)
{
  int iHisto;
  for (iHisto = 0; iHisto < NHisto; iHisto++){
    if (string(fHistosInfo[iHisto].name) == string(name)){
      break;
    };
  }

  assert ( (iHisto != NHisto) || (string("") == string(" wrong histo name ")) );
  if (iHisto == NHisto){
    cout << "ERROR: wrong histo name: " << name << endl;
    exit(1);
  }
  
  return fHistos[iHisto];
}

void AliHLTTPCCAPerformanceBase::Exec( bool PrintFlag )
{
  if ( !fTracker ) return;

    // Efficiency
  CheckMCTracks();
  MatchTracks();
  EfficiencyPerformance();
  if (PrintFlag) PrintEfficiency();
  
    // Histos
  FillHistos();

  Draw();
  
  fStatNEvents++;
} // Exec


void AliHLTTPCCAPerformanceBase::EfficiencyPerformance() // TODO add common parts of code
{
  fEff.IncNEvents();
  fEffStat += fEff;
}

void AliHLTTPCCAPerformanceBase::FillHistos()
{
  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mcD = mcData[iMCTr];
    AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[iMCTr];
    if ( mcD.IsReconstructable() ) {
      GetHisto("recosEffVsMCMom")  ->Fill( mcTr.P(),     mcD.IsReconstructed() );
      GetHisto("recosEffVsMCNHits")->Fill( mcTr.NHits(), mcD.IsReconstructed() );
    }
  }
} // void AliHLTTPCCAPerformanceBase::FillHistos()

void AliHLTTPCCAPerformanceBase::WriteDir2Current( TObject *obj )
{
  //* recursive function to copy the directory 'obj' to the current one
  if ( !obj->IsFolder() ) obj->Write();
  else {
    TDirectory *cur = gDirectory;
    TDirectory *sub = cur->mkdir( obj->GetName() );
    sub->cd();
    TList *listSub = ( ( TDirectory* )obj )->GetList();
    TIter it( listSub );
    while ( TObject *obj1 = it() ) WriteDir2Current( obj1 );
    cur->cd();
  }
}
