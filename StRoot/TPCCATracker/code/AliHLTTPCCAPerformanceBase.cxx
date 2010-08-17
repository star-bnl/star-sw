// $Id: AliHLTTPCCAPerformanceBase.cxx,v 1.8 2010/08/17 15:47:13 ikulakov Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
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

  const int MaxMomentum = 5.;
  const int MaxNHits    = 50.;
  const THistoInfo tmp[NHisto]=
  {
    { "resY",       "track Y resolution [cm]",  30, -0.25,  0.25 },
    { "resZ",       "track Z resolution [cm]",  30, -0.7,   0.7 },
    { "resSinPhi",  "track SinPhi resolution ", 30, -0.03,  0.03 },
    { "resDzDs",    "track DzDs resolution ",   30, -0.02, 0.02 },
    { "resPt",      "track Pt resolution",      30, -0.3,   0.3 },
    { "pullY",      "track Y pull",             30, -7.,   7. },
    { "pullZ",      "track Z pull",             30, -7.,   7. },
    { "pullSinPhi", "track SinPhi pull",        30, -7.,   7. },
    { "pullDzDs",   "track DzDs pull",          30, -7.,   7. },
    { "pullQPt",    "track Q/Pt pull",          30, -7.,   7. },
    
    { "resYHit",       "track Y resolution [cm]",  30, -0.25,  0.25 },
    { "resZHit",       "track Z resolution [cm]",  30, -1.,   1. },
    { "pullYHit",      "track Y pull",             30, -7.,   7. },
    { "pullZHit",      "track Z pull",             30, -7.,   7. },

    { "ghostsLength",   "N Ghosts vs N Hits",       MaxNHits+1, 0.,   MaxNHits },    // nGhosts vs nHits in reco track
    { "ghostsMom",      "N Ghosts vs Momentum",     50, 0.,           MaxMomentum }, // nGhosts vs momentum of MC track // TODO? reco Track
    
    { "recosLength",   "N Reco Tracks vs N Hits",   MaxNHits+1, 0.,   MaxNHits },    // vs nHits in reco track
    { "recosMom",      "N Reco Tracks vs Momentum", 50, 0.,           MaxMomentum }, // vs momentum of MC track // TODO? reco Track
    { "recosEffVsMCNHits", "Reconstruction Efficiency vs N Hits", MaxNHits+1, 0.,   MaxNHits },   // eff vs nHits in MC track
    { "recosEffVsMCMom", "Reconstruction Efficiency vs Momentum", 50, 0.,           MaxMomentum },// eff vs mom in MC track
    
  };
  for (int iHisto = 0; iHisto < NHisto; iHisto++){
    fHistosInfo[iHisto] = tmp[iHisto];
  }

} // void AliHLTTPCCAPerformanceBase::SetNewEvent

AliHLTTPCCAPerformanceBase::AliHLTTPCCAPerformanceBase():
  nRecoTracks(0), nMCTracks(0), fLocalMCPoints(0),fMCTracks(0),fHitLabels(0),fHistoDir(0),fTracker(0),fStatNEvents(0)
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
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
    }

    gDirectory->cd( ".." );
    gDirectory->mkdir( "Ghosts" );
    gDirectory->cd( "Ghosts" );
    
    for( int i = 0; i < NGhostsHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
    }
    for( int i = 0; i < NGhostsProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    
    gDirectory->cd( ".." );
    gDirectory->mkdir( "RecoTracks" );
    gDirectory->cd( "RecoTracks" );
    
    for( int i = 0; i < NRecoTracksHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
    }
    for( int i = 0; i < NRecoTracksProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    
    gDirectory->cd( ".." );
    curdir->cd();    
  }
  else{ // create not in file
    static int addName = 0; // haven't any subfolders so create with different names
    int ih = 0; // i of Histo
    for( int i = 0; i < NTracksPulls + NHitsPulls + NGhostsHisto; i++, ih++, addName++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
    }
    for( int i = 0; i < NGhostsProfiles; i++, ih++, addName++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NRecoTracksHisto; i++, ih++, addName++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
    }
    for( int i = 0; i < NRecoTracksProfiles; i++, ih++, addName++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].n, fHistosInfo[ih].l, fHistosInfo[ih].r);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NHisto; i++ ){
      fHistos[i]->SetDirectory(0);
    }
  }
}

void AliHLTTPCCAPerformanceBase::WriteHistos()
{
  if(fHistoDir) WriteDir2Current( fHistoDir );
}

TH1D *AliHLTTPCCAPerformanceBase::GetHisto(string name)
{
  int iHisto;
  for (iHisto = 0; iHisto < NHisto; iHisto++){
    if (fHistosInfo[iHisto].name == name){
      break;
    };
  }

  assert ( (iHisto != NHisto) || ("" == " wrong histo name ") );
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
