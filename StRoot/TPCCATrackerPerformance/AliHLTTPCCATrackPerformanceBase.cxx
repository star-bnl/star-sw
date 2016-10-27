// $Id: AliHLTTPCCATrackPerformanceBase.cxx,v 1.3 2013/11/21 13:07:29 mzyzak Exp $
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
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCATrackPerformanceBase.h"
#include "AliHLTTPCPerformanceBase.h"

#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCATracklet.h"

#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCADisplay.h"

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#endif


AliHLTTPCCATrackPerformanceBase::AliHLTTPCCATrackPerformanceBase()
{
#ifndef HLTCA_STANDALONE
  const int NHisto_tmp = NTracksPulls + NHitsPulls + NHits2DPulls
    + NAllHisto + NAllProfiles + NAll2DHisto
    + NGhostsHisto + NGhostsProfiles + NGhosts2DHisto
    + NRecoTracksHisto + NRecoTracksProfiles + NRecoTracks2DHisto;

  NHisto = NHisto_tmp;
  fHistosInfo = new THistoInfo[NHisto];
  fHistos = new TH1*[NHisto];
    
  const double MaxMomentum = 5.;
  const double MaxPt = 5.;
  const double MaxPhi = 180.;
  const int MaxNHits    = 50.;
  const double MaxChi2 = 10.;
  const THistoInfo tmp[NHisto_tmp]=
  {
    THistoInfo( "resY",       "track y residual [cm]",   30, -0.25, 0.25, 0,0,0, "Residual (y^{reco} - y^{mc}) [cm]","Entries" ),
    THistoInfo( "resZ",       "track z residual [cm]",   30, -0.7,   0.7, 0,0,0, "Residual (z^{reco} - z^{mc}) [cm]","Entries" ),
    THistoInfo( "resSinPhi",  "track sin#phi residual ", 30, -0.03, 0.03, 0,0,0, "Residual (sin#phi^{reco} - sin#phi^{mc}) [rad]","Entries" ),
    THistoInfo( "resDzDs",    "track dz/ds residual ",   30, -0.02, 0.02, 0,0,0, "Residual (dz/ds^{reco} - dz/ds^{mc})","Entries" ),
    THistoInfo( "resPt",      "track p_{t} resolution",  30, -0.3,   0.3, 0,0,0, "Resolution (p_{t}^{reco} - p_{t}^{mc})/p_{t}^{mc} [%]","Entries"  ),
    THistoInfo( "pullY",      "track y pull",            30, -7.,   7.,   0,0,0, "Pull y" ),
    THistoInfo( "pullZ",      "track z pull",            30, -7.,   7.,   0,0,0, "Pull z" ),
    THistoInfo( "pullSinPhi", "track sin#phi pull",      30, -7.,   7.,   0,0,0, "Pull sin#phi" ),
    THistoInfo( "pullDzDs",   "track dz/ds pull",        30, -7.,   7.,   0,0,0, "Pull dz/ds" ),
    THistoInfo( "pullQPt",    "track q/p_{t} pull",      30, -7.,   7.,   0,0,0, "Pull q/p_{t}" ),

    THistoInfo( "resXHit",       "hits X resolution [cm]",  100, -1.25, 1.25, 0,0,0, "Residual (x^{reco} - x^{mc}) [cm]","Entries" ),
    THistoInfo( "resYHit",       "hits Y resolution [cm]",  100, -0.5, 0.5, 0,0,0, "Residual (y^{reco} - y^{mc}) [cm]","Entries" ),
    THistoInfo( "resZHit",       "hits Z resolution [cm]",  100,   -1.,   1., 0,0,0, "Residual (z^{reco} - z^{mc}) [cm]","Entries" ),
    THistoInfo( "pullYHit",      "hits Y pull",             30,   -7.,   7., 0,0,0, "Pull y" ),
    THistoInfo( "pullZHit",      "hits Z pull",             30,   -7.,   7., 0,0,0, "Pull z" ),

    THistoInfo( "xMCPoint",       "MCPoint X position [cm]",  500, 0, 250, 0,0,0, "x^{mc} [cm]","Entries" ),
    THistoInfo( "rMCPoint",       "MCPoint R position [cm]",  500, 0, 250, 0,0,0, "r^{mc} [cm]","Entries" ),
    
    THistoInfo( "resXHitVsZ",    "hits X resolution [cm] vs Z",  50, -220, 220, 100, -1.25, 1.25, "z [cm]", "Residual (x^{reco} - x^{mc}) [cm]" ),
    THistoInfo( "resYHitVsZ",    "hits Y resolution [cm] vs Z",  50, -220, 220, 100, -0.25, 0.25, "z [cm]", "Residual (y^{reco} - y^{mc}) [cm]" ),
    THistoInfo( "resZHitVsZ",    "hits Z resolution [cm] vs Z",  50, -220, 220, 100, -1.00, 1.00, "z [cm]","Residual (z^{reco} - z^{mc}) [cm]" ),
    THistoInfo( "resXHitVsX",    "hits X resolution [cm] vs X",  50, 50, 200, 100, -1.25, 1.25, "x [cm]", "Residual (x^{reco} - x^{mc}) [cm]" ),
    THistoInfo( "resYHitVsX",    "hits Y resolution [cm] vs X",  50, 50, 200, 100, -0.25, 0.25, "x [cm]", "Residual (y^{reco} - y^{mc}) [cm]" ),
    THistoInfo( "resZHitVsX",    "hits Z resolution [cm] vs X",  50, 50, 200, 100,   -1.,   1., "x [cm]","Residual (z^{reco} - z^{mc}) [cm]" ),
    THistoInfo( "resXHitVsIS",    "hits X resolution [cm] vs IS",  52, 0, 25, 100, -1.25, 1.25, "i Slice", "Residual (x^{reco} - x^{mc}) [cm]" ),
    THistoInfo( "resYHitVsIS",    "hits Y resolution [cm] vs IS",  52, 0, 25, 100, -0.25, 0.25, "i Slice", "Residual (y^{reco} - y^{mc}) [cm]" ),
    THistoInfo( "resZHitVsIS",    "hits Z resolution [cm] vs IS",  52, 0, 25, 100,   -1.,   1., "i Slice","Residual (z^{reco} - z^{mc}) [cm]" ),

    
    THistoInfo( "purity",   "Purity of tracks", 101, 0.005, 1.005,
                0,0,0, "Purity", "Entries" ),
    THistoInfo( "mcTrackNRecoHits",   "NRecoHits on MCTrack", MaxNHits+1, 0, MaxNHits,
                0,0,0, "Purity", "Entries" ),
    THistoInfo( "nHitsOverNMCPointsVsRow",   "nHits / NMCPoints Vs Row", MaxNHits+1, 0, MaxNHits,
                0,0,0, "Row", "NHits / NMCPoints" ),
    THistoInfo( "nHitsOverNMCPointsVsMCMom",   "nHits / NMCPoints Vs MCMomentum", 50, 0, MaxMomentum,
        0,0,0, "MCMomentum", "NHits / NMCPoints" ),
    THistoInfo( "nHitsOverNMCPointsVsMCDzDs",   "nHits / NMCPoints Vs MCTrack DzDs", 50, -10, 10,
        0,0,0, "MCMomentum", "NHits / NMCPoints" ),
    THistoInfo( "nHitsOverNMCPointsVsNMCTracks",   "nHits / NMCPoints Vs NMCTracks", 20, 0, 5000,
                0,0,0, "NMCTracks", "NHits / NMCPoints" ),
    THistoInfo( "nMCPointsVsMCMom",   "NMCPoints Vs MCMomentum", 50, 0, MaxMomentum,
                MaxNHits*3+1,0,MaxNHits*3, "MCMomentum", "NMCPoints" ),

    
    THistoInfo( "ghostsLength",   "N_{hits} distribution, ghost tracks", MaxNHits+1, 0., MaxNHits,
                0,0,0, "Number of hits", "Entries" ),    // nGhosts vs nHits in reco track
    THistoInfo( "ghostsRMom",     "p^{reco} distriution, ghost tracks",  50, 0., MaxMomentum,
                0,0,0, "p^{reco} [GeV/c]", "Entries" ), // nGhosts vs momentum of reco track
    THistoInfo( "ghostsRPt",     "p_{t}^{reco} distriution, ghost tracks",        50, 0., MaxPt,
                0,0,0, "p_{t}^{reco} [GeV/c]", "Entries" ), // nGhosts vs Pt of reco track
    THistoInfo( "ghostsChi2",     "#chi^{2} distriution, ghost tracks",           50, 0., MaxChi2*10,
                0,0,0, "#chi^{2}", "Entries" ),
    THistoInfo( "ghostsProb",     "prob distriution, ghost tracks",           50, 0., 1,
                0,0,0, "prob", "Entries" ),
    THistoInfo( "ghostsLengthAndRMom",  "N Ghosts vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum,
                "Number of hits", "p^{reco} [GeV/c]" ),
    THistoInfo( "ghostsLengthAndChi2",  "N Ghosts vs N Hits and Chi2",           MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxChi2*10,
                "Number of hits", "p^{reco} [GeV/c]" ),

    THistoInfo( "recosLength",           "N_{hits} distribution, reco tracks",   MaxNHits+1, 0.,   MaxNHits,
                0,0,0, "Number of hits", "Entries" ),    // vs nHits in reco track
    THistoInfo( "recosRMom",             "p^{reco} distriution, reco tracks", 50, 0.,           MaxMomentum,
                0,0,0, "p^{reco} [GeV/c]", "Entries" ), // vs momentum of reco track
    THistoInfo( "recosMCMom",            "p^{MC} distriution, reco tracks", 50, 0.,           MaxMomentum,
                0,0,0, "p^{MC} [GeV/c]", "Entries" ), // vs momentum of MC track
    THistoInfo( "recosRPt",             "p_{t}^{reco} distriution, reco tracks", 50, 0.,           MaxPt,
                0,0,0, "p_{t}^{reco} [GeV/c]", "Entries" ), // vs Pt of reco track
    THistoInfo( "recosMCPt",            "p_{t}^{MC} distriution, reco tracks", 50, 0.,           MaxPt,
                0,0,0, "p_{t}^{MC} [GeV/c]", "Entries" ), // vs Pt of MC track
    THistoInfo( "recosChi2",             "#chi^{2} distriution, reco tracks",   50, 0.,       MaxChi2,
                0,0,0, "#chi^{2}", "Entries" ),
    THistoInfo( "recosProb",     "prob distriution, reco tracks",           50, 0., 1,
                0,0,0, "prob", "Entries" ),
    THistoInfo( "nHitsRecoTOverNHitsMCT",     "nHitsRecoTOverNHitsMCT",           50, 0., 2,
                0,0,0, "NHits^{reco} / NHits^{mc}", "Entries" ),
    THistoInfo( "recosEffVsMCNHits",     "Reconstruction Efficiency vs N Hits",   MaxNHits+1, 0.,   MaxNHits,
                0,0,0, "Number of hits", "Efficiency [%]" ),   // eff vs nHits in MC track
    THistoInfo( "recosEffVsMCMom",       "Reconstruction Efficiency vs Momentum, All tracks",          50, 0., MaxMomentum,
                0,0,0, "p^{MC} [GeV/c]", "Efficiency"),// eff vs mom in MC track
    THistoInfo( "recosEffVsMCMomPrim",   "Reconstruction Efficiency vs Momentum, Primary Tracks", 50, 0., MaxMomentum,
                0,0,0, "p^{MC} [GeV/c]", "Efficiency"),// eff vs mom in MC track
    THistoInfo( "recosEffVsMCMomRefPrim","Reconstruction Efficiency vs Momentum, Reference Primary Tracks", 50, 0., MaxMomentum,
                0,0,0, "p^{MC} [GeV/c]", "Efficiency"),// eff vs mom in MC track
    THistoInfo( "recosEffVsMCMomSec",    "Reconstruction Efficiency vs Momentum, Secondary Tracks", 50, 0., MaxMomentum,
                0,0,0, "p^{MC} [GeV/c]", "Efficiency"),// eff vs mom in MC track
    THistoInfo( "recosEffVsMCPtAll",     "Reconstruction Efficiency vs Pt, All Tracks",    50, 0., MaxPt,
                0,0,0, "p_{t}^{MC} [GeV/c]", "Efficiency"),// eff vs pt of all MC tracks
    THistoInfo( "recosEffVsMCPtPrim",    "Reconstruction Efficiency vs Pt, Primary",       50, 0., MaxPt,
                0,0,0, "p_{t}^{MC} [GeV/c]", "Efficiency"),// eff vs pt of primary MC tracks
    THistoInfo( "recosEffVsMCPtRefPrim", "Reconstruction Efficiency vs Pt, Ref Primary",   50, 0., MaxPt,
                0,0,0, "p_{t}^{MC} [GeV/c]", "Efficiency"),// eff vs pt of ref primary MC tracks
    THistoInfo( "recosEffVsMCPtSec",     "Reconstruction Efficiency vs Pt, Secondary",     50, 0., MaxPt,
                0,0,0, "p_{t}^{MC} [GeV/c]", "Efficiency"),// eff vs pt of secondary MC tracks
    THistoInfo( "recosEffVsMCPhiAll",     "Reconstruction Efficiency vs Phi, All Tracks",  120, -MaxPhi, MaxPhi,
                0,0,0, "#phi", "Efficiency"),// eff vs Phi of all tracks
    THistoInfo( "recosEffVsMCPhiPrim",    "Reconstruction Efficiency vs Phi, Primary Tracks",  120, -MaxPhi, MaxPhi,
                0,0,0, "#phi", "Efficiency"),// eff vs Phi of primary tracks
    THistoInfo( "recosEffVsMCPhiRefPrim", "Reconstruction Efficiency vs Phi, Ref Primary Tracks", 120, -MaxPhi, MaxPhi,
                0,0,0, "#phi", "Efficiency"),// eff vs Phi of ref primary
    THistoInfo( "recosEffVsMCPhiSec",     "Reconstruction Efficiency vs Phi, Secondary Tracks",   120, -MaxPhi, MaxPhi,
                0,0,0, "#phi", "Efficiency"),// eff vs Phi of secondary
    THistoInfo( "recosEffVsNMCTracks",       "Reconstruction Efficiency vs NMCTracks, All tracks",          20, 0., 5000,
                0,0,0, "NMCTracks", "Efficiency"),
    THistoInfo( "recosLengthAndRMom",     "N Reco Tracks vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum,
                "Number of hits", "p^{reco} [GeV/c]" ),
    THistoInfo( "recosLengthAndMCMom",    "N Reco Tracks vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum,
                "Number of hits", "p^{MC} [GeV/c]" ),
    THistoInfo( "recosLengthAndChi2",     "N Reco Tracks vs N Hits and Chi2",           MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxChi2,
                "Number of hits", "p^{reco} [GeV/c]" )
  };
  for (int iHisto = 0; iHisto < NHisto; iHisto++){
    fHistosInfo[iHisto] = tmp[iHisto];
  }
  
  for( int i=0; i < NHisto; i++ ){
    fHistos[i] = 0;
  }
#endif // HLTCA_STANDALONE
}

#ifndef HLTCA_STANDALONE
void AliHLTTPCCATrackPerformanceBase::CreateHistos(string histoDir, TFile* outFile)
{
  TDirectory *curdir = gDirectory;
  if ( (histoDir != "") && outFile) {  // create in file
    outFile->cd();
    fHistoDir = outFile->mkdir( TString(histoDir) );
    fHistoDir->cd();
    gDirectory->mkdir( "TrackFit" );
    gDirectory->cd( "TrackFit" );

    int ih = 0; // i of Histo
    for( int i = 0; i < NTracksPulls + NHitsPulls; i++, ih++ ){ // TODO separate Track & Hits pulls
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    
    for( int i = 0; i < NHits2DPulls; i++, ih++ ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }

    gDirectory->cd( ".." );
    gDirectory->mkdir( "AllTracks" );
    gDirectory->cd( "AllTracks" );

    for( int i = 0; i < NAllHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NAllProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
      fHistos[ih]->GetYaxis()->SetRangeUser(0.,1.05);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NAll2DHisto; i++, ih++ ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
        //      fHistos[ih]->SetDrawOption("colz"); doesn't help
    }
            
    gDirectory->cd( ".." );
    gDirectory->mkdir( "Ghosts" );
    gDirectory->cd( "Ghosts" );
    
    for( int i = 0; i < NGhostsHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NGhostsProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
      fHistos[ih]->GetYaxis()->SetRangeUser(0.,1.05);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NGhosts2DHisto; i++, ih++ ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
        //      fHistos[ih]->SetDrawOption("colz"); doesn't help
    }
    
    gDirectory->cd( ".." );
    gDirectory->mkdir( "RecoTracks" );
    gDirectory->cd( "RecoTracks" );
    
    for( int i = 0; i < NRecoTracksHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NRecoTracksProfiles; i++, ih++ ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
      fHistos[ih]->GetYaxis()->SetRangeUser(0.,1.05);
      fHistos[ih]->SetMarkerColor(2);
      fHistos[ih]->SetLineColor(2);
    }
    for( int i = 0; i < NRecoTracks2DHisto; i++, ih++ ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
        //      fHistos[ih]->SetDrawOption("colz"); doesn't help
    }
    
    gDirectory->cd( ".." );
    curdir->cd();    
  }
  else{ // create not in file
    static int iaddName = 0; // haven't any subfolders so create with different names
    TString addName = TString(iaddName);
    int ih = 0; // i of Histo
    for( int i = 0; i < NTracksPulls + NHitsPulls; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NHits2DPulls; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    
    for( int i = 0; i < NAllHisto; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NAllProfiles; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NAll2DHisto; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
           
    for( int i = 0; i < NGhostsHisto; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NGhostsProfiles; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NGhosts2DHisto; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
        
    for( int i = 0; i < NRecoTracksHisto; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NRecoTracksProfiles; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TProfile(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    for( int i = 0; i < NRecoTracks2DHisto; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
      fHistos[ih]->GetXaxis()->SetTitle(fHistosInfo[ih].XAxisName.Data());
      fHistos[ih]->GetYaxis()->SetTitle(fHistosInfo[ih].YAxisName.Data());
    }
    
    for( int i = 0; i < NHisto; i++ ){
      fHistos[i]->SetDirectory(0);
    }
  }
  SetHistoCreated();
}

void AliHLTTPCCATrackPerformanceBase::FillHistos()
{
  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mcD = mcData[iMCTr];
    AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[iMCTr];

    const int Multiplicity = (*fMCTracks).Size();
    
    if ( mcD.IsReconstructable() ) {
      //all reconstructed tracks

     double PhiMC = TMath::RadToDeg()*TMath::ATan2(mcTr.Py(),mcTr.Px());

      GetHisto("recosEffVsMCMom")   ->Fill( mcTr.P(),     mcD.IsReconstructed() );
      GetHisto("recosEffVsMCNHits") ->Fill( mcTr.NHits(), mcD.IsReconstructed() );
      GetHisto("recosEffVsMCPtAll") ->Fill( mcTr.Pt(),    mcD.IsReconstructed() );
      GetHisto("recosEffVsMCPhiAll")->Fill( PhiMC,    mcD.IsReconstructed() );
      GetHisto("recosEffVsNMCTracks")->Fill( Multiplicity,    mcD.IsReconstructed() );
  
      if(mcTr.MotherId() == -1) {
        //all reconstructed primary tracks
        GetHisto("recosEffVsMCMomPrim") ->Fill( mcTr.P(),  mcD.IsReconstructed() );
        GetHisto("recosEffVsMCPtPrim")  ->Fill( mcTr.Pt(), mcD.IsReconstructed() );
        GetHisto("recosEffVsMCPhiPrim") ->Fill( PhiMC, mcD.IsReconstructed() );
        if(mcTr.Set() == 2) {
          //reconstructed reference primary tracks
          GetHisto("recosEffVsMCMomRefPrim") ->Fill( mcTr.P(),  mcD.IsReconstructed() );
          GetHisto("recosEffVsMCPtRefPrim")  ->Fill( mcTr.Pt(), mcD.IsReconstructed() );
          GetHisto("recosEffVsMCPhiRefPrim") ->Fill( PhiMC, mcD.IsReconstructed() );
        }
      }
      else if(mcTr.MotherId() > -1) {
        //reconstructed secondary tracks
        GetHisto("recosEffVsMCMomSec") ->Fill( mcTr.P(),  mcD.IsReconstructed() );
        GetHisto("recosEffVsMCPtSec")  ->Fill( mcTr.Pt(), mcD.IsReconstructed() );
        GetHisto("recosEffVsMCPhiSec") ->Fill( PhiMC, mcD.IsReconstructed() );
      }
    }
  }
} // void AliHLTTPCCATrackPerformanceBase::FillHistos()
#endif //HLTCA_STANDALONE
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

