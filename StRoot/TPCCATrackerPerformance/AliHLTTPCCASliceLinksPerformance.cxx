// $Id: AliHLTTPCCASliceLinksPerformance.cxx,v 1.8 2012/08/13 19:35:05 fisyak Exp $
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
#include "AliHLTTPCCASliceLinksPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAClusterData.h"

#ifndef HLTCA_STANDALONE
// #ifdef MAIN_DRAW
// #define DRAW_LINKS_PERF
// #endif

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#endif

#ifdef DRAW_LINKS_PERF
#include "AliHLTTPCCADisplay.h"
#include "AliHLTTPCCAPerformance.h"
#endif // DRAW_LINKS_PERF


/*
AliHLTTPCCASliceLinksPerformance::AliHLTTPCCASliceLinksPerformance()
{
  const int NHisto_tmp = 
    + NGhostsHisto + NGhosts2DHisto
    + NRecoTracksHisto + NRecoTracks2DHisto;

  NHisto = NHisto_tmp;
  fHistosInfo = new THistoInfo[NHisto];
  fHistos = new TH1*[NHisto];
    
  const double MaxMomentum = 5.;
  const int MaxNHits    = 50.;
  const double MaxChi2 = 10.;
  const THistoInfo tmp[NHisto_tmp]=
  {
    THistoInfo( "ghostsLength",   "N Ghosts vs N Hits",       MaxNHits+1, 0.,   MaxNHits ),    // nGhosts vs nHits in reco track
    THistoInfo( "ghostsMCMom",    "N Ghosts vs MC Momentum",     50, 0.,           MaxMomentum ), // nGhosts vs momentum of MC track
    THistoInfo( "ghostsLengthAndMCMom",   "N Ghosts vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum),
    
    THistoInfo( "recosLength",   "N Reco Tracks vs N Hits",   MaxNHits+1, 0.,   MaxNHits ),    // vs nHits in reco track
    THistoInfo( "recosMCMom",      "N Reco Tracks vs Momentum", 50, 0.,           MaxMomentum ), // vs momentum of reco track
    THistoInfo( "recosLengthAndMCMom",   "N Reco Tracks vs N Hits and Momentum",       MaxNHits+1, 0.,   MaxNHits, 50, 0.,           MaxMomentum),
  };
  for (int iHisto = 0; iHisto < NHisto; iHisto++){
    fHistosInfo[iHisto] = tmp[iHisto];
  }
  
  for( int i=0; i < NHisto; i++ ){
    fHistos[i] = 0;
  }
}

void AliHLTTPCCASliceLinksPerformance::CreateHistos(string histoDir, TFile* outFile)
{
  TDirectory *curdir = gDirectory;
  if ( (histoDir != "") && outFile) {  // create in file
    outFile->cd();
    fHistoDir = outFile->mkdir( TString(histoDir) );
    fHistoDir->cd();

    gDirectory->mkdir( "Ghosts" );
    gDirectory->cd( "Ghosts" );
    
    int ih = 0; // i of Histo
    for( int i = 0; i < NGhostsHisto; i++, ih++ ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
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
    for( int i = 0; i < NGhosts2DHisto; i++, ih++, addName = TString(iaddName++)  ){
      fHistos[ih] = new TH2D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right, fHistosInfo[ih].ny, fHistosInfo[ih].low, fHistosInfo[ih].up);
    }
        
    for( int i = 0; i < NRecoTracksHisto; i++, ih++, addName = TString(iaddName++) ){
      fHistos[ih] = new TH1D(fHistosInfo[ih].name+addName, fHistosInfo[ih].title, fHistosInfo[ih].nx, fHistosInfo[ih].left, fHistosInfo[ih].right);
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
*/

void AliHLTTPCCASliceLinksPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCATrackPerformanceBase::SetNewEvent(tracker, hitLabels, mcTracks, localMCPoints);
    
  sliceTracker = &fTracker->Slice( fISlice );
  
  firstSliceHit = 0; endSliceHit = 0;
  for ( ; firstSliceHit < fTracker->NHits(); firstSliceHit++ ) {
    if ( fTracker->Hit( firstSliceHit ).ISlice() == fISlice ) break;
  }
  endSliceHit = firstSliceHit;
  for ( ; endSliceHit < fTracker->NHits(); endSliceHit++ ) {
    if ( fTracker->Hit( endSliceHit ).ISlice() != fISlice ) break;
  };
} // void AliHLTTPCCASliceLinksPerformance::SetNewEvent

void AliHLTTPCCASliceLinksPerformance::Exec( bool PrintFlag )
{
  CollectTracks();
  AliHLTTPCCASlicePerformance::Exec( PrintFlag );
} // Exec


  /// create tracks from neighbours hits chains
void AliHLTTPCCASliceLinksPerformance::CollectTracks()
{
  fRecoTracks.clear();
  nRecoTracks = sliceTracker->NTracklets();

  const SliceData &data = sliceTracker->Data();
  const Vc::vector<AliHLTTPCCAStartHitId>& startHits = sliceTracker->TrackletStartHits();
   
  for (int iTr = 0; iTr < nRecoTracks; iTr++) {
    PerfoTrack recoTrack;
    recoTrack.hits.clear();
    int iHit = startHits[iTr].fHit;
    int iRow = startHits[iTr].fRow;
    AliHLTTPCCARow row = data.Row( iRow );

    int iHitGB = firstSliceHit + data.ClusterDataIndex( row, iHit ); // firstSliceHit + iHit + clusterData.RowOffset( iRow )
    assert ( iHitGB < endSliceHit );
    recoTrack.hits.push_back( iHitGB );
       
    int iUpHit = data.HitLinkUpDataS( row, iHit );
    for(;iUpHit >= 0;) {
      iHit = iUpHit;
      iRow++;
      row = data.Row( iRow );

      iHitGB = firstSliceHit + data.ClusterDataIndex( row, iHit );
      assert ( iHitGB < endSliceHit );
      recoTrack.hits.push_back( iHitGB );
         
      iUpHit = data.HitLinkUpDataS( row, iHit );
    }

    fRecoTracks.push_back(recoTrack);
  }
} // void AliHLTTPCCASliceLinksPerformance::CollectTracks()


void AliHLTTPCCASliceLinksPerformance::MatchTracks()
{ // TODO make common for all performances
  recoData.resize(nRecoTracks);
  for ( int itr = 0; itr < nRecoTracks; itr++ ) {
    PerfoTrack& recoTrack = fRecoTracks[itr];

    int traLabels = -1;
    double traPurity = 0;

    int nhits = recoTrack.hits.size();
    int *lb = new int[nhits*3];
    int nla = 0;
      //cout<<"\nHit labels:"<<endl;
        // collect all labels

    if ( fHitLabels->Size() > 0 ) // if MC info is read
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int index = recoTrack.hits[ihit];
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
      if ( l.fLab[0] >= 0 ) lb[nla++] = l.fLab[0];
      if ( l.fLab[1] >= 0 ) lb[nla++] = l.fLab[1];
      if ( l.fLab[2] >= 0 ) lb[nla++] = l.fLab[2];
    }
        // find one with maximum entries.
    sort( lb, lb + nla );
    int labmax = -1, labcur = -1, lmax = 0, lcurr = 0;
    for ( int i = 0; i < nla; i++ ) {
      if ( lb[i] != labcur ) {
        if ( labcur >= 0 && lmax < lcurr ) {
          lmax = lcurr;
          labmax = labcur;
        }
        labcur = lb[i];
        lcurr = 0;
      }
      lcurr++;
    }
    if ( labcur >= 0 && lmax < lcurr ) {
      lmax = lcurr;
      labmax = labcur;
    }
        // count n hits with max label
    lmax = 0;
    if ( fHitLabels->Size() > 0 )
    for ( int ihit = 0; ihit < nhits; ihit++ ) {
      const int index = recoTrack.hits[ihit];
      const AliHLTTPCCAHitLabel &l = (*fHitLabels)[fTracker->Hit( index ).ID()];
      if ( l.fLab[0] == labmax || l.fLab[1] == labmax || l.fLab[2] == labmax
         ) lmax++;
      //cout << index << " ";
    }
    traLabels = labmax;
    traPurity = ( ( nhits > 0 ) ? double( lmax ) / double( nhits ) : 0 );
// cout<<"perf chain  "<<itr<<": "<<nhits<<" "<<labmax<<" "<<traPurity<<endl;
    if ( lb ) delete[] lb;

    recoData[itr].SetMCTrack(traLabels, traPurity, nhits);
    
    if ( recoData[itr].IsReco(SPParameters::MinTrackPurity, SPParameters::MinimumHitsForRecoTrack) ) mcData[traLabels].AddReconstructed();
  } // for itr
} // void AliHLTTPCCASliceLinksPerformance::MatchTracks()


#ifndef HLTCA_STANDALONE
void AliHLTTPCCASliceLinksPerformance::FillHistos()
{
  AliHLTTPCCATrackPerformanceBase::FillHistos();
    
  for(int iRTr=0; iRTr < nRecoTracks; iRTr++){  // TODO: make common
    AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[iRTr];
    const int NHits = fRecoTracks[iRTr].hits.size();
    
    if (  recoD.IsGhost(SPParameters::MinTrackPurity) ) {
      GetHisto(kghostsLength)->Fill( NHits );
    }
    else {
      AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[ recoD.GetMCTrackId() ];
      GetHisto(krecosLength)->Fill( NHits );
      GetHisto(krecosMCMom)->Fill( mcTr.P() );
      GetHisto(krecosLengthAndMCMom)->Fill( NHits , mcTr.P() );
    }
  }
} // void AliHLTTPCCASliceLinksPerformance::FillHistos()



void AliHLTTPCCASliceLinksPerformance::Draw()
{
#ifdef DRAW_LINKS_PERF
  if ( AliHLTTPCCADisplay::Instance().DrawType() != 3 ) return;
  // AliHLTTPCCAPerformance::Instance().Init();
  AliHLTTPCCAPerformance& gbPerfo = AliHLTTPCCAPerformance::Instance();
  AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
        
  disp.SetGB( gbPerfo.GetTracker() );
  disp.SetSliceView();
  disp.SetCurrentSlice( const_cast<AliHLTTPCCATracker *>(sliceTracker) ); // TODO rid of const cast
  disp.DrawSlice( const_cast<AliHLTTPCCATracker *>(sliceTracker), 0 );
  disp.DrawSliceHits(1,0.1);
  disp.DrawSliceLinks(-1,-1,0.03);

  for ( int imc = 0; imc < nMCTracks; imc++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[imc];
    bool doDraw = true;
    doDraw &= (mc.GetSet() == 2);
    doDraw &= mc.IsReconstructable();
    doDraw &= !mc.IsReconstructed();
    // doDraw &= !gbPerfo.GetSubPerformance("Global Performance")->GetMCData()[imc].IsReconstructed();
    if ( !doDraw ) continue;
    disp.SpecDrawMCTrackLocal( (*fMCTracks)[imc], fLocalMCPoints, fISlice );
    disp.SpecDrawHitsFromMCTrackLocal( imc, fHitLabels, fISlice );
  }

  AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "MCTracksDiff.pdf" );
  AliHLTTPCCADisplay::Instance().Ask();
#endif // DRAW_LINKS_PERF
}
#endif // HLTCA_STANDALONE




#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
