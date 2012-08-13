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

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCAPerformanceBase.h"
#include "AliHLTTPCCASliceLinksPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCAMCPoint.h"
#endif
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAClusterData.h"

#include "AliHLTTPCCADisplay.h"


#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
using namespace std;
void AliHLTTPCCASliceLinksPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCAPerformanceBase::SetNewEvent(Tracker, hitLabels, mcTracks, localMCPoints);
    
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
  AliHLTTPCCAHitId *startHits = sliceTracker->TrackletStartHits();
   
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



void AliHLTTPCCASliceLinksPerformance::FillHistos()
{
  AliHLTTPCCAPerformanceBase::FillHistos();
    
  for(int iRTr=0; iRTr < nRecoTracks; iRTr++){  // TODO: make common
    AliHLTTPCCAPerformanceRecoTrackData &recoD = recoData[iRTr];
    const int NHits = fRecoTracks[iRTr].hits.size();
    AliHLTTPCCAMCTrack &mcTr = (*fMCTracks)[ recoD.GetMCTrackId() ];
    
    if (  recoD.IsGhost(SPParameters::MinTrackPurity) ) {
      GetHisto("ghostsLength")->Fill( NHits );
      GetHisto("ghostsMCMom")->Fill( mcTr.P() );
    }
    else {
      GetHisto("recosLength")->Fill( NHits );
      GetHisto("recosMCMom")->Fill( mcTr.P() );
    }
  }
} // void AliHLTTPCCASliceLinksPerformance::FillHistos()
