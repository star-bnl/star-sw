// $Id: AliHLTTPCCAGlobalSlicesPerformance.cxx,v 1.7 2012/08/13 19:35:05 fisyak Exp $
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
#include "AliHLTTPCCAGlobalSlicesPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCAMCPoint.h"
#endif
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"

#include "AliHLTTPCCADisplay.h"


#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"

void AliHLTTPCCAGlobalSlicesPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCAPerformanceBase::SetNewEvent(Tracker, hitLabels, mcTracks, localMCPoints);

    /// Init subperformances
//   static bool first_call = true;
  if (first_call){
    slicePerformances.resize(fTracker->NSlices());
    for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
      slicePerformances[iPerf] = new AliHLTTPCCASlicePerformance(iPerf);
    }
  }
  first_call = false;
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->SetNewEvent(fTracker, fHitLabels, fMCTracks, fLocalMCPoints);
  }

} // void AliHLTTPCCAGlobalSlicesPerformance::SetNewEvent

void AliHLTTPCCAGlobalSlicesPerformance::CreateHistos(string histoDir, TFile* outFile)
{
  // don't use histos. Histos are builded in the SlicesPerformance.

  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    // slicePerformances[iPerf]->CreateHistos( histoDir + (string)TString(char(iPerf)), outFile ); // just set diff names, they anyway won't be written
    slicePerformances[iPerf]->CreateHistos( "" );
  }
}


void AliHLTTPCCAGlobalSlicesPerformance::Exec(bool print)
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->Exec(0);
  }
  AliHLTTPCCAPerformanceBase::Exec(print);
} // void AliHLTTPCCAGlobalSlicesPerformance::Exec

void AliHLTTPCCAGlobalSlicesPerformance::CheckMCTracks()
{
  mcData.resize(nMCTracks);
    // find reconstructable tracks
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    vector<AliHLTTPCCAPerformanceMCTrackData> &mcs = slicePerformances[iPerf]->mcData;
    for ( int iMCTrack = 0; iMCTrack < nMCTracks; iMCTrack++ ){
      // if (mcs[iMCTrack].IsReconstructable()) mcData[iMCTrack].SetAsReconstructable(); // reconstructable at least in one slice
      if ( (*fMCTracks)[iMCTrack].NMCPoints() >= PParameters::MinimumMCPointsForMCTrack ) mcData[iMCTrack].SetAsReconstructable(); // global defition of reconstractable track
    } // iMCTrack
  } // iSlice
  
    // calc set
  for ( int iMCTrack = 0; iMCTrack < nMCTracks; iMCTrack++ ){
    mcData[iMCTrack].SetSet( slicePerformances[0]->mcData[iMCTrack].GetSet() );
  } // iMCTrack
} // void AliHLTTPCCAGlobalSlicesPerformance::CheckMCTracks()

#define IsOutTrack1
void AliHLTTPCCAGlobalSlicesPerformance::MatchTracks()
{
    // get all reco tracks
  nRecoTracks = 0;
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    vector<AliHLTTPCCAPerformanceRecoTrackData> &rTrs = slicePerformances[iPerf]->recoData;
    int nSectorRecoTracks = rTrs.size();
    for ( int itr = 0; itr < nSectorRecoTracks; itr++ ) {
      recoData.push_back(rTrs[itr]);
    }
    nRecoTracks += nSectorRecoTracks;
  } // iSlice
    // find reconstructed tracks
  for ( int itr = 0; itr < nRecoTracks; itr++ ) {
    AliHLTTPCCAPerformanceRecoTrackData &rTr = recoData[itr];
    if ( rTr.IsReco( SPParameters::MinTrackPurity, SPParameters::MinimumHitsForRecoTrack) ) mcData[rTr.GetMCTrackId()].AddReconstructed();
  }
}


void AliHLTTPCCAGlobalSlicesPerformance::EfficiencyPerformance()
{
  for ( int iRTr = 0; iRTr < nRecoTracks; iRTr++ ) {
    if (  recoData[iRTr].IsGhost( PParameters::MinTrackPurity ) )
      fEff.ghosts++;
  }

  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable() ) continue;
    const bool reco = mc.IsReconstructed();
    const int clones = mc.GetNClones();

    if ( mc.GetSet() == 0){ // rest, out track
      fEff.Inc(reco,clones,"rest");
    }
    else{ // good
      fEff.Inc(reco,clones,"total");
      if ( mc.GetSet() == 1){
        fEff.Inc(reco,clones,"extra");
      }
      else{
        fEff.Inc(reco,clones,"ref");
      }
    }
  } // for iMCTr

  AliHLTTPCCAPerformanceBase::EfficiencyPerformance();
} // void AliHLTTPCCAGlobalSlicesPerformance::EfficiencyPerformance()

