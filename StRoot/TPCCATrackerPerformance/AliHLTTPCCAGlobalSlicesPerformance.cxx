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
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCATrackPerformanceBase.h"
#include "AliHLTTPCCAGlobalSlicesPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"

#include "AliHLTTPCCATracker.h"

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

void AliHLTTPCCAGlobalSlicesPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCATrackPerformanceBase::SetNewEvent(tracker, hitLabels, mcTracks, localMCPoints);

    /// Init subperformances
//   static bool first_call = true;
  if (first_call){
    if(fTracker)
      slicePerformances.resize(fTracker->NSlices());
    else
      slicePerformances.resize(0);
    for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
      slicePerformances[iPerf] = new AliHLTTPCCASlicePerformance(iPerf);
    }
  }
  first_call = false;
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->SetNewEvent(fTracker, fHitLabels, fMCTracks, fLocalMCPoints);
  }

} // void AliHLTTPCCAGlobalSlicesPerformance::SetNewEvent

#ifndef HLTCA_STANDALONE
void AliHLTTPCCAGlobalSlicesPerformance::CreateHistos(string histoDir, TFile* outFile)
{
  UNUSED_PARAM2(histoDir, outFile);
  // don't use histos. Histos are builded in the SlicesPerformance.

  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    // slicePerformances[iPerf]->CreateHistos( histoDir + (string)TString(char(iPerf)), outFile ); // just set diff names, they anyway won't be written
    slicePerformances[iPerf]->CreateHistos( "" );
  }
}
#endif

void AliHLTTPCCAGlobalSlicesPerformance::Exec(bool print)
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->Exec(0);
  }
  AliHLTTPCCATrackPerformanceBase::Exec(print);
} // void AliHLTTPCCAGlobalSlicesPerformance::Exec

void AliHLTTPCCAGlobalSlicesPerformance::CheckMCTracks()
{
  mcData.resize(nMCTracks);
    // find reconstructable tracks
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    for ( int iMCTrack = 0; iMCTrack < nMCTracks; iMCTrack++ ){
 //     if ( (*fMCTracks)[iMCTrack].NMCPoints() >= PParameters::MinimumMCPointsForMCTrack ) 
      if ( (*fMCTracks)[iMCTrack].NMCRows() >= PParameters::MinimumMCPointsForMCTrack ) 
		  mcData[iMCTrack].SetAsReconstructable(); // global defition of reconstractable track
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
      else if ( mc.GetSet() == 2 ) {
        fEff.Inc(reco,clones,"ref");
      }
      else {
        fEff.Inc(reco,clones,"ref");
        fEff.Inc(reco,clones,"long_ref");
      }
    }
  } // for iMCTr

  AliHLTTPCCATrackPerformanceBase::EfficiencyPerformance();
} // void AliHLTTPCCAGlobalSlicesPerformance::EfficiencyPerformance()
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

