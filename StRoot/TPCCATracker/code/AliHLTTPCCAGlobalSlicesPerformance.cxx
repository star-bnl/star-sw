// $Id: AliHLTTPCCAGlobalSlicesPerformance.cxx,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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

void AliHLTTPCCAGlobalSlicesPerformance::CreateHistos(string histoDir)
{
  // don't use histos. Histos are builded in the SlicesPerformance.

  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->CreateHistos( histoDir + (string)TString(char(iPerf)) ); // just set diff names, they anyway won't be written
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
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    vector<AliHLTTPCCAPerformanceMCTrackData> &mcs = slicePerformances[iPerf]->mcData;
    for ( int iMCTrack = 0; iMCTrack < nMCTracks; iMCTrack++ ){
      if (mcs[iMCTrack].IsReconstructable()) mcData[iMCTrack].SetAsReconstructable();
      if (mcs[iMCTrack].IsReconstructed()  ) mcData[iMCTrack].AddReconstructed();
    } // iMCTrack
  } // iSlice
  
  for ( int iMCTrack = 0; iMCTrack < nMCTracks; iMCTrack++ ){
    mcData[iMCTrack].SetSet( slicePerformances[0]->mcData[iMCTrack].GetSet() );
  } // iMCTrack
} // void AliHLTTPCCAGlobalSlicesPerformance::CheckMCTracks()

void AliHLTTPCCAGlobalSlicesPerformance::EfficiencyPerformance()
{
  for ( int iMCTr = 0; iMCTr < nMCTracks; iMCTr++ ) {
    AliHLTTPCCAPerformanceMCTrackData &mc = mcData[iMCTr];
    if ( !mc.IsReconstructable() ) continue;
    const bool reco = mc.IsReconstructed();
    const int clones = 0;

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

  fEffStat += fEff;
} // void AliHLTTPCCAGlobalSlicesPerformance::EfficiencyPerformance()


void AliHLTTPCCAGlobalSlicesPerformance::PrintEfficiencyStatistic()
{
  fEffStat.CalcEff();
  
  cout.setf(ios::fixed);
  cout.setf(ios::showpoint);
  cout.precision(3);
  cout << "Track category         : " << " Eff  "       <<" | "<< "All MC"  << endl;

  int NCounters = fEffStat.mc.NCounters;
  for (int iC = 0; iC < NCounters; iC++){
    cout << fEffStat.names[iC]  << "   : "
        << fEffStat.ratio_reco.counters[iC]
        << "  | " << fEffStat.mc.counters[iC]  << endl;
  }
}