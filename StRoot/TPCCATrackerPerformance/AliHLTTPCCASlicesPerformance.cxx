// $Id: AliHLTTPCCASlicesPerformance.cxx,v 1.5 2012/08/13 19:35:05 fisyak Exp $
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
#include "AliHLTTPCCASlicesPerformance.h"


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
#endif // HLTCA_STANDALONE

void AliHLTTPCCASlicesPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCATrackPerformanceBase::SetNewEvent(tracker, hitLabels, mcTracks, localMCPoints);

    /// Init subperformances
  if (fFirstCall){
    if(fTracker)
      slicePerformances.resize(fTracker->NSlices());
    else
      slicePerformances.resize(0);
    for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
      slicePerformances[iPerf] = new AliHLTTPCCASlicePerformance(iPerf);
    }
  }
  fFirstCall = false;
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->SetNewEvent(fTracker, fHitLabels, fMCTracks, fLocalMCPoints);
  }

} // void AliHLTTPCCASlicesPerformance::SetNewEvent

#ifndef HLTCA_STANDALONE
void AliHLTTPCCASlicesPerformance::CreateHistos(string histoDir, TFile* outFile)
{
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    // slicePerformances[iPerf]->CreateHistos( histoDir + (string)TString(char(iPerf)), outFile ); // just set diff names, they anyway won't be written
    slicePerformances[iPerf]->CreateHistos( "", 0 );
  }
  
  AliHLTTPCCATrackPerformanceBase::CreateHistos(histoDir, outFile);
}
#endif

void AliHLTTPCCASlicesPerformance::Exec(bool print)
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->Exec(0);
  }
  AliHLTTPCCATrackPerformanceBase::Exec(print);
} // void AliHLTTPCCASlicesPerformance::Exec

void AliHLTTPCCASlicesPerformance::EfficiencyPerformance()
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    fEff += slicePerformances[iPerf]->GetEff();
  }

  AliHLTTPCCATrackPerformanceBase::EfficiencyPerformance();
} // void AliHLTTPCCASlicesPerformance::EfficiencyPerformance()

#ifndef HLTCA_STANDALONE
void AliHLTTPCCASlicesPerformance::FillHistos()
{
  for (int iH = 0; iH < NHisto; iH++){
    for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
        // move all data
      fHistos[iH]->Add(slicePerformances[iPerf]->fHistos[iH]);
      slicePerformances[iPerf]->fHistos[iH]->Reset();
    }
  }
} // void AliHLTTPCCASlicesPerformance::FillHistos()
#endif
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
