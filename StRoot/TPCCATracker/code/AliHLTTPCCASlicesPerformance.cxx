// $Id: AliHLTTPCCASlicesPerformance.cxx,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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
#include "AliHLTTPCCASlicesPerformance.h"


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

void AliHLTTPCCASlicesPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
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

} // void AliHLTTPCCASlicesPerformance::SetNewEvent

void AliHLTTPCCASlicesPerformance::CreateHistos(string histoDir)
{
  AliHLTTPCCAPerformanceBase::CreateHistos(histoDir);
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->CreateHistos( histoDir + (string)TString(char(iPerf)) ); // just set diff names, they anyway won't be written
  }
}

void AliHLTTPCCASlicesPerformance::Exec(bool print)
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->Exec(0);
  }
  AliHLTTPCCAPerformanceBase::Exec(print);
} // void AliHLTTPCCASlicesPerformance::Exec

void AliHLTTPCCASlicesPerformance::EfficiencyPerformance()
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    fEff += slicePerformances[iPerf]->GetEff();
  }
  fEffStat += fEff;
} // void AliHLTTPCCASlicesPerformance::EfficiencyPerformance()

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