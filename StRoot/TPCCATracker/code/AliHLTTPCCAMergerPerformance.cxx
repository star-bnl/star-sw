// $Id: AliHLTTPCCAMergerPerformance.cxx,v 1.1 2010/08/12 19:35:39 mzyzak Exp $
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
#include "AliHLTTPCCAMergerPerformance.h"


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

#include "TBranch.h"

void AliHLTTPCCAMergerPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
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
    
    iData = 0;
    MPDTree = new TTree("MergerPerfDataTree","MergerPerfDataTree");
    MergerData = new TClonesArray("MergerPerfData");
    MPDTree->Branch("MergerPerfData","MergerPerfData",&MergerData);
  }
  first_call = false;
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->SetNewEvent(fTracker, fHitLabels, fMCTracks, fLocalMCPoints);
  }

} // void AliHLTTPCCAMergerPerformance::SetNewEvent

void AliHLTTPCCAMergerPerformance::CreateHistos(string histoDir)
{
}

void AliHLTTPCCAMergerPerformance::AddMergerData(int iSlice1, int iTrack1, int iSlice2, int iTrack2, bool IsDzDs, bool IsCovMatrixPositiv, bool IsCovMatrixFinite, float &chi2)
{
  new(MergerData->At(iData)) MergerPerfData();
  
  MergerPerfData *mpd = (MergerPerfData*) MergerData->At(iData);
  mpd->IsDzDs = IsDzDs;
  mpd->IsCovMatrixPositiv = IsCovMatrixPositiv;
  mpd->IsCovMatrixFinite = IsCovMatrixFinite;
  mpd->chi2 = chi2;
  
  const int iMC1 = slicePerformances[iSlice1]->recoData[iTrack1].GetMCTrackId();
  const int iMC2 = slicePerformances[iSlice2]->recoData[iTrack2].GetMCTrackId();
  
  if(iMC1 == iMC2)
    mpd->IsSameTrack = 1;
  else
    mpd->IsSameTrack = 0;
  
  std::cout << "DzDs  " << mpd->IsDzDs << " ICP " << mpd->IsCovMatrixPositiv << " ICF " << mpd->IsCovMatrixFinite << " IST " << mpd->IsSameTrack << " chi2 " << mpd->chi2 << std::endl;
  iData++;
}

void AliHLTTPCCAMergerPerformance::Exec(bool print)
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    if ( !(slicePerformances[iPerf]->fTracker) ) return;
    slicePerformances[iPerf]->CheckMCTracks();
    slicePerformances[iPerf]->MatchTracks();
  }
} // void AliHLTTPCCAMergerPerformance::Exec


void AliHLTTPCCAMergerPerformance::FillHistos()
{
  for (int iH = 0; iH < NHisto; iH++){
    for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
        // move all data
      fHistos[iH]->Add(slicePerformances[iPerf]->fHistos[iH]);
      slicePerformances[iPerf]->fHistos[iH]->Reset();
    }
  }
} // void AliHLTTPCCAMergerPerformance::FillHistos()

void AliHLTTPCCAMergerPerformance::WriteHistos()
{
  WriteDir2Current( MPDTree );
}
