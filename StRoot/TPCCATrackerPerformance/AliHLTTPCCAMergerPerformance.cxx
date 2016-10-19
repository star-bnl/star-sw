// $Id: AliHLTTPCCAMergerPerformance.cxx,v 1.4 2012/08/13 19:35:05 fisyak Exp $
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
#if 0//def DO_TPCCATRACKER_EFF_PERFORMANCE outdated

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCATrackPerformanceBase.h"
#include "AliHLTTPCCAMergerPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
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


//ClassImp(MergerPerfData)
//ClassImp(MergerPerfDataEvent)
AliHLTTPCCAMergerPerformance::AliHLTTPCCAMergerPerformance()
:iData(0),iData2Step(0),first_call(true),MPDTree(0)
,MPDTree2Step(0),MergerData(0),fMPDE(0),fMPDE2Step(0)
{}

void AliHLTTPCCAMergerPerformance::SetNewEvent(const AliHLTTPCCAGBTracker * const Tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  AliHLTTPCCATrackPerformanceBase::SetNewEvent(Tracker, hitLabels, mcTracks, localMCPoints);

    /// Init subperformances
//   static bool first_call = true;
  if (first_call){
    slicePerformances.resize(fTracker->NSlices());
    for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
      slicePerformances[iPerf] = new AliHLTTPCCASlicePerformance(iPerf);
    }
    
    iData = 0;
    iData2Step = 0;
//    MPDTree = new TTree("MergerPerfDataTree","MergerPerfDataTree");
//    MergerData = new TClonesArray("MergerPerfData");
//    MPDTree->Branch("TClonesArray","TClonesArray",&MergerData);
  }
  first_call = false;
  
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    slicePerformances[iPerf]->SetNewEvent(fTracker, fHitLabels, fMCTracks, fLocalMCPoints);
  }

} // void AliHLTTPCCAMergerPerformance::SetNewEvent

void AliHLTTPCCAMergerPerformance::CreateHistos(string histoDir, TFile* outFile)
{
  if(fIsHistoCreated) return;

  TDirectory *curdir = gDirectory;
  if ( (histoDir != "") && outFile) {  // create in file
    if (outFile) outFile->cd();
    fHistoDir = outFile->mkdir( TString(histoDir) );
    fHistoDir->cd();
    MPDTree = new TTree("MergerPerfDataTree","MergerPerfDataTree");
    MPDTree2Step = new TTree("MergerPerfDataTree2Step","MergerPerfDataTree2Step");
  
    gDirectory->cd( ".." );
    curdir->cd();    
  }
  else
  {
    MPDTree = new TTree("MergerPerfDataTree","MergerPerfDataTree");
    MPDTree2Step = new TTree("MergerPerfDataTree2Step","MergerPerfDataTree2Step");
  }
  
  fMPDE = new MergerPerfDataEvent();
  fMPDE->MergerData = new TClonesArray("MergerPerfData");
  fMPDE2Step = new MergerPerfDataEvent();
  fMPDE2Step->MergerData = new TClonesArray("MergerPerfData");

  MPDTree->Branch("MergerPerfDataEvent","MergerPerfDataEvent",&fMPDE);
  MPDTree2Step->Branch("MergerPerfDataEvent","MergerPerfDataEvent",&fMPDE2Step);

  SetHistoCreated();
}

void AliHLTTPCCAMergerPerformance::AddMergerData(int step, int iSlice1, int iTrack1, int iSlice2, int iTrack2, bool IsDzDs, bool IsCovMatrixPositiv, bool IsCovMatrixFinite, float &chi2, float &delta2, float &Si, float &dy, float &dz, float &dsin, int nClust1, int nClust2)
{
  MergerPerfDataEvent *event;
  int i;
  if(step == 1) { event = fMPDE; i = iData; }
  if(step == 0) { event = fMPDE2Step; i = iData2Step; }

  TClonesArray &mData = *(event->MergerData);
  new(mData[i]) MergerPerfData();
  MergerPerfData *mpd = (MergerPerfData*) event->MergerData->At(i);
  mpd->IsDzDs = IsDzDs;
  mpd->IsCovMatrixPositive = IsCovMatrixPositiv;
  mpd->IsCovMatrixFinite = IsCovMatrixFinite;
  mpd->chi2 = chi2;
  mpd->dy = dy;
  mpd->dz = dz;
  mpd->dsin = dsin;
  mpd->IsMerged = 0;
  mpd->nClust1 = nClust1;
  mpd->nClust2 = nClust2;
  mpd->delta2 = delta2;
  mpd->Si = Si;
      
  const int iMC1 = slicePerformances[iSlice1]->recoData[iTrack1].GetMCTrackId();
  const int iMC2 = slicePerformances[iSlice2]->recoData[iTrack2].GetMCTrackId();
  
  if(iMC1 == iMC2)
    mpd->IsSameTrack = 1;
  else
    mpd->IsSameTrack = 0;

  if(step == 1) iData++;
  if(step == 0) iData2Step++;
}

void AliHLTTPCCAMergerPerformance::SetMerged(int step)
{
  MergerPerfDataEvent *event;
  int i;
  if(step == 1) { event = fMPDE; i = iData; }
  if(step == 0) { event = fMPDE2Step; i = iData2Step; }

  if(i > 0)
  {
    MergerPerfData *mpd = (MergerPerfData*) event->MergerData->At(i - 1);
    mpd->IsMerged = 1;
  }
}

void AliHLTTPCCAMergerPerformance::FillTree()
{
  MPDTree->Fill();
  if(fMPDE->MergerData) delete fMPDE->MergerData;
  fMPDE->MergerData = 0;
  fMPDE->MergerData = new TClonesArray("MergerPerfData");

  MPDTree2Step->Fill();
  if(fMPDE2Step->MergerData) delete fMPDE2Step->MergerData;
  fMPDE2Step->MergerData = 0;
  fMPDE2Step->MergerData = new TClonesArray("MergerPerfData");

  iData = 0;
  iData2Step = 0;
}

void AliHLTTPCCAMergerPerformance::Exec(bool print)
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    if ( !(slicePerformances[iPerf]->fTracker) ) return;
    slicePerformances[iPerf]->CheckMCTracks();
    slicePerformances[iPerf]->MatchTracks();
  }
} // void AliHLTTPCCAMergerPerformance::Exec

void AliHLTTPCCAMergerPerformance::FillMC()
{
  for (unsigned int iPerf = 0; iPerf < slicePerformances.size(); iPerf++){
    if ( !(slicePerformances[iPerf]->fTracker) ) return;
    slicePerformances[iPerf]->CheckMCTracks();
    slicePerformances[iPerf]->MatchTracks();
  }
}

#ifndef HLTCA_STANDALONE
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
#endif // HLTCA_STANDALONE
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
