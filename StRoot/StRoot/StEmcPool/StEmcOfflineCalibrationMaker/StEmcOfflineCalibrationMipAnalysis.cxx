/*
 * StEmcOfflineCalibrationMipAnalysis.cxx
 * J. Kevin Adkins, University of Kentucky
 * June 18, 2014
 *
 * Based on the previous mip_histogram_maker.C
 * macro written by Matt Walker. Converted into 
 * compiled form to increase code stability when
 * running over files and to run over new version
 * of StEmcOfflineCalibrationEvent classes
 */

// User defined includes
#include "StEmcOfflineCalibrationMipAnalysis.h"
#include "StEmcOfflineCalibrationEvent.h"

// ROOT includes
#include "TH1D.h"
#include "TH2F.h"
#include "TChain.h"
#include "TFile.h"
#include  "TString.h"

// STD library includes
#include <set>
#include <map>

ClassImp(StEmcOfflineCalibrationMipAnalysis);

StEmcOfflineCalibrationMipAnalysis::StEmcOfflineCalibrationMipAnalysis(const char *name, const char* outfile, TChain *calibChain):StMaker(name),nTowers(4800){
  mCalibChain = calibChain;
  mOutfileName = (TString)outfile;
  mEvent = 0;
  mVertex = 0;
  mTrack = 0;
  mFile = 0;
}

StEmcOfflineCalibrationMipAnalysis::~StEmcOfflineCalibrationMipAnalysis(){
  if(mCalibChain) delete mCalibChain;
  if(mEvent) delete mEvent;
  if(mVertex) delete mVertex;
  if(mTrack) delete mTrack;
  if (mFile) delete mFile;
}

Int_t StEmcOfflineCalibrationMipAnalysis::Init(){  
  mFile = new TFile(mOutfileName, "RECREATE");
  mCalibChain->SetBranchAddress("event_branch",&mEvent);
  
  Char_t name[100];
  for (Int_t iTow = 0; iTow < nTowers; ++iTow){
    sprintf(name,"tower_histo_%i",iTow+1);
    towerHisto[iTow] = new TH1D(name,name,250,-50.5,199.5);
    towerHisto[iTow]->Sumw2();
  }

  mapcheck = new TH2F("mapcheck","check mapping",4800,0.5,4800.5,4800,0.5,4800.5);
  mapcheck->GetXaxis()->SetTitle("Track Projection ID");
  mapcheck->GetYaxis()->SetTitle("Tower hit above 5 rms");

   return StMaker::Init();
}

Int_t StEmcOfflineCalibrationMipAnalysis::Make(){
  pedSubAdc = 0.;
  assert(mEvent);

  trackTowers.clear();
  excludedTowers.clear();

  // Loop over tracks to get excluded towers
  for (Int_t iTrk = 0; iTrk < mEvent->nTracks(); ++iTrk){
    mTrack = mEvent->track(iTrk);
    Int_t softId = mTrack->towerId(0);
    
    if(trackTowers.find(softId) != trackTowers.end())
      excludedTowers.insert(softId);      
    else
      trackTowers.insert(softId);
  }

  // Begin loop over vertices
  for(Int_t iVert = 0; iVert < mEvent->nVertices(); ++iVert){
    mVertex = mEvent->vertex(iVert);
    assert(mVertex);
    if (mVertex->ranking() < 1e6) continue;
    if (TMath::Abs(mVertex->z()) > 30) continue;
    
    // Begin loop over tracks at vertex
    for(Int_t iTrack = 0; iTrack < mVertex->nTracks(); ++iTrack){
      mTrack = mVertex->track(iTrack);
      assert(mTrack);
      pedSubAdc = mTrack->towerAdc(0) - mTrack->towerPedestal(0);

      if (excludedTowers.find(mTrack->towerId(0)) != excludedTowers.end()) continue;
      if (mTrack->p() < 1.) continue;
      if (mTrack->towerId(0) != mTrack->towerExitId()) continue;
      
      for(Int_t k = 0; k < 9; k++){
        if(mTrack->towerAdc(k) - mTrack->towerPedestal(k) < 5*mTrack->towerPedestalRms(k)) continue;
        mapcheck->Fill(mTrack->towerId(0),mTrack->towerId(k));
      }
      
      if(mTrack->highestNeighbor() > 2.) continue;
      if(pedSubAdc < 1.5*mTrack->towerPedestalRms(0)) continue;
      
      Int_t index = mTrack->towerId(0) - 1;
      towerHisto[index]->Fill(pedSubAdc);
    }// Tracks loop
  }// Vertex Loop

 return kStOK;
}

Int_t StEmcOfflineCalibrationMipAnalysis::Finish()
{
  mFile->Write();
  mFile->Close();
  delete mFile;
  return kStOk;
}
