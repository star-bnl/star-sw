/**********************************************************************
 *
 * $Id: StEStructPhiWeight.cxx,v 1.2 2007/02/05 17:20:28 msd Exp $
 *
 * Author: David Kettler
 *
 **********************************************************************
 *
 * Description:  Phi Weight calculation
 *
 ***********************************************************************/
#include "StEStructPhiWeight.h"

#include "TH1F.h"
#include "TFile.h"
#include "PhysicalConstants.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

ClassImp(StEStructPhiWeight)

StEStructPhiWeight::StEStructPhiWeight() {
  //Initialize

  nPhiBins = 120;
  phiMin = -pi;
  phiMax = pi;
  mPhiHist = new TH1F("Phi","Phi",nPhiBins,phiMin,phiMax);
}

StEStructPhiWeight::~StEStructPhiWeight() {
  finish();
}

bool StEStructPhiWeight::doEvent(StEStructEvent* event) {

  if(!event) return false;
  mCurrentEvent=event;

  StEStructTrackCollection *tp = mCurrentEvent->TrackCollectionP();
  for(StEStructTrackIterator iter = tp->begin(); iter != tp->end(); iter++) {
    StEStructTrack* pTrack = *iter;
    Float_t phi = pTrack->Phi();

    if(pTrack->TopologyMapTPCNHits() >= 0 || (pTrack->TopologyMapData(0)==0 && pTrack->TopologyMapData(1)==0)) {
      mPhiHist->Fill(phi);
    } else {
    }
  }
  StEStructTrackCollection *tm = mCurrentEvent->TrackCollectionM();
  for(StEStructTrackIterator iter = tm->begin(); iter != tm->end(); iter++) {
    StEStructTrack* pTrack = *iter;
    Float_t phi = pTrack->Phi();
    mPhiHist->Fill(phi);
  }
  
  //itr.~StEStructTrackIterator();

  return true;
}

void StEStructPhiWeight::finish(){
  //Write out the histogram
  TFile * tf=new TFile(moutFileName,"RECREATE");
  tf->cd();
  mPhiHist->Write();
  tf->Close();
  delete mPhiHist;
  delete tf;
}

/***********************************************************************
 *
 * $Log: StEStructPhiWeight.cxx,v $
 * Revision 1.2  2007/02/05 17:20:28  msd
 * Added include statement
 *
 * Revision 1.1  2006/04/26 18:58:14  dkettler
 *
 * Simple version of weight calculation analysis
 *
 *
 *
 *********************************************************************/
