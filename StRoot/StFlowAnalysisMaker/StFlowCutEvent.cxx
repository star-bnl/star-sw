////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.cxx,v 1.1 1999/11/05 00:06:41 posk Exp $
//
// Author: Art Poskanzer, LBNL, Oct 1999
//
// Description:  Class for applying event cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.cxx,v $
// Revision 1.1  1999/11/05 00:06:41  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "StFlowCutEvent.hh"
#include "StFlowAnalysisMaker.h"
#include "StEvent.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorF.hh"

#define PR(x) cout << (#x) << " = " << (x) << endl;
ClassImp(StFlowCutEvent)

StFlowCutEvent::StFlowCutEvent() {
  // To apply event cuts
}

StFlowCutEvent::~StFlowCutEvent() {
}

Int_t    StFlowCutEvent::mMultCuts[2]    = {10, 10000};
Float_t  StFlowCutEvent::mVertexXCuts[2] = {-1., 1.};
Float_t  StFlowCutEvent::mVertexYCuts[2] = {-1., 1.};
Float_t  StFlowCutEvent::mVertexZCuts[2] = {-100., 100.};
UInt_t   StFlowCutEvent::mEventN         = 0;     
UInt_t   StFlowCutEvent::mGoodEventN     = 0;
UInt_t   StFlowCutEvent::mMultCut        = 0;
UInt_t   StFlowCutEvent::mVertexXCut     = 0;
UInt_t   StFlowCutEvent::mVertexYCut     = 0;
UInt_t   StFlowCutEvent::mVertexZCut     = 0;


Int_t StFlowCutEvent::CheckEvent(StEvent* mEvent) {
  // Returns kTRUE if the event survives all the cuts
  mEventN++;
  
  //Long_t mult = mEvent->summary->mNumberOfGoodPrimaryTracks();
  long mult = mEvent->trackCollection()->size();
  if (mMultCuts[1] > mMultCuts[0] && 
     (mult < mMultCuts[0] || mult >= mMultCuts[1])) {
    mMultCut++;
    return kFALSE;
  }
  
  //StThreeVectorF vertex = mEvent->summary->mPrimaryVertexPos();
  StVertex* pVertex = mEvent->primaryVertex();
  if (!pVertex) return kFALSE;
  const StThreeVectorF& vertex = pVertex->position();
 
  Float_t vertexX = vertex.x();
  if (mVertexXCuts[1] > mVertexXCuts[0] &&
     (vertexX < mVertexXCuts[0] || vertexX >= mVertexXCuts[1])) {
    mVertexXCut++;
    return kFALSE;
  }

  Float_t vertexY = vertex.y();
  if (mVertexYCuts[1] > mVertexYCuts[0] &&
     (vertexY < mVertexYCuts[0] || vertexY >= mVertexYCuts[1])) {
    mVertexYCut++;
    return kFALSE;
  }

  Float_t vertexZ = vertex.z();
  if (mVertexZCuts[1] > mVertexZCuts[0] &&
     (vertexZ < mVertexZCuts[0] || vertexZ >= mVertexZCuts[1])) {
    mVertexZCut++;
    return kFALSE;
  }

  mGoodEventN++;
  return kTRUE;
}

void StFlowCutEvent::PrintCutList() {
  // Prints the list of cuts
  cout << "#######################################################" << endl;
  cout << "# Total Events= " << mEventN << endl;
  cout << "# Event Cut List:" << endl;
  cout << "#   Mult cuts= " << mMultCuts[0] << ", " << mMultCuts[1]
       << "\t Events Cut= " << mMultCut << endl;
  cout << "#   VertexX cuts= " << mVertexXCuts[0] << ", " << mVertexXCuts[1]
       << "\t Events Cut= " << mVertexXCut << endl;
  cout << "#   VertexY cuts= " << mVertexYCuts[0] << ", " << mVertexYCuts[1]
       << "\t Events Cut= " << mVertexYCut << endl;
  cout << "#   VertexZ cuts= " << mVertexZCuts[0] << ", " << mVertexZCuts[1]
       << "\t Events Cut= " << mVertexZCut << endl;
  cout << "# Good Events = " << mGoodEventN << ", " << 
    (float)mGoodEventN/(float)mEventN/perCent << "%" << endl;
  cout << "#######################################################" << endl;
}



