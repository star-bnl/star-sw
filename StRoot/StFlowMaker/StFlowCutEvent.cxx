////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.cxx,v 1.17 2000/08/10 23:00:19 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying event cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.cxx,v $
// Revision 1.17  2000/08/10 23:00:19  posk
// New centralities. pt and eta cuts.
//
// Revision 1.16  2000/07/20 17:25:49  posk
// Fixed bug in readPico checkEvent.
//
// Revision 1.15  2000/07/14 23:49:03  snelling
// Changed to ConstIterator for new StEvent and removed comparison int uint
//
// Revision 1.14  2000/07/12 17:54:33  posk
// Added chi2 and dca cuts. Multiplied EtaSym by sqrt(mult).
// Apply cuts when reading picoevent file.
//
// Revision 1.13  2000/06/30 14:48:29  posk
// Using MessageMgr, changed Eta Symmetry cut.
//
// Revision 1.12  2000/06/01 18:26:32  posk
// Increased precision of Track integer data members.
//
// Revision 1.11  2000/05/26 21:29:26  posk
// Protected Track data members from overflow.
//
// Revision 1.10  2000/05/11 20:00:31  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.9  2000/03/02 23:02:38  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.5  1999/12/15 22:01:22  posk
// Added StFlowConstants.hh
//
// Revision 1.4  1999/12/04 00:10:30  posk
// Works with the new StEvent
//
// Revision 1.3  1999/11/30 18:52:47  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:09  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/11 23:08:47  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/05 00:06:41  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include "StFlowCutEvent.h"
#include "StFlowCutTrack.h"
#include "StEvent.h"
#include "StFlowPicoEvent.h"
#include "StEventTypes.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorF.hh"
#define PR(x) cout << "##### FlowCutEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutEvent)

//-----------------------------------------------------------------------

Int_t    StFlowCutEvent::mMultCuts[2]    = {10, 10000};
Float_t  StFlowCutEvent::mVertexXCuts[2] = {-1., 1.};
Float_t  StFlowCutEvent::mVertexYCuts[2] = {-1., 1.};
Float_t  StFlowCutEvent::mVertexZCuts[2] = {-75., 75.};
UInt_t   StFlowCutEvent::mEventN         = 0;     
UInt_t   StFlowCutEvent::mGoodEventN     = 0;
UInt_t   StFlowCutEvent::mMultCut        = 0;
UInt_t   StFlowCutEvent::mVertexXCut     = 0;
UInt_t   StFlowCutEvent::mVertexYCut     = 0;
UInt_t   StFlowCutEvent::mVertexZCut     = 0;
Float_t  StFlowCutEvent::mEtaSymCuts[2]  = {0., 0.};
UInt_t   StFlowCutEvent::mEtaSymCutN     = 0;     

//-----------------------------------------------------------------------

StFlowCutEvent::StFlowCutEvent() {
  // To apply event cuts
}

//-----------------------------------------------------------------------

StFlowCutEvent::~StFlowCutEvent() {
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEvent(StEvent* pEvent) {
  // Returns kTRUE if StEvent survives all the cuts
  
  // Primary vertex
  Long_t nvtx = pEvent->numberOfPrimaryVertices();
  if (nvtx == 0) return kFALSE;

  StPrimaryVertex* pVertex = pEvent->primaryVertex(0);
  if (!pVertex) return kFALSE;

  mEventN++;

  // Multiplicity
  Long_t mult = pVertex->numberOfDaughters();
  if (mMultCuts[1] > mMultCuts[0] && 
     (mult < mMultCuts[0] || mult >= mMultCuts[1])) {
    mMultCut++;
    return kFALSE;
  }
  
  const StThreeVectorF& vertex = pVertex->position();
 
  // Vertex x
  Float_t vertexX = vertex.x();
  if (mVertexXCuts[1] > mVertexXCuts[0] &&
     (vertexX < mVertexXCuts[0] || vertexX >= mVertexXCuts[1])) {
    mVertexXCut++;
    return kFALSE;
  }

  // Vertex y
  Float_t vertexY = vertex.y();
  if (mVertexYCuts[1] > mVertexYCuts[0] &&
     (vertexY < mVertexYCuts[0] || vertexY >= mVertexYCuts[1])) {
    mVertexYCut++;
    return kFALSE;
  }

  // Vertex z
  Float_t vertexZ = vertex.z();
  if (mVertexZCuts[1] > mVertexZCuts[0] &&
     (vertexZ < mVertexZCuts[0] || vertexZ >= mVertexZCuts[1])) {
    mVertexZCut++;
    return kFALSE;
  }

  mGoodEventN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEvent(StFlowPicoEvent* pPicoEvent) {
  // Returns kTRUE if picoevent survives all the cuts
  
  if (!pPicoEvent) return kFALSE;

  mEventN++;

  // Multiplicity
  Int_t mult = pPicoEvent->OrigMult();
  if (mMultCuts[1] > mMultCuts[0] && 
     (mult < mMultCuts[0] || mult >= mMultCuts[1])) {
    mMultCut++;
    return kFALSE;
  }
   
  // Vertex x
  Float_t vertexX = pPicoEvent->VertexX();
  if (mVertexXCuts[1] > mVertexXCuts[0] &&
     (vertexX < mVertexXCuts[0] || vertexX >= mVertexXCuts[1])) {
    mVertexXCut++;
    return kFALSE;
  }

  // Vertex y
  Float_t vertexY = pPicoEvent->VertexY();
  if (mVertexYCuts[1] > mVertexYCuts[0] &&
     (vertexY < mVertexYCuts[0] || vertexY >= mVertexYCuts[1])) {
    mVertexYCut++;
    return kFALSE;
  }

  // Vertex z
  Float_t vertexZ = pPicoEvent->VertexZ();
  if (mVertexZCuts[1] > mVertexZCuts[0] &&
     (vertexZ < mVertexZCuts[0] || vertexZ >= mVertexZCuts[1])) {
    mVertexZCut++;
    return kFALSE;
  }

  mGoodEventN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEtaSymmetry(StEvent* pEvent) {
  // Returns kTRUE if StEvent survives this Eta symmetry cut
  // Call at the end of the event after doing CheckTrack for each track
  // If kFALSE you should delete the last event

  float etaSymPosN = (float)StFlowCutTrack::EtaSymPos();
  float etaSymNegN = (float)StFlowCutTrack::EtaSymNeg();
  float etaSym = (etaSymPosN - etaSymNegN) / (etaSymPosN + etaSymNegN);
  StFlowCutTrack::EtaSymClear();

  StPrimaryVertex* pVertex = pEvent->primaryVertex(0);
  if (!pVertex) return kFALSE;
  const StThreeVectorF& vertex = pVertex->position();
  Float_t vertexZ = vertex.z();
  float etaSymZSlope = 0.003;
  etaSym += (etaSymZSlope * vertexZ); // correction for acceptance
  etaSym *= sqrt((double)(etaSymPosN + etaSymNegN)); // corrected for statistics

  if (mEtaSymCuts[1] > mEtaSymCuts[0] && 
      (etaSym < mEtaSymCuts[0] || etaSym >= mEtaSymCuts[1])) {
    mEtaSymCutN++;
    mGoodEventN--;
    return kFALSE;
  }

  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEtaSymmetry(StFlowPicoEvent* pPicoEvent) {
  // Returns kTRUE if picoevent survives this Eta symmetry cut
  // Call at the end of the event after doing CheckTrack for each track
  // If kFALSE you should delete the last event

  float etaSymPosN = (float)StFlowCutTrack::EtaSymPos();
  float etaSymNegN = (float)StFlowCutTrack::EtaSymNeg();
  float etaSym = (etaSymPosN - etaSymNegN) / (etaSymPosN + etaSymNegN);
  StFlowCutTrack::EtaSymClear();

  Float_t vertexZ = pPicoEvent->VertexZ();
  float etaSymZSlope = 0.003;
  etaSym += (etaSymZSlope * vertexZ); // correction for acceptance
  etaSym *= sqrt((double)(etaSymPosN + etaSymNegN)); // corrected for statistics

  if (mEtaSymCuts[1] > mEtaSymCuts[0] && 
      (etaSym < mEtaSymCuts[0] || etaSym >= mEtaSymCuts[1])) {
    mEtaSymCutN++;
    mGoodEventN--;
//     Int_t eventID = (Int_t)(pEvent->id());
//     cout << "EventID= " << eventID << ", EtaSym= " << etaSym << ", VertexZ= " << vertexZ << endl;
    return kFALSE;
  }

  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutEvent::PrintCutList() {
  // Prints the list of cuts

  cout << "#######################################################" << endl;
  cout << "# Primary Vertex Events= " << mEventN << endl;
  cout << "# Event Cut List:" << endl;
  cout << "#   Mult cuts= " << mMultCuts[0] << ", " << mMultCuts[1]
       << " :\t Events Cut= " << mMultCut << "\t (" <<  setprecision(3) << 
    (float)mMultCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   VertexX cuts= " << mVertexXCuts[0] << ", " << mVertexXCuts[1]
       << " :\t Events Cut= " << mVertexXCut << "\t (" <<  setprecision(3) << 
    (float)mVertexXCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   VertexY cuts= " << mVertexYCuts[0] << ", " << mVertexYCuts[1]
       << " :\t Events Cut= " << mVertexYCut << "\t (" <<  setprecision(3) << 
    (float)mVertexYCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   VertexZ cuts= " << mVertexZCuts[0] << ", " << mVertexZCuts[1]
       << " :\t Events Cut= " << mVertexZCut << "\t (" <<  setprecision(3) << 
    (float)mVertexZCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   EtaSym cuts= " << mEtaSymCuts[0] << ", " << mEtaSymCuts[1] 
       << " :\t Events Cut= " << mEtaSymCutN << "\t (" <<  setprecision(3) << 
    (float)mEtaSymCutN/(float)mEventN/perCent << "% cut)" << endl;
  cout << "# Good Events = " << mGoodEventN << ", " << setprecision(3) <<
    (float)mGoodEventN/(float)mEventN/perCent << "%" << endl;
  cout << "#######################################################" << endl;

}
