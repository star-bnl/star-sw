////////////////////////////////////////////////////////////////////////////
//
// $Id: StuCutEvent.cxx,v 1.3 2003/09/02 17:58:09 perev Exp $
//
// Author: Art Poskanzer, LBNL, Dec 1999
//
// Description:  Class for applying event cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StuCutEvent.cxx,v $
// Revision 1.3  2003/09/02 17:58:09  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1999/12/21 16:12:24  posk
// Updates.
//
// Revision 1.1  1999/12/17 00:07:03  posk
// Classes for StEvent cuts.
//
// Revision 1.0   posk
// First version of StEvent cut classes.
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include "StuCutEvent.hh"
#include "StuCutTrack.hh"
#include "StEvent.h"
#include "StEventTypes.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorF.hh"
#define PR(x) cout << "##### CutEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StuCutEvent)

//-----------------------------------------------------------------------

Int_t    StuCutEvent::mMultCuts[2]    = {10, 10000};
Float_t  StuCutEvent::mVertexXCuts[2] = {-1., 1.};
Float_t  StuCutEvent::mVertexYCuts[2] = {-1., 1.};
Float_t  StuCutEvent::mVertexZCuts[2] = {-30., 30.};
UInt_t   StuCutEvent::mEventN         = 0;     
UInt_t   StuCutEvent::mGoodEventN     = 0;
UInt_t   StuCutEvent::mMultCut        = 0;
UInt_t   StuCutEvent::mVertexXCut     = 0;
UInt_t   StuCutEvent::mVertexYCut     = 0;
UInt_t   StuCutEvent::mVertexZCut     = 0;
Float_t  StuCutEvent::mEtaSymCuts[2]  = {-0.1, 0.1};
UInt_t   StuCutEvent::mEtaSymCutN     = 0;     

//-----------------------------------------------------------------------

StuCutEvent::StuCutEvent() {
  // To apply event cuts
}

//-----------------------------------------------------------------------

StuCutEvent::~StuCutEvent() {
}

//-----------------------------------------------------------------------

Int_t StuCutEvent::CheckEvent(StEvent* pEvent) {
  // Returns kTRUE if the event survives all the cuts

  mEventN++;
  
  // Number of primary vertices
  Long_t nvtx = pEvent->numberOfPrimaryVertices();
  if (nvtx == 0) return kFALSE;

  // have to add a mechanism to select the most relevant primary
  // vertex and use only this one (for now only one vertex is assumed)

  StPrimaryVertex* pVertex = pEvent->primaryVertex(0);
  if (!pVertex) return kFALSE;

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

Int_t StuCutEvent::CheckEtaSymmetry() {
  // Returns kTRUE if the event survives this Eta symmetry cut
  // Call at the end of the event after doing CheckTrack for each track
  // If kFALSE you should delete the last event

  Float_t mEtaSymPosN = (float)StuCutTrack::EtaSymPos();
  Float_t mEtaSymNegN = (float)StuCutTrack::EtaSymNeg();
  Float_t EtaSym = (mEtaSymPosN - mEtaSymNegN) / 
    (mEtaSymPosN + mEtaSymNegN);
  StuCutTrack::EtaSymClear();
  if (mEtaSymCuts[1] > mEtaSymCuts[0] && 
      (EtaSym < mEtaSymCuts[0] || EtaSym >= mEtaSymCuts[1])) {
    mEtaSymCutN++;
    mGoodEventN--;
    return kFALSE;
  }

  return kTRUE;
}

//-----------------------------------------------------------------------

void StuCutEvent::PrintCutList() {
  // Prints the list of cuts

  cout << "#######################################################" << endl;
  cout << "# Total Events= " << mEventN << endl;
  cout << "# Event Cut List:" << endl;
  cout << "#   Mult cuts= " << mMultCuts[0] << ", " << mMultCuts[1]
       << " :\t Events Cut= " << mMultCut << endl;
  cout << "#   VertexX cuts= " << mVertexXCuts[0] << ", " << mVertexXCuts[1]
       << " :\t Events Cut= " << mVertexXCut << endl;
  cout << "#   VertexY cuts= " << mVertexYCuts[0] << ", " << mVertexYCuts[1]
       << " :\t Events Cut= " << mVertexYCut << endl;
  cout << "#   VertexZ cuts= " << mVertexZCuts[0] << ", " << mVertexZCuts[1]
       << " :\t Events Cut= " << mVertexZCut << endl;
  cout << "#   Eta Symmetry cuts= " << mEtaSymCuts[0] << ", " << mEtaSymCuts[1] 
       << " :\t " <<  setprecision(4) << (float)mEtaSymCutN/(float)mEventN/perCent
       << "% cut" << endl;
  cout << "# Good Events = " << mGoodEventN << ", " << setprecision(4) <<
    (float)mGoodEventN/(float)mEventN/perCent << "%" << endl;
  cout << "#######################################################" << endl;

}
