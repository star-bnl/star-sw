////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.cxx,v 1.4 2002/09/11 21:33:42 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
// Description:  Class for applying event cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.cxx,v $
// Revision 1.4  2002/09/11 21:33:42  posk
// Different S2 cuts for the two beam energies.
//
// Revision 1.3  2001/08/17 22:10:17  posk
// Now also can do 40 GeV data.
//
// Revision 1.2  2001/05/14 23:04:17  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.19  2000/09/05 16:11:30  snelling
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include "StFlowCutEvent.h"
#include "StFlowCutTrack.h"
#include "StEbyeEvent.h"
#include "StFlowEvent.h"
#include "StFlowConstants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#define PR(x) cout << "##### FlowCutEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutEvent)

//-----------------------------------------------------------------------

Int_t    StFlowCutEvent::mCentCuts[2]      = {0, 0};
Int_t    StFlowCutEvent::mMultCuts[2]      = {10, 10000};
Int_t    StFlowCutEvent::mFinalMultCuts[2] = {6, 10000};
Float_t  StFlowCutEvent::mVertexXCuts[2]   = {-0.5, 0.5};
Float_t  StFlowCutEvent::mVertexYCuts[2]   = {-0.5, 0.3};
Float_t  StFlowCutEvent::mVertexZCuts[2]   = {-579.5, -578.3};
Float_t  StFlowCutEvent::mEtaSymCuts[2]    = {0.35, 0.75};
Int_t    StFlowCutEvent::mAdcS3Cut         = 83;
UInt_t   StFlowCutEvent::mEventN           = 0;     
UInt_t   StFlowCutEvent::mGoodEventN       = 0;
UInt_t   StFlowCutEvent::mGoodFinalEventN  = 0;
UInt_t   StFlowCutEvent::mCentCut          = 0;
UInt_t   StFlowCutEvent::mMultCut          = 0;
UInt_t   StFlowCutEvent::mFinalMultCut     = 0;
UInt_t   StFlowCutEvent::mVertexXCut       = 0;
UInt_t   StFlowCutEvent::mVertexYCut       = 0;
UInt_t   StFlowCutEvent::mVertexZCut       = 0;
UInt_t   StFlowCutEvent::mEtaSymCutN       = 0;     
UInt_t   StFlowCutEvent::mVertexFlagCutN   = 0;     
UInt_t   StFlowCutEvent::mAdcS3CutN        = 0;     

//-----------------------------------------------------------------------

StFlowCutEvent::StFlowCutEvent() {
  // To apply event cuts
}

//-----------------------------------------------------------------------

StFlowCutEvent::~StFlowCutEvent() {
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEvent(StEbyeEvent* pMicroEvent) {
  // Returns kTRUE if picoevent survives all the cuts
  
  if (!pMicroEvent) return kFALSE;

  // Centrality
  Int_t cent = (Int_t)pMicroEvent->Centrality();
  if (mCentCuts[0] && mCentCuts[1] >= mCentCuts[0] && 
      (cent < mCentCuts[0] || cent > mCentCuts[1])) {
    mCentCut++;
    return kFALSE;
  }    
  
  mEventN++;

  // Multiplicity
  Int_t mult = pMicroEvent->OrigMult();
  if (mMultCuts[1] > mMultCuts[0] && 
     (mult < mMultCuts[0] || mult >= mMultCuts[1])) {
    mMultCut++;
    return kFALSE;
  }
   
  // Vertex Flag
  if (pMicroEvent->Viflag() != 0) {
    mVertexFlagCutN++;
    return kFALSE;
  }

  // Vertex x
  Float_t vertexX = pMicroEvent->Vx();
  if (mVertexXCuts[1] > mVertexXCuts[0] &&
     (vertexX < mVertexXCuts[0] || vertexX >= mVertexXCuts[1])) {
    mVertexXCut++;
    return kFALSE;
  }

  // Vertex y
  Float_t vertexY = pMicroEvent->Vy();
  if (mVertexYCuts[1] > mVertexYCuts[0] &&
     (vertexY < mVertexYCuts[0] || vertexY >= mVertexYCuts[1])) {
    mVertexYCut++;
    return kFALSE;
  }

  // Vertex z
  Float_t vertexZ = pMicroEvent->Vz();
  if (mVertexZCuts[1] > mVertexZCuts[0] &&
     (vertexZ < mVertexZCuts[0] || vertexZ >= mVertexZCuts[1])) {
    mVertexZCut++;
    return kFALSE;
  }

  // S3 ADC
  if (mAdcS3Cut > 0 &&pMicroEvent->ADCS3() >= mAdcS3Cut) {
    mAdcS3CutN++;
    return kFALSE;
  }
  
  mGoodEventN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEvent(StFlowEvent* pFlowEvent) {
  // Returns kTRUE if flowevent survives all the cuts
  
  if (!pFlowEvent) return kFALSE;

  // Multiplicity
  Int_t mult = pFlowEvent->FlowEventMult();
  if (mFinalMultCuts[1] > mFinalMultCuts[0] && 
     (mult < mFinalMultCuts[0] || mult >= mFinalMultCuts[1])) {
    mFinalMultCut++;
    return kFALSE;
  }
   
  mGoodFinalEventN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowCutEvent::CheckEtaSymmetry(StEbyeEvent* pMicroEvent) {
  // Returns kTRUE if picoevent survives this Eta symmetry cut
  // Call at the end of the event after doing CheckTrack for each track
  // If kFALSE you should delete the last event

  float etaSymPosN = (float)StFlowCutTrack::EtaSymPos();
  float etaSymNegN = (float)StFlowCutTrack::EtaSymNeg();
  float etaSym = (etaSymPosN - etaSymNegN) / (etaSymPosN + etaSymNegN);
  StFlowCutTrack::EtaSymClear();

  if (mEtaSymCuts[1] > mEtaSymCuts[0] && 
      (etaSym < mEtaSymCuts[0] || etaSym >= mEtaSymCuts[1])) {
    mEtaSymCutN++;
    mGoodEventN--;
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
  cout << "#   Centrality cuts= " << mCentCuts[0] << ", " << mCentCuts[1]
       << " :\t Events Cut= " << mCentCut << endl;
  cout << "#   Mult cuts= " << mMultCuts[0] << ", " << mMultCuts[1]
       << " :\t Events Cut= " << mMultCut << "\t (" <<  setprecision(3) << 
    (float)mMultCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   Vertex Flag cut= 0 :\t Events Cut= " << mVertexFlagCutN <<
    "\t (" << setprecision(3) << (float)mVertexFlagCutN/(float)mEventN/perCent
       << "% cut)" << endl; 
  cout << "#   VertexX cuts= " << mVertexXCuts[0] << ", " << mVertexXCuts[1]
       << " :\t Events Cut= " << mVertexXCut << "\t (" <<  setprecision(3) << 
    (float)mVertexXCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   VertexY cuts= " << mVertexYCuts[0] << ", " << mVertexYCuts[1]
       << " :\t Events Cut= " << mVertexYCut << "\t (" <<  setprecision(3) << 
    (float)mVertexYCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   VertexZ cuts= "  << setprecision(4) << mVertexZCuts[0] << ", " 
       << mVertexZCuts[1] << " :\t Events Cut= " << mVertexZCut << "\t (" 
       <<  setprecision(3) << (float)mVertexZCut/(float)mEventN/perCent 
       << "% cut)" << endl;
  cout << "#   EtaSym cuts= " << mEtaSymCuts[0] << ", " << mEtaSymCuts[1] 
       << " :\t Events Cut= " << mEtaSymCutN << "\t (" <<  setprecision(3)
       << (float)mEtaSymCutN/(float)mEventN/perCent << "% cut)" << endl;
  cout << "#   S3 ADC cut= " << mAdcS3Cut << " :\t\t Events Cut= " << mAdcS3CutN <<
    "\t (" << setprecision(3) << (float)mAdcS3CutN/(float)mEventN/perCent
       << "% cut)" << endl; 
  cout << "# Good Events = " << mGoodEventN << ", " << setprecision(3) <<
    (float)mGoodEventN/(float)mEventN/perCent << "%" << endl;
  cout << "#   Final Mult cuts= " << mFinalMultCuts[0] << ", " << mFinalMultCuts[1]
       << " :\t Events Cut= " << mFinalMultCut << "\t (" <<  setprecision(3) << 
    (float)mFinalMultCut/(float)mEventN/perCent << "% cut)" << endl;
  cout << "# Good Final Events = " << mGoodFinalEventN << ", " << setprecision(3) <<
    (float)mGoodFinalEventN/(float)mEventN/perCent << "%" << endl;
  cout << "#######################################################" << endl;

}
