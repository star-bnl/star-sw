////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.h,v 1.4 2002/09/11 21:33:43 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
// Description:  Class for applying flow event cuts
//               If lo >= hi no cut is applied
//               All functions and data members are static
//               Therefore, no need to instantiate
//               Just use StFlowCutEvent::func();
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.h,v $
// Revision 1.4  2002/09/11 21:33:43  posk
// Different S2 cuts for the two beam energies.
//
// Revision 1.3  2001/08/17 22:10:21  posk
// Now also can do 40 GeV data.
//
// Revision 1.2  2001/05/14 23:04:21  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.5  2000/08/31 18:58:19  posk
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowCutEvent_INCLUDED_
#define _StFlowCutEvent_INCLUDED_
#include <iostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StEbyeEvent;
class StFlowEvent;

class StFlowCutEvent {

 public:

                StFlowCutEvent();
  virtual       ~StFlowCutEvent();

  static Bool_t CheckEvent(StEbyeEvent* pMicroEvent);
  static Bool_t CheckEvent(StFlowEvent* pFlowEvent);
  static Bool_t CheckEtaSymmetry(StEbyeEvent* pMicroEvent);
  static void   PrintCutList();
  static void   SetCent(const Int_t lo, const Int_t hi);
  static void   SetMult(const Int_t lo, const Int_t hi);
  static void   SetFinalMult(const Int_t lo, const Int_t hi);
  static void   SetVertexX(const Float_t lo, const Float_t hi);
  static void   SetVertexY(const Float_t lo, const Float_t hi);
  static void   SetVertexZ(const Float_t lo, const Float_t hi);
  static void   SetEtaSym(Float_t lo, Float_t hi);
  static void   SetAdcS3(Int_t hi);
  
 private:

  static UInt_t  mEventN;                // number of events
  static UInt_t  mGoodEventN;            // number of accepted events   
  static UInt_t  mGoodFinalEventN;       // number of accepted events   
						
  static UInt_t  mCentCut;               // number of not accepted events
  static Int_t   mCentCuts[2];           // range of multiplicity

  static UInt_t  mMultCut;               // number of not accepted events
  static Int_t   mMultCuts[2];           // range of multiplicity

  static UInt_t  mFinalMultCut;          // number of not accepted events
  static Int_t   mFinalMultCuts[2];      // range of multiplicity

  static UInt_t  mVertexXCut;            // number of not accepted events
  static Float_t mVertexXCuts[2];        // range of X vertex position

  static UInt_t  mVertexYCut;            // number of not accepted events
  static Float_t mVertexYCuts[2];        // range of Y vertex position

  static UInt_t  mVertexZCut;            // number of not accepted events
  static Float_t mVertexZCuts[2];        // range of Z vertex position

  static UInt_t  mEtaSymCutN;            // number not accepted
  static Float_t mEtaSymCuts[2];         // range

  static UInt_t  mVertexFlagCutN;        // number of not accepted events

  static UInt_t  mAdcS3CutN;             // number of not accepted events
  static Int_t   mAdcS3Cut;              // upper limit of accepted adcS3

  ClassDef(StFlowCutEvent,1)             // macro for rootcint
}; 

inline void StFlowCutEvent::SetCent(const Int_t lo, const Int_t hi) {
  mCentCuts[0] = lo; mCentCuts[1] = hi; }

inline void StFlowCutEvent::SetMult(const Int_t lo, const Int_t hi) {
  mMultCuts[0] = lo; mMultCuts[1] = hi; }

inline void StFlowCutEvent::SetFinalMult(const Int_t lo, const Int_t hi) {
  mFinalMultCuts[0] = lo; mFinalMultCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexX(const Float_t lo, const Float_t hi) {
  mVertexXCuts[0] = lo; mVertexXCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexY(const Float_t lo, const Float_t hi) {
  mVertexYCuts[0] = lo; mVertexYCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexZ(const Float_t lo, const Float_t hi) {
  mVertexZCuts[0] = lo; mVertexZCuts[1] = hi; }

inline void StFlowCutEvent::SetEtaSym(Float_t lo, Float_t hi) {
  mEtaSymCuts[0] = lo; mEtaSymCuts[1] = hi; }

inline void StFlowCutEvent::SetAdcS3(Int_t hi) {
  mAdcS3Cut = hi; }

#endif
