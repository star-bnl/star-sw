////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.hh,v 1.1 1999/11/05 00:06:42 posk Exp $
//
// Author: Art Poskanzer, LBNL, Oct 1999
//
// Description:  Class for applying flow event cuts
//               If lo >= hi no cut is applied
//               All functions and data members are static
//               Therefor, no need to instantiate
//               Just use StFlowCutEvent::func();
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.hh,v $
// Revision 1.1  1999/11/05 00:06:42  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////
#ifndef _StFlowCutEvent_INCLUDED_
#define _StFlowCutEvent_INCLUDED_

#include <iostream.h>
#include <stdlib.h>
#include "StFlowAnalysisMaker.h"

class StFlowCutEvent {

 public:

               StFlowCutEvent();
               ~StFlowCutEvent();
  static void  SetMult(Int_t lo, Int_t hi);
  static void  SetVertexX(Float_t lo, Float_t hi);
  static void  SetVertexY(Float_t lo, Float_t hi);
  static void  SetVertexZ(Float_t lo, Float_t hi);
  static Int_t CheckEvent(StEvent* mEvent);
  static void  PrintCutList();
  
 private:

  static UInt_t  mEventN;                // number of events
  static UInt_t  mGoodEventN;            // number of accepted events   
						
  static UInt_t  mMultCut;               // number of not accepted events
  static Int_t   mMultCuts[2];           // range of multiplicity

  static UInt_t  mVertexXCut;            // number of not accepted events
  static Float_t mVertexXCuts[2];        // range of X vertex position

  static UInt_t  mVertexYCut;            // number of not accepted events
  static Float_t mVertexYCuts[2];        // range of Y vertex position

  static UInt_t  mVertexZCut;            // number of not accepted events
  static Float_t mVertexZCuts[2];        // range of Z vertex position

  ClassDef(StFlowCutEvent,1)             // macro for rootcint
}; 

inline void StFlowCutEvent::SetMult(Int_t lo, Int_t hi) {
  mMultCuts[0] = lo; mMultCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexX(Float_t lo, Float_t hi) {
  mVertexXCuts[0] = lo; mVertexXCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexY(Float_t lo, Float_t hi) {
  mVertexYCuts[0] = lo; mVertexYCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexZ(Float_t lo, Float_t hi) {
  mVertexZCuts[0] = lo; mVertexZCuts[1] = hi; }

#endif
