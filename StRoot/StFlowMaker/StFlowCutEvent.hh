////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.hh,v 1.3 1999/11/30 18:52:48 snelling Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying flow event cuts
//               If lo >= hi no cut is applied
//               All functions and data members are static
//               Therefore, no need to instantiate
//               Just use StFlowCutEvent::func();
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.hh,v $
// Revision 1.3  1999/11/30 18:52:48  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:10  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/11 23:08:49  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/05 00:06:42  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowCutEvent_INCLUDED_
#define _StFlowCutEvent_INCLUDED_
#include <iostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StEvent;

class StFlowCutEvent {

 public:

               StFlowCutEvent();
  virtual      ~StFlowCutEvent();
  static void  SetMult(const Int_t lo, const Int_t hi);
  static void  SetVertexX(const Float_t lo, const Float_t hi);
  static void  SetVertexY(const Float_t lo, const Float_t hi);
  static void  SetVertexZ(const Float_t lo, const Float_t hi);
  static void  SetEtaSym(Float_t lo, Float_t hi);
  static Int_t CheckEvent(StEvent* pEvent);
  static Int_t CheckEtaSymmetry();
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

  static UInt_t  mEtaSymCutN;            // number not accepted
  static Float_t mEtaSymCuts[2];         // range

}; 

inline void StFlowCutEvent::SetMult(const Int_t lo, const Int_t hi) {
  mMultCuts[0] = lo; mMultCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexX(const Float_t lo, const Float_t hi) {
  mVertexXCuts[0] = lo; mVertexXCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexY(const Float_t lo, const Float_t hi) {
  mVertexYCuts[0] = lo; mVertexYCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexZ(const Float_t lo, const Float_t hi) {
  mVertexZCuts[0] = lo; mVertexZCuts[1] = hi; }

inline void StFlowCutEvent::SetEtaSym(Float_t lo, Float_t hi) 
  {mEtaSymCuts[0] = lo; mEtaSymCuts[1] = hi;}

#endif
