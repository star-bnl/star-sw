////////////////////////////////////////////////////////////////////////////
//
// $Id: StuCutEvent.hh,v 1.3 2003/09/02 17:58:09 perev Exp $
//
// Author: Art Poskanzer, LBNL, Dec 1999
//
// Description:  Class for applying event cuts.
//               If lo >= hi no cut is applied.
//               All functions and data members are static.
//               Therefore, no need to instantiate.
//               Just use StuCutEvent::func();
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StuCutEvent.hh,v $
// Revision 1.3  2003/09/02 17:58:09  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1999/12/21 16:12:25  posk
// Updates.
//
// Revision 1.1  1999/12/17 00:07:05  posk
// Classes for StEvent cuts.
//
// Revision 1.0  posk
// First versions of StEvent cut classes.
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StuCutEvent_INCLUDED_
#define _StuCutEvent_INCLUDED_
#include <Stiostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StEvent;

class StuCutEvent {

 public:

               StuCutEvent();
  virtual      ~StuCutEvent();

  static Int_t CheckEvent(StEvent* pEvent);
  static Int_t CheckEtaSymmetry();
  static void  PrintCutList();
  static void  SetMult(const Int_t lo, const Int_t hi);
  static void  SetVertexX(const Float_t lo, const Float_t hi);
  static void  SetVertexY(const Float_t lo, const Float_t hi);
  static void  SetVertexZ(const Float_t lo, const Float_t hi);
  static void  SetEtaSym(Float_t lo, Float_t hi);
  
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

  ClassDef(StuCutEvent,1)               // macro for rootcint
}; 

inline void StuCutEvent::SetMult(const Int_t lo, const Int_t hi) {
  mMultCuts[0] = lo; mMultCuts[1] = hi; }

inline void StuCutEvent::SetVertexX(const Float_t lo, const Float_t hi) {
  mVertexXCuts[0] = lo; mVertexXCuts[1] = hi; }

inline void StuCutEvent::SetVertexY(const Float_t lo, const Float_t hi) {
  mVertexYCuts[0] = lo; mVertexYCuts[1] = hi; }

inline void StuCutEvent::SetVertexZ(const Float_t lo, const Float_t hi) {
  mVertexZCuts[0] = lo; mVertexZCuts[1] = hi; }

inline void StuCutEvent::SetEtaSym(Float_t lo, Float_t hi) {
  mEtaSymCuts[0] = lo; mEtaSymCuts[1] = hi; }

#endif
