////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowSelection.h,v 1.2 2001/05/14 23:04:41 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
// Description:  Class for making selections
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowSelection.h,v $
// Revision 1.2  2001/05/14 23:04:41  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.11  2000/09/22 22:03:01  posk
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowSelection_INCLUDED_
#define _StFlowSelection_INCLUDED_
#include <iostream.h>
#include <string.h>
#include <stdlib.h>
#include "Rtypes.h"
#include "StObject.h"
#include "StFlowConstants.h"
class StFlowTrack;
class StFlowEvent;

class StFlowSelection : public StObject {

 public:

          StFlowSelection();
  virtual ~StFlowSelection();

  Char_t* PidPart();
  Int_t   Sel() const;
  Int_t   Har() const;
  Int_t   Sub() const;
  Bool_t  Select(StFlowEvent*);
  Bool_t  Select(StFlowTrack*);
  Bool_t  SelectPart(StFlowTrack*);
  Float_t PtMaxPart() const;
  void    PrintList() const;
  void    SetPidPart(const Char_t*);
  void    SetPtPart(const Float_t, const Float_t);
  void    SetPPart(const Float_t, const Float_t);
  void    SetEtaPart(const Float_t, const Float_t);
  void    SetYPart(const Float_t, const Float_t);
  void    SetFitPtsPart(const Int_t, const Int_t);
  void    SetFitOverMaxPtsPart(const Float_t, const Float_t);
  void    SetChiSqPart(const Float_t, const Float_t);
  void    SetDcaPart(const Float_t, const Float_t);
  void    SetHarmonic(const Int_t&);
  void    SetSelection(const Int_t&);
  void    SetSubevent(const Int_t&);
  
 private:

  Char_t  mPidPart[10];                      // PID for particles wrt plane
  Float_t mPtPart[2];                        // for parts. wrt plane
  Float_t mPPart[2];                         // for parts. wrt plane
  Float_t mEtaPart[2];                       // for parts. wrt plane
  Float_t mYPart[2];                         // for parts. wrt plane 
  Int_t   mFitPtsPart[2];                    // for parts. wrt plane
  Float_t mFitOverMaxPtsPart[2];             // for parts. wrt plane
  Float_t mChiSqPart[2];                     // for parts. wrt plane
  Float_t mDcaPart[2];                       // for parts. wrt plane

  Int_t   mHarmonic;
  Int_t   mSelection;
  Int_t   mSubevent;

  ClassDef(StFlowSelection,1)               // macro for rootcint
}; 

inline Char_t* StFlowSelection::PidPart() { return mPidPart; }

inline Float_t StFlowSelection::PtMaxPart() const { return mPtPart[1]; }

inline Int_t StFlowSelection::Sel() const { return mSelection; }

inline Int_t StFlowSelection::Har() const { return mHarmonic; }

inline Int_t StFlowSelection::Sub() const { return mSubevent; }

inline void StFlowSelection::SetPidPart(const Char_t* pid)  { 
  strncpy(mPidPart, pid, 9); mPidPart[9] = '\0'; }

inline void StFlowSelection::SetPtPart(Float_t lo, Float_t hi) {
  mPtPart[0] = lo; mPtPart[1] = hi; }

inline void StFlowSelection::SetPPart(Float_t lo, Float_t hi) {
  mPPart[0] = lo; mPPart[1] = hi; }

inline void StFlowSelection::SetEtaPart(Float_t lo, Float_t hi) {
  mEtaPart[0] = lo; mEtaPart[1] = hi; }

inline void StFlowSelection::SetYPart(Float_t lo, Float_t hi) {
  mYPart[0] = lo; mYPart[1] = hi; }

inline void StFlowSelection::SetFitPtsPart(Int_t lo, Int_t hi) {
  mFitPtsPart[0] = lo; mFitPtsPart[1] = hi; }

inline void StFlowSelection::SetFitOverMaxPtsPart(Float_t lo, Float_t hi) {
  mFitOverMaxPtsPart[0] = lo; mFitOverMaxPtsPart[1] = hi; }

inline void StFlowSelection::SetChiSqPart(Float_t lo, Float_t hi) {
  mChiSqPart[0] = lo; mChiSqPart[1] = hi; }

inline void StFlowSelection::SetDcaPart(Float_t lo, Float_t hi) {
  mDcaPart[0] = lo; mDcaPart[1] = hi; }

inline void StFlowSelection::SetHarmonic(const Int_t& harN) {
  if (harN < 0 || harN >= Flow::nHars) {
    cout << "### Harmonic " << harN << " not valid" << endl;
    mHarmonic = 0;
  } else { mHarmonic = harN; } }

inline void StFlowSelection::SetSelection(const Int_t& selN) {
  if (selN < 0 || selN >= Flow::nSels) {
    cout << "### Selection " << selN << " not valid" << endl;
    mSelection = 0;
  } else { mSelection = selN; } }

inline void StFlowSelection::SetSubevent(const Int_t& subN) {
  if (subN < -1 || subN > Flow::nSubs) {
    cout << "### Subevent " << subN << " not valid" << endl;
    mSubevent = -1;
  } else { mSubevent = subN; } }

#endif
