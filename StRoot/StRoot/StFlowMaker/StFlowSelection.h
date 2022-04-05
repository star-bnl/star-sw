////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowSelection.h,v 1.19 2010/03/05 16:49:46 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Mar 2000
//
// Description:  Class for making selections
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowSelection_INCLUDED_
#define _StFlowSelection_INCLUDED_
#include <Stiostream.h>
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
  Int_t   PtBinsPart() const;
  void    PrintList() const;
  void    SetPidPart(const Char_t*);
  void    SetPtPart(const Float_t, const Float_t);
  void    SetPtBinsPart(const Int_t);
  void    SetPPart(const Float_t, const Float_t);
  void    SetEtaPart(const Float_t, const Float_t);
  void    SetYPart(const Float_t, const Float_t);
  void    SetFitPtsPart(const Int_t, const Int_t);
  void    SetDedxPtsPart(const Int_t, const Int_t);
  void    SetFitOverMaxPtsPart(const Float_t, const Float_t);
  void    SetChiSqPart(const Float_t, const Float_t);
  void    SetDcaGlobalPart(const Float_t, const Float_t);
  void    SetHarmonic(const Int_t&);
  void    SetSelection(const Int_t&);
  void    SetSubevent(const Int_t&);
  
 private:

  // h+, h-, pi-, pi+, pi, k+, k-, k, pr+, pr-, pr, d+, d-, d, e+, e-, e
  Char_t  mPidPart[10];                      // PID for particles wrt plane
  Float_t mPtPart[2];                        // for parts. wrt plane
  Int_t   mPtBinsPart;                       // for parts. wrt plane
  Float_t mPPart[2];                         // for parts. wrt plane
  Float_t mEtaPart[2];                       // for parts. wrt plane
  Float_t mYPart[2];                         // for parts. wrt plane 
  Int_t   mFitPtsPart[2];                    // for parts. wrt plane
  Int_t   mDedxPtsPart[2];                   // for parts. wrt plane
  Float_t mFitOverMaxPtsPart[2];             // for parts. wrt plane
  Float_t mChiSqPart[2];                     // for parts. wrt plane
  Float_t mDcaGlobalPart[2];                 // for parts. wrt plane

  Int_t   mHarmonic;
  Int_t   mSelection;
  Int_t   mSubevent;

  ClassDef(StFlowSelection,1)               // macro for rootcint
}; 

inline Char_t* StFlowSelection::PidPart() { return mPidPart; }

inline Float_t StFlowSelection::PtMaxPart() const { return mPtPart[1]; }

inline Int_t StFlowSelection::PtBinsPart() const { return mPtBinsPart; }

inline Int_t StFlowSelection::Sel() const { return mSelection; }

inline Int_t StFlowSelection::Har() const { return mHarmonic; }

inline Int_t StFlowSelection::Sub() const { return mSubevent; }

inline void StFlowSelection::SetPidPart(const Char_t* pid)  { 
  strncpy(mPidPart, pid, 9); mPidPart[9] = '\0'; }

inline void StFlowSelection::SetPtPart(const Float_t lo, const Float_t hi) {
  mPtPart[0] = lo; mPtPart[1] = hi; }

inline void StFlowSelection::SetPtBinsPart(const Int_t bins) {
  mPtBinsPart = bins; }

inline void StFlowSelection::SetPPart(const Float_t lo, const Float_t hi) {
  mPPart[0] = lo; mPPart[1] = hi; }

inline void StFlowSelection::SetEtaPart(const Float_t lo, const Float_t hi) {
  mEtaPart[0] = lo; mEtaPart[1] = hi; }

inline void StFlowSelection::SetYPart(const Float_t lo, const Float_t hi) {
  mYPart[0] = lo; mYPart[1] = hi; }

inline void StFlowSelection::SetFitPtsPart(const Int_t lo, const Int_t hi) {
  mFitPtsPart[0] = lo; mFitPtsPart[1] = hi; }

inline void StFlowSelection::SetDedxPtsPart(const Int_t lo, const Int_t hi) {
  mDedxPtsPart[0] = lo; mDedxPtsPart[1] = hi; }

inline void StFlowSelection::SetFitOverMaxPtsPart(const Float_t lo, const Float_t hi) {
  mFitOverMaxPtsPart[0] = lo; mFitOverMaxPtsPart[1] = hi; }

inline void StFlowSelection::SetChiSqPart(const Float_t lo, const Float_t hi) {
  mChiSqPart[0] = lo; mChiSqPart[1] = hi; }

inline void StFlowSelection::SetDcaGlobalPart(const Float_t lo, const Float_t hi) {
  mDcaGlobalPart[0] = lo; mDcaGlobalPart[1] = hi; }

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

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowSelection.h,v $
// Revision 1.19  2010/03/05 16:49:46  posk
// Compatable with ROOT 5.22
//
// Revision 1.18  2003/09/02 17:58:12  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.17  2003/05/15 06:08:41  aihong
// default PID is changed from none to NA, SetDedxPtsPart() added
//
// Revision 1.16  2002/01/31 01:04:55  posk
// *** empty log message ***
//
// Revision 1.15  2001/12/18 19:22:38  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.14  2001/11/09 21:11:00  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.13  2001/05/22 20:18:01  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.12  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
//
// Revision 1.10  2000/09/16 22:20:34  snelling
// Added selection on P and global DCA and fixed rapidity calulation
//
// Revision 1.9  2000/09/15 22:51:34  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.8  2000/09/15 01:20:03  snelling
// Added methods for P and Y and added selection on Y
//
// Revision 1.7  2000/09/13 00:32:27  snelling
// Added selections for particles correlated with reaction plane
//
// Revision 1.6  2000/08/31 18:58:27  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.5  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.4  2000/05/26 21:29:33  posk
// Protected Track data members from overflow.
//
// Revision 1.3  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
// Revision 1.2  2000/03/28 23:21:04  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.1  2000/03/15 23:28:54  posk
// Added StFlowSelection.
//
////////////////////////////////////////////////////////////////////////////
