////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowSelection.h,v 1.1 2000/03/15 23:28:54 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Mar 2000
//
// Description:  Class for making selections
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowSelection.h,v $
// Revision 1.1  2000/03/15 23:28:54  posk
// Added StFlowSelection.
//
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowSelection_INCLUDED_
#define _StFlowSelection_INCLUDED_
#include <iostream.h>
#include <string.h>
#include <stdlib.h>
#include "Rtypes.h"
#include "StFlowConstants.h"
class StFlowTrack;
class StFlowEvent;

class StFlowSelection {

 public:

          StFlowSelection();
  virtual ~StFlowSelection();

  UInt_t  Number() const;
  UInt_t   Centrality() const;
  Char_t* Pid();
  Char_t* PidPart();
  Int_t   Sel() const;
  Int_t   Har() const;
  Int_t   Sub() const;
  Int_t   Select(StFlowEvent*);
  Int_t   Select(StFlowTrack*);
  Int_t   SelectPart(StFlowTrack*);
  void    SetNumber(const UInt_t&);
  void    SetCentrality(const UInt_t&);
  void    SetPid(const Char_t*);
  void    SetPidPart(const Char_t*);
  void    SetHarmonic(const Int_t&);
  void    SetSelection(const Int_t&);
  void    SetSubevent(const Int_t&);
  
 private:

  UInt_t mNumber;                           // selection number
  UInt_t mCentrality;                       // centrality bin
  Char_t mPid[10];                          // "pi-", "pi+", or "proton"
  Char_t mPidPart[10];                      // PID for particles wrt plane
  Int_t  mHarmonic;
  Int_t  mSelection;
  Int_t  mSubevent;

  ClassDef(StFlowSelection,1)               // macro for rootcint
}; 

inline UInt_t StFlowSelection::Number() const { return mNumber; }

inline UInt_t StFlowSelection::Centrality() const { return mCentrality; }

inline Char_t* StFlowSelection::Pid() { return mPid; }

inline Char_t* StFlowSelection::PidPart() { return mPidPart; }

inline Int_t StFlowSelection::Sel() const { return mSelection; }

inline Int_t StFlowSelection::Har() const { return mHarmonic; }

inline Int_t StFlowSelection::Sub() const { return mSubevent; }

inline void StFlowSelection::SetNumber(const UInt_t& number) {
 mNumber = number; }

inline void StFlowSelection::SetCentrality(const UInt_t& cent) {
 mCentrality = cent; }

inline void StFlowSelection::SetPid(const Char_t* pid)  { 
  strncpy(mPid, pid, 9); mPid[9] = '\0'; }

inline void StFlowSelection::SetPidPart(const Char_t* pid)  { 
  strncpy(mPidPart, pid, 9); mPidPart[9] = '\0'; }

inline void StFlowSelection::SetHarmonic(const Int_t& harN) {
  if (harN < 0 || harN >= Flow::nHars) {
    cout << "### Harmonic " << harN << " not valid" << endl;
    mHarmonic = 0;
    return;
  }
  mHarmonic = harN; }

inline void StFlowSelection::SetSelection(const Int_t& selN) {
  if (selN < 0 || selN >= Flow::nSels) {
    cout << "### Selection " << selN << " not valid" << endl;
    mSelection = 0;
    return;
  }
  mSelection = selN; }

inline void StFlowSelection::SetSubevent(const Int_t& subN) {
  if (subN < -1 || subN > Flow::nSubs) {
    cout << "### Subevent " << subN << " not valid" << endl;
    mSubevent = -1;
    return;
  }
  mSubevent = subN; }


#endif
