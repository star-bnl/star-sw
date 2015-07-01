/***************************************************************************
 *
 * $Id: StBTofHit.cxx,v 2.6 2012/10/23 20:16:32 fisyak Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBTofHit.cxx,v $
 * Revision 2.6  2012/10/23 20:16:32  fisyak
 * Add print out
 *
 * Revision 2.5  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.4  2011/10/17 15:37:04  fisyak
 * One line print out
 *
 * Revision 2.3  2009/03/04 04:36:58  ullrich
 * Added missing check for valid pointer to operator<<
 *
 * Revision 2.2  2009/01/15 00:46:25  ullrich
 * tray() now returns int.
 *
 * Revision 2.1  2008/12/22 20:30:57  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include "StBTofHit.h"
#include "StTrack.h"
#include "TString.h"
const Float_t StBTofHit::mBTofPadWidth = 3.45; 
ClassImp(StBTofHit);
//________________________________________________________________________________
StBTofHit::StBTofHit() {
  mTray             = 0;
  mModule           = 0;
  mCell             = 0;
  mLeadingEdgeTime  = 0.;
  mTrailingEdgeTime = 0.;
  mAssociatedTrack  = 0;
}
//________________________________________________________________________________
const StTrack* StBTofHit::associatedTrack() const { return mAssociatedTrack; }
      StTrack* StBTofHit::associatedTrack()       { return mAssociatedTrack; }
//________________________________________________________________________________
void
StBTofHit::setAssociatedTrack(StTrack* val) { mAssociatedTrack = val; }
//________________________________________________________________________________
const StThreeVectorF& StBTofHit::position() const {
  static StThreeVectorF pos;
  pos.set(0.,mBTofPadWidth*(cell() - 3.5), 0.);
  return *&pos;
}
//________________________________________________________________________________
ostream& operator<<(ostream &os, const StBTofHit& hit) {
  os << Form("Tray:%3i",hit.tray()) 
     << Form(" Module:%2i",hit.module())
     << Form(" Cell:%2i",hit.cell())
     << Form(" LeTime %7.2f",hit.leadingEdgeTime())
     << Form(" TeTime %7.2f",hit.trailingEdgeTime())
     << Form(" Track %5i",(hit.associatedTrack() ? hit.associatedTrack()->key() : 0))
     << Form(" IdTruth %5i",hit.idTruth())
     << Form(" Quality%3i",hit.qaTruth());
  return os;
}
//________________________________________________________________________________
