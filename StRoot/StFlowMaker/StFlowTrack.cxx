//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.cxx,v 1.3 2000/06/01 18:26:41 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowEvent 
//   FlowTrack is the main component of StFlowEvent
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.cxx,v $
// Revision 1.3  2000/06/01 18:26:41  posk
// Increased precision of Track integer data members.
//
// Revision 1.2  2000/05/26 21:29:34  posk
// Protected Track data members from overflow.
//
// Revision 1.1  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
//////////////////////////////////////////////////////////////////////

#include "StFlowTrack.h"

ClassImp(StFlowTrack)

Float_t StFlowTrack::maxInt  = 32.;
Float_t StFlowTrack::maxUInt = 6.5;

StFlowTrack::StFlowTrack() : mSelection(0) {
}

StFlowTrack::~StFlowTrack() {
}
