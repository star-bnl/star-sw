//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.cxx,v 1.2 2001/05/14 23:04:45 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowEvent 
//   FlowTrack is the main component of StFlowEvent
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.cxx,v $
// Revision 1.2  2001/05/14 23:04:45  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.4  2000/09/05 16:11:39  snelling
//
//////////////////////////////////////////////////////////////////////

#include "StFlowTrack.h"

ClassImp(StFlowTrack)

StFlowTrack::StFlowTrack() : mSelection(0) {
}

StFlowTrack::~StFlowTrack() {
}
