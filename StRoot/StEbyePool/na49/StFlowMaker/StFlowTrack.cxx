//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.cxx,v 1.1 2001/02/23 00:51:46 posk Exp $
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
// Revision 1.1  2001/02/23 00:51:46  posk
// NA49 version of STAR software.
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
