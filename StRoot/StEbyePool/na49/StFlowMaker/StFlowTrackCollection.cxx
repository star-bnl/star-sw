//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrackCollection.cxx,v 1.2 2001/05/14 23:04:51 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowEvent 
//   The Collection of Tracks is the main component of StFlowEvent
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrackCollection.cxx,v $
// Revision 1.2  2001/05/14 23:04:51  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.1  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
//////////////////////////////////////////////////////////////////////
#include "StFlowTrack.h"
#include "StFlowTrackCollection.h"

StCollectionImp(FlowTrack)

