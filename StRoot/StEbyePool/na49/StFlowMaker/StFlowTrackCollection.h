//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrackCollection.h,v 1.2 2001/05/14 23:04:53 posk Exp $
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
// $Log: StFlowTrackCollection.h,v $
// Revision 1.2  2001/05/14 23:04:53  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.5  2000/05/26 21:29:35  posk
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowTrackCollection_h
#define StFlowTrackCollection_h
#include "StArray.h"
#include "StFlowTrack.h"

StCollectionDef(FlowTrack)
typedef StSPtrVecFlowTrack StFlowTrackCollection;

#endif

