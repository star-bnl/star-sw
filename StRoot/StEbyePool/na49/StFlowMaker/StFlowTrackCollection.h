//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrackCollection.h,v 1.1 2001/02/23 00:52:05 posk Exp $
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
// Revision 1.1  2001/02/23 00:52:05  posk
// NA49 version of STAR software.
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

