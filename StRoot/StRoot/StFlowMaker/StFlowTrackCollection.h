//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrackCollection.h,v 1.8 2002/05/23 18:54:14 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowEvent 
//   The Collection of Tracks is the main component of StFlowEvent
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowTrackCollection_h
#define StFlowTrackCollection_h
#include "StArray.h"
#include "StFlowTrack.h"

StCollectionDef(FlowTrack)
typedef StSPtrVecFlowTrack StFlowTrackCollection;
//typedef StFlowTrack**      StFlowTrackIterator;

#endif

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrackCollection.h,v $
// Revision 1.8  2002/05/23 18:54:14  posk
// Moved centrality cuts into StFlowConstants
//
// Revision 1.7  2001/05/22 20:18:08  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.6  2000/12/12 20:22:06  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.5  2000/05/26 21:29:35  posk
// Protected Track data members from overflow.
//
// Revision 1.4  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
// Revision 1.2  2000/03/15 23:28:55  posk
// Added StFlowSelection.
//
// Revision 1.1  2000/03/02 23:03:00  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.2  1999/11/30 18:52:56  snelling
// First modification for the new StEvent
//
// Revision 1.1  1999/11/04 19:02:09  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////
