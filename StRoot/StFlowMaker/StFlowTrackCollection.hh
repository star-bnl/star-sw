//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrackCollection.hh,v 1.2 1999/11/30 18:52:56 snelling Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowEvent 
//   The Collection of Tracks is the main component of StFlowEvent
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrackCollection.hh,v $
// Revision 1.2  1999/11/30 18:52:56  snelling
// First modification for the new StEvent
//
// Revision 1.1  1999/11/11 23:09:00  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:09  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowTrackCollection_hh
#define StFlowTrackCollection_hh
#include "StFlowTrack.hh"
#include <vector>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StFlowTrack*, allocator<StFlowTrack*> > StFlowTrackCollection;
typedef vector<StFlowTrack*, allocator<StFlowTrack*> >::iterator StFlowTrackIterator;
#else
typedef vector<StFlowTrack*>            StFlowTrackCollection;
typedef vector<StFlowTrack*>::iterator  StFlowTrackIterator;
#endif

#endif

