//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrackCollection.hh,v 1.1 1999/11/11 23:09:00 posk Exp $
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
#include <list>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StFlowTrack*, allocator<StFlowTrack*> > StFlowTrackCollection;
typedef list<StFlowTrack*, allocator<StFlowTrack*> >::iterator StFlowTrackIterator;
#else
typedef list<StFlowTrack*>            StFlowTrackCollection;
typedef list<StFlowTrack*>::iterator  StFlowTrackIterator;
#endif

#endif

