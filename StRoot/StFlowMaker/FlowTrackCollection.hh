/***************************************************************************
 *
 * $Id: FlowTrackCollection.hh,v 1.1 1999/11/04 19:02:09 snelling Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer
 ***************************************************************************
 *
 * Description: part of Flow Framework: 
 *   The Collection of Tracks is the main component of the FlowEvent
 *
 ***************************************************************************
 *
 * $Log: FlowTrackCollection.hh,v $
 * Revision 1.1  1999/11/04 19:02:09  snelling
 * First check in of StFlowMaker. It contains the common code from
 * StFlowTagMaker and StFlowAnalysisMaker.
 *
 *
 **************************************************************************/

#ifndef FlowTrackCollection_hh
#define FlowTrackCollection_hh
#include "FlowTrack.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<FlowTrack*, allocator<FlowTrack*> >            FlowTrackCollection;
typedef list<FlowTrack*, allocator<FlowTrack*> >::iterator  FlowTrackIterator;
#else
typedef list<FlowTrack*>            FlowTrackCollection;
typedef list<FlowTrack*>::iterator  FlowTrackIterator;
#endif

#endif

