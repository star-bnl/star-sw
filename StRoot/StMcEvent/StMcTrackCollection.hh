/***************************************************************************
 *
 * $Id: StMcTrackCollection.hh,v 1.2 1999/09/23 21:25:54 calderon Exp $
 * $Log: StMcTrackCollection.hh,v $
 * Revision 1.2  1999/09/23 21:25:54  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTrackCollection_hh
#define StMcTrackCollection_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StMcTrack;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMcTrack*, allocator<StMcTrack*> >            StMcTrackCollection;
typedef vector<StMcTrack*, allocator<StMcTrack*> >::iterator  StMcTrackIterator;
typedef vector<StMcTrack*, allocator<StMcTrack*> >::const_iterator StMcTrackConstIterator;
#else
typedef vector<StMcTrack*>            StMcTrackCollection;
typedef vector<StMcTrack*>::iterator  StMcTrackIterator;
typedef vector<StMcTrack*>::const_iterator StMcTrackConstIterator;
#endif

#endif
