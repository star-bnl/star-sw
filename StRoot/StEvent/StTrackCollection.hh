/***************************************************************************
 *
 * $Id: StTrackCollection.hh,v 1.3 1999/01/30 23:03:16 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackCollection.hh,v $
 * Revision 1.3  1999/01/30 23:03:16  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.3  1999/01/30 23:03:16  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:54:03  wenaus
 * version with constructors for table-based loading
 *
#ifndef StTrackCollection_hh
#define StTrackCollection_hh
using namespace std;
#include <vector>
class StGlobalTrack;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StGlobalTrack*, allocator<StGlobalTrack*> >            StTrackCollection;
typedef vector<StGlobalTrack*, allocator<StGlobalTrack*> >::iterator  StTrackIterator;
typedef vector<StGlobalTrack*, allocator<StGlobalTrack*> >::const_iterator StTrackConstIterator;
#else
typedef vector<StGlobalTrack*>            StTrackCollection;
typedef vector<StGlobalTrack*>::iterator  StTrackIterator;
typedef vector<StGlobalTrack*>::const_iterator StTrackConstIterator;
#endif

#endif
