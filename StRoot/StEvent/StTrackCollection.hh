/***************************************************************************
 *
 * $Id: StTrackCollection.hh,v 1.1 1999/01/15 20:40:12 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackCollection.hh,v $
 * Revision 1.1  1999/01/15 20:40:12  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:54:03  wenaus
 * version with constructors for table-based loading
 *
#include <list>
#define StTrackCollection_hh
using namespace std;
#include <vector>
typedef list<StGlobalTrack*, allocator<StGlobalTrack*> >            StTrackCollection;
typedef list<StGlobalTrack*, allocator<StGlobalTrack*> >::iterator  StTrackIterator;
typedef list<StGlobalTrack*, allocator<StGlobalTrack*> >::const_iterator StTrackConstIterator;
typedef vector<StGlobalTrack*, allocator<StGlobalTrack*> >            StTrackCollection;
typedef list<StGlobalTrack*>            StTrackCollection;
typedef list<StGlobalTrack*>::iterator  StTrackIterator;
typedef list<StGlobalTrack*>::const_iterator StTrackConstIterator;
typedef vector<StGlobalTrack*>            StTrackCollection;
typedef vector<StGlobalTrack*>::iterator  StTrackIterator;
typedef vector<StGlobalTrack*>::const_iterator StTrackConstIterator;
#endif

#endif
