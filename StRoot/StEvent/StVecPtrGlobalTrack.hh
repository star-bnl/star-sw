/***************************************************************************
 *
 * $Id: StVecPtrGlobalTrack.hh,v 1.5 1999/03/10 12:12:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrGlobalTrack.hh,v $
 * Revision 1.5  1999/03/10 12:12:15  ullrich
 * Added iterators
 *
 * Revision 1.5  1999/03/10 12:12:15  ullrich
 * Added iterators
 *
 * Revision 1.4  1999/03/04 18:17:38  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:08  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:18  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVecPtrGlobalTrack_hh
#define StVecPtrGlobalTrack_hh
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StGlobalTrack;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StGlobalTrack*, allocator<StGlobalTrack*> > StVecPtrGlobalTrack;
#else
typedef vector<StGlobalTrack*> StVecPtrGlobalTrack;
typedef vector<StGlobalTrack*>::iterator StVecPtrGlobalTrackIterator;
#endif

#endif
