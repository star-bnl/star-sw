/***************************************************************************
 *
 * $Id: StVecPtrGlobalTrack.hh,v 1.1 1999/01/15 20:40:22 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrGlobalTrack.hh,v $
 * Revision 1.1  1999/01/15 20:40:22  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.4  1999/03/04 18:17:38  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *

 * add std namespace for Sun CC5 compatibility
 *
 *
#include <vector>
#ifndef StVecPtrGlobalTrack_hh
#define StVecPtrGlobalTrack_hh
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StGlobalTrack;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
#else
typedef vector<StGlobalTrack*> StVecPtrGlobalTrack;
typedef vector<StGlobalTrack*>::iterator StVecPtrGlobalTrackIterator;
#endif

#endif
