/***************************************************************************
 *
 * $Id: StHbtV0TrackCollection.hh,v 1.1 1999/07/21 21:14:32 hardtke Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Collection of Tracks is the main component of the HbtEvent,
 *   which is essentially the transient microDST
 *
 ***************************************************************************/


#ifndef StHbtV0TrackCollection_hh
#define StHbtV0TrackCollection_hh
#include "StHbtMaker/Infrastructure/StHbtV0Track.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtV0Track*, allocator<StHbtV0Track*> >            StHbtV0TrackCollection;
typedef list<StHbtV0Track*, allocator<StHbtV0Track*> >::iterator  StHbtV0TrackIterator;
#else
typedef list<StHbtV0Track*>            StHbtV0TrackCollection;
typedef list<StHbtV0Track*>::iterator  StHbtV0TrackIterator;
#endif

#endif

