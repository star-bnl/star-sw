/***************************************************************************
 *
 * $Id: StHbtTrackCutCollection.hh,v 1.1 2000/04/03 16:21:51 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The TrackCutCollection contains pointers to multiple tracks cuts
 *
 ***************************************************************************
 *
 **************************************************************************/

#ifndef StHbtTrackCutCollection_hh
#define StHbtTrackCutCollection_hh

#include "StHbtMaker/Base/StHbtTrackCut.h"

#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtTrackCut*, allocator<StHbtTrackCut*> >            StHbtTrackCutCollection;
typedef list<StHbtTrackCut*, allocator<StHbtTrackCut*> >::iterator  StHbtTrackCutIterator;
#else
typedef list<StHbtTrackCut*>            StHbtTrackCutCollection;
typedef list<StHbtTrackCut*>::iterator  StHbtTrackCutIterator;
#endif

#endif
