/***************************************************************************
 *
 * $Id: StHbtTrackCollection.hh,v 1.2 2000/02/01 00:33:32 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Collection of Tracks is the main component of the HbtEvent,
 *   which is essentially the transient microDST
 *
 ***************************************************************************
 *
 * $Log: StHbtTrackCollection.hh,v $
 * Revision 1.2  2000/02/01 00:33:32  laue
 * namespaces changed to run on the new Solaris Compiler CC5
 * since we can use member templates in franks1Histo.hh we are doing it
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtTrackCollection_hh
#define StHbtTrackCollection_hh
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtTrack*, allocator<StHbtTrack*> >            StHbtTrackCollection;
typedef list<StHbtTrack*, allocator<StHbtTrack*> >::iterator  StHbtTrackIterator;
#else
typedef list<StHbtTrack*>            StHbtTrackCollection;
typedef list<StHbtTrack*>::iterator  StHbtTrackIterator;
#endif

#endif
