/***************************************************************************
 *
 * $Id: StHbtPicoEventCollection.hh,v 1.2 2000/02/01 00:33:32 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A Collection of PicoEvents is what makes up the EventMixingBuffer
 *   of each Analysis
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEventCollection.hh,v $
 * Revision 1.2  2000/02/01 00:33:32  laue
 * namespaces changed to run on the new Solaris Compiler CC5
 * since we can use member templates in franks1Histo.hh we are doing it
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtPicoEventCollection_hh
#define StHbtPicoEventCollection_hh
#include "StHbtMaker/Infrastructure/StHbtPicoEvent.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtPicoEvent*, allocator<StHbtPicoEvent*> >            StHbtPicoEventCollection;
typedef list<StHbtPicoEvent*, allocator<StHbtPicoEvent*> >::iterator  StHbtPicoEventIterator;
#else
typedef list<StHbtPicoEvent*>            StHbtPicoEventCollection;
typedef list<StHbtPicoEvent*>::iterator  StHbtPicoEventIterator;
#endif

#endif
