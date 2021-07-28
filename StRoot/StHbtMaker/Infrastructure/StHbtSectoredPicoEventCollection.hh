/***************************************************************************
 *
 * $Id: StHbtSectoredPicoEventCollection.hh,v 1.1 2000/04/12 01:47:13 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A Collection of SectoredPicoEvents is what makes up the EventMixingBuffer
 *   of the SectoredAnalysis
 *
 ***************************************************************************
 *
 * $Log: StHbtSectoredPicoEventCollection.hh,v $
 * Revision 1.1  2000/04/12 01:47:13  willson
 * Initial Installation
 *
 *
 **************************************************************************/

#ifndef StHbtSectoredPicoEventCollection_hh
#define StHbtSectoredPicoEventCollection_hh
#include "StHbtMaker/Infrastructure/StHbtSectoredPicoEvent.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtSectoredPicoEvent*, allocator<StHbtSectoredPicoEvent*> >            StHbtSectoredPicoEventCollection;
typedef list<StHbtSectoredPicoEvent*, allocator<StHbtSectoredPicoEvent*> >::iterator  StHbtSectoredPicoEventIterator;
#else
typedef list<StHbtSectoredPicoEvent*>            StHbtSectoredPicoEventCollection;
typedef list<StHbtSectoredPicoEvent*>::iterator  StHbtSectoredPicoEventIterator;
#endif

#endif
