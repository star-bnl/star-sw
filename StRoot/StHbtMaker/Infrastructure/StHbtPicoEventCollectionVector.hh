/***************************************************************************
 *
 * $Id: StHbtPicoEventCollectionVector.hh,v 1.1 2000/07/16 21:44:11 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEventCollectionVector.hh,v $
 * Revision 1.1  2000/07/16 21:44:11  laue
 * Collection and analysis for vertex dependent event mixing
 *
 *
 **************************************************************************/

#ifndef StHbtPicoEventCollectionVector_hh
#define StHbtPicoEventCollectionVector_hh
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"
#include <vector>
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::vector;
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StHbtPicoEventCollection*, allocator<StHbtPicoEventCollection*> >            StHbtPicoEventCollectionVector;  //!
typedef vector<StHbtPicoEventCollection*, allocator<StHbtPicoEventCollection*> >::iterator  StHbtPicoEventCollectionIterator;//!
#else
typedef vector<StHbtPicoEventCollection*>            StHbtPicoEventCollectionVector;//!
typedef vector<StHbtPicoEventCollection*>::iterator  StHbtPicoEventCollectionIterator;//!
#endif

#endif
