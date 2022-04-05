/***************************************************************************
 *
 * $Id: StHbtKinkCollection.hh,v 1.1 2001/05/25 23:23:59 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Collection of Kinks is the a component of the HbtEvent,
 *   which is essentially the transient microDST
 *
 ****************************************************************************
 *
 * $Log: StHbtKinkCollection.hh,v $
 * Revision 1.1  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 *
 ***************************************************************************/


#ifndef StHbtKinkCollection_hh
#define StHbtKinkCollection_hh
#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtKink*, allocator<StHbtKink*> >            StHbtKinkCollection;
typedef list<StHbtKink*, allocator<StHbtKink*> >::iterator  StHbtKinkIterator;
#else
typedef list<StHbtKink*>            StHbtKinkCollection;
typedef list<StHbtKink*>::iterator  StHbtKinkIterator;
#endif

#endif

