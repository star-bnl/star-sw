/***************************************************************************
 *
 * $Id: StHbtV0Collection.hh,v 1.2 2000/02/01 00:33:33 laue Exp $
 *
 * Author: Tom Humanic, Ohio State, humanic@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Collection of v0s is the main component of the HbtEvent,
 *   which is essentially the transient microDST
 *
 ***************************************************************************/


#ifndef StHbtV0Collection_hh
#define StHbtV0Collection_hh
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtV0*, allocator<StHbtV0*> >            StHbtV0Collection;
typedef list<StHbtV0*, allocator<StHbtV0*> >::iterator  StHbtV0Iterator;
#else
typedef list<StHbtV0*>            StHbtV0Collection;
typedef list<StHbtV0*>::iterator  StHbtV0Iterator;
#endif

#endif

