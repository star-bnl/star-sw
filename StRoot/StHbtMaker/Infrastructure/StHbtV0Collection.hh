/***************************************************************************
 *
 * $Id: StHbtV0Collection.hh,v 1.1 1999/09/16 18:47:59 lisa Exp $
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
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtV0*, allocator<StHbtV0*> >            StHbtV0Collection;
typedef list<StHbtV0*, allocator<StHbtV0*> >::iterator  StHbtV0Iterator;
#else
typedef list<StHbtV0*>            StHbtV0Collection;
typedef list<StHbtV0*>::iterator  StHbtV0Iterator;
#endif

#endif

