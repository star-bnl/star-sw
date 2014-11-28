/***************************************************************************
 *
 * $Id: StHbtXiCollection.hh,v 1.1 2001/09/05 20:41:44 laue Exp $
 *
 * Author: Frank Laue, BNL
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Collection of v0s is the main component of the HbtEvent,
 *   which is essentially the transient microDST
 *
 ***************************************************************************/


#ifndef StHbtXiCollection_hh
#define StHbtXiCollection_hh
#include "StHbtMaker/Infrastructure/StHbtXi.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtXi*, allocator<StHbtXi*> >            StHbtXiCollection;
typedef list<StHbtXi*, allocator<StHbtXi*> >::iterator  StHbtXiIterator;
#else
typedef list<StHbtXi*>            StHbtXiCollection;
typedef list<StHbtXi*>::iterator  StHbtXiIterator;
#endif

#endif

