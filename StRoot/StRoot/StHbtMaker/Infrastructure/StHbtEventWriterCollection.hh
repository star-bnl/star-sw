/***************************************************************************
 *
 * $Id: StHbtEventWriterCollection.hh,v 1.1 2000/02/18 21:32:23 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  The EventWriterCollection is pointed to by the Manager, and holds pointers
 *  to all EventWriter objects currently active
 *
 ***************************************************************************
 *
 **************************************************************************/

#ifndef StHbtEventWriterCollection_hh
#define StHbtEventWriterCollection_hh

#include "StHbtMaker/Base/StHbtEventWriter.hh"

#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtEventWriter*, allocator<StHbtEventWriter*> >            StHbtEventWriterCollection;
typedef list<StHbtEventWriter*, allocator<StHbtEventWriter*> >::iterator  StHbtEventWriterIterator;
#else
typedef list<StHbtEventWriter*>            StHbtEventWriterCollection;
typedef list<StHbtEventWriter*>::iterator  StHbtEventWriterIterator;
#endif

#endif
