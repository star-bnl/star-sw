/***************************************************************************
 *
 * $Id: StHbtEventCutCollection.hh,v 1.1 2001/07/20 20:03:53 rcwells Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  The EventCutCollection is used by the ManyEventCuts EventCut, and holds pointers
 *  to several StHbtEventCuts
 *
 ***************************************************************************
 *
 * $Log: StHbtEventCutCollection.hh,v $
 * Revision 1.1  2001/07/20 20:03:53  rcwells
 * Added pT weighting and moved event angle cal. to event cut
 *
 *
 **************************************************************************/

#ifndef StHbtEventCutCollection_hh
#define StHbtEventCutCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
class StHbtEventCut;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtEventCut*, allocator<StHbtEventCut*> >            StHbtEventCutCollection;
typedef list<StHbtEventCut*, allocator<StHbtEventCut*> >::iterator  StHbtEventCutIterator;
#else
typedef list<StHbtEventCut*>            StHbtEventCutCollection;
typedef list<StHbtEventCut*>::iterator  StHbtEventCutIterator;
#endif

#endif
