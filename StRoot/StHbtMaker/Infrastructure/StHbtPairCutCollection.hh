/***************************************************************************
 *
 * $Id: StHbtPairCutCollection.hh,v 1.1 2000/07/31 01:19:26 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  The PairCutCollection is used by the ManyPairCuts PairCut, and holds pointers
 *  to several StHbtPairCuts
 *
 ***************************************************************************
 *
 * $Log: StHbtPairCutCollection.hh,v $
 * Revision 1.1  2000/07/31 01:19:26  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 *
 **************************************************************************/

#ifndef StHbtPairCutCollection_hh
#define StHbtPairCutCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
class StHbtPairCut;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtPairCut*, allocator<StHbtPairCut*> >            StHbtPairCutCollection;
typedef list<StHbtPairCut*, allocator<StHbtPairCut*> >::iterator  StHbtPairCutIterator;
#else
typedef list<StHbtPairCut*>            StHbtPairCutCollection;
typedef list<StHbtPairCut*>::iterator  StHbtPairCutIterator;
#endif

#endif
