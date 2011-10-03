/***************************************************************************
 *
 * $Id: StHbtAnalysisCollection.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  The AnalysisCollection is pointed to by the Manager, and holds pointers
 *  to all Analysis objects currently active
 *
 ***************************************************************************
 *
 * $Log: StHbtAnalysisCollection.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtAnalysisCollection_hh
#define StHbtAnalysisCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StHbtAnalysis;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtAnalysis*, allocator<StHbtAnalysis*> >            StHbtAnalysisCollection;
typedef list<StHbtAnalysis*, allocator<StHbtAnalysis*> >::iterator  StHbtAnalysisIterator;
#else
typedef list<StHbtAnalysis*>            StHbtAnalysisCollection;
typedef list<StHbtAnalysis*>::iterator  StHbtAnalysisIterator;
#endif

#endif
