/***************************************************************************
 *
 * $Id: StHbtCorrFctnCollection.hh,v 1.2 2000/02/01 00:33:32 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The CorrFctnCollection contains pointers to all Correlation Functions
 *   that are associated with a particular Analysis object.
 *
 ***************************************************************************
 *
 * $Log: StHbtCorrFctnCollection.hh,v $
 * Revision 1.2  2000/02/01 00:33:32  laue
 * namespaces changed to run on the new Solaris Compiler CC5
 * since we can use member templates in franks1Histo.hh we are doing it
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtCorrFctnCollection_hh
#define StHbtCorrFctnCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
class StHbtCorrFctn;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtCorrFctn*, allocator<StHbtCorrFctn*> >            StHbtCorrFctnCollection;
typedef list<StHbtCorrFctn*, allocator<StHbtCorrFctn*> >::iterator  StHbtCorrFctnIterator;
#else
typedef list<StHbtCorrFctn*>            StHbtCorrFctnCollection;
typedef list<StHbtCorrFctn*>::iterator  StHbtCorrFctnIterator;
#endif

#endif
