/***************************************************************************
 *
 * $Id: StVertexCollection.hh,v 1.5 1999/05/22 18:34:37 perev Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertexCollection.hh,v $
 * Revision 1.5  1999/05/22 18:34:37  perev
 * Lists replaced by vectors
 *
 * Revision 1.4  1999/03/04 18:17:49  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:11  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:23  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVertexCollection_hh
#define StVertexCollection_hh
#include "StEvent/StVertex.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StVertex*, allocator<StVertex*> >            StVertexCollection;
typedef vector<StVertex*, allocator<StVertex*> >::iterator  StVertexIterator;
#else
typedef vector<StVertex*>            StVertexCollection;
typedef vector<StVertex*>::iterator  StVertexIterator;
#endif

#endif
