/***************************************************************************
 *
 * $Id: StMcVertexCollection.hh,v 1.2 1999/09/23 21:25:55 calderon Exp $
 * $Log: StMcVertexCollection.hh,v $
 * Revision 1.2  1999/09/23 21:25:55  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcVertexCollection_hh
#define StMcVertexCollection_hh
#include <list>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcVertex;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StMcVertex*, allocator<StMcVertex*> >            StMcVertexCollection;
typedef list<StMcVertex*, allocator<StMcVertex*> >::iterator  StMcVertexIterator;
#else
typedef list<StMcVertex*>            StMcVertexCollection;
typedef list<StMcVertex*>::iterator  StMcVertexIterator;
#endif

#endif
