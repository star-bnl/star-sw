/***************************************************************************
 *
 * $Id: StVertexCollection.hh,v 1.2 1999/01/15 22:54:23 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertexCollection.hh,v $
 * Revision 1.2  1999/01/15 22:54:23  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:54:23  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#define StVertexCollection_hh
#include "StEvent/StVertex.hh"
using namespace std;
#include <list>

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StVertex*, allocator<StVertex*> >            StVertexCollection;
typedef list<StVertex*, allocator<StVertex*> >::iterator  StVertexIterator;
#else
typedef list<StVertex*>            StVertexCollection;
typedef list<StVertex*>::iterator  StVertexIterator;
#endif

#endif
