/***************************************************************************
 *
 * $Id: StVecMwcSector.hh,v 1.5 1999/03/10 12:12:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecMwcSector.hh,v $
 * Revision 1.5  1999/03/10 12:12:13  ullrich
 * Added iterators
 *
 * Revision 1.4  1999/03/04 18:17:34  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:07  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:17  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVecMwcSector_hh
#define StVecMwcSector_hh
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StMwcSector;
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMwcSector, allocator<StMwcSector> > StVecMwcSector;
typedef vector<StMwcSector, allocator<StMwcSector> >::iterator StVecMwcSectorIterator;
#else
typedef vector<StMwcSector> StVecMwcSector;
typedef vector<StMwcSector>::iterator StVecMwcSectorIterator;
#endif

#endif
