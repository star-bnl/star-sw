/***************************************************************************
 *
 * $Id: StVecMwcSector.hh,v 1.2 1999/01/15 22:54:17 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecMwcSector.hh,v $
 * Revision 1.2  1999/01/15 22:54:17  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.3  1999/03/04 15:57:07  wenaus
 * add std namespace for Sun CC5 compatibility
 *

 * version with constructors for table-based loading
 *
#define StVecMwcSector_hh
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StMwcSector;
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMwcSector, allocator<StMwcSector> > StVecMwcSector;
#else
typedef vector<StMwcSector> StVecMwcSector;
#endif

#endif
