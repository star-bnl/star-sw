/***************************************************************************
 *
 * $Id: StVecZdcSegment.hh,v 1.2 1999/01/15 22:54:21 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecZdcSegment.hh,v $
 * Revision 1.2  1999/01/15 22:54:21  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.3  1999/03/04 15:57:11  wenaus
 * add std namespace for Sun CC5 compatibility
 *

 * version with constructors for table-based loading
 *
#define StVecZdcSegment_hh
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StZdcSegment;
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StZdcSegment, allocator<StZdcSegment> > StVecZdcSegment;
#else
typedef vector<StZdcSegment> StVecZdcSegment;
#endif

#endif
