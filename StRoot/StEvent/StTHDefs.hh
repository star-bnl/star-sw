/***************************************************************************
 *
 * $Id: StTHDefs.hh,v 1.2 1999/01/15 22:53:57 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTHDefs.hh,v $
 * Revision 1.2  1999/01/15 22:53:57  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:53:57  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#define StTHDefs_hh

using namespace std;
#include <vector>
#include "TH1.h"
#include "TH2.h"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<TH1F, allocator<TH1F> > StVecTH1F;
typedef vector<TH2F, allocator<TH2F> > StVecTH2F;
#else
typedef vector<TH1F> StVecTH1F;
typedef vector<TH2F> StVecTH2F;
#endif

#endif
