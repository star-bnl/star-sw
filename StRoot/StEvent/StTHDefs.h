/***************************************************************************
 *
 * $Id: StTHDefs.h,v 1.1 1999/01/30 03:58:08 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTHDefs.h,v $
 * Revision 1.1  1999/01/30 03:58:08  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/03/04 15:57:03  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:53:57  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifdef __ROOT__
#include "TList.h"
typedef TList* StVecTH1F;
typedef TList* StVecTH2F;
#else
#define StTHDefs_hh

#ifndef __ROOT__
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<TH1F, allocator<TH1F> > StVecTH1F;
typedef vector<TH2F, allocator<TH2F> > StVecTH2F;
typedef vector<TH1F> StVecTH1F;
#endif
#endif
#endif

#endif
