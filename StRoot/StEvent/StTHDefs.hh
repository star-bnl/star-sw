/***************************************************************************
 *
 * $Id: StTHDefs.hh,v 1.4 1999/03/04 18:17:23 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTHDefs.hh,v $
 * Revision 1.4  1999/03/04 18:17:23  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:03  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:53:57  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTHDefs_hh
#define StTHDefs_hh

#include <vector>
#include "TH1.h"
#include "TH2.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<TH1F, allocator<TH1F> > StVecTH1F;
typedef vector<TH2F, allocator<TH2F> > StVecTH2F;
#else
typedef vector<TH1F> StVecTH1F;
typedef vector<TH2F> StVecTH2F;
#endif

#endif
