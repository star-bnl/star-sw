/***************************************************************************
 *
 * $Id: StPidAmpSliceVector.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpSliceVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpSliceVector_hh
#define StPidAmpSliceVector_hh

#include "StPidAmpMaker/Infrastructure/StPidAmpSlice.hh"

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

//class StPidAmpSlice;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpSlice*, allocator<StPidAmpSlice*> > StPidAmpSliceVector;
typedef vector<StPidAmpSlice*, allocator<StPidAmpSlice*> >::iterator StPidAmpSliceIter;
typedef vector<StPidAmpSlice*, allocator<StPidAmpSlice*> >::const_iterator StPidAmpSliceConstIter;
#else
typedef vector<StPidAmpSlice*>                 StPidAmpSliceVector;
typedef vector<StPidAmpSlice*>::iterator       StPidAmpSliceIter;
typedef vector<StPidAmpSlice*>::const_iterator StPidAmpSliceConstIter; 
#endif

#endif
