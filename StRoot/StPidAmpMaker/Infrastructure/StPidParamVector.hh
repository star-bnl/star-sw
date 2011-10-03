/***************************************************************************
 *
 * $Id: StPidParamVector.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidParamVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidParamVector_hh
#define StPidParamVector_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<double, allocator<double> >  StPidParamVector;
typedef vector<double, allocator<double> >::iterator StPidParamIter;
typedef vector<double, allocator<double> >::const_iterator StPidParamConstIter;
#else
typedef vector<double> StPidParamVector;
typedef vector<double>::iterator StPidParamIter;
typedef vector<double>::const_iterator StPidParamConstIter;
#endif

#endif
