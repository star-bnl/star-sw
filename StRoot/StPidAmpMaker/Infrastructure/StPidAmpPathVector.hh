/***************************************************************************
 *
 * $Id: StPidAmpPathVector.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpPathVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpPathVector_hh
#define StPidAmpPathVector_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StPidAmpPath;


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpPath*, allocator<StPidAmpPath*> > StPidAmpPathVector;
typedef vector<StPidAmpPath*, allocator<StPidAmpPath*> >::iterator StPidAmpPathIter;
typedef vector<StPidAmpPath*, allocator<StPidAmpPath*> >::const_iterator StPidAmpPathConstIter;
#else
typedef vector<StPidAmpPath*>                 StPidAmpPathVector;
typedef vector<StPidAmpPath*>::iterator       StPidAmpPathIter;
typedef vector<StPidAmpPath*>::const_iterator StPidAmpPathConstIter;
#endif

#endif
