/***************************************************************************
 *
 * $Id: StPidAmpCutVector.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpCutVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpCutVector_hh
#define StPidAmpCutVector_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#include "StPidAmpMaker/Infrastructure/StPidAmpCut.hh"

//class StPidAmpCut;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpCut, allocator<StPidAmpCut> > StPidAmpCutVector;
typedef vector<StPidAmpCut, allocator<StPidAmpCut> >::iterator StPidAmpCutIter;
typedef vector<StPidAmpCut, allocator<StPidAmpCut> >::const_iterator StPidAmpCutConstIter;
#else
typedef vector<StPidAmpCut>          StPidAmpCutVector;
typedef vector<StPidAmpCut>::iterator StPidAmpCutIter;
typedef vector<StPidAmpCut>::const_iterator StPidAmpCutConstIter;
#endif

#endif

//if use vector of obj instead of vector of pointers,
//the compiler would create a /.sun4x_56/obj/StRoot/StPidAmpMaker/Templates.DB
//interesting.

//also if it is a vector of obj instead of pointers, 
//need to the line"#include "..../obj.hh""

//if it is just a vector of pointers
//the line "class StPidAmpCut " is enough.
