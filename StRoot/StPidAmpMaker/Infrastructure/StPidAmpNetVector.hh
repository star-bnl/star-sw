/***************************************************************************
 *
 * $Id: StPidAmpNetVector.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpNetVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpNetVector_hh
#define StPidAmpNetVector_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StPidAmpNet;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpNet*, allocator<StPidAmpNet*> >  StPidAmpNetVector;
typedef vector<StPidAmpNet*, allocator<StPidAmpNet*> >::iterator StPidAmpNetIter;
typedef vector<StPidAmpNet*, allocator<StPidAmpNet*> >::const_iterator StPidAmpNetConstIter;
#else
typedef vector<StPidAmpNet*>  StPidAmpNetVector;
typedef vector<StPidAmpNet*>::iterator StPidAmpNetIter;
typedef vector<StPidAmpNet*>::const_iterator StPidAmpNetConstIter;
#endif 

#endif
