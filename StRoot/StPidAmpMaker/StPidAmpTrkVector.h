/***************************************************************************
 *
 * $Id: StPidAmpTrkVector.h,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             vector of StPidAmpTrack
 ***************************************************************************
 *
 * $Log: StPidAmpTrkVector.h,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpTrkVector_h
#define StPidAmpTrkVector_h

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

//#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"
class StPidAmpTrk;


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpTrk*, allocator<StPidAmpTrk*> > StPidAmpTrkVector;
typedef vector<StPidAmpTrk*, allocator<StPidAmpTrk*> >::iterator StPidAmpTrkIter;
#else
typedef vector<StPidAmpTrk*>  StPidAmpTrkVector;
typedef vector<StPidAmpTrk*>::iterator StPidAmpTrkIter;
#endif 

#endif
