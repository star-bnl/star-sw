/***************************************************************************
 *
 * $Id: StPidAmpChannelVector.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpChannelVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpChannelVector_hh
#define StPidAmpChannelVector_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif
class StPidAmpChannel;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpChannel*, allocator<StPidAmpChannel*> > StPidAmpChannelVector;
typedef vector<StPidAmpChannel*, allocator<StPidAmpChannel*> >::iterator StPidAmpChannelIter;
typedef vector<StPidAmpChannel*, allocator<StPidAmpChannel*> >::const_iterator StPidAmpChannelConstIter;
#else
typedef vector<StPidAmpChannel*>          StPidAmpChannelVector;
typedef vector<StPidAmpChannel*>::iterator StPidAmpChannelIter;
typedef vector<StPidAmpChannel*>::const_iterator StPidAmpChannelConstIter;
#endif

#endif
