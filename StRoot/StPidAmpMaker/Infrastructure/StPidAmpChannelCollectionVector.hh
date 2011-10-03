/***************************************************************************
 *
 * $Id: StPidAmpChannelCollectionVector.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpChannelCollectionVector.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpChannelCollectionVector_hh
#define StPidAmpChannelCollectionVector_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif
class StPidAmpChannelCollection;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StPidAmpChannelCollection*, allocator<StPidAmpChannelCollection*> > StPidAmpChannelCollectionVector;
typedef vector<StPidAmpChannelCollection*, allocator<StPidAmpChannelCollection*> >::iterator StPidAmpChannelCollectionIter;
typedef vector<StPidAmpChannelCollection*, allocator<StPidAmpChannelCollection*> >::const_iterator StPidAmpChannelCollectionConstIter;
#else
typedef vector<StPidAmpChannelCollection*>          StPidAmpChannelCollectionVector;
typedef vector<StPidAmpChannelCollection*>::iterator StPidAmpChannelCollectionIter;
typedef vector<StPidAmpChannelCollection*>::const_iterator StPidAmpChannelCollectionConstIter;
#endif

#endif
