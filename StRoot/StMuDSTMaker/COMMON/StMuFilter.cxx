/***************************************************************************
 *
 * $Id: StMuFilter.cxx,v 1.2 2002/05/04 23:56:30 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuFilter.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StContainers.h"
#include "StEvent/StDedxPidTraits.h"

#define __MIN_HITS__ 11


ClassImp(StMuFilter)

bool StMuFilter::accept( const StEvent* e) { cout << "StMuFilter::accept( const StEvent* e) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StV0Vertex* v) { cout << "StMuFilter::accept(const StV0Vertex* v) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StXiVertex* x) { cout << "StMuFilter::accept(const StXiVertex* x) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StKinkVertex* k) { cout << "StMuFilter::accept(const StKinkVertex* k) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StV0MuDst* v) { cout << "StMuFilter::accept(const StV0MuDst* v) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StXiMuDst* x) { cout << "StMuFilter::accept(const StXiMuDst* x) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StKinkMuDst* k) { cout << "StMuFilter::accept(const StKinkMuDst* k) not overwritten, returning true" << endl; return true;}



bool StMuFilter::accept(const StTrack* track) {
  if ( !track->detectorInfo() ) return false;
  if ( track->detectorInfo()->numberOfPoints(kTpcId)<__MIN_HITS__ ) return false;

  return true;
}


/***************************************************************************
 *
 * $Log: StMuFilter.cxx,v $
 * Revision 1.2  2002/05/04 23:56:30  laue
 * some documentation added
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
