/***************************************************************************
 *
 * $Id: StarMuFilter.cxx,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StarMuFilter.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StContainers.h"
#include "StEvent/StDedxPidTraits.h"

#define __MIN_HITS__ 11


ClassImp(StarMuFilter)

bool StarMuFilter::accept( const StEvent* e) { cout << "StarMuFilter::accept( const StEvent* e) not overwritten, returning true" << endl; return true;}
bool StarMuFilter::accept( const StV0Vertex* v) { cout << "StarMuFilter::accept(const StV0Vertex* v) not overwritten, returning true" << endl; return true;}
bool StarMuFilter::accept( const StXiVertex* x) { cout << "StarMuFilter::accept(const StXiVertex* x) not overwritten, returning true" << endl; return true;}
bool StarMuFilter::accept( const StKinkVertex* k) { cout << "StarMuFilter::accept(const StKinkVertex* k) not overwritten, returning true" << endl; return true;}



bool StarMuFilter::accept(const StTrack* track) {
  if ( !track->detectorInfo() ) return false;
  if ( track->detectorInfo()->numberOfPoints(kTpcId)<__MIN_HITS__ ) return false;

  return true;
}


/***************************************************************************
 *
 * $Log: StarMuFilter.cxx,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
