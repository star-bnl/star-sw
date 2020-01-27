/***************************************************************************
 *
 * $Id: StMuFilter.cxx,v 1.7 2020/01/27 21:28:31 genevb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuFilter.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StContainers.h"
#include "StEvent/StDedxPidTraits.h"
#include "TMath.h"
#define __MIN_HITS__     11
#define __MIN_HITS_TPC__ 11
#define __MIN_HITS_FTPC__ 5
#define __MIN_HITS_FTS__  3


ClassImp(StMuFilter)
StMuFilter::StMuFilter() 
{
  mMinHits 	= __MIN_HITS__;
  mMinTpcHits 	= __MIN_HITS_TPC__;
  mMinFTpcHits 	= __MIN_HITS_FTPC__;
  mMinFtsHits 	= __MIN_HITS_FTS__;
}


bool StMuFilter::accept( const StEvent*      e) { cout << "StMuFilter::accept( const StEvent* e) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StV0Vertex*   v) { cout << "StMuFilter::accept(const StV0Vertex* v) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StXiVertex*   x) { cout << "StMuFilter::accept(const StXiVertex* x) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StKinkVertex* k) { cout << "StMuFilter::accept(const StKinkVertex* k) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StV0MuDst*    v) { cout << "StMuFilter::accept(const StV0MuDst* v) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StXiMuDst*    x) { cout << "StMuFilter::accept(const StXiMuDst* x) not overwritten, returning true" << endl; return true;}
bool StMuFilter::accept( const StKinkMuDst*  k) { cout << "StMuFilter::accept(const StKinkMuDst* k) not overwritten, returning true" << endl; return true;}



bool StMuFilter::accept(const StTrack* track) {

  /** if the list of encoding methods is empty, then accept all methods
      else check for at least one good encoding method
  */
  bool method = false;
  if (mEncodedMethods.size()) {
    for ( UnsignedShortIterator iter = mEncodedMethods.begin(); iter!=mEncodedMethods.end(); iter++) {
      //      cout << *iter << " " << track->encodedMethod() << endl;
      if (track->encodedMethod() == *iter) method=true;
    }
    if (!method) return false;
  } 

  if ( !track->detectorInfo() ) return false;
  if ( TMath::Abs(track->flag())%100 == 11) return true;
  if ( TMath::Abs(track->flag())%100 == 12) return true;
  if ( track->detectorInfo()->numberOfPoints(kTpcId     )	<mMinTpcHits 	&&
#ifdef kFtsIdentifier
       track->detectorInfo()->numberOfPoints(kFtsId     )	<mMinFtsHits 	&& 
#endif
       track->detectorInfo()->numberOfPoints(kFtpcWestId)	<mMinFTpcHits 	&& 
       track->detectorInfo()->numberOfPoints(kFtpcEastId)	<mMinFTpcHits  
       ) return false;

  return true;
}


/***************************************************************************
 *
 * $Log: StMuFilter.cxx,v $
 * Revision 1.7  2020/01/27 21:28:31  genevb
 * Add short tracks toward ETOF
 *
 * Revision 1.6  2015/11/13 00:25:16  perev
 * Added changable constants and FTS
 *
 * Revision 1.5  2006/08/28 17:07:15  fisyak
 * Don't applay no. fit points cut for short tracks pointing to EEMC (+x11)
 *
 * Revision 1.4  2002/09/11 21:02:41  laue
 * added cut on track encoded method for ITTF
 *
 * Revision 1.3  2002/06/12 16:02:43  laue
 * Change of the number of hits cut, so that also FTPC tracks are written out.
 *
 * Revision 1.2  2002/05/04 23:56:30  laue
 * some documentation added
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
