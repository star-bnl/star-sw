//StFtpcTrackToStEvent.h
/*******************************************************************************
 *
 * $Id: StFtpcTrackToStEvent.hh,v 1.2 2004/05/07 14:39:39 oldi Exp $
 *
 * Author: Markus D. Oldenburg 
 * (changed version of StiStEventFiller by Manuel Calderon de la Barca Sanchez)
 *******************************************************************************
 *
 *
 **************************************************************************/
#ifndef StFtpcTrackToStEvent_HH
#define StFtpcTrackToStEvent_HH
#include <map>
using std::map;
#include "StFtpcPoint.hh"
#include "StFtpcTrack.hh"
#include "TObjArray.h"

class StEvent;
class StTrackNode;
class StTrackDetectorInfo;
class StTrack;
class StFtpcTrack;
class StFtpcPoint;
class StHelix;
class StHelixModel;
class StPhysicalHelixD;

/*! \class StFtpcTrackToStEvent
    StFtpcTrackToStEvent is a utilitity class meant to properly convert StFtpcTrack
    objects into StTrack (Global/Primary) objects and hang these on the StEvent
    Track-node.

    \author Markus D. Oldenburg
 */
class StFtpcTrackToStEvent
{

public:
  StFtpcTrackToStEvent();
  virtual ~StFtpcTrackToStEvent();
  StEvent* FillEvent(StEvent*, TObjArray*);
  StEvent* FillEventPrimaries(StEvent*, TObjArray*);
  void FillDetectorInfo(StTrackDetectorInfo* detInfo, StFtpcTrack* kTrack);
  void FillGeometry(StTrack* track, StFtpcTrack* kTrack, bool outer);
  void FillFitTraits(StTrack* track, StFtpcTrack* kTrack);
  void FillPidTraits(StTrack* track, StFtpcTrack* kTrack);
  void FilldEdxInfo(StTrack* track, StFtpcTrack* kTrack);
  void FillTrack(StTrack* track, StFtpcTrack* kTrack);
  void FillTopologyMap(StTrack* track, StFtpcTrack* kTrack);
  unsigned short EncodedStEventFitPoints(StFtpcTrack* kTrack); 
  float ImpactParameter(StFtpcTrack* kTrack);

private:
  StEvent* mEvent;
  TObjArray* mTrackStore;
  map<StFtpcTrack*, StTrackNode*> mTrkNodeMap;
  
  unsigned short mStiEncoded;
  //helix parameters
  StThreeVectorD *originD;
  StPhysicalHelixD * physicalHelix;

};

#endif
