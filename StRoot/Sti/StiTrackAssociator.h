#ifndef StiTrackAssociator_H_INCLUDED
#define StiTrackAssociator_H_INCLUDED
#include "StiTrack.h"
#include "StiTrack.h"
#include "StiTrackContainer.h"
typedef map<StiTrack*,StiTrack*> TrackAssociator;

class StiTrackAssociator : public TrackAssociator
{
 public:

  StiTrackAssociator(Factory<Association> * associationFactory);
  virtual ~StiTrackAssociator();
  void reset();
  void build(StiTrackContainer * firstTrackContainer,
	     StiTrackContainer * secondTrackContainer);
  StiTrackToTrackMap * getEvalTrackToRefTrackMap();
  StiTrackToTrackMap * getRefTrackToEvalTrackMap();
  StiHitToHitMap     * getEvalHitToRefHitMap();
  StiHitToHitMap     * getRefHitToEvalHitMap();
  StiHitToTrackMap   * getRefHitToRefTrackMap();
  StiHitToTrackMap   * getEvalHitToEvalTrackMap();

 protected:
  Factory<Association> * _associationFactory;
  
  StiTrackToTrackMap * _evalTrackToRefTrackMap;
  StiTrackToTrackMap * _refTrackToEvalTrackMap;
  StiHitToHitMap     * _evalHitToRefHitMap;
  StiHitToHitMap     * _refHitToEvalHitMap;
  StiHitToTrackMap   * _refHitToRefTrackMap;
  StiHitToTrackMap   * _evalHitToEvalTrackMap;

};


#endif
