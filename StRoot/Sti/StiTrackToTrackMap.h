#ifndef StiTrackToTrackMap_H_INCLUDED
#define StiTrackToTrackMap_H_INCLUDED
#include "Sti/Base/Factory.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiHitToHitMap.h"
#include "Sti/StiHitToTrackMap.h"
#include "Sti/StiTrackAssociation.h"

typedef map<StiTrack*,StiTrackAssociation*> TrackToTrackAssociationMapType;

class StiTrackToTrackMap : public TrackToTrackAssociationMapType
{
 public:

  StiTrackToTrackMap(Factory<StiTrackAssociation> * associationFactory);
  virtual ~StiTrackToTrackMap();
  void build(StiTrackContainer * firstTrackContainer,
	     StiHitToHitMap    * hitToHitMap,
	     StiHitToTrackMap  * hitToTrackMap);

 protected:
  Factory<StiTrackAssociation> * _associationFactory;
};


#endif
