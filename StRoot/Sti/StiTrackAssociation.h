#ifndef StiTrackAssociation_H_INCLUDED
#define StiTrackAssociation_H_INCLUDED
#include "Sti/Base/Factory.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/AssociationQuality.h"
typedef map<StiTrack*,AssociationQuality*> TrackToAssociationQualityMapType;

class StiTrackAssociation : public TrackToAssociationQualityMapType
{
public: 
  StiTrackAssociation();
  virtual ~StiTrackAssociation();
  /// Determines whether the association is empty
  bool isEmpty() const;
  /// Get the number of associated tracks
  int getAssociatedCount() const;
  /// Get the best Association
  AssociationQuality* getBestAssociation() const;
  /// Get best associated track
  StiTrack * getBestAssociatedTrack() const;
  /// Add a new track
  /// Takes care of creating the necessary AssociationQuality object
  void addTrack(StiTrack*);
protected:
  Factory<AssociationQuality> * _associationQualityFactory;
};

#endif
