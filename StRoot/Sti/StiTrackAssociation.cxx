#include "Sti/StiTrackAssociation.h"

StiTrackAssociation::StiTrackAssociation()
{}

StiTrackAssociation::~StiTrackAssociation()
{}
  
/// Determines whether the association is empty
bool  StiTrackAssociation::isEmpty() const
{
  return size()==0;
}

/// Get the number of associated tracks
int  StiTrackAssociation::getAssociatedCount() const
{
  return size();
}

/// Get the best Association
AssociationQuality*  StiTrackAssociation::getBestAssociation() const
{
  TrackToAssociationQualityMapType::const_iterator iter;
  TrackToAssociationQualityMapType::const_iterator best=end();
  double quality;
  double bestQuality = 0;
  for (iter=begin();iter!=end();++iter)
    {
      quality = iter->second->getQuality();
      if (quality>bestQuality)
	{
	  best=iter;
	  bestQuality=quality;
	}
    }
  if (best!=end())
    return best->second;
  else
    return 0;
}

/// Get best associated track
StiTrack *  StiTrackAssociation::getBestAssociatedTrack() const
{
  AssociationQuality * aq = getBestAssociation();
  if (aq)
    return aq->getSecond();
  else
    return 0;
}

/// Add a new track
/// Takes care of creating the necessary AssociationQuality object
void  StiTrackAssociation::addTrack(StiTrack* track)
{
  AssociationQuality * associationQuality;
  TrackToAssociationQualityMapType::iterator iter = find(track);
  if (iter==end())
    {
      associationQuality = _associationQualityFactory->getInstance();
      insert(  TrackToAssociationQualityMapType::value_type(track,associationQuality) );
      associationQuality->incrementQuality();
    }
  else
    iter->second->incrementQuality();
}
