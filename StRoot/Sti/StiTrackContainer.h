//StiTrackContainer.h
//M.L. Miller (Yale Software)
//05/01

/*!
  \class StiTrackContainer
  
  StiTrackContainer is meant to provide the interface between the StiTracker and the persistency model.
  That is, StiTrackContainer will be the only thing that StiTracker will have to know about and
  StiTrackContainer will interface to (most likely) StEvent, unless we decide to make StiTrackContainer
  persistent.
  <p>
  StiTrackContainer is polymorphic and can hold all forms of StiTrack objects. That includes in
  particular StiKalmanTrack and StiMcTrack.

  \author M.L. Miller (Yale Software)
*/

#ifndef StiTrackContainer_HH
#define StiTrackContainer_HH

#include <map>
using namespace std;

class StiTrack;

///Define the Less-Than operator for track ordering in the track container.
struct StiTrackLessThan
{
    bool operator()(const StiTrack* lhs, const StiTrack* rhs) const;
};

typedef map<StiTrack*, StiTrack*, StiTrackLessThan> TrackMap;
typedef TrackMap::value_type TrackMapValType;

class StiTrackContainer : public TrackMap
{
public:
    
    /// Add given track to the container
    void add(StiTrack * track);

    ///Preserve simple interface to add tracks
    void push_back(StiTrack*);
    
    StiTrackContainer();
    virtual ~StiTrackContainer();
    
};

inline void StiTrackContainer::add(StiTrack * track)
{
  insert(  TrackMapValType(track, track) );
}

inline void StiTrackContainer::push_back(StiTrack* track)
{
    insert(  TrackMapValType(track, track) );
}

#endif
