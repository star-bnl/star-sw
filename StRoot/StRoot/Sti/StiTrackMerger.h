#ifndef StiTrackMerger_HH
#define StiTrackMerger_HH

class StiTrackContainer;

/*! \class StiTrackMerger
  StiTrackMerger is a pure virtual class that defines the interface for a class that
  encapsulates a track-merging algorithm.  It's purpose is to
  combine track segments that belong to the same trajectory, but were initially reconstructed
  as seperate pieces.
  
  \author M.L. Miller (Yale Software)
  
  \note All information passed to and from an instance of StiTrackMerger is performed
  via the pointer to StiTrackContainer.

 */
class StiTrackMerger
{
public:
    ///One must provide a valid pointer to the track container.
    StiTrackMerger(StiTrackContainer*);
    virtual ~StiTrackMerger();
    ///Merge the tracks in the track container.
    virtual void mergeTracks() = 0;
protected:
    StiTrackMerger(); //This is not implemented
    StiTrackContainer* mTrackStore;
};

#endif
