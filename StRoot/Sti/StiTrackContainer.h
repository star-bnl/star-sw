//StiTrackContainer.h
//M.L. Miller (Yale Software)
//05/01

/*
  
  StiTrackContainer is meant to provide the interface between the StiTracker and the persistency model.
  That is, StiTrackContainer will be the only thing that StiTracker will have to know about and
  StiTrackContainer will interface to (most likely) StEvent, unless we decide to make StiTrackContainer
  persistent.

  For now, we store pointers to StiTracks in a member vector ("has a vector") instead of publicly
  inheriting from vector ("is a vector").

  Implemented as a singleton
  
*/

#ifndef StiTrackContainer_HH
#define StiTrackContainer_HH

#include <vector>
using std::vector;

class StiTrack;

typedef vector<StiTrack*> stitrackvec;

class StiTrackContainer : public stitrackvec
{
public:

    friend class nobody;
    
    //Singleton access
    static StiTrackContainer* instance();
    static void kill();

protected:
    StiTrackContainer();
    virtual ~StiTrackContainer();

private:
    static StiTrackContainer* sinstance;
};

#endif
