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

class StiTrackContainer
{
public:
    typedef vector<StiTrack*> stitrackvec;

    friend class nobody;
    
    //Singleton access
    static StiTrackContainer* instance();
    static void kill();

    //Mimic STL interface
    void push_back(StiTrack*);
    void clear();
    void clearAndDestroy();
    unsigned int size() const;

    //Iterator bounds, etc
    stitrackvec::const_iterator begin() const;
    stitrackvec::const_iterator end() const;
    
    stitrackvec::const_reverse_iterator rbegin() const;
    stitrackvec::const_reverse_iterator rend() const;
    
protected:
    StiTrackContainer();
    virtual ~StiTrackContainer();

private:
    static StiTrackContainer* sinstance;
    stitrackvec mvec; 
};

#endif
