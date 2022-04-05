/*!
  \class StiTrackContainer
  
  StiTrackContainer is a container of track based on the STL vector class.
  <p>
  StiTrackContainer is polymorphic and can hold all forms of StiTrack objects. That includes in
  particular StiKalmanTrack.

  \author M.L. Miller (Yale Software)
  \author C.A. Pruneau(Wayne State University)
*/

#ifndef StiTrackContainer_HH
#define StiTrackContainer_HH
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include <vector>
using namespace std;

class StiTrack;
template<class Filtered>class Filter;

///Define the Less-Than operator for track ordering in the track container.
struct StiTrackLessThan
{
    bool operator()(const StiTrack* lhs, const StiTrack* rhs) const;
};

class StiTrackContainer : public vector<StiTrack*>, public Named, public Described
{
public:
    
    StiTrackContainer(const string & name, const string & description);
    virtual ~StiTrackContainer();  
    void add(StiTrack * track);
    int getTrackCount(Filter<StiTrack> * filter) const;
    void sort();
    
};


/// Add the given track to the container
inline void StiTrackContainer::add(StiTrack * track)
{
  push_back(track);
}

#endif
